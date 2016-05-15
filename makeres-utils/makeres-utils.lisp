;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2016 Gary Hollis
;;;;
;;;; This file is part of cl-ana.
;;;;
;;;; cl-ana is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; cl-ana is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with cl-ana.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis (me!) via email at
;;;; ghollisjr@gmail.com

(in-package :cl-ana.makeres-utils)

(defmacro deffitres
    (fit-id-prefix
     data-form
     fn-form
     &key
       fit-options
       draw-p
       (x-range '(cons "*" "*"))
       (y-range '(cons "*" "*"))
       flags
       force-p
       nparams
       (param-init-value 1d0)
       param-init-list)
  "Defines fit result, fit parameter and fit function targets for
fitting a function against data.  force-p non-NIL means defres is
used, whereas NIL means defres-uniq is used for defining the targets.
flags should be a list of identifiers appended to the created targets'
IDs.  param-inits can be a constant or a list of initial parameter
guesses.  draw-p NIL means don't draw the fit results, non-NIL means
draw in an interactive window.  Note that drawing only works for
single parameter data at the moment.  x-range and y-range apply to the
optional plot.  param-init-value can be used to set a constant default
parameter value, whereas param-init-list can be used to specify a list
of default parameter values. If fn-form is a simple lambda form, then
the number of parameters can be inferred, otherwise param-init-list
needs to be specified explicitly.  fit-options are supplied to fit.
Defines the following targets:

* (fit-id-prefix fit-results): fit results, parameters, and the rest from
the multiple fit values.
* (fit-id-prefix fit-params): fit parameters
* (fit-id-prefix fit): fit function with applied parameters
* (fit-id-prefix fit-plot): optional plot target"
  (let* ((defres-op (if force-p
                        'defres
                        'defres-uniq))
         (id-fit-results `(,fit-id-prefix fit-results ,@flags))
         (id-fit-params `(,fit-id-prefix fit-params ,@flags))
         (id-fit `(,fit-id-prefix fit ,@flags))
         (id-fit-plot `(,fit-id-prefix fit-plot ,@flags))
         (param-init-form
          (cond
            ;; Explicit case
            (param-init-list
             param-init-list)
            ;; Implicit case
            ((numberp nparams)
             `(list ,@(loop for i below nparams
                         collecting param-init-value)))
            ;; Error when neither works
            (t
             (error "Must supply explicit param init list with non-lambda fit function form")))))
    `(progn
       (,defres-op ,id-fit-results
           (rest
            (multiple-value-list
             (fit ,data-form ,fn-form
                  ,param-init-form
                  ,@fit-options))))
       (,defres-op ,id-fit-params
           (first (res ,id-fit-results)))
       (,defres-op ,id-fit
           (let ((params (res ,id-fit-params))
                 (fit-fn ,fn-form))
             (lambda (x)
               (funcall fit-fn params x))))
       ,@(when draw-p
               `((,defres-op ,id-fit-plot
                     (let* ((xr ,x-range)
                            (yr ,y-range)
                            (sampling-list
                             (if (and (numberp (car xr))
                                      (numberp (cdr xr)))
                                 (list :low (car xr)
                                       :high (cdr xr)
                                       :nsamples 1000)))
                            (data ,data-form)
                            (fn (res ,id-fit)))
                       (draw
                        (page (list
                               (plot2d (list
                                        (line data
                                              :style "points"
                                              :title "data"
                                              :color "red")
                                        (apply #'line
                                               fn
                                               :title "fit"
                                               :style "lines"
                                               :color "black"
                                               (when sampling-list
                                                 (list :sampling
                                                       sampling-list))))
                                       :x-range xr
                                       :y-range yr))))))))
       )))
