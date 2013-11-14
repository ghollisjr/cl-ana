#include <hdf5.h>
#include <iostream>

int main()
{
  const hsize_t RANK = 1;
  hsize_t dim[1] = {5};
  hsize_t maxdim[1] = {10};
  int data[5];
  hid_t cparms = H5Pcreate(H5P_DATASET_CREATE);
  hsize_t chunkdim[] = {1};
  H5Pset_chunk(cparms,1,chunkdim);
  hid_t file = H5Fcreate("test.h5",H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT);
  
  hid_t dataspace = H5Screate_simple(RANK,dim,maxdim);
  hid_t dataset = H5Dcreate1(file,"dataset",H5T_NATIVE_INT,dataspace,cparms);
  std::cout << dataset << std::endl;

  for(int i = 0; i < 5; ++i)
    data[i] = i*i;

  hid_t memspace = H5Screate_simple(RANK,dim,maxdim);

  H5Dwrite(dataset,H5T_NATIVE_INT,memspace,dataspace,H5P_DEFAULT,data);
  H5Dclose(dataset);
  H5Sclose(memspace);
  H5Sclose(dataspace);

  H5Fclose(file);
}
