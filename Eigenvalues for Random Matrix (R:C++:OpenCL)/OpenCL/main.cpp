#include <stdlib.h>
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif
#include <cmath>
#include<cstdlib>
#include<ctime>
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <string>
#include <chrono>

using namespace std;
using namespace std::chrono;

// A basis for the code is taken from http://www.thebigblob.com/getting-started-with-opencl-and-gpu-computing/

#define MAX_SOURCE_SIZE (0x100000)
    
    int main(void) {
        // Time measurement
        high_resolution_clock::time_point t1 = high_resolution_clock::now();
        
        ofstream outStream;
        outStream.open("/Users/a1/Desktop/resultsOpenCL.txt");
        // Create the two input vectors
        int i;
        const int LIST_SIZE = 64;
        int *A = (int*)malloc(sizeof(int)*LIST_SIZE);
        int *B = (int*)malloc(sizeof(int)*LIST_SIZE);
        // A is used just as an index
        for(i = 0; i < LIST_SIZE; i++) {
            A[i] = i;
        }
        // B is used as a random seed
        srand(time(0));
        for(i = 0; i < LIST_SIZE; i++) {
            B[i] = rand() % 200;
        }
        
        // Load the kernel source code into the array source_str
        FILE *fp;
        char *source_str;
        size_t source_size;
        
        fp = fopen("/Users/a1/Desktop/2/2/2/kernel.cl", "r");
        if (!fp) {
            fprintf(stderr, "Incorrect path to the kernel. Please change the path.\n");
            exit(1);
        }
        source_str = (char*)malloc(MAX_SOURCE_SIZE);
        source_size = fread( source_str, 1, MAX_SOURCE_SIZE, fp);
        fclose( fp );
        
        // Get platform and device information
        cl_platform_id platform_id = NULL;
        cl_device_id device_id = NULL;
        cl_uint ret_num_devices;
        cl_uint ret_num_platforms;
        cl_int ret = clGetPlatformIDs(1, &platform_id, &ret_num_platforms);
        ret = clGetDeviceIDs( platform_id, CL_DEVICE_TYPE_DEFAULT, 1,
                             &device_id, &ret_num_devices);
        
        // Create an OpenCL context
        cl_context context = clCreateContext( NULL, 1, &device_id, NULL, NULL, &ret);
        
        // Create a command queue
        cl_command_queue command_queue = clCreateCommandQueue(context, device_id, 0, &ret);
        
        // Create memory buffers on the device for each vector
        cl_mem a_mem_obj = clCreateBuffer(context, CL_MEM_READ_ONLY,
                                          LIST_SIZE * sizeof(int), NULL, &ret);
        cl_mem b_mem_obj = clCreateBuffer(context, CL_MEM_READ_ONLY,
                                          LIST_SIZE * sizeof(int), NULL, &ret);
        cl_mem c_mem_obj = clCreateBuffer(context, CL_MEM_WRITE_ONLY,
                                          LIST_SIZE * sizeof(float), NULL, &ret);
        
        // Copy the lists A and B to their respective memory buffers
        ret = clEnqueueWriteBuffer(command_queue, a_mem_obj, CL_TRUE, 0,
                                   LIST_SIZE * sizeof(int), A, 0, NULL, NULL);
        ret = clEnqueueWriteBuffer(command_queue, b_mem_obj, CL_TRUE, 0,
                                   LIST_SIZE * sizeof(int), B, 0, NULL, NULL);
        
        // Create a program from the kernel source
        cl_program program = clCreateProgramWithSource(context, 1,
                                                       (const char **)&source_str, (const size_t *)&source_size, &ret);
        
        // Build the program
        ret = clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);
        
        // Create the OpenCL kernel
        cl_kernel kernel = clCreateKernel(program, "vector_add", &ret);
        
        // Set the arguments of the kernel
        ret = clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&a_mem_obj);
        ret = clSetKernelArg(kernel, 1, sizeof(cl_mem), (void *)&b_mem_obj);
        ret = clSetKernelArg(kernel, 2, sizeof(cl_mem), (void *)&c_mem_obj);
        
        // Execute the OpenCL kernel on the list
        size_t global_item_size = LIST_SIZE; // Process the entire lists
        size_t local_item_size = 2; // Divide work items into groups of 2
        ret = clEnqueueNDRangeKernel(command_queue, kernel, 1, NULL,
                                     &global_item_size, &local_item_size, 0, NULL, NULL);
        
        // Read the memory buffer C on the device to the local variable C
        float *C = (float*)malloc(sizeof(float)*LIST_SIZE);
        ret = clEnqueueReadBuffer(command_queue, c_mem_obj, CL_TRUE, 0,
                                  LIST_SIZE * sizeof(float), C, 0, NULL, NULL);
        
        // Display the result to the screen
        for(i = 0; i < LIST_SIZE; i++)
        printf("%d -> %d -> %f\n", A[i], B[i], C[i]);
        
        
        // Put some data into a file

        high_resolution_clock::time_point t2 = high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::microseconds>( t2 - t1 ).count();
        outStream << "duration: " << duration << " microsec " << "matrix size: 49 power: 2^8=256\n";
        // Put the result into a file
        for(i = 0; i < LIST_SIZE; i++)
        outStream << C[i] << "\n";

        
        cout << duration;
        
        // Clean up
        outStream.close();
        ret = clFlush(command_queue);
        ret = clFinish(command_queue);
        ret = clReleaseKernel(kernel);
        ret = clReleaseProgram(program);
        ret = clReleaseMemObject(a_mem_obj);
        ret = clReleaseMemObject(b_mem_obj);
        ret = clReleaseMemObject(c_mem_obj);
        ret = clReleaseCommandQueue(command_queue);
        ret = clReleaseContext(context);
        free(A);
        free(B);
        free(C);
        

        return 0;
    }