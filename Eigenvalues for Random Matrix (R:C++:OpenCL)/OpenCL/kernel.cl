// Kernel block.
// Compute largest eigenvalues for 64 random realizations.


kernel void vector_add(global const int *A, global const int *B, global float *C) {
    int idd = get_global_id(0);
    int sqN=7, N=sqN*sqN, NN=N*N, degree=8, Power=1, i, j, k, l;
    for(l=0;l<degree;l++){
        Power=Power*2;
    }
    float mat[NN], mat1[NN], mat2[NN], temp;
    
    // I had to write my own fast random number generator that is compatible with OpenCL
    // Fill the matrix mat with random numbers
    for(l=0;l<NN;l++){
        mat[l]=0.635*((((B[idd]+l) * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1) % 100) - 50)/(100*sqN);
    }
    // 0.635 is used to prevent overflow
    
    // Make the matrix symmetric
    for(i=0;i<N;i++){
        for(j=0;j<N;j++){
            mat[i*N+j]=mat[j*N+i];
        }
    }
    // Copy mat
    for(l=0;l<NN;l++){
        mat1[l]=mat[l];
    }
    // Compute power of mat
    for(k=0;k<Power-1;k++){
        for(l=0;l<NN;l++){
            mat2[l]=mat[l];
        }
        for(i=0;i<N;i++){
            for(j=0;j<N;j++){
                temp=0;
                for(l=0;l<N;l++){
                    temp=temp+mat2[i+N*l]* mat1[l+N*j];
                }
                mat[i+N*j]=temp;
            }
        }
    }
    // Compute the trace
    temp=0;
    for(l=0;l<N;l++){
        temp=temp+mat[l+N*l];
    }
    // Take root of the trace
    for(l=0;l<degree;l++){
        temp=sqrt(temp);
    }
    // Load the result into C
    C[idd] =temp/0.635;
}