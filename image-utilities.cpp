#include <stdio.h>
#include <iostream>

#define SIZE 1024
int img1[2*SIZE][2*SIZE][3];
int img2[2*SIZE][2*SIZE][3];
int img3[2*SIZE][2*SIZE][3];

void initialize_imageCPP1()
{
	
    FILE* file = fopen ("input/Dream.txt", "r");
    int i = 0, j=0,k=0;
    int count = 0;

  	fscanf (file, "%d %d %d", &i, &j, &k);    
  	while (!feof (file))
    	{ 	
	         img1[count/SIZE][count%SIZE][0] = i;
	         img1[count/SIZE][count%SIZE][1] = j;
	         img1[count/SIZE][count%SIZE][2] = k;
		 count ++;
      		 fscanf (file, "%d %d %d", &i, &j, &k);          
    	}
  	fclose (file);    
}

void initialize_imageCPP2()
{

    FILE* file = fopen ("input/frame2.txt", "r");
    int i = 0, j=0,k=0;
    int count = 0;

        fscanf (file, "%d %d %d", &i, &j, &k);
        while (!feof (file))
        {
                 img2[count/SIZE][count%SIZE][0] = i;
                 img2[count/SIZE][count%SIZE][1] = j;
                 img2[count/SIZE][count%SIZE][2] = k;
                 count ++;
                 fscanf (file, "%d %d %d", &i, &j, &k);
        }
        fclose (file);
}

void initialize_imageCPP3()
{

    FILE* file = fopen ("input/mask2.txt", "r");
    int i = 0, j=0,k=0;
    int count = 0;

        fscanf (file, "%d %d %d", &i, &j, &k);
        while (!feof (file))
        {
                 img3[count/SIZE][count%SIZE][0] = i;
                 img3[count/SIZE][count%SIZE][1] = j;
                 img3[count/SIZE][count%SIZE][2] = k;
                 count ++;
                 fscanf (file, "%d %d %d", &i, &j, &k);
        }
        fclose (file);
}



extern "C" {
	        void  initialize_image()
		{
			initialize_imageCPP1();
			//initialize_imageCPP2();
			//initialize_imageCPP3();
		}
}

extern "C" 
{
	int readPixel1( int ri, int cj, int ch )
	{
		if(ri < SIZE && cj <SIZE)
		return img1[ri][cj][ch];
		else
		return 0;
	}
}
extern "C"
{
        int readPixel2( int ri, int cj, int ch )
        {
               
		if(ri < SIZE && cj <SIZE)
                return img2[ri][cj][ch];
		else
		return 0;
        }
}
extern "C"
{
        int readPixel3( int ri, int cj, int ch )
        {
                return img3[ri][cj][ch];
        }
}
/*int main()
{
	initialize_image();
	for(int i=0; i< SIZE ; i++)
		for(int j=0;j<SIZE ;j++)
			std::cout<<img2[i][j][0]<<img1[i][j][0]<<img3[i][j][0];
	return 0;
}*/
