#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <vector>
using namespace std;

#include <math.h>
#include <sys/types.h>
#include <GL/glut.h>
#include <GL/glu.h>
#include "FrameXform.h"
#include "WaveFrontOBJ.h"

#ifndef M_PI
#define M_PI 3.1415926535897932384626433832795
#endif

bool rotatemodel = false;
float rx, ry, rz, rsum;
float trans; float trans_state = 0; float trans_state_backup = 0;
float oldmouselocax;float oldmouselocay;
float spacestate = 1;
float cx, cy, cz, csum;
float selectionMode = 0;
float objectcolor;
float backbuffer;
float select_nothing = 0, camera_trans_state;
float zoomfactor = 45;

// 'cameras' stores infomation of 5 cameras.
double cameras[5][9] = 
{
	{28,18,28, 0,2,0, 0,1,0},   
	{28,18,-28, 0,2,0, 0,1,0}, 
	{-28,18,28, 0,2,0, 0,1,0}, 
	{-12,12,0, 0,2,0, 0,1,0},  
	{0,100,0,  0,0,0, 1,0,0}
};
int cameraCount = sizeof( cameras ) / sizeof( cameras[0] );

int cameraIndex, camID;
vector<FrameXform> wld2cam, cam2wld; 
WaveFrontOBJ* cam;

// Variables for 'cow' object.
FrameXform cow2wld;
WaveFrontOBJ* cow;
int cowID;

// Variables for 'beethovan' object.
FrameXform bet2wld;
WaveFrontOBJ* bet;
int betID;

// Variables for 'beethovan' object.
FrameXform luxo2wld;
int luxoID;
FrameXform luxobase2wld, luxobody2wld, luxoneck2wld, luxohead2wld;
FrameXform wld2luxobase, wld2luxobody, wld2luxoneck, wld2luxohead;
int luxobaseID,  luxobodyID,  luxoneckID,  luxoheadID ;
int luxobase, luxobody, luxoneck;
int oldluxohead,oldluxoneck,oldluxobody;

FrameXform* currentobject = &cow2wld;
FrameXform luxoheadtrans,luxonecktrans,luxobodytrans;

unsigned floorTexID;
int frame = 0;
int width, height;
int selectMode, oldX, oldY;


void drawFrame(float len);

//------------------------------------------------------------------------------
void munge( int x, double& r, double& g, double& b)
{
	r = (x & 255)/double(255);
	g = ((x >> 8) & 255)/double(255);
	b = ((x >> 16) & 255)/double(255);
}

//------------------------------------------------------------------------------
int unmunge( double r, double g, double b)
{
	return (int(r) + (int(g) << 8) + (int(b) << 16));
}

//------------------------------------------------------------------------------
void setCamera()
{
	int i;
	if (frame == 0)
	{
		// intialize camera model.
		cam = new WaveFrontOBJ("camera.obj");	// Read information of camera from camera.obj.
		camID = glGenLists(1);					// Create display list of the camera.
		glNewList(camID, GL_COMPILE);			// Begin compiling the display list using camID.
		cam->Draw();							// Draw the camera. you can do this job again through camID..
		glEndList();							// Terminate compiling the display list.

		// initialize camera frame transforms.
		for (i=0; i < cameraCount; i++ )
		{
			double* c = cameras[i];											// 'c' points the coordinate of i-th camera.
			wld2cam.push_back(FrameXform());								// Insert {0} matrix to wld2cam vector.
			glPushMatrix();													// Push the current matrix of GL into stack.
			glLoadIdentity();												// Set the GL matrix Identity matrix.
			gluLookAt(c[0],c[1],c[2], c[3],c[4],c[5], c[6],c[7],c[8]);		// Setting the coordinate of camera.
			glGetDoublev( GL_MODELVIEW_MATRIX, wld2cam[i].matrix() );		// Read the world-to-camera matrix computed by gluLookAt.
			glPopMatrix();													// Transfer the matrix that was pushed the stack to GL.
			cam2wld.push_back(wld2cam[i].inverse());						// Get the camera-to-world matrix.
		}
		cameraIndex = 0;
	}

	// set viewing transformation.
	glLoadMatrixd(wld2cam[cameraIndex].matrix());

	// draw other cameras.
	for (i=0; i < (int)wld2cam.size(); i++ )
	{
		if (i != cameraIndex)
		{
			glPushMatrix();												// Push the current matrix on GL to stack. The matrix is wld2cam[cameraIndex].matrix().
			glMultMatrixd(cam2wld[i].matrix());							// Multiply the matrix to draw i-th camera.
			if (selectMode == 0)										// selectMode == 1 means backbuffer mode.
			{
				drawFrame(5);											// Draw x, y, and z axis.
				float frontColor[] = {0.2, 0.2, 0.2, 1.0};
				glEnable(GL_LIGHTING);									
				glMaterialfv(GL_FRONT, GL_AMBIENT, frontColor);			// Set ambient property frontColor.
				glMaterialfv(GL_FRONT, GL_DIFFUSE, frontColor);			// Set diffuse property frontColor.
			}
			else
			{
				double r,g,b;				
				glDisable(GL_LIGHTING);									// Disable lighting in backbuffer mode.
				munge(i+1, r,g,b);										// Match the corresponding (i+1)th color to r, g, b. You can change the color of camera on backbuffer.
				glColor3f(r, g, b);										// Set r, g, b the color of camera.
			}
			glScaled(0.5,0.5,0.5);										// Reduce camera size by 1/2.
			glTranslated(1.1,1.1,0.0);									// Translate it (1.1, 1.1, 0.0).
			glCallList(camID);											// Re-draw using display list from camID. 
			glPopMatrix();												// Call the matrix on stack. wld2cam[cameraIndex].matrix() in here.
		}
	}
}

/*********************************************************************************
* Draw x, y, z axis of current frame on screen.
* x, y, and z are corresponded Red, Green, and Blue, resp.
**********************************************************************************/
void drawFrame(float len)
{
	glDisable(GL_LIGHTING);		// Lighting is not needed for drawing axis.
	glBegin(GL_LINES);			// Start drawing lines.
	glColor3d(1,0,0);			// color of x-axis is red.
	glVertex3d(0,0,0);			
	glVertex3d(len,0,0);		// Draw line(x-axis) from (0,0,0) to (len, 0, 0). 
	glColor3d(0,1,0);			// color of y-axis is green.
	glVertex3d(0,0,0);			
	glVertex3d(0,len,0);		// Draw line(y-axis) from (0,0,0) to (0, len, 0).
	glColor3d(0,0,1);			// color of z-axis is  blue.
	glVertex3d(0,0,0);
	glVertex3d(0,0,len);		// Draw line(z-axis) from (0,0,0) - (0, 0, len).
	glEnd();					// End drawing lines.
}

/*********************************************************************************
* Draw 'cow' object.
**********************************************************************************/
void drawCow()
{  
	if (frame == 0)
	{
		// Initialization part.

		// Read information from cow.obj.
		cow = new WaveFrontOBJ( "cow.obj" );

		// Make display list. After this, you can draw cow using 'cowID'.
		cowID = glGenLists(1);				// Create display lists
		glNewList(cowID, GL_COMPILE);		// Begin compiling the display list using cowID
		cow->Draw();						// Draw the cow on display list.
		glEndList();						// Terminate compiling the display list. Now, you can draw cow using 'cowID'.
		glPushMatrix();						// Push the current matrix of GL into stack.
		glLoadIdentity();					// Set the GL matrix Identity matrix.
		glTranslated(0,-cow->bbmin.y,-8);	// Set the location of cow.
		glRotated(-90, 0, 1, 0);			// Set the direction of cow. These information are stored in the matrix of GL.
		glGetDoublev(GL_MODELVIEW_MATRIX, cow2wld.matrix());	// Read the modelview matrix about location and direction set above, and store it in cow2wld matrix.
		glPopMatrix();						// Pop the matrix on stack to GL.
	}

	glPushMatrix();		// Push the current matrix of GL into stack. This is because the matrix of GL will be change while drawing cow.

	// The information about location of cow to be drawn is stored in cow2wld matrix.
	// (Project2 hint) If you change the value of the cow2wld matrix or the current matrix, cow would rotate or move.
	glMultMatrixd(cow2wld.matrix());

	if (selectMode == 0)									// selectMode == 1 means backbuffer mode.
	{
		drawFrame(5);										// Draw x, y, and z axis.
		float frontColor[] = {0.8, 0.2, 0.9, 1.0};
		glEnable(GL_LIGHTING);
		glMaterialfv(GL_FRONT, GL_AMBIENT, frontColor);		// Set ambient property frontColor.
		glMaterialfv(GL_FRONT, GL_DIFFUSE, frontColor);		// Set diffuse property frontColor.
	}
	else
	{
		double r,g,b;  
		glDisable(GL_LIGHTING);								// Disable lighting in backbuffer mode.
		munge(255, r,g,b );									// Match the corresponding constant color to r, g, b. You can change the color of camera on backbuffer
		glColor3d(r, g, b);									
	}
	glCallList(cowID);		// Draw cow. 
	glPopMatrix();			// Pop the matrix in stack to GL. Change it the matrix before drawing cow.
}

/*********************************************************************************
* Draw 'beethovan' object.
**********************************************************************************/
void drawBet()
{  
	if (frame == 0)
	{
		// Initialization part.

		// Read information from beethovan.obj.
		bet = new WaveFrontOBJ( "beethovan.obj" );

		// Make display list. After this, you can draw beethovan using 'betID'.
		betID = glGenLists(1);
		glNewList(betID, GL_COMPILE);
		bet->Draw();
		glEndList();
		glPushMatrix();
		glLoadIdentity();
		glTranslated(0,-bet->bbmin.y,8);
		glRotated(180, 0, 1, 0);
		glGetDoublev(GL_MODELVIEW_MATRIX, bet2wld.matrix());
		glPopMatrix();
	}

	glPushMatrix();
	glMultMatrixd(bet2wld.matrix());
	if (selectMode == 0)
	{
		drawFrame(8);
		float frontColor[] = {0.8, 0.3, 0.1, 1.0};
		glEnable(GL_LIGHTING);
		glMaterialfv(GL_FRONT, GL_AMBIENT, frontColor);
		glMaterialfv(GL_FRONT, GL_DIFFUSE, frontColor);
	}
	else
	{	
		double r,g,b;  
		glDisable(GL_LIGHTING);
		munge(255*256, r,g,b );
		glColor3d(r, g, b);    
	}
	glCallList(betID);
	glPopMatrix();
}

//==============================================================================
class Vertexlx {
//==============================================================================
public:
  double x, y, z;

  Vertexlx( double x=0, double y=0, double z=0 ) {
    setCoordinates( x, y, z );
  }
  void setCoordinates( double xval, double yval, double zval ) {
    x = xval; y = yval; z = zval;
  }
};
//==============================================================================
class Triangle
//==============================================================================
{
public:
  int v0, v1, v2;

  Triangle( int v0=-1, int v1=-1, int v2=-1 ) : v0(v0), v1(v1), v2(v2) {}
};

//==============================================================================
class Lamp
//==============================================================================
{
public:

  enum {
    HEAD = 0,
    NECK = 1,
    BODY = 2,
    BASE = 3
  };

  vector<Vertexlx> headVert;
  vector<Triangle> headTri;
  vector<Vertexlx> neckVert;
  vector<Triangle> neckTri;
  vector<Vertexlx> bodyVert;
  vector<Triangle> bodyTri;
  vector<Vertexlx> baseVert;
  vector<Triangle> baseTri;
  
  vector<Vertexlx> nullVert;
  vector<Triangle> nullTri;

  Lamp( ) {
    int i;
    float x, y;
    int nFaces;
    int v, t;

      // create head
      nFaces = 16;
      headVert.resize(3*nFaces+1);
      headTri.resize(5*nFaces);
      headVert[0] = Vertexlx(1.0f, 0.0f,  2.0f);
      t = 0;
      v = 1;
      for (i = 0; i < nFaces; i++) {
          x = (float) cos(2.0*M_PI*i/nFaces);
          y = (float) sin(2.0*M_PI*i/nFaces);
          headVert[v++] = Vertexlx(x+1.0, y,  2.0f);
          headVert[v++] = Vertexlx(x+1.0, y, -1.0f);
          headVert[v++] = Vertexlx(3*x+1.0, 3*y, -4.0f);
          if (i > 0) {
              headTri[t++] = Triangle(0, 3*i-2, 3*i+1);
              headTri[t++] = Triangle(3*i-2, 3*i-1, 3*i+1);
              headTri[t++] = Triangle(3*i-1, 3*i+2, 3*i+1);
              headTri[t++] = Triangle(3*i-1, 3*i, 3*i+2);
              headTri[t++] = Triangle(3*i, 3*i+3, 3*i+2);
          }
      }
      i = nFaces;
      headTri[t++] = Triangle(0, 3*i-2, 1);
      headTri[t++] = Triangle(3*i-2, 3*i-1, 1);
      headTri[t++] = Triangle(3*i-1, 2, 1);
      headTri[t++] = Triangle(3*i-1, 3*i, 2);
      headTri[t++] = Triangle(3*i, 3, 2);

      // create neck
      neckVert.resize(8+8+12);
      neckTri.resize(12+12+4);
      v = 0;
      t = 0;
      box(v,t, neckVert, neckTri, 1.0f, 11.0f, -0.15f, 0.15f, 0.2f, 0.5f);
      box(v,t, neckVert, neckTri, 1.0f, 11.0f, -0.15f, 0.15f, -0.5f, -0.2f);
      neckVert[v++] = Vertexlx(0.0f, -0.16f, 0.0f);
      neckVert[v++] = Vertexlx(1.0f, -0.16f, -0.5f);
      neckVert[v++] = Vertexlx(1.0f, -0.16f, 0.5f);
      neckTri[t++] = Triangle(v-3, v-2, v-1);
      neckVert[v++] = Vertexlx(0.0f, 0.16f, 0.0f);
      neckVert[v++] = Vertexlx(1.0f, 0.16f, 0.5f);
      neckVert[v++] = Vertexlx(1.0f, 0.16f, -0.5f);
      neckTri[t++] = Triangle(v-3, v-2, v-1);
      neckVert[v++] = Vertexlx(12.0f, -0.16f, 0.0f);
      neckVert[v++] = Vertexlx(11.0f, -0.16f, 0.5f);
      neckVert[v++] = Vertexlx(11.0f, -0.16f, -0.5f);
      neckTri[t++] = Triangle(v-3, v-2, v-1);
      neckVert[v++] = Vertexlx(12.0f, 0.16f, 0.0f);
      neckVert[v++] = Vertexlx(11.0f, 0.16f, -0.5f);
      neckVert[v++] = Vertexlx(11.0f, 0.16f, 0.5f);
      neckTri[t++] = Triangle(v-3, v-2, v-1);

      // create body
      bodyVert.resize(8+8+12);
      bodyTri.resize(12+12+4);
      v = 0;
      t = 0;
      box(v,t,bodyVert, bodyTri, 1.0f, 11.0f, -0.15f, 0.15f, 0.2f, 0.5f);
      box(v,t,bodyVert, bodyTri, 1.0f, 11.0f, -0.15f, 0.15f, -0.5f, -0.2f);
      bodyVert[v++] = Vertexlx(0.0f, -0.16f, 0.0f);
      bodyVert[v++] = Vertexlx(1.0f, -0.16f, -0.5f);
      bodyVert[v++] = Vertexlx(1.0f, -0.16f, 0.5f);
      bodyTri[t++] = Triangle(v-3, v-2, v-1);
      bodyVert[v++] = Vertexlx(0.0f, 0.16f, 0.0f);
      bodyVert[v++] = Vertexlx(1.0f, 0.16f, 0.5f);
      bodyVert[v++] = Vertexlx(1.0f, 0.16f, -0.5f);
      bodyTri[t++] = Triangle(v-3, v-2, v-1);
      bodyVert[v++] = Vertexlx(12.0f, -0.16f, 0.0f);
      bodyVert[v++] = Vertexlx(11.0f, -0.16f, 0.5f);
      bodyVert[v++] = Vertexlx(11.0f, -0.16f, -0.5f);
      bodyTri[t++] = Triangle(v-3, v-2, v-1);
      bodyVert[v++] = Vertexlx(12.0f, 0.16f, 0.0f);
      bodyVert[v++] = Vertexlx(11.0f, 0.16f, -0.5f);
      bodyVert[v++] = Vertexlx(11.0f, 0.16f, 0.5f);
      bodyTri[t++] = Triangle(v-3, v-2, v-1);

      //create base
      baseVert.resize(2*nFaces+2+6);
      baseTri.resize(4*nFaces+2);
      int q = 2*nFaces+1;
      baseVert[0] = Vertexlx(0.0f, 0.0f,  1.0f);
      baseVert[q] = Vertexlx(0.0f, 0.0f,  0.0f);
      t = 0;
      v = 1;
      for (i = 0; i < nFaces; i++) {
          x = (float) cos(2.0*M_PI*i/nFaces);
          y = (float) sin(2.0*M_PI*i/nFaces);
          baseVert[v++] = Vertexlx(4*x, 4*y, 0.75f);
          baseVert[v++] = Vertexlx(4*x, 4*y, 0.0f);
          if (i > 0) {
              baseTri[t++] = Triangle(0, 2*i-1, 2*i+1);
              baseTri[t++] = Triangle(2*i-1, 2*i, 2*i+2);
              baseTri[t++] = Triangle(2*i-1, 2*i+2, 2*i+1);
              baseTri[t++] = Triangle(2*i, q, 2*i+2);
          }
      }
      i = nFaces;
      v++;
      baseTri[t++] = Triangle(0, 2*i-1, 1);
      baseTri[t++] = Triangle(2*i-1, 2*i, 2);
      baseTri[t++] = Triangle(2*i-1, 2, 1);
      baseTri[t++] = Triangle(2*i, q, 2);

      baseVert[v++] = Vertexlx(-1.0f, -0.16f, 0.75f);
      baseVert[v++] = Vertexlx(1.0f, -0.16f, 0.75f);
      baseVert[v++] = Vertexlx(0.0f, -0.16f, 2.5f);
      baseTri[t++] = Triangle(v-3, v-2, v-1);

      baseVert[v++] = Vertexlx(1.0f, 0.16f, 0.75f);
      baseVert[v++] = Vertexlx(-1.0f, 0.16f, 0.75f);
      baseVert[v++] = Vertexlx(0.0f, 0.16f, 2.5f);
      baseTri[t++] = Triangle(v-3, v-2, v-1);
  }

  void box(int& v, int& t, vector<Vertexlx>& vList, vector<Triangle>& tList,
      float xmin, float xmax,
      float ymin, float ymax,
      float zmin, float zmax) {
      // 8 verts and 12 tris
      int b = v;
      vList[v++] = Vertexlx(xmin, ymin, zmin);
      vList[v++] = Vertexlx(xmax, ymin, zmin);
      vList[v++] = Vertexlx(xmin, ymax, zmin);
      vList[v++] = Vertexlx(xmax, ymax, zmin);
      vList[v++] = Vertexlx(xmin, ymin, zmax);
      vList[v++] = Vertexlx(xmax, ymin, zmax);
      vList[v++] = Vertexlx(xmin, ymax, zmax);
      vList[v++] = Vertexlx(xmax, ymax, zmax);
      tList[t++] = Triangle(b+0, b+2, b+3);
      tList[t++] = Triangle(b+0, b+3, b+1);
      tList[t++] = Triangle(b+0, b+6, b+2);
      tList[t++] = Triangle(b+0, b+4, b+6);
      tList[t++] = Triangle(b+0, b+1, b+5);
      tList[t++] = Triangle(b+0, b+5, b+4);
      tList[t++] = Triangle(b+7, b+3, b+2);
      tList[t++] = Triangle(b+7, b+2, b+6);
      tList[t++] = Triangle(b+7, b+5, b+1);
      tList[t++] = Triangle(b+7, b+1, b+3);
      tList[t++] = Triangle(b+7, b+6, b+4);
      tList[t++] = Triangle(b+7, b+4, b+5);
  }

   int getElements() {
      return 4;
  }

   vector<Vertexlx>& getVertices(int element) {
      if (element == HEAD) return headVert;
      if (element == NECK) return neckVert;
      if (element == BODY) return bodyVert;
      if (element == BASE) return baseVert;
      return nullVert;
      
  }

   vector<Triangle> getTriangles(int element) {
      if (element == HEAD) return headTri;
      if (element == NECK) return neckTri;
      if (element == BODY) return bodyTri;
      if (element == BASE) return baseTri;
      return nullTri;
  }

   void Draw(int element) {
      vector<Vertexlx>& vert = getVertices(element);
      vector<Triangle>& tri = getTriangles(element);
      int v;

      for (int i = 0; i < (int)tri.size(); i++) {
          glBegin(GL_POLYGON);
          v = tri[i].v0;
          glVertex3d(vert[v].x, vert[v].y, vert[v].z);
          v = tri[i].v1;
          glVertex3d(vert[v].x, vert[v].y, vert[v].z);
          v = tri[i].v2;
          glVertex3d(vert[v].x, vert[v].y, vert[v].z);
          glEnd();
      }
  }
};

void drawLuxobase()
{  
	if (frame == 0)
	{
		luxobaseID = glGenLists(1);
		glNewList(luxobaseID, GL_COMPILE);
		Lamp mom;
		mom.Draw(Lamp::BASE);
		glEndList();
		glPushMatrix();
		glLoadIdentity();
		glTranslated(9,0,0);
		glRotated(-90,1,0,0);
		glScaled(0.5,0.5,0.5);
		glGetDoublev(GL_MODELVIEW_MATRIX, luxobase2wld.matrix());
		glPopMatrix();
	}

	if (luxobase)
	{
		glPushMatrix();
		glLoadIdentity();
		glMultMatrixd(luxobase2wld.matrix());
		glMultMatrixd(luxobodytrans.matrix());
		glTranslated(0,0,2.5);                  // body-to-base transform
		glRotated(-30, 0, 1, 0);				// rotate body at base pivot
		glGetDoublev(GL_MODELVIEW_MATRIX, luxobody2wld.matrix());
		glPopMatrix();
	}


	glPushMatrix();
	glMultMatrixd(luxobase2wld.matrix());
	if (selectMode == 0)
	{
		drawFrame(8);
		float frontColor[] = {0, 0, 1, 1.0};
		glEnable(GL_LIGHTING);
		glMaterialfv(GL_FRONT, GL_AMBIENT, frontColor);
		glMaterialfv(GL_FRONT, GL_DIFFUSE, frontColor);
	}
	else
	{	
		double r,g,b;  
		glDisable(GL_LIGHTING);
		munge(255*256*256, r,g,b );
		glColor3d(r, g, b);    
	}
	glCallList(luxobaseID);
	glPopMatrix();
}
void drawLuxobody()
{  
	if (frame == 0)
	{
		luxobodyID = glGenLists(1);
		glNewList(luxobodyID, GL_COMPILE);
		Lamp mom;
		mom.Draw(Lamp::BODY);
		glEndList();
		glPushMatrix();
		glLoadIdentity();
		glMultMatrixd(luxobase2wld.matrix());

		glTranslated(0,0,2.5);                  // body-to-base transform
		glRotated(-30, 0, 1, 0);				// rotate body at base pivot
		
		glGetDoublev(GL_MODELVIEW_MATRIX, luxobody2wld.matrix());
		glPopMatrix();

		glPushMatrix();
		glLoadIdentity();
		glGetDoublev(GL_MODELVIEW_MATRIX, luxobodytrans.matrix());
		glPopMatrix();
	}

	if (oldluxobody)
	{
		glPushMatrix();
		glLoadIdentity();
		glMultMatrixd((luxobase2wld.inverse()).matrix());
		glMultMatrixd(luxobody2wld.matrix());
		glTranslated(0,0,-2.5);                  
		glRotated(30, 0, 1, 0);  
		glGetDoublev(GL_MODELVIEW_MATRIX, luxobodytrans.matrix());
		glPopMatrix();
	}


	if (luxobody)
	{
		glPushMatrix();
		glLoadIdentity();
		glMultMatrixd(luxobody2wld.matrix());
		glMultMatrixd(luxonecktrans.matrix());
		glTranslated(12,0,0);                   // neck-to-body transform
		glRotated(-115, 0, 1, 0);	
		glGetDoublev(GL_MODELVIEW_MATRIX, luxoneck2wld.matrix());
		glPopMatrix();
	}

	glPushMatrix();
	glMultMatrixd(luxobody2wld.matrix());
	if (selectMode == 0)
	{
		float frontColor[] = {0, 0, 1, 1.0};
		glEnable(GL_LIGHTING);
		glMaterialfv(GL_FRONT, GL_AMBIENT, frontColor);
		glMaterialfv(GL_FRONT, GL_DIFFUSE, frontColor);
	}
	else
	{	
		double r,g,b;  
		glDisable(GL_LIGHTING);
		munge(64+64*256+255*256*256, r,g,b );
		glColor3d(r, g, b);    
	}
	glCallList(luxobodyID);
	glPopMatrix();
}

void drawLuxoneck()
{  
	if (frame == 0)
	{
		luxoneckID = glGenLists(1);
		glNewList(luxoneckID, GL_COMPILE);
		Lamp mom;
					// rotate neck at body pivot

		mom.Draw(Lamp::NECK);

		glEndList();
		glPushMatrix();
		glLoadIdentity();

		glMultMatrixd(luxobody2wld.matrix());
		
		glTranslated(12,0,0);                   // neck-to-body transform
		glRotated(-115, 0, 1, 0);	
		glGetDoublev(GL_MODELVIEW_MATRIX, luxoneck2wld.matrix());
		glPopMatrix();

		glPushMatrix();
		glLoadIdentity();
		glGetDoublev(GL_MODELVIEW_MATRIX, luxonecktrans.matrix());
		glPopMatrix();
	}

	if (oldluxoneck)
	{
		glPushMatrix();
		glLoadIdentity();
		glMultMatrixd((luxobody2wld.inverse()).matrix());
		glMultMatrixd(luxoneck2wld.matrix());
		glTranslated(-12,0,0);                  
		glRotated(115, 1, 0, 0);  
		glGetDoublev(GL_MODELVIEW_MATRIX, luxonecktrans.matrix());
		glPopMatrix();
	}

	if (luxoneck)
	{
		glPushMatrix();
		glLoadIdentity();
		glMultMatrixd(luxoneck2wld.matrix());
		glMultMatrixd(luxoheadtrans.matrix());
		glTranslated(12,0,0);                  
		glRotated(180, 1, 0, 0);
		glGetDoublev(GL_MODELVIEW_MATRIX, luxohead2wld.matrix());
		glPopMatrix();
	}

	glPushMatrix();
	glMultMatrixd(luxoneck2wld.matrix());
	if (selectMode == 0)
	{
		float frontColor[] = {0, 0, 1, 1.0};
		glEnable(GL_LIGHTING);
		glMaterialfv(GL_FRONT, GL_AMBIENT, frontColor);
		glMaterialfv(GL_FRONT, GL_DIFFUSE, frontColor);
	}
	else
	{	
		double r,g,b;  
		glDisable(GL_LIGHTING);
		munge(128+128*256+255*256*256, r,g,b );
		glColor3d(r, g, b);    
	}
	glCallList(luxoneckID);
	glPopMatrix();
}

void drawLuxohead()
{  
	if (frame == 0)
	{
		luxoheadID = glGenLists(1);
		glNewList(luxoheadID, GL_COMPILE);
		Lamp mom;
		mom.Draw(Lamp::HEAD);
		glEndList();

		glPushMatrix();
		glLoadIdentity();
		glMultMatrixd(luxoneck2wld.matrix());
		glTranslated(12,0,0);                  
		glRotated(180, 1, 0, 0);              
		glGetDoublev(GL_MODELVIEW_MATRIX, luxohead2wld.matrix());
		glPopMatrix();

		glPushMatrix();
		glLoadIdentity();
		glGetDoublev(GL_MODELVIEW_MATRIX, luxoheadtrans.matrix());
		glPopMatrix();
	}

	if (oldluxohead)
	{
		glPushMatrix();
		glLoadIdentity();
		glMultMatrixd((luxoneck2wld.inverse()).matrix());
		glMultMatrixd(luxohead2wld.matrix());
		glTranslated(-12,0,0);                  
		glRotated(-180, 1, 0, 0);  
		glGetDoublev(GL_MODELVIEW_MATRIX, luxoheadtrans.matrix());
		glPopMatrix();
	}

	glPushMatrix();
	glMultMatrixd(luxohead2wld.matrix());
	if (selectMode == 0)
	{
		float frontColor[] = {0, 0, 1, 1.0};
		glEnable(GL_LIGHTING);
		glMaterialfv(GL_FRONT, GL_AMBIENT, frontColor);
		glMaterialfv(GL_FRONT, GL_DIFFUSE, frontColor);
	}
	else
	{	
		double r,g,b;  
		glDisable(GL_LIGHTING);
		munge(192+192*256+255*256*256, r,g,b );
		glColor3d(r, g, b);    
	}
	glCallList(luxoheadID);
	glPopMatrix();
}



/*********************************************************************************
* Draw floor on 3D plane.
**********************************************************************************/
void drawFloor()
{  
	if (frame == 0)
	{
		// Initialization part.
		// After making checker-patterned texture, use this repetitively.

		// Insert color into checker[] according to checker pattern.
		const int size = 8;
		unsigned char checker[size*size*3];
		for( int i=0; i < size*size; i++ )
		{
			if (((i/size) ^ i) & 1)
			{
				checker[3*i+0] = 200;
				checker[3*i+1] = 32;
				checker[3*i+2] = 32;
			}
			else
			{
				checker[3*i+0] = 200;
				checker[3*i+1] = 200;
				checker[3*i+2] = 32;
			}
		}

		// Make texture which is accessible through floorTexID. 
		glGenTextures( 1, &floorTexID );				
		glBindTexture(GL_TEXTURE_2D, floorTexID);		
		glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
		glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
		glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
		glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
		glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
		glTexImage2D(GL_TEXTURE_2D, 0, 3, size, size, 0, GL_RGB, GL_UNSIGNED_BYTE, checker);
	}

	glDisable(GL_LIGHTING);

	// Set background color.
	if (selectMode == 0)
		glColor3d(0.35, .2, 0.1);
	else
	{
		// In backbuffer mode.
		double r,g,b;
		munge(34, r,g,b);
		glColor3d(r, g, b);
	}

	// Draw background rectangle. 
	glBegin(GL_POLYGON);
	glVertex3f( 2000,-0.2, 2000);
	glVertex3f( 2000,-0.2,-2000);
	glVertex3f(-2000,-0.2,-2000);
	glVertex3f(-2000,-0.2, 2000);
	glEnd();

	
	// Set color of the floor.
	if (selectMode == 0)
	{
		// Assign checker-patterned texture.
		glEnable(GL_TEXTURE_2D);
		glBindTexture(GL_TEXTURE_2D, floorTexID );
	}
	else
	{
		// Assign color on backbuffer mode.
		double r,g,b;
		munge(24+24*256+24*256*256, r,g,b);
		glColor3d(r, g, b);
	}

	// Draw the floor. Match the texture's coordinates and the floor's coordinates resp. 
	glBegin(GL_POLYGON);
	glTexCoord2d(0,0);
	glVertex3d(-12,-0.1,-12);		// Texture's (0,0) is bound to (-12,-0.1,-12).
	glTexCoord2d(1,0);
	glVertex3d( 12,-0.1,-12);		// Texture's (1,0) is bound to (12,-0.1,-12).
	glTexCoord2d(1,1);
	glVertex3d( 12,-0.1, 12);		// Texture's (1,1) is bound to (12,-0.1,12).
	glTexCoord2d(0,1);
	glVertex3d(-12,-0.1, 12);		// Texture's (0,1) is bound to (-12,-0.1,12).
	glEnd();

	if (selectMode == 0)
	{
		glDisable(GL_TEXTURE_2D);	
		drawFrame(5);				// Draw x, y, and z axis.
	}
}




/*********************************************************************************
* Call this part whenever display events are needed. 
* Display events are called in case of re-rendering by OS. ex) screen movement, screen maximization, etc.
* Or, user can occur the events by using glutPostRedisplay() function directly.
* this part is called in main() function by registering on glutDisplayFunc(display).
**********************************************************************************/
void display()
{
	// selectMode == 1 means backbuffer mode.
	if (selectMode == 0)
		glClearColor(0, 0.6, 0.8, 1);								// Clear color setting
	else
		glClearColor(0, 0, 0, 1);									// When the backbuffer mode, clear color is set to black
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);				// Clear the screen
	setCamera();													// Locate the camera's position, and draw all of them.

	
	drawFloor();	// Draw floor.
	drawCow();														// Draw cow.
	drawBet();
	drawLuxobase();
	drawLuxobody();
	drawLuxoneck();
	drawLuxohead();

	if (!select_nothing)
	{
	if (selectMode == 0)
	{
		if (trans_state == 0)
		{
			if (!spacestate)
			{
				if (rotatemodel)
				{
					glPushMatrix();
					glTranslatef(currentobject->m[12],currentobject->m[13],currentobject->m[14]);
					
					cx = cam2wld[cameraIndex].m[0];
					cy = cam2wld[cameraIndex].m[1];
					cz = cam2wld[cameraIndex].m[2];
					csum = sqrt(cx*cx+cy*cy+cz*cz);
					cx = cx/csum;
					cy = cy/csum;
					cz = cz/csum;


					glDisable(GL_LIGHTING);
					glBegin(GL_LINES);	
					glColor3d(1,1,1);
					glVertex3d(5*cx,5*cy,5*cz);			
					glVertex3d(-5*cx,-5*cy,-5*cz);
					glEnd();
					glPopMatrix();
				}
			}
			else
			{
				if (rotatemodel)
				{
					glPushMatrix();	
					glMultMatrixd(currentobject->matrix());
					glDisable(GL_LIGHTING);		
					glBegin(GL_LINES);			
					glColor3d(1,1,1);		
					glVertex3d(0,0,0);			
					glVertex3d(5*rx,5*ry,5*rz);
					glEnd();
					glPopMatrix();
				}
			}
		}		
	}
	}

	glFlush();
	
	
	// If it is not backbuffer mode, swap the screen. In backbuffer mode, this is not necessary because it is not presented on screen.
	if (selectMode == 0)
		glutSwapBuffers();
	if (selectMode == 1)
	{
		if (backbuffer)
		{
			glutSwapBuffers();
		}
	}


	frame += 1;					
}


/*********************************************************************************
* Call this part whenever size of the window is changed. 
* This part is called in main() function by registering on glutReshapeFunc(reshape).
**********************************************************************************/
void reshape( int w, int h)
{
	width = w;
	height = h;
	glViewport(0, 0, width, height);
	glMatrixMode(GL_PROJECTION);            // Select The Projection Matrix
	glLoadIdentity();                       // Reset The Projection Matrix
	// Define perspective projection frustum
	double aspect = width/double(height);
	gluPerspective(45, aspect, 1, 1024);
	glMatrixMode(GL_MODELVIEW);             // Select The Modelview Matrix
	glLoadIdentity();                       // Reset The Projection Matrix
}

//------------------------------------------------------------------------------
void initialize()
{
	// Set up OpenGL state
	glShadeModel(GL_SMOOTH);         // Set Smooth Shading
	glEnable(GL_DEPTH_TEST);         // Enables Depth Testing
	glDepthFunc(GL_LEQUAL);          // The Type Of Depth Test To Do
	// Use perspective correct interpolation if available
	glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
	// Initialize the matrix stacks
	reshape(width, height);
	// Define lighting for the scene
	float lightDirection[]   = {1.0, 1.0, 1.0, 0};
	float ambientIntensity[] = {0.1, 0.1, 0.1, 1.0};
	float lightIntensity[]   = {0.9, 0.9, 0.9, 1.0};
	glLightfv(GL_LIGHT0, GL_AMBIENT, ambientIntensity);
	glLightfv(GL_LIGHT0, GL_DIFFUSE, lightIntensity);
	glLightfv(GL_LIGHT0, GL_POSITION, lightDirection);
	glEnable(GL_LIGHT0);
}

/*********************************************************************************
* Call this part whenever mouse button is clicked.
* This part is called in main() function by registering on glutMouseFunc(onMouseButton).
**********************************************************************************/
void onMouseButton(int button, int state, int x, int y)
{
	y = height - y - 1;
	if (button == GLUT_LEFT_BUTTON)
	{
		if (state == GLUT_DOWN)
		{
			printf( "Left mouse click at (%d, %d)\n", x, y );

			// (Project 4) After drawing object on backbuffer, you can recognize which object is selected by reading pixel of (x, y).
			// Change the value of selectMode to 1, then draw the object on backbuffer when display() function is called. 
			selectMode = 1;
			display();
			glReadBuffer(GL_BACK);
			unsigned char pixel[3];
			glReadPixels(x, y, 1, 1, GL_RGB, GL_UNSIGNED_BYTE, pixel);
			printf( "pixel = %d\n", unmunge(pixel[0],pixel[1],pixel[2]));
			selectMode = 0;
			oldmouselocax=x;
			oldmouselocay=y;
			// (Project 4) TODO : Perform the proper task about selected object.
			// hint : you can recognize which object is selected by pixel value.
			if (selectionMode)
			{
				objectcolor = unmunge(pixel[0],pixel[1],pixel[2]);
				if (objectcolor == 255)
				{
					currentobject = &cow2wld;
					select_nothing = 0;
					printf("cow selected\n");
				}
				else if (objectcolor == 255*256)
				{
					currentobject = &bet2wld;
					select_nothing = 0;
					printf("bet selected\n");
				}
				else if (objectcolor == 255*256*256)
				{
					currentobject = &luxobase2wld;
					luxobase = 1;
					luxobody = 1;
					luxoneck = 1; 
					oldluxohead = 0;
					oldluxoneck = 0;
					oldluxobody = 0;
					select_nothing = 0;
					printf("luxo selected\n");
				}
				else if (objectcolor == 64+64*256+255*256*256)
				{
					if (trans_state == 4 || rotatemodel)
					{
					currentobject = &luxobody2wld;
					select_nothing = 0;
					luxobase = 0;
					luxobody = 1;
					luxoneck = 1;
					oldluxohead = 0;
					oldluxoneck = 0;
					oldluxobody = 1;
					printf("luxo selected\n");
					}
					else
					{
					currentobject = &luxobase2wld;
					luxobase = 1;
					luxobody = 1;
					luxoneck = 1; 
					oldluxohead = 0;
					oldluxoneck = 0;
					oldluxobody = 0;
					select_nothing = 0;
					printf("luxo selected\n");
					}
				}
				else if (objectcolor == 128+128*256+255*256*256)
				{
					if (trans_state == 4 || rotatemodel)
					{
					currentobject = &luxoneck2wld;
					select_nothing = 0;
					luxobase = 0;
					luxobody = 0;
					luxoneck = 1; 
					oldluxohead = 0;
					oldluxoneck = 1;
					oldluxobody = 0;
					printf("luxo selected\n");
					}
					else
					{
					currentobject = &luxobase2wld;
					luxobase = 1;
					luxobody = 1;
					luxoneck = 1; 
					oldluxohead = 0;
					oldluxoneck = 0;
					oldluxobody = 0;
					select_nothing = 0;
					printf("luxo selected\n");
					}
				}
				else if (objectcolor == 192+192*256+255*256*256)
				{
					if (trans_state == 4 || rotatemodel)
					{
					currentobject = &luxohead2wld;
					select_nothing = 0;
					luxobase = 0;
					luxobody = 0;
					luxoneck = 0; 
					oldluxohead = 1;
					oldluxoneck = 0;
					oldluxobody = 0;
					printf("luxo selected\n");
					}
					else
					{
					currentobject = &luxobase2wld;
					luxobase = 1;
					luxobody = 1;
					luxoneck = 1; 
					oldluxohead = 0;
					oldluxoneck = 0;
					oldluxobody = 0;
					select_nothing = 0;
					printf("luxo selected\n");
					}
				}
				else
				{
					currentobject = NULL;
					select_nothing = 1;
					
					printf("Nothing selected\n");
				}

			}
			if (backbuffer)
				selectMode = 1;
			else 
				selectMode = 0;
			// Save current clicked location of mouse here, and then use this on onMouseDrag function. 
			oldX = x;
			oldY = y;
		}
	}
	else if (button == GLUT_RIGHT_BUTTON)
	{
		printf( "Right mouse click at (%d, %d)\n",x,y );
	}
	glutPostRedisplay();
}



/*********************************************************************************
* Call this part whenever user drags mouse. 
* Input parameters x, y are coordinate of mouse on dragging. 
* Value of global variables oldX, oldY is stored on onMouseButton, 
* Then, those are used to verify value of x - oldX,  y - oldY to know its movement.
**********************************************************************************/


class Vectortrackball
{
public:
	double x, y, z;

	Vectortrackball()
	{
		x = 0.0f;	y = 0.0f;	z = 0.0f;
	}

	Vectortrackball(double xx, double yy, double zz)
	{
		x = xx;	y = yy;	z = zz;
	}

	string str()
	{
		char buffer[50];
		sprintf(buffer, "[%f, %f, %f]", x,y,z);
		return string(buffer);
	}

	double length()
	{
		return sqrt(x*x + y*y + z*z);
	}

	Vectortrackball normalize()
	{
		double l = sqrt(x*x + y*y + z*z);
		if (l != 0)	l = 1.0f/l;
		return Vectortrackball(l*x, l*y, l*z);
	}

	Vectortrackball cross(Vectortrackball v)
	{
		return Vectortrackball(y*v.z - z*v.y, z*v.x - x*v.z, x*v.y - y*v.x);
	}

	Vectortrackball dot(Vectortrackball v)
	{
		return Vectortrackball(y*v.z - z*v.y, z*v.x - x*v.z, x*v.y - y*v.x);
	}

};

void onMouseDrag( int x, int y)
{
	y = height - y - 1;
	printf( "in drag (%d, %d)\n", x - oldX,  y - oldY );
	if (select_nothing)
	{
		if (camera_trans_state == 1)
		{
			glPushMatrix();
			glLoadIdentity();
			glMultMatrixd(cam2wld[cameraIndex].matrix());
			glTranslated(-(x-oldmouselocax)/20,-(y-oldmouselocay)/20,0);
			glGetDoublev(GL_MODELVIEW_MATRIX, cam2wld[cameraIndex].matrix());
			glPopMatrix();
			glPushMatrix();
			glLoadIdentity();
			glTranslated((x-oldmouselocax)/20,(y-oldmouselocay)/20,0);
			glMultMatrixd(wld2cam[cameraIndex].matrix());
			glGetDoublev(GL_MODELVIEW_MATRIX, wld2cam[cameraIndex].matrix());
			glPopMatrix();
			glutPostRedisplay();
			oldmouselocax=x;
			oldmouselocay=y;		
		}
		else if (camera_trans_state == 2)
		{
			glPushMatrix();
			glLoadIdentity();
			glMultMatrixd(cam2wld[cameraIndex].matrix());
			glTranslated(0,0,(y-oldmouselocay)/20);
			glGetDoublev(GL_MODELVIEW_MATRIX, cam2wld[cameraIndex].matrix());
			glPopMatrix();
			glPushMatrix();
			glLoadIdentity();
			glTranslated(0,0,-(y-oldmouselocay)/20);
			glMultMatrixd(wld2cam[cameraIndex].matrix());
			glGetDoublev(GL_MODELVIEW_MATRIX, wld2cam[cameraIndex].matrix());
			glPopMatrix();
			glutPostRedisplay();
			oldmouselocay=y;		
		}
		else if (camera_trans_state == 3)
		{
			glPushMatrix();
	        glMatrixMode (GL_PROJECTION);
		    glLoadIdentity ();
			double aspect = width/double(height);
			zoomfactor = zoomfactor+(oldmouselocay-y)/20;
			gluPerspective(zoomfactor, aspect, 1, 1024);
		    glMatrixMode(GL_MODELVIEW);
			glLoadIdentity();	
			glPopMatrix();
			glutPostRedisplay();
			oldmouselocay=y;
		}
		else if (camera_trans_state == 4)
		{
			Vectortrackball v0, v1, axis;
			double angle;
			v0 = Vectortrackball(double(oldX) - 0.5*width, -0.5*height + double(oldY), 0.5*width).normalize();
			v1 = Vectortrackball(double(x) -    0.5*width, -0.5*height + double(y),    0.5*width).normalize();
			axis = v0.cross(v1);
			angle = asin(axis.length());
			axis = axis.normalize();

			
			glPushMatrix();
			glLoadIdentity();
			glTranslatef(wld2cam[cameraIndex].m[12],wld2cam[cameraIndex].m[13],wld2cam[cameraIndex].m[14]);
			glRotated(angle*180.0/M_PI , axis.x,axis.y,axis.z);
			glTranslatef(-wld2cam[cameraIndex].m[12],-wld2cam[cameraIndex].m[13],-wld2cam[cameraIndex].m[14]);
			glMultMatrixd(wld2cam[cameraIndex].matrix());
			glGetDoublev(GL_MODELVIEW_MATRIX, wld2cam[cameraIndex].matrix());
			glPopMatrix();
			glPushMatrix();
			glLoadIdentity();
			glMultMatrixd((wld2cam[cameraIndex].inverse()).matrix());
			glGetDoublev(GL_MODELVIEW_MATRIX, cam2wld[cameraIndex].matrix());
			glPopMatrix();
		

			glutPostRedisplay();
			oldX=x;
			oldY=y;
		}
	}
	else
	{
		if (trans_state != 0)
		{			
			if (spacestate)
			{
				if (x>oldmouselocax)
				{
					trans = (x-oldmouselocax)/50;
				}
				else
				{
					if (x<oldmouselocax)
					{
						trans = (x-oldmouselocax)/50;
					}
				}
				if (trans_state == 1)
				{
					glPushMatrix();
					glLoadIdentity();
					glMultMatrixd(currentobject->matrix());
					glTranslated(trans,0,0);
					glGetDoublev(GL_MODELVIEW_MATRIX, currentobject->matrix());
					glPopMatrix();
					glutPostRedisplay();
				}
				if (trans_state == 2)
				{
					glPushMatrix();
					glLoadIdentity();
					glMultMatrixd(currentobject->matrix());
					glTranslated(0,trans,0);
					glGetDoublev(GL_MODELVIEW_MATRIX, currentobject->matrix());
					glPopMatrix();
					glutPostRedisplay();
				}
				if (trans_state == 3)
				{
					glPushMatrix();
					glLoadIdentity();
					glMultMatrixd(currentobject->matrix());
					glTranslated(0,0,trans);
					glGetDoublev(GL_MODELVIEW_MATRIX, currentobject->matrix());
					glPopMatrix();
					glutPostRedisplay();
				}
				if (trans_state == 4)
				{
					Vectortrackball v0, v1, axis;
					double angle;
					v0 = Vectortrackball(double(oldX) - 0.5*width, -0.5*height + double(oldY), 0.5*width).normalize();
					v1 = Vectortrackball(double(x) -    0.5*width, -0.5*height + double(y),    0.5*width).normalize();
					axis = v0.cross(v1);
					angle = asin(axis.length());
					axis = axis.normalize();
					glPushMatrix();
					glLoadIdentity();
					glTranslatef(-cam2wld[cameraIndex].m[12]+currentobject->m[12],-cam2wld[cameraIndex].m[13]+currentobject->m[13],-cam2wld[cameraIndex].m[14]+currentobject->m[14]);
					glMultMatrixd(cam2wld[cameraIndex].matrix());
					glRotated(angle*180.0/M_PI , axis.x, axis.y, axis.z);
					glMultMatrixd(wld2cam[cameraIndex].matrix());
					glTranslatef(cam2wld[cameraIndex].m[12]-currentobject->m[12],cam2wld[cameraIndex].m[13]-currentobject->m[13],cam2wld[cameraIndex].m[14]-currentobject->m[14]);
					glMultMatrixd(currentobject->matrix());
					glGetDoublev(GL_MODELVIEW_MATRIX,currentobject->matrix());
					glPopMatrix();
					glutPostRedisplay();
					oldX=x;
					oldY=y;

				}
				oldmouselocax= x;
			}	
			else
			{
				if (trans_state == 1 || trans_state == 2)
				{
					glPushMatrix();
					glLoadIdentity();
					glMultMatrixd(cam2wld[cameraIndex].matrix());
					glTranslated((x-oldmouselocax)/20,(y-oldmouselocay)/20,0);
					glMultMatrixd(wld2cam[cameraIndex].matrix());
					glMultMatrixd(currentobject->matrix());
					glGetDoublev(GL_MODELVIEW_MATRIX, currentobject->matrix());
					glPopMatrix();
					glutPostRedisplay();
					oldmouselocax=x;
					oldmouselocay=y;
				}
				if (trans_state == 3)
				{
					glPushMatrix();
					glLoadIdentity();
					glMultMatrixd(cam2wld[cameraIndex].matrix());
					glTranslated(0,0,(oldmouselocax-x)/20);
					glMultMatrixd(wld2cam[cameraIndex].matrix());
					glMultMatrixd(currentobject->matrix());
					glGetDoublev(GL_MODELVIEW_MATRIX, currentobject->matrix());
					glPopMatrix();
					glutPostRedisplay();
					oldmouselocax=x;
				}
				if (trans_state == 4)
				{
					Vectortrackball v0, v1, axis;
					double angle;
					v0 = Vectortrackball(double(oldX) - 0.5*width, -0.5*height + double(oldY), 0.5*width).normalize();
					v1 = Vectortrackball(double(x) -    0.5*width, -0.5*height + double(y),    0.5*width).normalize();
					axis = v0.cross(v1);
					angle = asin(axis.length());
					axis = axis.normalize();
					glPushMatrix();
					glLoadIdentity();
					glTranslatef(-cam2wld[cameraIndex].m[12]+currentobject->m[12],-cam2wld[cameraIndex].m[13]+currentobject->m[13],-cam2wld[cameraIndex].m[14]+currentobject->m[14]);
					glMultMatrixd(cam2wld[cameraIndex].matrix());
					glRotated(angle*180.0/M_PI , axis.x, axis.y, axis.z);
					glMultMatrixd(wld2cam[cameraIndex].matrix());
					glTranslatef(cam2wld[cameraIndex].m[12]-currentobject->m[12],cam2wld[cameraIndex].m[13]-currentobject->m[13],cam2wld[cameraIndex].m[14]-currentobject->m[14]);
					glMultMatrixd(currentobject->matrix());
					glGetDoublev(GL_MODELVIEW_MATRIX,currentobject->matrix());
					glPopMatrix();
					glutPostRedisplay();
					
					oldX=x;
					oldY=y;

				}
			}
		}
		else
		{
			if (!spacestate)
			{
				if (rotatemodel)
				{	
					glPushMatrix();
					glLoadIdentity();
					glTranslatef(-cam2wld[cameraIndex].m[12]+currentobject->m[12],-cam2wld[cameraIndex].m[13]+currentobject->m[13],-cam2wld[cameraIndex].m[14]+currentobject->m[14]);
					glMultMatrixd(cam2wld[cameraIndex].matrix());
					glRotatef(-(oldmouselocax-x), 1,	0, 0);
					glMultMatrixd(wld2cam[cameraIndex].matrix());
					glTranslatef(cam2wld[cameraIndex].m[12]-currentobject->m[12],cam2wld[cameraIndex].m[13]-currentobject->m[13],cam2wld[cameraIndex].m[14]-currentobject->m[14]);
					glMultMatrixd(currentobject->matrix());
					glGetDoublev(GL_MODELVIEW_MATRIX, currentobject->matrix());
					glPopMatrix();
					glutPostRedisplay();
					oldmouselocax=x;
				}
			}
		}
	}
	// (Project 2,3,4) TODO : Implement here to perform properly when drag the mouse on each cases resp.
	glutPostRedisplay();
}
/*********************************************************************************
* Call this part whenever user types keyboard. 
* This part is called in main() function by registering on glutKeyboardFunc(onKeyPress).
**********************************************************************************/
void spinDisplay(void)
{
	if (!select_nothing)
	{
    glPushMatrix();
	glLoadIdentity();
	glMultMatrixd(currentobject->matrix());			
	glRotated(2,rx,ry,rz);
	glGetDoublev(GL_MODELVIEW_MATRIX, currentobject->matrix());
	glPopMatrix();
	}
	glutPostRedisplay();
}



void onKeyPress( unsigned char key, int x, int y)
{
	// If 'c' or space bar are pressed, alter the camera.
	// If a number is pressed, alter the camera corresponding the number.
	if ((key == ' ') || (key == 'c'))
	{    
		printf( "Toggle camera %d\n", cameraIndex );
		cameraIndex += 1;
	}      
	else if ((key >= '0') && (key <= '9'))
		cameraIndex = key - '0';
	
	if (key == 'm')
	{
		if (!spacestate)
		{
			spacestate = 1;
			printf("Transformation will be performed in modeling space\n");
		}
	}

	if (key == 'v')
	{
		if (spacestate)
		{
			spacestate = 0;
			printf("Transformation will be performed in veiwing space\n");
			rx = rand();
			ry = rand();
			rz = rand();
			rsum = sqrt(rx*rx + ry*ry + rz*rz);
			rx = rx/rsum;
			ry = ry/rsum;
			rz = rz/rsum;
		}	
	}

	if (key == 'r')
	{
		rx = rand();
		ry = rand();
		rz = rand();
		rsum = sqrt(rx*rx + ry*ry + rz*rz);
		rx = rx/rsum;
		ry = ry/rsum;
		rz = rz/rsum;
		rotatemodel = !rotatemodel;
		camera_trans_state = 0;
		if (spacestate)
		{
			if (rotatemodel)
			{
				trans_state_backup = trans_state;
				trans_state = 0;
				printf("rotate on\n");
			}
			else
			{
				printf("rotate off\n");
				trans_state = trans_state_backup;
			}
		}
		else
		{
			if (rotatemodel)
			{
				printf("rotate on\n");
				trans_state_backup = trans_state;
				trans_state = 0;
			}
			else
			{
				printf("rotate off\n");
				trans_state = trans_state_backup;
			}	
		}
	}

	
		if (rotatemodel)
		{
			if (spacestate)
			{
				glutIdleFunc(spinDisplay);
			}
			else
			{
				glutIdleFunc(NULL);
			}
		}
		else
		{
			if (spacestate)
			{
				glutIdleFunc(NULL);
			}	
		}

	
	if (key == 'x')
	{
		camera_trans_state = 0;
		if (spacestate)
		{
			trans_state = 1;
			if (rotatemodel)
			{
				rotatemodel = !rotatemodel;
				glutIdleFunc(NULL);
			}
		}	
		else
		{
			trans_state = 1;
			if (rotatemodel)
			{
				rotatemodel = !rotatemodel;
			}
		}
	}
	
	if (key == 'y')
	{
		camera_trans_state = 0;
		if (spacestate)
		{
			trans_state = 2;
			if (rotatemodel)
			{
				rotatemodel = !rotatemodel;
				glutIdleFunc(NULL);
			}
		}
		else
		{
			trans_state = 2;
			if (rotatemodel)
			{
				rotatemodel = !rotatemodel;
			}
		}
	}
	
	if  (key == 'z')
	{
		camera_trans_state = 0;
		if (spacestate)
		{
			trans_state = 3;
			if (rotatemodel)
			{
				rotatemodel = !rotatemodel;
				glutIdleFunc(NULL);
			}
		}
		else
		{
			trans_state = 3;
			if (rotatemodel)
			{
				rotatemodel = !rotatemodel;
			}
		}
	}

	if  (key == 's')
	{
		selectionMode = !selectionMode;
		if (selectionMode == 1)
			printf("select mode : ON\n");
		else
			printf("select mode : OFF\n");
	}

	if (key == 'b')
	{ 
		if (selectMode == 0)
		{
			selectMode = 1;
			backbuffer = 1;
			printf("Show back buffer");
		}
		else
		{
			selectMode = 0;
			backbuffer = 0;
			printf("Show front buffer");
		}
	}

	if (key == 'p')
	{
		select_nothing = 1;
		camera_trans_state = 1;
		printf("pan");
	}

	if (key == 'd')
	{
		select_nothing = 1;
		camera_trans_state = 2;
		printf("dolly");
	}

	if (key == 'o')
	{
		select_nothing = 1;
		camera_trans_state = 3;
		printf("zoom");
	}

	if (key == 't')
	{
		select_nothing = 0;
		camera_trans_state = 4;
		trans_state = 4;
		if (rotatemodel)
		{
			rotatemodel = !rotatemodel;
			glutIdleFunc(NULL);
		}
		printf("zoom");
	}

	if (key == 'h')
	{
		printf("===========================\nKey Map\n===========================\ns:selection mode toggle\nx,y,z:translate each axis\nr:rotate\nv:veiwing space\nm:modeling sapce\np:pan\nd:dolly\no:zoom\nt:trackball\nb:toggle show back buffer\n===========================\n");
	}

	if (cameraIndex >= (int)wld2cam.size() )
		cameraIndex = 0;

	// (Project 2,3,4) TODO : Implement here to handle keyboard input.
	glutPostRedisplay();
}

//------------------------------------------------------------------------------
void main( int argc, char* argv[] )
{
	width = 800;
	height = 600;
	frame = 0;
	glutInit( &argc, argv );						// Initialize openGL.
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);	// Initialize display mode. This project will use double buffer and RGB color.
	glutInitWindowSize(width, height);				// Initialize window size.
	glutInitWindowPosition(100, 100);				// Initialize window coordinate.
	glutCreateWindow("Simple Scene");				// Make window whose name is "Simple Scene".
	glutDisplayFunc(display);						// Register display function to call that when drawing screen event is needed.
	glutReshapeFunc(reshape);						// Register reshape function to call that when size of the window is changed.
	glutKeyboardFunc(onKeyPress);					// Register onKeyPress function to call that when user presses the keyboard.
	glutMouseFunc(onMouseButton);					// Register onMouseButton function to call that when user moves mouse.
	glutMotionFunc(onMouseDrag);					// Register onMouseDrag function to call that when user drags mouse.
	int rv,gv,bv;
	glGetIntegerv(GL_RED_BITS,&rv);					// Get the depth of red bits from GL.
	glGetIntegerv(GL_GREEN_BITS,&gv);				// Get the depth of green bits from GL.
	glGetIntegerv(GL_BLUE_BITS,&bv);				// Get the depth of blue bits from GL.
	printf( "Pixel depth = %d : %d : %d\n", rv, gv, bv );
	initialize();									// Initialize the other thing.
	glutMainLoop();									// Execute the loop which handles events.
}
