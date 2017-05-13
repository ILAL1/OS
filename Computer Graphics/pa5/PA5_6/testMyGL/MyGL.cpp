#include "stdafx.h"
#include "MyGL.h"


//------------------------------------------------------------------------------
MyGL::MyGL()
: 
_doLighting( false ),
_doClipping( false ),
_doTriangulate( false ),
_doRasterize( false ),
_doBackCulling(false)
{}

//------------------------------------------------------------------------------
MyGL::~MyGL()
{}

//------------------------------------------------------------------------------
bool MyGL::ComputeLighting( vector<GLVertex>& verts )
{ 
	if( !_doLighting )
		return false;

	//
	// YOUR CODE HERE 
	//

	// for just multiply material and lighting diffuse together
	if( light.enabled )
	{
		for( int i=0; i < (int)verts.size(); i++ )
			verts[i].color = light.diffuse * material.diffuse;
	}
	return true;
}

//------------------------------------------------------------------------------
bool MyGL::ClipPolygon( const vector<GLVertex>& vertsIn, vector<GLVertex>& vertsOut )
{  
	if( !_doClipping )
		return false;

	//
	// YOUR CODE HERE	
	//

	// 클립핑의 경우 새로운 position 값은 수업시간이나 슬라이드에 나와있는 방법으로 하면 됩니다.
	// 이때 GLVertex 에 있는 다른 값인 color, normal, texture coordinate 도 역시 같은 방법으로 합니다.

	// implement cliping. you can use method that is learned in the class.
	// other values in GLVertex calculate the same way. (color, normal, texture coordinat)
	// just copy the polygon so we can get to the other parts of the pipeline

	

	vertsOut = vertsIn;
	for (int h=0; h<4; h++)
	{
		int pos = (int)vertsOut.size();
		int verts_size_back_up = (int)vertsOut.size();
		float Tx, Ty, Tz, Td;

		//comment : 화면의 4개의 edge에 대한 상수들 설정한다. 
		if (h==0)
		{
			Tx = 1;
			Ty = 0;
			Tz = 0;
			Td = -1;
		}
		if (h==1)
		{
			Tx = -1;
			Ty = 0;
			Tz = 0;
			Td = -1;
		}
		if (h==2)
		{
			Tx = 0;
			Ty = 1;
			Tz = 0;
			Td = -1;
		}
		if (h==3)
		{
			Tx = 0;
			Ty = -1;
			Tz = 0;
			Td = -1;
		}
		//comment : vertsOut에 있는 점들에 대해 다음을 실행
		for(int i=0; i < (int)vertsOut.size(); i++ )
		{
			//comment : vertsOut에 있는 점들에 대한 기본적인 정보를 받는다. 
			float p1x = vertsOut[i].position[0];
			float p1y = vertsOut[i].position[1];
			float p1z = vertsOut[i].position[2];
			float p1w = vertsOut[i].position[3];
			float c1x = vertsOut[i].color[0];
			float c1y = vertsOut[i].color[1];
			float c1z = vertsOut[i].color[2];
			float c1w = vertsOut[i].color[3];
			float t1x = vertsOut[i].texCoord[0];
			float t1y = vertsOut[i].texCoord[1];
			float n1x = vertsOut[i].normal[0];
			float n1y = vertsOut[i].normal[1];
			float n1z = vertsOut[i].normal[2];
			//comment : 이웃하고 있는 점을 p0라 하고 p0의 기본적인 정보를 받는다. p1이 제일 첫번째 점일때에는 p0는 마지막 점이 된다. 
			float p0x, p0y, p0z, p0w, c0x, c0y, c0z, c0w, t0x, t0y, n0x, n0y, n0z;
			if (i>0)
			{
				p0x = vertsOut[i-1].position[0];
				p0y = vertsOut[i-1].position[1];
				p0z = vertsOut[i-1].position[2];
				p0w = vertsOut[i-1].position[3];
				c0x = vertsOut[i-1].color[0];
				c0y = vertsOut[i-1].color[1];
				c0z = vertsOut[i-1].color[2];
				c0w = vertsOut[i-1].color[3];
				t0x = vertsOut[i-1].texCoord[0];
				t0y = vertsOut[i-1].texCoord[1];
				n0x = vertsOut[i-1].normal[0];
				n0y = vertsOut[i-1].normal[1];
				n0z = vertsOut[i-1].normal[2];
			}
			else
			{
				p0x = vertsOut[(int)vertsOut.size()-1].position[0];
				p0y = vertsOut[(int)vertsOut.size()-1].position[1];
				p0z = vertsOut[(int)vertsOut.size()-1].position[2];
				p0w = vertsOut[(int)vertsOut.size()-1].position[3];
				c0x = vertsOut[(int)vertsOut.size()-1].color[0];
				c0y = vertsOut[(int)vertsOut.size()-1].color[1];
				c0z = vertsOut[(int)vertsOut.size()-1].color[2];
				c0w = vertsOut[(int)vertsOut.size()-1].color[3];
				t0x = vertsOut[(int)vertsOut.size()-1].texCoord[0];
				t0y = vertsOut[(int)vertsOut.size()-1].texCoord[1];
				n0x = vertsOut[(int)vertsOut.size()-1].normal[0];
				n0y = vertsOut[(int)vertsOut.size()-1].normal[1];
				n0z = vertsOut[(int)vertsOut.size()-1].normal[2];
			}
			//comment : p1이 위에서 지정한 화면의 edge중 하나보다 안에 있고 p0는 밖에 있는 경우거나 혹은 그 반대의 경우에는 다음을 실행한다. 
			if ((p1x*Tx+p1y*Ty+p1z*Tz+p1w*Td > 0 && p0x*Tx+p0y*Ty+p0z*Tz+p0w*Td < 0) || (p1x*Tx+p1y*Ty+p1z*Tz+p1w*Td < 0 && p0x*Tx+p0y*Ty+p0z*Tz+p0w*Td > 0))
			{
				//comment : p1과 p0를 잇는 edge와 화면의 edge의 교점을 new_p1이라 하자. 
				float t;
				float Tp0, Tp1p0;
				float new_p1x, new_p1y, new_p1z, new_p1w;
				float new_c1x, new_c1y, new_c1z, new_c1w;
				float new_t1x, new_t1y;
				float new_n1x, new_n1y, new_n1z;
				//comment : new_p1을 구하기 위한 t값을 구한다. 
				Tp0 = p0x*Tx+p0y*Ty+p0z*Tz+p0w*Td;
				Tp1p0 = (p1x-p0x)*Tx+(p1y-p0y)*Ty+(p1z-p0z)*Tz+(p1w-p0w)*Td;
				//comment : tp1p0가 0일 경우가 혹시 있을지 모르니 에러를 피하기 위해 이렇게 하였다. 
				if (Tp1p0 != 0)
				{
					//comment : t를 이용하여 교점과 그 교점의 컬러와 텍스트, 노말을 구한다. 
					t = -Tp0/Tp1p0;
					new_p1x = p0x+t*(p1x-p0x);
					new_p1y = p0y+t*(p1y-p0y);
					new_p1z = p0z+t*(p1z-p0z);
					new_p1w = p0w+t*(p1w-p0w);
					new_c1x = c0x+t*(c1x-c0x);
					new_c1y = c0y+t*(c1y-c0y);
					new_c1z = c0z+t*(c1z-c0z);
					new_c1w = c0w+t*(c1w-c0w);
					new_t1x = t0x+t*(t1x-t0x);
					new_t1y = t0y+t*(t1y-t0y);
					new_n1x = n0x+t*(n1x-n0x);
					new_n1y = n0y+t*(n1y-n0y);
					new_n1z = n0z+t*(n1z-n0z);
					//comment : new_p1을 vertsout에 넣기 위해 공간을 만든다. p1과 p1 뒤에 있는 모든 점을 한칸씩 뒤로 옮긴다. 
					vertsOut.resize(pos+1);
					for (int j=(int)vertsOut.size()-1; j>i; j--)
					{
						vertsOut[j]= vertsOut[j-1];
					}
					//comment : new_p1을 예전의 p1자리에 넣는다. 
					vertsOut[i].position[0] = new_p1x;
					vertsOut[i].position[1] = new_p1y;
					vertsOut[i].position[2] = new_p1z;
					vertsOut[i].position[3] = new_p1w;
					vertsOut[i].color[0] = new_c1x;
					vertsOut[i].color[1] = new_c1y;
					vertsOut[i].color[2] = new_c1z;
					vertsOut[i].color[3] = new_c1w;
					vertsOut[i].texCoord[0] = new_t1x;
					vertsOut[i].texCoord[1] = new_t1y;
					vertsOut[i].normal[0] = new_n1x;
					vertsOut[i].normal[1] = new_n1y;
					vertsOut[i].normal[2] = new_n1z;
					pos = pos+1;
				}
			}
		}

		int count = 0;

		//comment : vertsOut의 점 갯수가 바뀌지 않았다는 것은 clipping할 점이없었다는 것이고 
		//			vertsOut 중 1점이라도 화면의 edge바깥에 있다면 그 도형은 화면 바깥에 있으므로 vertOut의 점 개수를 0으로 만들어서 redering 시키지 않는다. 
		if (verts_size_back_up == (int)vertsOut.size() &&  vertsOut[0].position[0]*Tx+vertsOut[0].position[1]*Ty+vertsOut[0].position[2]*Tz+vertsOut[0].position[3]*Td> 0)
		{
			vertsOut.resize(0);
			return true;
		}
		else
		{
			//comment: 위의 경우가 아니라는 것은 도형이 화면에 걸쳐있거나 화면 안에 있다는 것이므로 rendering을 한다.
			//		   그런데 이 때 위에서 클리핑을 하고 새로운 교점을 만들고 나서 화면 밖에 있는 점을 지우지 않았다.
			//         따라서 점들을 돌면서 화면 밖에 있는 점을 지운다. 
			for (int h=0; h < 3; h++ )
			{
				for(int i=0; i < (int)vertsOut.size(); i++ )
				{
					float p1x = vertsOut[i].position[0];
					float p1y = vertsOut[i].position[1];
					float p1z = vertsOut[i].position[2];
					float p1w = vertsOut[i].position[3];
					if (p1x*Tx+p1y*Ty+p1z*Tz+p1w*Td > 0)
					{
						for (int j=0; j<(int)vertsOut.size()-i-1; j++)
						{
							vertsOut[i+j]= vertsOut[i+j+1];
						}
						count++;
						vertsOut.resize(pos-count);
					}
				}
			}
		}
	}
	return true;
}

//------------------------------------------------------------------------------
bool MyGL::TriangulatePolygon( const vector<GLVertex>& polygonVerts,
							  vector<GLVertex>& triangleVerts )
{
	if( !_doTriangulate )
		return false;

	//
	// YOUR CODE HERE
	//

	// 여기에 삼각화(Triangulate) 를 구현 한다.
	// F1 키를 누를때 나오는 사각형의 색이 삼각화(triangulize) 결과에 따라서 다르게 나타나는데,
	// 크게 신경쓰지 않아도 됩니다.
	// 사각형에 색이 제대로 채워지는지는 보면 알 수 있습니다.

	// implement Triangulate
	// if you press F1, a box is appeared
	// the box's color will be depend on your implementation. 
	// Maybe it is different from hardware rendering. you don't need to worry about it.
	// but you must fill color right way. you can know if you see your result.
	// 


	// just make a triangle out of the first three vertices for now

	//comment: polygonVerts.size()가 0이면 삼각화할게 없으므로 바로 끝내버린다. 
	if (polygonVerts.size() == 0)
	{
		return true;	
	}
	//comment: polygonVerts에 있는 점들에 대해 삼각화를 한다.  
	for (int i=0; i < polygonVerts.size()-1; i++)
	{
		if (_doBackCulling)
		{
			//comment: 만약 backCulling을 하면 우선 그 삼각형의 area를 구한다. 
			GLVertex v1 =  polygonVerts[0];
			GLVertex v2 =  polygonVerts[i];
			GLVertex v3 =  polygonVerts[i+1];
			float ar1 = polygonVerts[i].position[0]*polygonVerts[i+1].position[1];
			float ar2 = polygonVerts[i+1].position[0]*polygonVerts[i].position[1];
			float ar3 = polygonVerts[0].position[0]*polygonVerts[i+1].position[1];
			float ar4 = polygonVerts[i+1].position[0]*polygonVerts[0].position[1];
			float ar5 = polygonVerts[0].position[0]*polygonVerts[i].position[1];
			float ar6 = polygonVerts[i].position[0]*polygonVerts[0].position[1];
			float area = (ar1-ar2)-(ar3-ar4)+(ar5-ar6);
			//comment: 그 area가 0보다 작으면 back-face이므로 0초과인 area를 가지는 삼각형만 push_back한다. 
			if (area>0)
			{
				triangleVerts.push_back( polygonVerts[0] );
				triangleVerts.push_back( polygonVerts[i]);
				triangleVerts.push_back( polygonVerts[i+1]);
			}
		}
		else
		{
			//comment: area에 상관없이 다 집어넣는다. 
			triangleVerts.push_back( polygonVerts[0] );
			triangleVerts.push_back( polygonVerts[i]);
			triangleVerts.push_back( polygonVerts[i+1]);
		}
	}
	return true;
}

//------------------------------------------------------------------------------
bool MyGL::RasterizeTriangle( GLVertex verts[3] )
{
	if( !_doRasterize )
		return false;

	// 
	// YOUR CODE HERE
	//



	// 여기에 Rasterization 을 구현하면 된다.
	// w 키를 눌렀을때 나타나는 wire mode 는 구현할 필요 없다.

	// Implement rasterization 
	// you don't need to implement wire mode which is appeared if you press key 'w'

	// just render the vertices for now
	
	//comment: triangulation에서 3개로 받은 점에 대한 정보를 받는다. 
	GLVertex& v1 = verts[0];
	GLVertex& v2 = verts[1];
	GLVertex& v3 = verts[2];
	double edge1[3],edge2[3],edge3[3];
	RGBAColor color1 = v1.color;
	RGBAColor color2 = v2.color;
	RGBAColor color3 = v3.color;
	//comment: 삼각형의 edge 식을 구한다. 
	edge1[0] = v1.position[1]-v2.position[1];
	edge1[1] = v2.position[0]-v1.position[0];
	edge1[2] = v1.position[0]*v2.position[1]-v2.position[0]*v1.position[1];
	edge2[0] = v2.position[1]-v3.position[1];
	edge2[1] = v3.position[0]-v2.position[0];
	edge2[2] = v2.position[0]*v3.position[1]-v3.position[0]*v2.position[1];
	edge3[0] = v3.position[1]-v1.position[1];
	edge3[1] = v1.position[0]-v3.position[0];
	edge3[2] = v3.position[0]*v1.position[1]-v1.position[0]*v3.position[1];
	
	//comment: 삼각형의 area를 구한다. 이상하게 한 식에다 다 구현하면 오류가 나서 ar1~arg6으로 나눠서 구함. 
	double ar1 = v2.position[0]*v3.position[1];
	double ar2 = v3.position[0]*v2.position[1];
	double ar3 = v1.position[0]*v3.position[1];
	double ar4 = v3.position[0]*v1.position[1];
	double ar5 = v1.position[0]*v2.position[1];
	double ar6 = v2.position[0]*v1.position[1];
	double area = (ar1-ar2)-(ar3-ar4)+(ar5-ar6);

	//comment: color와 z의 line interpolation을 구한다. 
	double r_lineinter[3];
	double g_lineinter[3];
	double b_lineinter[3];
	double z_lineinter[3];
	r_lineinter[0]=(color1[0]*edge2[0]+color2[0]*edge3[0]+color3[0]*edge1[0])/(area);
	r_lineinter[1]=(color1[0]*edge2[1]+color2[0]*edge3[1]+color3[0]*edge1[1])/(area);
	r_lineinter[2]=(color1[0]*edge2[2]+color2[0]*edge3[2]+color3[0]*edge1[2])/(area);
	g_lineinter[0]=(color1[1]*edge2[0]+color2[1]*edge3[0]+color3[1]*edge1[0])/(area);
	g_lineinter[1]=(color1[1]*edge2[1]+color2[1]*edge3[1]+color3[1]*edge1[1])/(area);
	g_lineinter[2]=(color1[1]*edge2[2]+color2[1]*edge3[2]+color3[1]*edge1[2])/(area);
	b_lineinter[0]=(color1[2]*edge2[0]+color2[2]*edge3[0]+color3[2]*edge1[0])/(area);
	b_lineinter[1]=(color1[2]*edge2[1]+color2[2]*edge3[1]+color3[2]*edge1[1])/(area);
	b_lineinter[2]=(color1[2]*edge2[2]+color2[2]*edge3[2]+color3[2]*edge1[2])/(area);
	z_lineinter[0]=(v1.position[2]*edge2[0]+v2.position[2]*edge3[0]+v3.position[2]*edge1[0])/(area);
	z_lineinter[1]=(v1.position[2]*edge2[1]+v2.position[2]*edge3[1]+v3.position[2]*edge1[1])/(area);
	z_lineinter[2]=(v1.position[2]*edge2[2]+v2.position[2]*edge3[2]+v3.position[2]*edge1[2])/(area);

	//comment: rasterization을 하는 범위를 구하기 위해 w,h를 알아낸다. 
	int w, h;
	frameBuffer.GetSize(w,h);
	RGBAColor color;
	using std::max;
	using std::min;
	float z;
	//comment: rasterization을좀더 과정을 줄이기 위해 삼각형이 들어가는 딱 맞는 사각형의 점을 구한다. 
	double w1 = max(max(v1.position[0],v2.position[0]), v3.position[0]);
	double w2 = min(min(v1.position[0],v2.position[0]), v3.position[0]);
	double h1 = max(max(v1.position[1],v2.position[1]), v3.position[1]);
	double h2 = min(min(v1.position[1],v2.position[1]), v3.position[1]);

	//comment: 위에서 구한 사각형 내에서 rasterization을 한다. 
	for (int x=w2; x<w1; x++)
	{
		for (int y=h2; y<h1; y++)
		{
			bool t1, t2, t3;
			//comment: shared edge에 대해 tie breaking을 하기 위해 t1을 설정한다. ppt에 있는 대로 했어요; 
			if (edge1[0] != 0)
			{	
				if (edge1[0]>0)
				{
					t1=true;
				}
				else
					t1=false;
			}
			else
			{
				if (edge1[1]>=0)
				{
					t1=true;
				}
				else
					t1=false;
			}
			if (edge2[0] != 0)
			{	
				if (edge2[0]>0)
				{
					t2=true;
				}
				else
					t2=false;
			}
			else
			{
				if (edge3[1]>0)
				{
					t2=true;
				}
				else
					t2=false;
			}
			if (edge3[0] != 0)
			{	
				if (edge3[0]>0)
				{
					t3=true;
				}
				else
					t3=false;
			}
			else
			{
				if (edge3[1]>0)
				{
					t3=true;
				}
				else
					t3=false;
			}

			//comment: 삼각형 내에 있는 점에 대해 그리고 삼각형의 edge 위에 있는 점은 tie breaking의 결과에 따라 rasterization을 한다. 
			if  ((edge1[0]*x+edge1[1]*y+edge1[2]>0) || (edge1[0]*x+edge1[1]*y+edge1[2]==0 && t1))
				if ((edge2[0]*x+edge2[1]*y+edge2[2]>0) || (edge2[0]*x+edge2[1]*y+edge2[2]==0 && t2))
					if ((edge3[0]*x+edge3[1]*y+edge3[2]>0) || (edge3[0]*x+edge3[1]*y+edge3[2]==0 && t3))
					{
						//comment: texture 모드이면 texture에 대한 정보를 가져온다. 
						if( textureEnabled  )
						{
							if( texture.id != 0 )
							{
								double argx=1;
								double argy=1;

								//comment: texture가 화면에 걸쳐있을 때 화면상에 걸쳐 있는 점의 texture정보를 구한다. 
								//		   화면에 걸쳐있게 되면 texCoord가 0과 1사이이기 때문에 그 정보로 화면에 걸쳐있는지 아닌지를 판단했다.
								if (v1.texCoord[0] != 1 && v1.texCoord[0] != 0)
								{
									argx = v1.texCoord[0];
								}
								else if (v2.texCoord[0] != 1 && v2.texCoord[0] != 0)
								{
									argx = v2.texCoord[0];
								}
								else if (v3.texCoord[0] != 1 && v3.texCoord[0] != 0)
								{
									argx = v3.texCoord[0];
								}

								if (v1.texCoord[1] != 1 && v1.texCoord[1] != 0)
								{
									argy = v1.texCoord[1];
								}
								else if (v2.texCoord[1] != 1 && v2.texCoord[1] != 0)
								{
									argy = v2.texCoord[1];
								}
								else if (v3.texCoord[1] != 1 && v3.texCoord[1] != 0)
								{
									argy = v3.texCoord[1];
								}

								//comment: 화면상의 점이 texture상에서 어느 점으로 mapping되는지 구하는 과정이다. 
								//		   도형이 화면에 걸쳐 있지 않으면 texcoorx와 texcoory는 texture상의 위치가 된다. 
								//		   도형내에서의 x y 위치를 구한 후 도형의 각 변의 크기로 나누었기 때문. 
								double texcoorx = (x-w2)/(w1-w2);
								double texcoory = (y-h2)/(h1-h2);

								//comment: argx나 argy가 1이 아니라는 것은 도형이 화면에 걸쳐있다는 것. 
								//		   argx를 이용해 x y의 위치를 texture상의 위치로 바꾼다. 
								if (argx != 1 && argx !=0)
								{
									if (min(min(v1.texCoord[0],v2.texCoord[0]), v3.texCoord[0]) != 0)
									{
										texcoorx =argx+(x-w2)/(w1-w2)*(1-argx);
									}
									else if (max(max(v1.texCoord[0],v2.texCoord[0]), v3.texCoord[0]) != 1) 
									{
										texcoorx = (x-w2)/(w1-w2)*argx;
									}
								}
								if (argy != 1 && argy !=0)
								{
									if  (min(min(v1.texCoord[1],v2.texCoord[1]), v3.texCoord[1]) != 0)
									{
										texcoory =(y-h2)/(h1-h2)*(1-argy);
										
									}
									else if (max(max(v1.texCoord[1],v2.texCoord[1]), v3.texCoord[1]) !=1)
									{
										texcoory =argy-(y-h2)/(h1-h2)*argy;
									}
								}
								//comment: texture상의 위치를 구한후 texture의 위치에 있는 색을 구한다. 
								int a = texcoorx * (texture.width - 1);
								int b = texcoory * (texture.height - 1);
								color = texture.GetPixel( a, b, 0 );
							}
						}
						else
						{
							//comment: color의 line interpolation 식으로 rgb값을 구한다. 
							color[0] = r_lineinter[0]*x+r_lineinter[1]*y+r_lineinter[2];
							color[1] = g_lineinter[0]*x+g_lineinter[1]*y+g_lineinter[2];
							color[2] = b_lineinter[0]*x+b_lineinter[1]*y+b_lineinter[2];
						}
						
						if( depthTestEnabled )
						{
							//comment: depthtest를 하므로 각 점에서의 z값을 z의 line_interpolation을 이용해 구한다. 
							z = z_lineinter[0]*x+z_lineinter[1]*y+z_lineinter[2];
							
							//comment: 원래있던 frameBuffer의 z값보다 작으면 그 점이 z-buffer보다 앞에 있다는 것이므로
							//		   그 점에 색을 입힌 후 framebuffer에 새로운 z값을 저장한다. 
							if (z<frameBuffer.GetDepth( x, y))
							{
								frameBuffer.SetPixel( x, y, color);
								frameBuffer.SetDepth( x, y, z);
							}
						}
						else
						{
							//comment: depthtest를 하지 않으므로 z값과 관계없이 색을 입힌다. 
							frameBuffer.SetPixel( x, y, color );
						}
						
			}

		}
	}

	return true;
}

