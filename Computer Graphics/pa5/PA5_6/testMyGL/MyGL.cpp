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

	// Ŭ������ ��� ���ο� position ���� �����ð��̳� �����̵忡 �����ִ� ������� �ϸ� �˴ϴ�.
	// �̶� GLVertex �� �ִ� �ٸ� ���� color, normal, texture coordinate �� ���� ���� ������� �մϴ�.

	// implement cliping. you can use method that is learned in the class.
	// other values in GLVertex calculate the same way. (color, normal, texture coordinat)
	// just copy the polygon so we can get to the other parts of the pipeline

	

	vertsOut = vertsIn;
	for (int h=0; h<4; h++)
	{
		int pos = (int)vertsOut.size();
		int verts_size_back_up = (int)vertsOut.size();
		float Tx, Ty, Tz, Td;

		//comment : ȭ���� 4���� edge�� ���� ����� �����Ѵ�. 
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
		//comment : vertsOut�� �ִ� ���鿡 ���� ������ ����
		for(int i=0; i < (int)vertsOut.size(); i++ )
		{
			//comment : vertsOut�� �ִ� ���鿡 ���� �⺻���� ������ �޴´�. 
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
			//comment : �̿��ϰ� �ִ� ���� p0�� �ϰ� p0�� �⺻���� ������ �޴´�. p1�� ���� ù��° ���϶����� p0�� ������ ���� �ȴ�. 
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
			//comment : p1�� ������ ������ ȭ���� edge�� �ϳ����� �ȿ� �ְ� p0�� �ۿ� �ִ� ���ų� Ȥ�� �� �ݴ��� ��쿡�� ������ �����Ѵ�. 
			if ((p1x*Tx+p1y*Ty+p1z*Tz+p1w*Td > 0 && p0x*Tx+p0y*Ty+p0z*Tz+p0w*Td < 0) || (p1x*Tx+p1y*Ty+p1z*Tz+p1w*Td < 0 && p0x*Tx+p0y*Ty+p0z*Tz+p0w*Td > 0))
			{
				//comment : p1�� p0�� �մ� edge�� ȭ���� edge�� ������ new_p1�̶� ����. 
				float t;
				float Tp0, Tp1p0;
				float new_p1x, new_p1y, new_p1z, new_p1w;
				float new_c1x, new_c1y, new_c1z, new_c1w;
				float new_t1x, new_t1y;
				float new_n1x, new_n1y, new_n1z;
				//comment : new_p1�� ���ϱ� ���� t���� ���Ѵ�. 
				Tp0 = p0x*Tx+p0y*Ty+p0z*Tz+p0w*Td;
				Tp1p0 = (p1x-p0x)*Tx+(p1y-p0y)*Ty+(p1z-p0z)*Tz+(p1w-p0w)*Td;
				//comment : tp1p0�� 0�� ��찡 Ȥ�� ������ �𸣴� ������ ���ϱ� ���� �̷��� �Ͽ���. 
				if (Tp1p0 != 0)
				{
					//comment : t�� �̿��Ͽ� ������ �� ������ �÷��� �ؽ�Ʈ, �븻�� ���Ѵ�. 
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
					//comment : new_p1�� vertsout�� �ֱ� ���� ������ �����. p1�� p1 �ڿ� �ִ� ��� ���� ��ĭ�� �ڷ� �ű��. 
					vertsOut.resize(pos+1);
					for (int j=(int)vertsOut.size()-1; j>i; j--)
					{
						vertsOut[j]= vertsOut[j-1];
					}
					//comment : new_p1�� ������ p1�ڸ��� �ִ´�. 
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

		//comment : vertsOut�� �� ������ �ٲ��� �ʾҴٴ� ���� clipping�� ���̾����ٴ� ���̰� 
		//			vertsOut �� 1���̶� ȭ���� edge�ٱ��� �ִٸ� �� ������ ȭ�� �ٱ��� �����Ƿ� vertOut�� �� ������ 0���� ���� redering ��Ű�� �ʴ´�. 
		if (verts_size_back_up == (int)vertsOut.size() &&  vertsOut[0].position[0]*Tx+vertsOut[0].position[1]*Ty+vertsOut[0].position[2]*Tz+vertsOut[0].position[3]*Td> 0)
		{
			vertsOut.resize(0);
			return true;
		}
		else
		{
			//comment: ���� ��찡 �ƴ϶�� ���� ������ ȭ�鿡 �����ְų� ȭ�� �ȿ� �ִٴ� ���̹Ƿ� rendering�� �Ѵ�.
			//		   �׷��� �� �� ������ Ŭ������ �ϰ� ���ο� ������ ����� ���� ȭ�� �ۿ� �ִ� ���� ������ �ʾҴ�.
			//         ���� ������ ���鼭 ȭ�� �ۿ� �ִ� ���� �����. 
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

	// ���⿡ �ﰢȭ(Triangulate) �� ���� �Ѵ�.
	// F1 Ű�� ������ ������ �簢���� ���� �ﰢȭ(triangulize) ����� ���� �ٸ��� ��Ÿ���µ�,
	// ũ�� �Ű澲�� �ʾƵ� �˴ϴ�.
	// �簢���� ���� ����� ä���������� ���� �� �� �ֽ��ϴ�.

	// implement Triangulate
	// if you press F1, a box is appeared
	// the box's color will be depend on your implementation. 
	// Maybe it is different from hardware rendering. you don't need to worry about it.
	// but you must fill color right way. you can know if you see your result.
	// 


	// just make a triangle out of the first three vertices for now

	//comment: polygonVerts.size()�� 0�̸� �ﰢȭ�Ұ� �����Ƿ� �ٷ� ����������. 
	if (polygonVerts.size() == 0)
	{
		return true;	
	}
	//comment: polygonVerts�� �ִ� ���鿡 ���� �ﰢȭ�� �Ѵ�.  
	for (int i=0; i < polygonVerts.size()-1; i++)
	{
		if (_doBackCulling)
		{
			//comment: ���� backCulling�� �ϸ� �켱 �� �ﰢ���� area�� ���Ѵ�. 
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
			//comment: �� area�� 0���� ������ back-face�̹Ƿ� 0�ʰ��� area�� ������ �ﰢ���� push_back�Ѵ�. 
			if (area>0)
			{
				triangleVerts.push_back( polygonVerts[0] );
				triangleVerts.push_back( polygonVerts[i]);
				triangleVerts.push_back( polygonVerts[i+1]);
			}
		}
		else
		{
			//comment: area�� ������� �� ����ִ´�. 
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



	// ���⿡ Rasterization �� �����ϸ� �ȴ�.
	// w Ű�� �������� ��Ÿ���� wire mode �� ������ �ʿ� ����.

	// Implement rasterization 
	// you don't need to implement wire mode which is appeared if you press key 'w'

	// just render the vertices for now
	
	//comment: triangulation���� 3���� ���� ���� ���� ������ �޴´�. 
	GLVertex& v1 = verts[0];
	GLVertex& v2 = verts[1];
	GLVertex& v3 = verts[2];
	double edge1[3],edge2[3],edge3[3];
	RGBAColor color1 = v1.color;
	RGBAColor color2 = v2.color;
	RGBAColor color3 = v3.color;
	//comment: �ﰢ���� edge ���� ���Ѵ�. 
	edge1[0] = v1.position[1]-v2.position[1];
	edge1[1] = v2.position[0]-v1.position[0];
	edge1[2] = v1.position[0]*v2.position[1]-v2.position[0]*v1.position[1];
	edge2[0] = v2.position[1]-v3.position[1];
	edge2[1] = v3.position[0]-v2.position[0];
	edge2[2] = v2.position[0]*v3.position[1]-v3.position[0]*v2.position[1];
	edge3[0] = v3.position[1]-v1.position[1];
	edge3[1] = v1.position[0]-v3.position[0];
	edge3[2] = v3.position[0]*v1.position[1]-v1.position[0]*v3.position[1];
	
	//comment: �ﰢ���� area�� ���Ѵ�. �̻��ϰ� �� �Ŀ��� �� �����ϸ� ������ ���� ar1~arg6���� ������ ����. 
	double ar1 = v2.position[0]*v3.position[1];
	double ar2 = v3.position[0]*v2.position[1];
	double ar3 = v1.position[0]*v3.position[1];
	double ar4 = v3.position[0]*v1.position[1];
	double ar5 = v1.position[0]*v2.position[1];
	double ar6 = v2.position[0]*v1.position[1];
	double area = (ar1-ar2)-(ar3-ar4)+(ar5-ar6);

	//comment: color�� z�� line interpolation�� ���Ѵ�. 
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

	//comment: rasterization�� �ϴ� ������ ���ϱ� ���� w,h�� �˾Ƴ���. 
	int w, h;
	frameBuffer.GetSize(w,h);
	RGBAColor color;
	using std::max;
	using std::min;
	float z;
	//comment: rasterization������ ������ ���̱� ���� �ﰢ���� ���� �� �´� �簢���� ���� ���Ѵ�. 
	double w1 = max(max(v1.position[0],v2.position[0]), v3.position[0]);
	double w2 = min(min(v1.position[0],v2.position[0]), v3.position[0]);
	double h1 = max(max(v1.position[1],v2.position[1]), v3.position[1]);
	double h2 = min(min(v1.position[1],v2.position[1]), v3.position[1]);

	//comment: ������ ���� �簢�� ������ rasterization�� �Ѵ�. 
	for (int x=w2; x<w1; x++)
	{
		for (int y=h2; y<h1; y++)
		{
			bool t1, t2, t3;
			//comment: shared edge�� ���� tie breaking�� �ϱ� ���� t1�� �����Ѵ�. ppt�� �ִ� ��� �߾��; 
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

			//comment: �ﰢ�� ���� �ִ� ���� ���� �׸��� �ﰢ���� edge ���� �ִ� ���� tie breaking�� ����� ���� rasterization�� �Ѵ�. 
			if  ((edge1[0]*x+edge1[1]*y+edge1[2]>0) || (edge1[0]*x+edge1[1]*y+edge1[2]==0 && t1))
				if ((edge2[0]*x+edge2[1]*y+edge2[2]>0) || (edge2[0]*x+edge2[1]*y+edge2[2]==0 && t2))
					if ((edge3[0]*x+edge3[1]*y+edge3[2]>0) || (edge3[0]*x+edge3[1]*y+edge3[2]==0 && t3))
					{
						//comment: texture ����̸� texture�� ���� ������ �����´�. 
						if( textureEnabled  )
						{
							if( texture.id != 0 )
							{
								double argx=1;
								double argy=1;

								//comment: texture�� ȭ�鿡 �������� �� ȭ��� ���� �ִ� ���� texture������ ���Ѵ�. 
								//		   ȭ�鿡 �����ְ� �Ǹ� texCoord�� 0�� 1�����̱� ������ �� ������ ȭ�鿡 �����ִ��� �ƴ����� �Ǵ��ߴ�.
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

								//comment: ȭ����� ���� texture�󿡼� ��� ������ mapping�Ǵ��� ���ϴ� �����̴�. 
								//		   ������ ȭ�鿡 ���� ���� ������ texcoorx�� texcoory�� texture���� ��ġ�� �ȴ�. 
								//		   ������������ x y ��ġ�� ���� �� ������ �� ���� ũ��� �������� ����. 
								double texcoorx = (x-w2)/(w1-w2);
								double texcoory = (y-h2)/(h1-h2);

								//comment: argx�� argy�� 1�� �ƴ϶�� ���� ������ ȭ�鿡 �����ִٴ� ��. 
								//		   argx�� �̿��� x y�� ��ġ�� texture���� ��ġ�� �ٲ۴�. 
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
								//comment: texture���� ��ġ�� ������ texture�� ��ġ�� �ִ� ���� ���Ѵ�. 
								int a = texcoorx * (texture.width - 1);
								int b = texcoory * (texture.height - 1);
								color = texture.GetPixel( a, b, 0 );
							}
						}
						else
						{
							//comment: color�� line interpolation ������ rgb���� ���Ѵ�. 
							color[0] = r_lineinter[0]*x+r_lineinter[1]*y+r_lineinter[2];
							color[1] = g_lineinter[0]*x+g_lineinter[1]*y+g_lineinter[2];
							color[2] = b_lineinter[0]*x+b_lineinter[1]*y+b_lineinter[2];
						}
						
						if( depthTestEnabled )
						{
							//comment: depthtest�� �ϹǷ� �� �������� z���� z�� line_interpolation�� �̿��� ���Ѵ�. 
							z = z_lineinter[0]*x+z_lineinter[1]*y+z_lineinter[2];
							
							//comment: �����ִ� frameBuffer�� z������ ������ �� ���� z-buffer���� �տ� �ִٴ� ���̹Ƿ�
							//		   �� ���� ���� ���� �� framebuffer�� ���ο� z���� �����Ѵ�. 
							if (z<frameBuffer.GetDepth( x, y))
							{
								frameBuffer.SetPixel( x, y, color);
								frameBuffer.SetDepth( x, y, z);
							}
						}
						else
						{
							//comment: depthtest�� ���� �����Ƿ� z���� ������� ���� ������. 
							frameBuffer.SetPixel( x, y, color );
						}
						
			}

		}
	}

	return true;
}

