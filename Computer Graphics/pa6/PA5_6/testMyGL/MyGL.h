#ifndef MYGL_H
#define MYGL_H

#include "GLRenderer.h"

/**
Rendering Pipeling 의 일부를 이 클래스에 구현해야 한다.
*/
//==============================================================================
class MyGL : public GLRenderer
//==============================================================================
{
public:
  MyGL();
  ~MyGL();

  // Methods to turn the various stages on/off
  bool GetDoLighting()            { return _doLighting; }
  void SetDoLighting( bool v )    { _doLighting = v; }
  bool GetDoClipping()            { return _doClipping; }
  void SetDoClipping( bool v )    { _doClipping = v; }
  bool GetDoTriangulate()         { return _doTriangulate; }
  void SetDoTriangulate( bool v ) { _doTriangulate = v; }
  bool GetDoRasterize()           { return _doRasterize; }
  void SetDoRasterize( bool v )   { _doRasterize = v; }
  bool GetDoBackCulling()           { return _doBackCulling; }
  void SetDoBackCulling( bool v )   { _doBackCulling = v; }

protected:
	// 아래 4개의 함수들은 GLRenderer::processPolygon() 에서 호출된다. 
  virtual bool ComputeLighting( vector<GLVertex>& verts );
  virtual bool ClipPolygon( const vector<GLVertex>& vertsIn, vector<GLVertex>& vertsOut );
  virtual bool TriangulatePolygon( const vector<GLVertex>& polygonVerts, 
                                   vector<GLVertex>& triangleVerts );
  virtual bool RasterizeTriangle( GLVertex triVerts[3] );

private:
  bool _doLighting;
  bool _doClipping;
  bool _doTriangulate;
  bool _doRasterize;
  bool _doBackCulling;
};

#endif // MYGL_H