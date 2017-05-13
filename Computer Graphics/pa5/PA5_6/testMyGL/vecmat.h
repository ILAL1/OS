////////////////////////////////////////////////////////////////////////////////
//
// Matrix and generalized vector routines
//
//   Based in part on glh_linear.h by Cass Everitt
//
////////////////////////////////////////////////////////////////////////////////
#ifndef VECMAT_H
#define VECMAT_H

#define _USE_MATH_DEFINES
#include <math.h>

//==============================================================================
template < int N, class T >
class vec
//==============================================================================
{
public:
  T v[N];

  // Constructors
  vec() { set( T() ); }
  vec( const T* pT )
  { for( int i=0; i < N; i++ ) (*this).v[i] = pT[i]; }
  vec( const vec& u )
  { for( int i=0; i < N; i++ ) (*this).v[i] = u.v[i]; }


  // Conversions to T*
  operator T*() { return v; }
  operator const T*() const { return v; }

  // Array operator
  T & operator [] ( int i )              { return v[i]; }
  const T & operator [] ( int i ) const  { return v[i]; }

  // Assignment
  vec<N,T>& set( T s )
  { for(int i = 0; i < N; i++) v[i] = s; return *this; }

  vec<N,T>& set( const T* pT )
  { *this = *(vec<T,N>*)pT; return *this; }

  void get( const T* pT ) const
  { for(int i = 0; i < N; i++) pT[i] = v[i]; }

  vec<N,T> & operator += ( const vec<N,T>& u )
  { for(int i = 0; i < N; i++) (*this).v[i] += u.v[i]; return *this;}
  
  vec<N,T> & operator -= ( const vec<N,T>& u )
  { for(int i = 0; i < N; i++) (*this).v[i] -= u.v[i]; return *this;}

  vec<N,T> & operator *= ( T s )
  { for(int i = 0; i < N; i++) v[i] *= s; return *this;}
  
  vec<N,T> & operator *= ( const vec<N,T> & u )
  { for(int i = 0; i < N; i++) (*this).v[i] *= u.v[i]; return *this;}
  
  vec<N,T> & operator /= ( T s )
  { if(s == 0) return *this; for(int i = 0; i < N; i++) v[i] /= s; return *this;}

  
  // unary operators
  vec<N,T> operator - () const
  { vec<N,T> rv = v; rv.negate(); return rv; }


  // binary operators
  vec<N,T> operator + ( const vec<N,T> &v) const
  { vec<N,T> ret(*this); return ret += v; }
  
  vec<N,T> operator - ( const vec<N,T> &v) const
  { vec<N,T> ret(*this); return ret -= v; }
  
  vec<N,T> operator * ( T s ) const                 // scalar multiply
  { vec<N,T> ret(*this); return ret *= s; }

  vec<N,T> operator * ( const vec<N,T> &u ) const   // component-wise multiply
  { vec<N,T> ret(*this); return ret *= u; }

  vec<N,T> operator / ( T s ) const                 // scalar divide
  { vec<N,T> ret(*this); return ret /= s; }


  //----------------------------
  // misc functions
  //----------------------------
  vec<N,T>& negate()
  { for( int i=0; i < N; i++ ) v[i] = -v[i]; return *this;  }

  T dot( const vec<N,T>& u ) const
  { T sum(0); for( int i=0; i < N; i++ ) sum += (*this).v[i]*u.v[i]; return sum; }

  T length() const { return sqrt( this->dot( *this ) ); }
  
  T normalize()
  { T len = length(); if( len > 0 ) *this /= len; return len; }

  // sets (*this)[i] = min( (*this[i], u[i] )
  void  minimize( const vec<N,T> & u )
  { for(int i = 0; i < N; i++) if( u.v[i] < (*this).v[i] ) (*this).v[i] = u.v[i]; }

  // sets (*this)[i] = max( (*this[i], u[i] )
  void  maximize( const vec<N,T> & u )
  { for(int i = 0; i < N; i++) if( u.v[i] > (*this).v[i] ) (*this).v[i] = u.v[i]; }

  // returns <1/(*this)[0], 1/(*this[1]), ... >
  vec<N,T> inverse() const
  { vec<N,T> rv; for(int i = 0; i < N; i++) rv.v[i] = 1.0f / (*this).v[i]; return rv;}
};

// additional binary operators on vec<N,T>

template <int N, class T> inline
vec<N,T> operator * ( T s, const vec<N,T> & u )                  // s * v
{ return u*s; }

template <int N, class T> inline
bool operator == ( const vec<N,T> & v1, const vec<N,T> & v2 )    // v1 == v2
{
  for(int i = 0; i < N; i++)
    if(v1.v[i] != v2.v[i]) return false;
  return true;
}

template <int N, class T> inline
bool operator != ( const vec<N,T> & v1, const vec<N,T> & v2 )    // v1 != v2
{ return !(v1 == v2); }


//==============================================================================
template< class T > 
class vec3 : public vec<3,T>
//==============================================================================
{
public:
  vec3() : vec<3,T>() {}
  vec3(const vec<3,T>& u) : vec<3,T>(u) {}
  vec3(const T* pT) : vec<3,T>(pT)    {}
  vec3( T x, T y, T z) { v[0] = x; v[1] = y; v[2] = z; }
  
  vec3 cross( const vec3 &u ) const
  {
    vec3 ret;
    ret.v[0] = (*this).v[1]*u.v[2]-(*this).v[2]*u.v[1];
    ret.v[1] = (*this).v[2]*u.v[0]-(*this).v[0]*u.v[2];
    ret.v[2] = (*this).v[0]*u.v[1]-(*this).v[1]*u.v[0];  
    return ret;
  }
      
  vec3& set( T x, T y, T z )
  { v[0] = x; v[1] = y; v[2] = z; return *this; }  
};


//==============================================================================
template< class T > 
class vec4 : public vec<4,T>
//==============================================================================
{
public:
  vec4() : vec<4,T>() {}
  vec4(const vec<4,T>& u) : vec<4,T>(u) {}  
  vec4(const T* pT) : vec<4,T>(pT)    {}
  vec4( T x, T y, T z, T w) { v[0] = x; v[1] = y; v[2] = z; v[3] = w; }
  vec4(const vec<3,T>& u, T w ) 
  { v[0] = u.v[0]; v[1] = u.v[1]; v[2] = u.v[2]; v[3] = w; }
  
  vec4& set( T x, T y, T z, T w )
  { v[0] = x; v[1] = y; v[2] = z; v[3] = w; return *this; }  
};


//==============================================================================
template < int R, int C, class T >
class matrix
//==============================================================================
{
public:      
  T m[R * C];

  matrix() { identity(); }
  matrix( const T* pT )
  { set(pT); }
  matrix( const matrix<R,C,T>& M )
  { *this = M;  }
  
  // Conversion to T*     
  operator T*()                            
  { return m; }
  operator const T*() const               
  { return m; }

  T & operator () (int r, int c)             { return m[c*R+r]; }
  const T & operator () (int r, int c) const { return m[c*R+r]; }
  
  void get( T* pT ) const
  {
    int pos = 0;
    for(int c=0; c < C; c++)
      for(int r=0; r < R; r++)
        pT[pos++] = (*this)(r,c);
  }
    
  void set( const T* pT )
  {
    int pos = 0;
    for(int c=0; c < C; c++)
      for(int r=0; r < R; r++)
         (*this)(r,c) = pT[pos++];
  }
     
  void set( T s )
  {
    int pos = 0;
    for(int c=0; c < C; c++)
      for(int r=0; r < R; r++)
         (*this)(r,c) = s;
  }  

  void identity()
  {
    set( T(0) ); 
    for( int i=0; i < C && i < R; i++ )
      (*this)(i,i) = T(1);
  }
 
  void setRow(int r, const vec<C,T>& v )
  { for(int i=0; i < C; i++ ) (*this)(r,i) = v.v[i]; }

  void getRow(int r, vec<C,T>& v ) const
  { for(int i=0; i < C; i++ ) v.v[i] = (*this)(r,i); }

  vec<C,T> getRow( int r ) const
  { vec<C,T> ret; getRow(r, ret); return ret; }

  void setCol(int c, const vec<R,T>& v )
  { for(int i=0; i < C; i++ ) (*this)(i,c) = v.v[i]; }
  
  void getCol(int c, vec<R,T>& v ) const
  { for(int i=0; i < R; i++ ) v.v[i] = (*this)(i,c); }

  vec<R,T> getCol( int c ) const
  { vec<R,T> ret; getCol(c, ret); return ret; }
  
  
  matrix<C,R,T> transpose() const
  {
    matrix<C,R,T> M;
    for(int c=0; c < C; c++)
      for(int r=0; r < R; r++)
         M(c,r) = (*this)(r,c);
    return M;
  }

  void multMatrixVec( const vec<C,T>& src, vec<R,T>& dest ) const
  {
    for(int r=0; r < R; r++)
    {
      dest.v[r] = 0;
      for(int c=0; c < C; c++)
        dest.v[r] += (*this)(r,c) * src.v[c];
    }
  }

  void multVecMatrix( const vec<R,T>& src, vec<C,T>& dest ) const
  {
    for(int c=0; c < C; c++)
    {
      dest.v[c] = 0;
      for(int r=0; r < R; r++)
        dest.v[c] += (*this)(r,c) * src.v[r];
    }
  }
  
  // Assignment operators
  matrix<R,C,T>& operator += ( const matrix<R,C,T>& M )
  {
    for (int i = 0; i < R*C; i++)
      (*this).m[i] += M.m[i];
    return *this;
  }

  matrix<R,C,T>& operator -= ( const matrix<R,C,T>& M )
  {
    for (int i = 0; i < R*C; i++)
      (*this).m[i] -= M.m[i];
    return *this;
  }

  matrix<R,C,T>& operator *= ( T s )
  {
    for (int i = 0; i < R*C; i++)
      (*this).m[i] *= s;
    return *this;
  }

  matrix<R,C,T>& operator /= ( T s )
  {
    for (int i = 0; i < R*C; i++)
      (*this).m[i] /= s;
    return *this;
  }


  // binary operators
  matrix<R,C,T> operator + ( const matrix<R,C,T>& M ) const
  {  matrix ret = *this;  ret += M;  return ret;}

  matrix<R,C,T> operator - ( const matrix<R,C,T>& M ) const
  {  matrix ret = *this;  ret -= M;  return ret;}

  matrix<R,C,T> operator * ( T s ) const
  {  matrix ret = *this;  ret *= s;  return ret;}

  vec<C,T> operator * ( const vec<R,T>& v ) const
  { vec<C,T> ret; (*this).multMatrixVec( v, ret ); return ret; }

  matrix<R,C,T> operator / ( T s ) const
  {  matrix ret = *this;  ret /= s;  return ret;}

};

// misc binary operators
template < int R, int C, class T > inline 
matrix<R,C,T> operator * ( T s, const matrix<R,C,T>& M )            // s * M
{  matrix<R,C,T> ret = M;  ret *= s;  return ret;}

template < int R, int C, class T > inline 
vec<R,T> operator * ( const vec<C,T>& v, const matrix<R,C,T>& M  )  // v * M
{ vec<R,T> ret; M.multVecMatrix( v, ret ); return ret; }

template < int R, int I, int C, class T > inline 
matrix<R,C,T> operator * ( const matrix<R,I,T>& M1, const matrix<I,C,T>& M2  )  // M1 * M2
{ 
  matrix<R,C,T> ret; 
  for( int r=0; r < R; r++ )
    for( int c=0; c < C; c++ )
      for( int i=0; i < I; i++ )
        ret(r,c) = M1(r,i)*M2(i,c);
  return ret;
}

template < int R, int C, class T > inline 
bool operator ==( const matrix<R,C,T>& M1, const matrix<R,C,T>& M2 )// M1 == M2
{
  for( int r = 0; r < R; r++ )
    for( int c = 0; c < C; c++ )
      if( M1(r,c) != M2(r,c) ) return false;
  return true;
}

template < int R, int C, class T > inline 
bool operator !=( const matrix<R,C,T>& M1, const matrix<R,C,T>& M2 )// M1 != M2
{ return !(M1 == M2); }


//==============================================================================
template < class T >
class matrix4 : public matrix<4,4,T>
//==============================================================================
{
public:        
  matrix4(  T m00=1, T m01=0, T m02=0, T m03 = 0,
            T m10=1, T m11=0, T m12=0, T m13 = 0,
            T m20=1, T m21=0, T m22=0, T m23 = 0,
            T m30=1, T m31=0, T m32=0, T m33 = 0 )
  {
    (*this)(0,0) = m00;  (*this)(0,1) = m01;  (*this)(0,2) = m02; (*this)(0,3) = m03;
    (*this)(1,0) = m10;  (*this)(1,1) = m11;  (*this)(1,2) = m12; (*this)(1,3) = m13;
    (*this)(2,0) = m20;  (*this)(2,1) = m21;  (*this)(2,2) = m22; (*this)(2,3) = m23;
    (*this)(3,0) = m30;  (*this)(3,1) = m31;  (*this)(3,2) = m32; (*this)(3,3) = m33;
  }
  matrix4( const T* pT )
  { set(pT); }
  matrix4( const matrix4<T>& M )
  { set(M.m); }
  matrix4( const matrix<4,4,T>& M )
  { set(M.m); }

  matrix4 inverse()
  {
    matrix4 Minv;
    
    T r1[8], r2[8], r3[8], r4[8];
    T *s[4], *tmprow;
    
    s[0] = &r1[0];
    s[1] = &r2[0];
    s[2] = &r3[0];
    s[3] = &r4[0];
    
    register int i,j,p,jj;
    for(i=0;i<4;i++)
    {
        for(j=0;j<4;j++)
        {
            s[i][j] = (*this)(i,j);
            if(i==j) s[i][j+4] = 1.0;
            else     s[i][j+4] = 0.0;
        }
    }
    T scp[4];
    for(i=0;i<4;i++)
    {
        scp[i] = T(fabs(s[i][0]));
        for(j=1;j<4;j++)
            if(T(fabs(s[i][j])) > scp[i]) scp[i] = T(fabs(s[i][j]));
            if(scp[i] == 0.0) return Minv; // singular matrix!
    }
    
    int pivot_to;
    T scp_max;
    for(i=0;i<4;i++)
    {
        // select pivot row
        pivot_to = i;
        scp_max = T(fabs(s[i][i]/scp[i]));
        // find out which row should be on top
        for(p=i+1;p<4;p++)
            if(T(fabs(s[p][i]/scp[p])) > scp_max)
            { scp_max = T(fabs(s[p][i]/scp[p])); pivot_to = p; }
            // Pivot if necessary
            if(pivot_to != i)
            {
                tmprow = s[i];
                s[i] = s[pivot_to];
                s[pivot_to] = tmprow;
                T tmpscp;
                tmpscp = scp[i];
                scp[i] = scp[pivot_to];
                scp[pivot_to] = tmpscp;
            }
            
            T mji;
            // perform gaussian elimination
            for(j=i+1;j<4;j++)
            {
                mji = s[j][i]/s[i][i];
                s[j][i] = 0.0;
                for(jj=i+1;jj<8;jj++)
                    s[j][jj] -= mji*s[i][jj];
            }
    }
    if(s[3][3] == 0.0) return Minv; // singular matrix!
    
    //
    // Now we have an upper triangular matrix.
    //
    //  x x x x | y y y y
    //  0 x x x | y y y y 
    //  0 0 x x | y y y y
    //  0 0 0 x | y y y y
    //
    //  we'll back substitute to get the inverse
    //
    //  1 0 0 0 | z z z z
    //  0 1 0 0 | z z z z
    //  0 0 1 0 | z z z z
    //  0 0 0 1 | z z z z 
    //
    
    T mij;
    for(i=3;i>0;i--)
    {
        for(j=i-1;j > -1; j--)
        {
            mij = s[j][i]/s[i][i];
            for(jj=j+1;jj<8;jj++)
                s[j][jj] -= mij*s[i][jj];
        }
    }
    
    for(i=0;i<4;i++)
        for(j=0;j<4;j++)
            Minv(i,j) = s[i][j+4] / s[i][i];
        
    return Minv;
  }

  void multVec3Matrix( const vec3<T>& src, vec3<T>& dest ) const
  {
    dest[0] = src[0]*(*this)(0,0) + src[1]*(*this)(1,0) + src[2]*(*this)(2,0);
    dest[1] = src[0]*(*this)(0,1) + src[1]*(*this)(1,1) + src[2]*(*this)(2,1);
    dest[2] = src[0]*(*this)(0,2) + src[1]*(*this)(1,2) + src[2]*(*this)(2,2);
  }
   
  void multMatrixVec3( const vec3<T>& src, vec3<T>& dest ) const
  {
    dest[0] = (*this)(0,0)*src[0] + (*this)(0,1)*src[1] + (*this)(0,2)*src[2];
    dest[1] = (*this)(1,0)*src[0] + (*this)(1,1)*src[1] + (*this)(1,2)*src[2];
    dest[2] = (*this)(2,0)*src[0] + (*this)(2,1)*src[1] + (*this)(2,2)*src[2];
  }
  
  vec3<T> multVec3Matrix( const vec3<T>& src ) const
  { vec3<T> ret; multVec3Matrix( src, ret ); return ret; }
    
  vec3<T> multMatrixVec3( const vec3<T>& src ) const
  { vec3<T> ret; multMatrixVec3( src, ret ); return ret; }
};



typedef matrix4<float> matrix4f;
typedef vec3<float> vec3f;
typedef vec4<float> vec4f;
typedef matrix4<double> matrix4d;
typedef vec3<double> vec3d;
typedef vec4<double> vec4d;



#endif // VECMAT_H