#ifndef _SpatialVector_h
#define _SpatialVector_h
//#     Filename:       SpatialVector.h
//#
//#     Standard 3-d vector class
//#
//#
//#     Author:         Peter Z. Kunszt, based on A. Szalay's code
//#     
//#     Date:           October 15, 1998
//#
//#
//#
//# (c) Copyright The Johns Hopkins University 1998
//# All Rights Reserved
//#
//# The software and information contained herein are proprietary to The
//# Johns Hopkins University, Copyright 1998.  This software is furnished
//# pursuant to a written license agreement and may be used, copied,
//# transmitted, and stored only in accordance with the terms of such
//# license and with the inclusion of the above copyright notice.  This
//# software and information or any other copies thereof may not be
//# provided or otherwise made available to any other person.
//#
//#
#include <math.h>
#include <stdio.h>
#include <iostream.h>
#include "SpatialGeneral.h"

//########################################################################
/**

   The SpatialVector is a 3D vector usually living on the surface of
   the sphere. The corresponding ra, dec can be obtained if the vector
   has unit length. That can be ensured with the normalize() function.

*/

class LINKAGE SpatialVector {
public:
  /// constructs (1,0,0), ra=0, dec=0.
  SpatialVector();

  /// Constructor from three coordinates, not necessarily normed to 1
  SpatialVector(float64 x,
		float64 y,
		float64 z);

  /// Constructor from ra/dec, this is always normed to 1
  SpatialVector(float64 ra,
		float64 dec);

  /// Copy constructor
  SpatialVector(const SpatialVector &);

  /// Assignment
  SpatialVector& operator =(const SpatialVector &);

  /// Set member function: set values - always normed to 1
  void set(const float64 &x,
	   const float64 &y,
	   const float64 &z);

  /// Set member function: set values - always normed to 1
  void set(const float64 &ra,
	   const float64 &dec);

  /// Get x,y,z
  void get( float64 &x,
	    float64 &y,
	    float64 &z) const;

  /// Get ra,dec - normalizes x,y,z
  void get( float64 &ra,
	    float64 &dec);

  /// return length of vector
  float64 length() const;

  /// return x (only as rvalue)
  float64 x() const;

  /// return y
  float64 y() const;

  /// return z
  float64 z() const;

  /// return ra - this norms the vector to 1 if not already done so
  float64 ra();

  /// return dec - this norms the vector to 1 if not already done so
  float64 dec();

  /// Normalize vector length to 1
  void normalize();

  /// Printf it to stdout
  void show() const;

  /// Read vector from a stream
  void read(istream &);

  /// Write vector to a stream
  void write(ostream &) const;

  /// Comparison
  int operator ==(const SpatialVector & ) const;

  /// dot product
  float64 operator *(const SpatialVector & ) const;

  /// cross product
  SpatialVector operator ^(const SpatialVector & ) const;

  /// addition
  SpatialVector operator +(const SpatialVector & ) const;

  /// subtraction
  SpatialVector operator -(const SpatialVector & ) const;

  /**@name Scalar products with int and float */
  //@{
  /**@name operator *= */
  SpatialVector & operator *=(float64);
  SpatialVector & operator *=(int);
  friend SpatialVector operator *(float64, const SpatialVector &);
  friend SpatialVector operator *(int,     const SpatialVector &);
  friend SpatialVector operator *(const SpatialVector &, float64);
  friend SpatialVector operator *(const SpatialVector &, int);
  //@}

private:
  float64 x_;
  float64 y_;
  float64 z_;
  float64 ra_;
  float64 dec_;
  bool okRaDec_;

  void updateXYZ();
  void updateRaDec();

  friend class SpatialIndex;
  friend class SpatialDomain;
  friend class sxSpatialDomain;
};

#include "SpatialVector.hxx"

#endif

