package com.stefperf.impatient.chapter11

class Matrix(val nRows: Int, val nCols: Int, initElements: Seq[Double]) {
  if (nRows < 1 || nCols < 1)
    throw new IllegalArgumentException(s"Both nRows and nCols must be greater than zero")
  private val _arr = new Array[Double](nRows * nCols)
  val nElements: Int = nRows * nCols
  if (nElements != initElements.length)
    throw new IllegalArgumentException(s"Needed $nElements elements, ${initElements.length} were provided")
  initElements.copyToArray(_arr)
  val rowIndices: Seq[Int] = 0 until nRows
  val colIndices: Seq[Int] = 0 until nCols

  def elements: Seq[Double] = _arr.clone()

  def copy(new_nRows: Int = nRows, new_nCols: Int = nCols, new_elements: Seq[Double] = _arr): Matrix =
    new Matrix(new_nRows, new_nCols, new_elements)

  def apply(row: Int, col: Int): Double = _arr(row * nCols + col)

  def update(row: Int, col: Int, newValue: Double): Unit = _arr(row * nCols + col) = newValue

  override def toString: String =
    s"[${
      (for (r <- rowIndices) yield s"[${
        (for (c <- colIndices; i = r * nCols + c) yield _arr(i).formatted("%.3f")).mkString(", ")
      }]").mkString(",\n ")
    }]"

  // arbitrary element-wise binary operation between same-shaped matrices
  def elementWiseBinOp(that: Matrix, binOp: (Double, Double) => Double): Matrix = {
    if (nRows != that.nRows || nCols != that.nCols)
      throw new IllegalArgumentException("element-wise binary operations require same-shaped matrices")
    val result = Matrix.default(nRows, nCols)
    for (r <- rowIndices; c <- colIndices; i = r * nCols + c)
      result._arr(i) = binOp(_arr(i), that._arr(i))
    result
  }

  // element-wise arithmetic operators between same-shaped matrices
  def +(that: Matrix): Matrix = elementWiseBinOp(that, _ + _)
  def -(that: Matrix): Matrix = elementWiseBinOp(that, _ - _)
  def *(that: Matrix): Matrix = elementWiseBinOp(that, _ * _)
  def /(that: Matrix): Matrix = elementWiseBinOp(that, _ / _)

  // arbitrary binary operation between matrix and scalar
  def matrixAndScalarBinOp(scalar: Double, binOp: (Double, Double) => Double): Matrix = {
    val result = Matrix.default(nRows, nCols)
    for (r <- rowIndices; c <- colIndices; i = r * nCols + c)
      result._arr(i) = binOp(_arr(i), scalar)
    result
  }

  // arithmetic operators between matrix and scalar
  def +(scalar: Double): Matrix = matrixAndScalarBinOp(scalar, _ + _)
  def -(scalar: Double): Matrix = matrixAndScalarBinOp(scalar, _ - _)
  def *(scalar: Double): Matrix = matrixAndScalarBinOp(scalar, _ * _)
  def /(scalar: Double): Matrix = matrixAndScalarBinOp(scalar, _ / _)
}

object Matrix {
  def apply(nRows: Int, nCols: Int, els: Seq[Double]): Matrix = new Matrix(nRows, nCols, els)
  def fill(nRows: Int, nCols: Int, fillValue: Double) = new Matrix(nRows, nCols, Array.fill(nRows * nCols)(fillValue))
  def zeros(nRows: Int, nCols: Int): Matrix = fill(nRows, nCols, 0.0)
  def ones(nRows: Int, nCols: Int): Matrix = fill(nRows, nCols, 1.0)
  def NaNs(nRows: Int, nCols: Int): Matrix = fill(nRows, nCols, Double.NaN)
  def default(nRows: Int, nCols: Int): Matrix = new Matrix(nRows, nCols, new Array[Double](nRows * nCols))
}

object Chapter11Exercise08 extends App {
  val m0 = Matrix(2, 3, Seq(1, 2, 3, 4, 5, 6))
  println(s"M0 = \n$m0\n")
  val m1 = Matrix.ones(2, 3)
  println(s"M1 = \n$m1\n")
  val m2 = Matrix(2, 3, Seq(1, -1, 1, -1, 1, -1))
  println(s"M2 = \n$m2\n")
  println(s"(M0 + M1 * 2) * M2 + 7 = \n${(m0 + m1 * 2) * m2 + 7}\n")
}