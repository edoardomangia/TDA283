int square(int n) {
  return n * n;
}

int main() {
  double d = 3.14;
  return square(d);  // square expects int, not double
}

