#include <stdio.h>
#include <stdlib.h>

#include "../quaint.hpp"

#define UNITTEST_MAIN
#include "unittest.h"

int main(int argc, const char * argv[]) {

#include "unittest.cpp"

	summary();
	return 0;
}
