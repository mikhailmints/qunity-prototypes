OPENQASM 3.0;
include "stdgates.inc";
qubit[2] q;
bit[1] out;
bit[0] err;
reset q[0];
out[0] = measure q[1];
