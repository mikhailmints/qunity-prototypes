OPENQASM 3.0;
include "stdgates.inc";
qubit[6] q;
bit[6] out;
bit[0] err;
h q[0];
h q[1];
h q[2];
h q[3];
h q[4];
h q[5];
out[0] = measure q[0];
out[1] = measure q[1];
out[2] = measure q[2];
out[3] = measure q[3];
out[4] = measure q[4];
out[5] = measure q[5];
