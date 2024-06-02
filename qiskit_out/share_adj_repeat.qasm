OPENQASM 3.0;
include "stdgates.inc";
bit[2] out;
bit[3] err;
qubit[5] q;
x q[0];
cx q[0], q[1];
cx q[0], q[1];
cx q[0], q[2];
cx q[0], q[2];
cx q[0], q[3];
cx q[0], q[3];
cx q[0], q[4];
out[0] = measure q[0];
out[1] = measure q[4];
err[0] = measure q[1];
err[1] = measure q[2];
err[2] = measure q[3];