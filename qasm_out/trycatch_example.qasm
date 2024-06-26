OPENQASM 3.0;
include "stdgates.inc";
qubit[6] q;
bit[1] out;
bit[1] err;
h q[2];
x q[3];
x q[0];
swap q[3], q[1];
negctrl @ x q[2], q[0];
negctrl @ swap q[2], q[3], q[1];
reset q[1];
reset q[2];
x q[4];
h q[4];
reset q[2];
ctrl @ swap q[0], q[1], q[3];
ctrl @ swap q[0], q[1], q[4];
negctrl @ swap q[0], q[4], q[5];
negctrl @ swap q[0], q[1], q[4];
negctrl @ swap q[0], q[3], q[1];
ctrl @ x q[3], q[0];
ctrl @ swap q[3], q[0], q[1];
ctrl @ swap q[3], q[1], q[4];
ctrl @ swap q[3], q[4], q[5];
reset q[0];
reset q[4];
reset q[5];
out[0] = measure q[1];
err[0] = measure q[3];
