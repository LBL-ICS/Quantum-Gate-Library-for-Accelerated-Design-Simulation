package QSU_Test.Gates

import QuantumStateUnit.Gates._
import _root_.Complex_FPU._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/*
ALL BITWIDTH FOR THE GATE UNITS WITH THE MARIO COMPLEX UNITS MUST USE THE FOLLOWING ACCEPTED VALUES
Accepted bit width is:      32, 64,128,256
accepted propagation delay:  1,  3,  7,  8, 10, 13
 */
