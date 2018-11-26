(* techmap_celltype = "$add" *)
module _sysdsp_add (A, B, X);
    parameter WIDTH = 32;

    input [WIDTH-1:0] A, B;
    output [WIDTH-1:0] X;

    SB_MAC16 #(
        .A_REG(0),
        .B_REG(0),
        .C_REG(0),
        .D_REG(0),
        .TOPADDSUB_UPPERINPUT(1'b1),
        .TOPADDSUB_CARRYSELECT(2'b11),
        .BOTADDSUB_UPPERINPUT(1'b1)
    ) dsp (
        .A(A[31:16]),
        .B(A[15:0]),
        .C(B[31:16]),
        .D(B[15:0]),
        .O(X),
        .CE(1'b1),
        .ADDSUBTOP(0),
        .ADDSUBBOT(0)
    );
endmodule

