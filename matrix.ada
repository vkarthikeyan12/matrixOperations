with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Matrix_Operations is

   subtype Index is Integer range 1 .. 3;
   type Matrix is array(Index, Index) of Integer;

   -- Hardcoded matrices
   A : Matrix := ((1, 2, 3),
                  (4, 5, 6),
                  (7, 8, 9));

   B : Matrix := ((9, 8, 7),
                  (6, 5, 4),
                  (3, 2, 1));

   C : Matrix;

   -- Function to compute determinant (supports 2x2 and 3x3 only)
   function Determinant(M : Matrix; N : Integer) return Integer is
   begin
      if N = 2 then
         return M(1,1)*M(2,2) - M(1,2)*M(2,1);
      elsif N = 3 then
         return M(1,1)*(M(2,2)*M(3,3)-M(2,3)*M(3,2)) -
                M(1,2)*(M(2,1)*M(3,3)-M(2,3)*M(3,1)) +
                M(1,3)*(M(2,1)*M(3,2)-M(2,2)*M(3,1));
      else
         return 0;
      end if;
   end Determinant;

begin
   -- Addition
   Put_Line("Matrix Addition (A + B):");
   for I in Index loop
      for J in Index loop
         C(I,J) := A(I,J) + B(I,J);
         Put(C(I,J)'Img & " ");
      end loop;
      New_Line;
   end loop;

   -- Subtraction
   Put_Line("Matrix Subtraction (A - B):");
   for I in Index loop
      for J in Index loop
         C(I,J) := A(I,J) - B(I,J);
         Put(C(I,J)'Img & " ");
      end loop;
      New_Line;
   end loop;

   -- Multiplication
   Put_Line("Matrix Multiplication (A * B):");
   for I in Index loop
      for J in Index loop
         C(I,J) := 0;
         for K in Index loop
            C(I,J) := C(I,J) + A(I,K)*B(K,J);
         end loop;
         Put(C(I,J)'Img & " ");
      end loop;
      New_Line;
   end loop;

   -- Determinants
   Put_Line("Determinant of Matrix A: " & Determinant(A, 3)'Img);
   Put_Line("Determinant of Matrix B: " & Determinant(B, 3)'Img);

end Matrix_Operations;
