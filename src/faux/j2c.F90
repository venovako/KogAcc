!>@brief \b J2C returns a character representing J.
ELEMENTAL FUNCTION J2C(J)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: J
  CHARACTER :: J2C

  SELECT CASE (J)
  CASE (0)
     J2C = '0'
  CASE (1)
     J2C = '1'
  CASE (2)
     J2C = '2'
  CASE (3)
     J2C = '3'
  CASE (4)
     J2C = '4'
  CASE (5)
     J2C = '5'
  CASE (6)
     J2C = '6'
  CASE (7)
     J2C = '7'
  CASE (8)
     J2C = '8'
  CASE (9)
     J2C = '9'
  CASE (10)
     J2C = 'A'
  CASE (11)
     J2C = 'B'
  CASE (12)
     J2C = 'C'
  CASE (13)
     J2C = 'D'
  CASE (14)
     J2C = 'E'
  CASE (15)
     J2C = 'F'
  CASE (16)
     J2C = 'G'
  CASE (17)
     J2C = 'H'
  CASE (18)
     J2C = 'I'
  CASE (19)
     J2C = 'J'
  CASE (20)
     J2C = 'K'
  CASE (21)
     J2C = 'L'
  CASE (22)
     J2C = 'M'
  CASE (23)
     J2C = 'N'
  CASE (24)
     J2C = 'O'
  CASE (25)
     J2C = 'P'
  CASE (26)
     J2C = 'Q'
  CASE (27)
     J2C = 'R'
  CASE (28)
     J2C = 'S'
  CASE (29)
     J2C = 'T'
  CASE (30)
     J2C = 'U'
  CASE (31)
     J2C = 'V'
  CASE (32)
     J2C = 'W'
  CASE (33)
     J2C = 'X'
  CASE (34)
     J2C = 'Y'
  CASE (35)
     J2C = 'Z'
  CASE (36)
     J2C = '_'
  CASE (37)
     J2C = 'a'
  CASE (38)
     J2C = 'b'
  CASE (39)
     J2C = 'c'
  CASE (40)
     J2C = 'd'
  CASE (41)
     J2C = 'e'
  CASE (42)
     J2C = 'f'
  CASE (43)
     J2C = 'g'
  CASE (44)
     J2C = 'h'
  CASE (45)
     J2C = 'i'
  CASE (46)
     J2C = 'j'
  CASE (47)
     J2C = 'k'
  CASE (48)
     J2C = 'l'
  CASE (49)
     J2C = 'm'
  CASE (50)
     J2C = 'n'
  CASE (51)
     J2C = 'o'
  CASE (52)
     J2C = 'p'
  CASE (53)
     J2C = 'q'
  CASE (54)
     J2C = 'r'
  CASE (55)
     J2C = 's'
  CASE (56)
     J2C = 't'
  CASE (57)
     J2C = 'u'
  CASE (58)
     J2C = 'v'
  CASE (59)
     J2C = 'w'
  CASE (60)
     J2C = 'x'
  CASE (61)
     J2C = 'y'
  CASE (62)
     J2C = 'z'
  CASE (63)
     J2C = '-'
  CASE DEFAULT
     J2C = ' '
  END SELECT
END FUNCTION J2C
