     INFO = 0
     QG(1,1) = G(1,1)
     QG(2,1) = G(2,1)
     QG(1,2) = G(1,2)
     QG(2,2) = G(2,2)
     CALL QKSVD2(QG, QU, QV, QS, INFO)
     IF (INFO(1) .LT. -HUGE(0)) CALL STHALT('QKSVD2')
     QS(1) = SCALE(QS(1), INFO(2) - INFO(1))
     QS(2) = SCALE(QS(2), INFO(3) - INFO(1))
     Q = QS(1) / QS(2)
     IF (Q .NE. Q) THEN
        Q = QZERO
        Q = QONE / Q
     END IF
     ! \kappa_2(G)
     F(1,1) = MAX(Q, F(1,1))
     F(2,1) = MAX(-Q, F(2,1))
#ifdef _WIN32
     NS(1) = PVN_TIME_MONO_TICKS()
#else
     NS(1) = PVN_TIME_MONO_NS()
#endif
     INFO = 0
     CALL KSVD2(G, U, V, S, INFO)
#ifdef _WIN32
     NS(1) = PVN_TIME_MONO_TICKS() - NS(1)
#else
     NS(1) = PVN_TIME_MONO_NS() - NS(1)
#endif
     IF (INFO(1) .LT. -HUGE(0)) CALL STHALT('KSVD2')
     NSTIME(1) = NSTIME(1) + NS(1)
     CALL KERR2(G, U, V, S, E(1,1), INFO)
     Q = S(1)
     Q = SCALE(Q, INFO(2) - INFO(1))
     E(4,1) = MAX(ABS(QS(1) - Q) / QS(1), QZERO)
     Q = S(2)
     Q = SCALE(Q, INFO(3) - INFO(1))
     E(5,1) = MAX(ABS(QS(2) - Q) / QS(2), QZERO)
     F(1,2) = MAX(E(1,1), F(1,2))
     F(2,2) = MAX(-E(1,1), F(2,2))
     F(1,3) = MAX(E(2,1), F(1,3))
     F(2,3) = MAX(-E(2,1), F(2,3))
     F(1,4) = MAX(E(3,1), F(1,4))
     F(2,4) = MAX(-E(3,1), F(2,4))
     F(1,5) = MAX(E(4,1), F(1,5))
     F(2,5) = MAX(-E(4,1), F(2,5))
     F(1,6) = MAX(E(5,1), F(1,6))
     F(2,6) = MAX(-E(5,1), F(2,6))
#ifdef _WIN32
     NS(2) = PVN_TIME_MONO_TICKS()
#else
     NS(2) = PVN_TIME_MONO_NS()
#endif
     INFO = 0
     CALL LWSV2(G, U, V, S, INFO(1))
#ifdef _WIN32
     NS(2) = PVN_TIME_MONO_TICKS() - NS(2)
#else
     NS(2) = PVN_TIME_MONO_NS() - NS(2)
#endif
     IF (INFO(1) .LT. -HUGE(0)) CALL STHALT('LWSV2')
     NSTIME(2) = NSTIME(2) + NS(2)
     IF ((S(1) .LT. ZERO) .OR. (S(2) .LT. ZERO)) THEN
#include "gr2ud.F90"
        IF (S(1) .LT. ZERO) THEN
           U(1,1) = -U(1,1)
           U(2,1) = -U(2,1)
           S(1) = -S(1)
        END IF
        IF (S(2) .LT. ZERO) THEN
           U(1,2) = -U(1,2)
           U(2,2) = -U(2,2)
           S(2) = -S(2)
        END IF
        IF (S(1) .LT. S(2)) THEN
           G(2,1) = U(1,1)
           U(1,1) = U(1,2)
           U(1,2) = G(2,1)
           G(2,1) = U(2,1)
           U(2,1) = U(2,2)
           U(2,2) = G(2,1)
           G(2,1) = S(1)
           S(1) = S(2)
           S(2) = G(2,1)
           G(2,1) = V(1,1)
           V(1,1) = V(1,2)
           V(1,2) = G(2,1)
           G(2,1) = V(2,1)
           V(2,1) = V(2,2)
           V(2,2) = G(2,1)
           G(2,1) = ZERO
        END IF
     END IF
     CALL KERR2(G, U, V, S, E(1,2), INFO)
     Q = S(1)
     E(4,2) = MAX(ABS(QS(1) - Q) / QS(1), QZERO)
     Q = S(2)
     E(5,2) = MAX(ABS(QS(2) - Q) / QS(2), QZERO)
     F(1,7) = MAX(E(1,2), F(1,7))
     F(2,7) = MAX(-E(1,2), F(2,7))
     F(1,8) = MAX(E(2,2), F(1,8))
     F(2,8) = MAX(-E(2,2), F(2,8))
     F(1,9) = MAX(E(3,2), F(1,9))
     F(2,9) = MAX(-E(3,2), F(2,9))
     F(1,10) = MAX(E(4,2), F(1,10))
     F(2,10) = MAX(-E(4,2), F(2,10))
     F(1,11) = MAX(E(5,2), F(1,11))
     F(2,11) = MAX(-E(5,2), F(2,11))
     DO L = 1, 5
        Q = E(L,2) / E(L,1)
        M = 11 + L
        F(1,M) = MAX(Q, F(1,M))
        F(2,M) = MAX(-Q, F(2,M))
     END DO
     F(1,17) = MAX((NS(1) / Q9), F(1,17))
     F(2,17) = MAX((NS(2) / Q9), F(2,17))
