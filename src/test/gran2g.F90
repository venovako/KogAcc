     INFO = 0
     QG(1,1) = G(1,1)
     QG(2,1) = G(2,1)
     QG(1,2) = G(1,2)
     QG(2,2) = G(2,2)
     CALL QKSVD2(QG, QU, QV, QS, INFO)
     IF (INFO .LE. -HUGE(INFO)) CALL STHALT('QKSVD2')
     IF (INFO .NE. 0) THEN
        L = -INFO
        QS(1) = SCALE(QS(1), L)
        QS(2) = SCALE(QS(2), L)
        INFO = 0
     END IF
     Q = QS(1) / QS(2)
     IF (Q .NE. Q) THEN
        Q = QZERO
        Q = QONE / Q
     END IF
     ! \kappa_2(G)
     F(1,1) = MAX(Q, F(1,1))
     F(2,1) = MAX(-Q, F(2,1))
     CALL KSVD2(G, U, V, S, INFO)
     IF (INFO .LE. -HUGE(INFO)) CALL STHALT('KSVD2')
     L = -INFO
     CALL KERR2(G, U, V, S, E(1,1), INFO)
     Q = S(1)
     IF (L .NE. 0) Q = SCALE(Q, L)
     E(4,1) = MAX(ABS(QS(1) - Q) / QS(1), QZERO)
     Q = S(2)
     IF (L .NE. 0) Q = SCALE(Q, L)
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
