#ifndef NDEBUG
        !$OMP CRITICAL
        WRITE (ERROR_UNIT,1,ADVANCE='NO') 'G ', G(1,1)
        WRITE (ERROR_UNIT,1) ' ', G(1,2)
        WRITE (ERROR_UNIT,1,ADVANCE='NO') 'G ', G(2,1)
        WRITE (ERROR_UNIT,1) ' ', G(2,2)
        WRITE (ERROR_UNIT,1,ADVANCE='NO') 'U ', U(1,1)
        WRITE (ERROR_UNIT,1) ' ', U(1,2)
        WRITE (ERROR_UNIT,1,ADVANCE='NO') 'U ', U(2,1)
        WRITE (ERROR_UNIT,1) ' ', U(2,2)
        WRITE (ERROR_UNIT,1,ADVANCE='NO') 'V ', V(1,1)
        WRITE (ERROR_UNIT,1) ' ', V(1,2)
        WRITE (ERROR_UNIT,1,ADVANCE='NO') 'V ', V(2,1)
        WRITE (ERROR_UNIT,1) ' ', V(2,2)
        WRITE (ERROR_UNIT,1) 'S ', S(1)
        WRITE (ERROR_UNIT,1) 'S ', S(2)
        !$OMP END CRITICAL
#endif
