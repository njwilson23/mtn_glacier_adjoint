module numeric_kinds

    integer, parameter      :: real32 = kind(4), &
                               real64 = kind(8), &
                               int32 = kind(4), &
                               int64 = kind(8), &
                               cmplx64 = kind(8), &
                               cmplx128 = kind(16)

end module
