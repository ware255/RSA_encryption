! FortranでRSA暗号を作る(C++からの書き換え)
module rsa
    implicit none
    type rsa_class
        integer(8) p, q, n, l, e, d, m
    end type

    interface operator(.nxor.)
        module procedure nxor
    end interface

    interface operator(.lsh.)
        module procedure l_shift
    end interface

    interface operator(.rsh.)
        module procedure r_shift
    end interface
contains
    integer(8) function random(w)
        implicit none
        integer(8), intent(inout) :: w
        integer(8), save :: x = 123456789, y = 362436069, z = 521288629
        integer(8) t
        t = x .nxor. (x .lsh. 11_8)
        x = y; y = z; z = w
        w = (w .nxor. (w .rsh. 19_8)) .nxor. (t .nxor. (t .rsh. 8_8))
        random = w 
    end function random

    subroutine init(rc)
        implicit none
        logical(8), allocatable :: sieve(:)
        integer(8) :: MAX = 8000000
        integer(8) tmp, t, i, j
        type(rsa_class), intent(inout) :: rc
        allocate(sieve(MAX))
        sieve(:) = .true.
        sieve(1) = .false.
        do i = 3, MAX, 2
            if (sieve(i)) then
                do j = i * 2, MAX, i
                    sieve(j) = .false.
                end do
            end if
        end do
        t = time()
        tmp = mod(random(t), MAX) + 1
        do
            if (sieve(tmp)) then
                rc%q = tmp
                exit
            end if
            tmp = tmp + 1
        end do
        tmp = mod(random(t), MAX) + 1
        do
            if (sieve(tmp)) then
                rc%p = tmp
                exit
            end if
            tmp = tmp + 1
        end do
        if (rc%p < rc%q) then
            tmp  = rc%p
            rc%p = rc%q
            rc%q = tmp
        end if
        deallocate(sieve)
    end subroutine init

    integer(8) function nxor(a, b) result(c)
        implicit none
        integer(8), intent(in) :: a, b
        c = xor(a, b)
    end function nxor

    integer(8) function l_shift(a, b) result(c)
        implicit none
        integer(8), intent(in) :: a, b
        c = lshift(a, b)
    end function l_shift

    integer(8) function r_shift(a, b) result(c)
        implicit none
        integer(8), intent(in) :: a, b
        c = rshift(a, b)
    end function r_shift
end module rsa

program main
    use rsa
    implicit none
    integer(8) plain_num, encrypted_num
    type(rsa_class) :: rc
    call init(rc)
    rc%n = rc%p * rc%q
    rc%l = lcm(rc%p - 1, rc%q - 1)
    rc%e = 65537
    do while (gcd(rc%l, rc%e) .ne. 1)
        rc%e = rc%e + 1
    end do
    rc%d = extended_euclidean(rc%l, rc%e)
    do while (mod((rc%e * rc%d), rc%l) .ne. 1)
        rc%d = rc%d + 1
    end do
    print '("P : ", I0, "  Q : ", I0)', rc%p, rc%q
    print '("N : ", I0, "  L : ", I0, "  E : ", I0, "  D : ", I0)', rc%n, rc%l, rc%e, rc%d
    write (*, '(A)', advance='no') "plain_num     : "
    read *, plain_num
    encrypted_num = modPow(plain_num, rc%e, rc%n)
    print '("encrypted_num : ", I0)', encrypted_num
    rc%m = Chinese_Remainder_Theorem(rc%p, rc%q, encrypted_num, rc%d)
    print '("decrypted_num : ", I0)', rc%m
contains
    pure integer(8) function gcd(a, b)
        implicit none
        integer(8), intent(in) :: a, b
        integer(8) r, x, y
        if (a .eq. 0) gcd = b
        if (b .eq. 0) gcd = a
        x = a
        y = b
        do
            r = mod(x, y)
            if (r .eq. 0) exit
            x = y
            y = r
        end do
        gcd = y
    end function gcd

    pure integer(8) function Lcm(x, y)
        implicit none
        integer(8), intent(in) :: x, y
        Lcm = x / gcd(x, y) * y
    end function Lcm

    pure integer(8) function extended_euclidean(a, b)
        implicit none
        integer(8), intent(in) :: a, b
        integer(8) s, t, xs, ys, xt, yt
        integer(8) tmp, u, xu, yu
        s  = a
        t  = b
        xs = 1
        ys = 0
        xt = 0
        yt = 1
        do while (mod(s, t) .ne. 0)
            tmp = s / t
            u   = s - t * tmp
            xu  = xs - xt * tmp
            yu  = ys - yt * tmp
            s   = t
            xs  = xt
            ys  = yt
            t   = u
            xt  = xu
            yt  = yu
        end do
        if (xt > yt) then
            extended_euclidean = xt
        else
            extended_euclidean = yt
        end if
    end function extended_euclidean

    pure integer(8) function modPow(a, k, n)
        implicit none
        integer(8), intent(in) :: a, k, n
        integer(8) i, va, t
        t = mod(a, n)
        if (a .eq. 0 .or. n .eq. 0) then
            modPow = 0
        else if (k .eq. 0) then
            modPow = mod(1, n)
        end if
        va = 1
        do i = 0, k-1
            va = va * t
            if (va >= n) va = mod(va, n)
        end do
        modPow = va
    end function modPow

    pure integer(8) function modinv(a, m)
        implicit none
        integer(8), intent(in) :: a, m
        integer(8) j, i, b, c, x, y
        b = m
        c = a
        j = 1
        i = 0
        do while (c .ne. 0)
            x = b / c
            y = b - x*c
            b = c
            c = y
            y = j
            j = i - j*x
            i = y
        end do
        if (i < 0) i = i + m
        modinv = i
    end function modinv

    pure integer(8) function Chinese_Remainder_Theorem(p, q, c, d)
        implicit none
        integer(8), intent(in) :: p, q, c, d
        integer(8) m1, m2, dp, dq, qinv, m, h
        qinv = modinv(q, p)
        dp   = mod(d, (p-1))
        dq   = mod(d, (q-1))
        m1   = modPow(c, dp, p)
        m2   = modPow(c, dq, q)
        h    = qinv * (m1 - m2)
        m    = m2 + h * q
        Chinese_Remainder_Theorem = m
    end function Chinese_Remainder_Theorem
end program main

