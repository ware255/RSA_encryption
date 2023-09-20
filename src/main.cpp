#include <cstdio>
#include <windows.h>
using bint = unsigned long long;

class crypto {
private:
    bint p, q, n, l, e, d;
    bint M, MAX = 8000000;
public:
    bint random(bint);
    bint sqrt(double);
    bool isPrime(bint);
    void init();
    void init();
    crypto() { init(); }
    bint extended_euclidean(bint, bint);
    bint gcd(bint, bint);
    bint lcm(bint, bint);
    bint modPow(bint, bint, bint);
    bint modinv(const bint&, const bint&);
    bint Chinese_Remainder_Theorem(bint&, bint&, bint&, bint&);
    void start();
};

bint crypto::extended_euclidean(bint a, bint b) {
    bint x1 = 0, y1 = 1, r1 = b;
    bint x2 = 1, y2 = 0, r2 = a;
    bint x;
    bint qq, rr;
    bint xx, yy;
    
    while (1) {
        qq = r1 / r2;
        rr = r1 % r2;

        xx = x1 - qq * x2;
        yy = y1 - qq * y2;

        if (rr == 0) {
            x = x2;
            break;
        }

        x1 = x2; y1 = y2; r1 = r2;
        x2 = xx; y2 = yy; r2 = rr;
    }
    while ( x <= 0 ) x += b;

    return x;
}

bint crypto::gcd(bint x, bint y) {
    while (1) {
        if (y == 0) return x;
        x = x % y;
        if (x == 0) return y;
        y = y % x;
    }
}

bint crypto::lcm(bint a, bint b) {
    return a * b / gcd(a, b);
}

bint crypto::modPow(bint a, bint k, bint n) {
    a %= n;

    if (a == 0 || n == 0) return 0;
    if (k == 0) return 1 % n;

    bint i;
    bint value = 1;
    for(i = 0; i < k; i++) {
        value *= a;
        if(value >= n) {
            value %= n;
        }
    }
    return value;
}

bint crypto::modinv(const bint &a, const bint &m) {
    bint j = 1, i = 0, b = m, c = a, x, y;
    while (c != 0) {
        x = b / c;
        y = b - x*c;
        b = c;
        c = y;
        y = j;
        j = i - j*x;
        i = y;
    }
    if (i < 0) i += m;
    return i;
}

bint crypto::Chinese_Remainder_Theorem(bint &p, bint &q, bint &c, bint &d) {
    bint m1, m2, dp, dq, qinv, m, h;
    qinv = modinv(q, p);

    dp = d % (p-1);
    dq = d % (q-1);

    m1 = modPow(c, dp, p);
    m2 = modPow(c, dq, q);

    h = qinv * (m1 - m2);
    m = m2 + h * q;
    return m;
}

bint crypto::random(bint n) {
    static bint x = 123456789;
    static bint y = 362436069;
    static bint z = 521288629;
    n = n ^ (n << 7); n = n ^ (n >> 9);
    static bint w = n ^ 2463534242;
    int t, m;
    m = n % 10;

    for (int i = 0; i < m; i++) {
        t = x ^ (x << 11);
        x = y; y = z; z = w;
        w = (w ^ (w >> 19)) ^ (t ^ (t >> 8));
    }
    return w;
}

bint crypto::sqrt(double x) {
    double s, last;

    if (x > 1.0) s = x;
    else s = 1.0;

    do {
        last = s;
        s = (x / s + s) / 2.0;
    } while (s < last);

    return static_cast<int>(last);
}

bool crypto::isPrime(bint a) {
    if (a % 2 == 0 || a <= 2) return false;
    for (bint i = 3; i <= sqrt(a); i += 2) if (a % i == 0) return false;
    return true;
}

void crypto::init() {
    bint tmp;
    bint randseed = 0x12345678 ^ GetTickCount();
    p = random(randseed) % MAX;
    q = random(randseed) % MAX;
    while (1) {
        if (!isPrime(p)) p++;
        else break;
    }
    while (1) {
        if (!isPrime(q)) q++;
        else break;
    }
    if (p < q) {
        tmp = p;
        p = q;
        q = tmp;
    }
}

void crypto::start() {
    init();
    n = p * q;
    l = lcm(p-1, q-1);
    e = 3;
    while (gcd(e, l) != 1) e += 2;
    d = extended_euclidean(e, l);
    while(e * d % l != 1) d++;

    printf("P : %lld   Q : %lld\n", p, q);

    printf("N : %lld   L : %lld   E : %lld   D : %lld\n", n, l, e, d);

    bint plain_num;
    printf("plain_num : ");
    scanf("%llu", &plain_num);

    //暗号化
    bint encrypted_num = modPow(plain_num, e, n);
    printf("encrypted_num: %llu\n", encrypted_num);

    //復号
    M = Chinese_Remainder_Theorem(p, q, encrypted_num, d);
    printf("decrypted_num(CRT): %llu\n", M);
    bint decrypted_num = modPow(encrypted_num, d, n);
    printf("decrypted_num:      %llu\n", decrypted_num);
}

int main() {
    crypto* t;
    t = new crypto();
    t->start();
    delete t;
    return 0;
}
