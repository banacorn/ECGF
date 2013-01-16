#include <stdlib.h>
#include <stdio.h>

typedef long long Lange;

//
//  GALOIS FIELD ARITHMETICS
//


typedef struct {
    Lange number;
    Lange order;
} GF;


typedef struct {
    Lange a;
    Lange b;
    Lange c;
} GCDEXT;


GCDEXT gcdExt (Lange a, Lange b) {
    if (b == 0) {
        GCDEXT c = {
            a: 1,
            b: 0,
            c: a
        };
        return c;
    } else {
        Lange   q = a / b,
                r = a % b;
        GCDEXT c = gcdExt(b, r);
        GCDEXT d = {
            a: c.b,
            b: c.a - q * c.b,
            c: c.c
        };
        return d;
    }
}

GF normGF (GF a) {
    a.number = (a.number % a.order) + a.order;
    a.number %= a.order;
    return a;
}

GF addGF (GF a, GF b) {
    a.number = a.number + b.number;
    return normGF(a);
}

GF subGF (GF a, GF b) {
    a.number -= b.number;
    return normGF(a);
}

GF mulGF (GF a, GF b) {
    a.number *= b.number;
    return normGF(a);
}

GF invGF (GF a) {
    a.number = gcdExt(a.number, a.order).a % a.order;
    return normGF(a);
}

GF divGF (GF a, GF b) {
    return normGF(mulGF(a, invGF(b)));
}


//
//  ELLIPTIC CURVE ARITHMETICS OVER GALOIS FIELD
//

typedef struct {
    Lange order;
    GF a;
    GF b;
} EC;

typedef struct {
    EC ec;
    GF x;
    GF y; 
} Point;


Point doubling (Point a) {

    GF three = { order: a.x.order, number: 3 };
    GF slope = divGF(addGF(mulGF(mulGF(a.x, a.x), three), a.ec.a), addGF(a.y, a.y));
    GF x = subGF(subGF(mulGF(slope, slope), a.x), a.x);
    GF y = subGF(mulGF(subGF(a.x, x), slope), a.y);

    Point c = {
        ec: a.ec,
        x: x,
        y: y
    };
    return c;

}

Point addition (Point a, Point b) {

    GF slope = divGF(subGF(b.y, a.y), subGF(b.x, a.x));
    GF x, y;

    if (a.x.number == 0 && a.y.number == 0) {
        return b;
    } else if (b.x.number == 0 && b.y.number == 0) {
        return a;
    } else if (a.x.number == b.x.number && a.y.number == b.y.number) {
        return doubling(a);
    } else if (subGF(b.x, a.x).number == 0) {

        x.number = 0;
        x.order = a.ec.order;
        y.number = 0;
        y.order = a.ec.order;

    } else {

        x = subGF(subGF(mulGF(slope, slope), a.x), b.x);
        y = subGF(mulGF(subGF(a.x, x), slope), a.y);

    }

    Point c = {
        ec: a.ec,
        x: x,
        y: y
    };
    return c;
}


Point multiplication (Point a, Lange n) {
    if (n == 1) {
        return a;
    } else if (n % 2 == 0) {
        return doubling(multiplication(a, n / 2));
    } else if (n % 2 == 1) {
        return addition(a, multiplication(a, n - 1));
    }
}

int validate (EC ec, Point p) {
    return normGF(mulGF(p.y, p.y)).number == normGF(addGF(addGF(mulGF(p.x, mulGF(p.x, p.x)), mulGF(ec.a, p.x)), ec.b)).number;
}

int main () {

    EC ec;
    Point a, b, c;
    Lange n;

    scanf("%lld", &ec.order);
    a.x.order = ec.order;
    a.y.order = ec.order;
    ec.a.order = ec.order;
    ec.b.order = ec.order;
    scanf("%lld", &ec.a.number);
    scanf("%lld", &ec.b.number);
    a.ec = ec;
    scanf("%lld", &a.x.number);
    scanf("%lld", &a.y.number);
    scanf("%lld", &n);

    if (validate(ec, a)) {

        Point c = multiplication(a, n);

        if (c.x.number == 0 && c.y.number == 0) {

            printf("infinity\n");

        } else {

            printf("%lld\n%lld\n", c.x.number, c.y.number);
        }

    } else {
        printf("wrong\n");
    }

    return 0;
}