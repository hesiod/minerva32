int main() {
    volatile int acc = 1;

    for (int i = 1; i < 100000; i++) {
        acc += acc;
    }

    return 0;
}
