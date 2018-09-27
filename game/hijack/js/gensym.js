const USED = new Set();

export function gensym() {
    const n = Math.floor(Math.random() * 0xFFFFFFFF);

    if (USED.has(n))
        return gensym();

    USED.add(n);
    return n;
};
