#include <cctype>
#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

typedef long double num;

static const num PI = 3.141592653589793238462643383279502884L;
static const num E = 2.718281828459045235360287471352662498L;

class Parser {
public:
    Parser(const std::string &src, bool degrees, std::map<std::string, num> &vars)
        : s(src), pos(0), deg(degrees), vars(vars) {}

    num parse() {
        num v = expr();
        skip();
        if (pos < s.size())
            throw std::runtime_error(std::string("caractere inesperado '") + s[pos] + "'");
        return v;
    }

private:
    std::string s;
    size_t pos;
    bool deg;
    std::map<std::string, num> &vars;

    void skip() {
        while (pos < s.size() && std::isspace(static_cast<unsigned char>(s[pos]))) pos++;
    }

    char peek() {
        skip();
        return pos < s.size() ? s[pos] : '\0';
    }

    bool accept(char c) {
        if (peek() == c) {
            pos++;
            return true;
        }
        return false;
    }

    void expect(char c) {
        if (!accept(c)) throw std::runtime_error(std::string("esperado '") + c + "'");
    }

    num toRad(num x) const { return deg ? x * PI / 180.0L : x; }
    num fromRad(num x) const { return deg ? x * 180.0L / PI : x; }

    num expr() {
        num v = term();
        for (;;) {
            if (accept('+')) v += term();
            else if (accept('-')) v -= term();
            else return v;
        }
    }

    num term() {
        num v = unary();
        for (;;) {
            if (accept('*')) {
                v *= unary();
            } else if (accept('/')) {
                num d = unary();
                if (d == 0) throw std::runtime_error("divisao por zero");
                v /= d;
            } else if (accept('%')) {
                num d = unary();
                if (d == 0) throw std::runtime_error("modulo por zero");
                v = fmodl(v, d);
            } else {
                return v;
            }
        }
    }

    num unary() {
        if (accept('-')) return -unary();
        if (accept('+')) return unary();
        return power();
    }

    num power() {
        num base = postfix();
        if (accept('^')) {
            num ex = unary();
            if (base == 0 && ex < 0) throw std::runtime_error("divisao por zero");
            num r = powl(base, ex);
            if (std::isnan(r)) throw std::runtime_error("resultado nao real");
            return r;
        }
        return base;
    }

    num postfix() {
        num v = primary();
        while (accept('!')) v = factorial(v);
        return v;
    }

    static num factorial(num x) {
        if (x < 0 || x != floorl(x)) throw std::runtime_error("fatorial exige inteiro nao negativo");
        if (x > 1500) throw std::runtime_error("fatorial muito grande");
        return roundl(tgammal(x + 1));
    }

    num number() {
        skip();
        size_t start = pos;
        while (pos < s.size() && (std::isdigit(static_cast<unsigned char>(s[pos])) || s[pos] == '.')) pos++;
        if (pos < s.size() && (s[pos] == 'e' || s[pos] == 'E')) {
            size_t p = pos + 1;
            if (p < s.size() && (s[p] == '+' || s[p] == '-')) p++;
            if (p < s.size() && std::isdigit(static_cast<unsigned char>(s[p]))) {
                while (p < s.size() && std::isdigit(static_cast<unsigned char>(s[p]))) p++;
                pos = p;
            }
        }
        std::string t = s.substr(start, pos - start);
        char *end = nullptr;
        num v = strtold(t.c_str(), &end);
        if (end == t.c_str() || *end != '\0') throw std::runtime_error("numero invalido '" + t + "'");
        return v;
    }

    num primary() {
        char c = peek();
        if (std::isdigit(static_cast<unsigned char>(c)) || c == '.') return number();
        if (std::isalpha(static_cast<unsigned char>(c)) || c == '_') {
            size_t start = pos;
            while (pos < s.size() && (std::isalnum(static_cast<unsigned char>(s[pos])) || s[pos] == '_')) pos++;
            std::string name = s.substr(start, pos - start);
            if (peek() == '(') {
                pos++;
                std::vector<num> args;
                if (!accept(')')) {
                    do {
                        args.push_back(expr());
                    } while (accept(','));
                    expect(')');
                }
                return call(name, args);
            }
            if (name == "pi") return PI;
            if (name == "e") return E;
            std::map<std::string, num>::iterator it = vars.find(name);
            if (it != vars.end()) return it->second;
            throw std::runtime_error("variavel desconhecida '" + name + "'");
        }
        if (accept('(')) {
            num v = expr();
            expect(')');
            return v;
        }
        if (c == '\0') throw std::runtime_error("expressao incompleta");
        throw std::runtime_error(std::string("caractere inesperado '") + c + "'");
    }

    static void need(const std::string &name, const std::vector<num> &a, size_t n) {
        if (a.size() != n)
            throw std::runtime_error("funcao '" + name + "' espera " + std::to_string(n) + " argumento(s)");
    }

    static num combination(num n, num k) {
        if (n < 0 || k < 0 || n != floorl(n) || k != floorl(k) || k > n)
            throw std::runtime_error("combinacao exige inteiros com 0 <= k <= n");
        if (k > n - k) k = n - k;
        num r = 1;
        for (num i = 1; i <= k; i++) r = r * (n - k + i) / i;
        return roundl(r);
    }

    num call(const std::string &f, const std::vector<num> &a) {
        if (f == "sin") { need(f, a, 1); return sinl(toRad(a[0])); }
        if (f == "cos") { need(f, a, 1); return cosl(toRad(a[0])); }
        if (f == "tan") {
            need(f, a, 1);
            num r = toRad(a[0]);
            if (fabsl(cosl(r)) < 1e-15L) throw std::runtime_error("tangente indefinida");
            return tanl(r);
        }
        if (f == "asin") {
            need(f, a, 1);
            if (a[0] < -1 || a[0] > 1) throw std::runtime_error("asin fora do dominio");
            return fromRad(asinl(a[0]));
        }
        if (f == "acos") {
            need(f, a, 1);
            if (a[0] < -1 || a[0] > 1) throw std::runtime_error("acos fora do dominio");
            return fromRad(acosl(a[0]));
        }
        if (f == "atan") { need(f, a, 1); return fromRad(atanl(a[0])); }
        if (f == "atan2") { need(f, a, 2); return fromRad(atan2l(a[0], a[1])); }
        if (f == "sinh") { need(f, a, 1); return sinhl(a[0]); }
        if (f == "cosh") { need(f, a, 1); return coshl(a[0]); }
        if (f == "tanh") { need(f, a, 1); return tanhl(a[0]); }
        if (f == "sqrt") {
            need(f, a, 1);
            if (a[0] < 0) throw std::runtime_error("raiz quadrada de numero negativo");
            return sqrtl(a[0]);
        }
        if (f == "cbrt") { need(f, a, 1); return cbrtl(a[0]); }
        if (f == "root") {
            need(f, a, 2);
            if (a[1] == 0) throw std::runtime_error("indice de raiz zero");
            if (a[0] < 0) {
                if (a[1] == floorl(a[1]) && fmodl(a[1], 2) != 0) return -powl(-a[0], 1.0L / a[1]);
                throw std::runtime_error("raiz de numero negativo");
            }
            return powl(a[0], 1.0L / a[1]);
        }
        if (f == "pow") { need(f, a, 2); return powl(a[0], a[1]); }
        if (f == "exp") { need(f, a, 1); return expl(a[0]); }
        if (f == "ln") {
            need(f, a, 1);
            if (a[0] <= 0) throw std::runtime_error("logaritmo de numero nao positivo");
            return logl(a[0]);
        }
        if (f == "log" || f == "log10") {
            need(f, a, 1);
            if (a[0] <= 0) throw std::runtime_error("logaritmo de numero nao positivo");
            return log10l(a[0]);
        }
        if (f == "log2") {
            need(f, a, 1);
            if (a[0] <= 0) throw std::runtime_error("logaritmo de numero nao positivo");
            return log2l(a[0]);
        }
        if (f == "logb") {
            need(f, a, 2);
            if (a[0] <= 0 || a[1] <= 0 || a[1] == 1) throw std::runtime_error("logaritmo fora do dominio");
            return logl(a[0]) / logl(a[1]);
        }
        if (f == "abs") { need(f, a, 1); return fabsl(a[0]); }
        if (f == "floor") { need(f, a, 1); return floorl(a[0]); }
        if (f == "ceil") { need(f, a, 1); return ceill(a[0]); }
        if (f == "round") { need(f, a, 1); return roundl(a[0]); }
        if (f == "trunc") { need(f, a, 1); return truncl(a[0]); }
        if (f == "fact") { need(f, a, 1); return factorial(a[0]); }
        if (f == "mod") {
            need(f, a, 2);
            if (a[1] == 0) throw std::runtime_error("modulo por zero");
            return fmodl(a[0], a[1]);
        }
        if (f == "max") { need(f, a, 2); return a[0] > a[1] ? a[0] : a[1]; }
        if (f == "min") { need(f, a, 2); return a[0] < a[1] ? a[0] : a[1]; }
        if (f == "ncr") { need(f, a, 2); return combination(a[0], a[1]); }
        if (f == "npr") {
            need(f, a, 2);
            num c = combination(a[0], a[1]);
            return roundl(c * tgammal(a[1] + 1));
        }
        if (f == "gcd" || f == "mdc") {
            need(f, a, 2);
            long long x = llroundl(fabsl(a[0])), y = llroundl(fabsl(a[1]));
            while (y) { long long t = x % y; x = y; y = t; }
            return static_cast<num>(x);
        }
        if (f == "lcm" || f == "mmc") {
            need(f, a, 2);
            long long x = llroundl(fabsl(a[0])), y = llroundl(fabsl(a[1]));
            if (x == 0 || y == 0) return 0;
            long long g = x, h = y;
            while (h) { long long t = g % h; g = h; h = t; }
            return static_cast<num>(x / g * y);
        }
        throw std::runtime_error("funcao desconhecida '" + f + "'");
    }
};

static std::string format(num v) {
    if (std::isinf(v)) return v > 0 ? "infinito" : "-infinito";
    if (std::isnan(v)) return "indefinido";
    std::ostringstream o;
    if (fabsl(v) < 1e15L && v == roundl(v)) {
        o << std::fixed << std::setprecision(0) << static_cast<double>(v);
    } else {
        o << std::setprecision(15) << static_cast<double>(v);
    }
    std::string r = o.str();
    if (r == "-0") r = "0";
    return r;
}

static void help() {
    std::cout <<
        "Operadores: + - * / % ^ !  e parenteses\n"
        "Constantes: pi, e, ans (ultimo resultado)\n"
        "Funcoes de 1 argumento: sin cos tan asin acos atan sinh cosh tanh\n"
        "  sqrt cbrt exp ln log log2 abs floor ceil round trunc fact\n"
        "Funcoes de 2 argumentos: pow(a,b) root(x,n) logb(x,base) atan2(y,x)\n"
        "  mod(a,b) max(a,b) min(a,b) ncr(n,k) npr(n,k) mdc(a,b) mmc(a,b)\n"
        "Variaveis: nome = expressao\n"
        "Comandos: deg, rad, vars, ajuda, sair\n"
        "Exemplos: 2^10, sqrt(144)+3!, sin(30) em modo deg, x = 5, x*2\n";
}

static std::string trim(const std::string &t) {
    size_t a = t.find_first_not_of(" \t\r\n");
    if (a == std::string::npos) return "";
    size_t b = t.find_last_not_of(" \t\r\n");
    return t.substr(a, b - a + 1);
}

static bool isIdent(const std::string &t) {
    if (t.empty() || !(std::isalpha(static_cast<unsigned char>(t[0])) || t[0] == '_')) return false;
    for (size_t i = 0; i < t.size(); i++)
        if (!(std::isalnum(static_cast<unsigned char>(t[i])) || t[i] == '_')) return false;
    return true;
}

static void evaluate(const std::string &line, bool deg, std::map<std::string, num> &vars) {
    std::string target;
    std::string body = line;
    size_t eq = line.find('=');
    if (eq != std::string::npos) {
        target = trim(line.substr(0, eq));
        body = line.substr(eq + 1);
        if (!isIdent(target) || target == "pi" || target == "e" || target == "ans")
            throw std::runtime_error("nome de variavel invalido");
    }
    Parser p(body, deg, vars);
    num r = p.parse();
    if (std::isnan(r)) throw std::runtime_error("resultado indefinido");
    vars["ans"] = r;
    if (!target.empty()) vars[target] = r;
    std::cout << (target.empty() ? "" : target + " = ") << format(r) << "\n";
}

int main(int argc, char **argv) {
    std::map<std::string, num> vars;
    bool deg = false;

    if (argc > 1) {
        std::string line;
        for (int i = 1; i < argc; i++) line += std::string(argv[i]) + " ";
        try {
            evaluate(line, deg, vars);
        } catch (const std::exception &e) {
            std::cerr << "erro: " << e.what() << "\n";
            return 1;
        }
        return 0;
    }

    std::cout << "Calculadora (digite 'ajuda' para ver os recursos, 'sair' para encerrar)\n";
    std::string line;
    while (true) {
        std::cout << "> " << std::flush;
        if (!std::getline(std::cin, line)) break;
        line = trim(line);
        if (line.empty()) continue;
        if (line == "sair" || line == "exit" || line == "quit") break;
        if (line == "ajuda" || line == "help") { help(); continue; }
        if (line == "deg") { deg = true; std::cout << "modo: graus\n"; continue; }
        if (line == "rad") { deg = false; std::cout << "modo: radianos\n"; continue; }
        if (line == "vars") {
            for (std::map<std::string, num>::iterator it = vars.begin(); it != vars.end(); ++it)
                std::cout << it->first << " = " << format(it->second) << "\n";
            continue;
        }
        try {
            evaluate(line, deg, vars);
        } catch (const std::exception &e) {
            std::cout << "erro: " << e.what() << "\n";
        }
    }
    return 0;
}
