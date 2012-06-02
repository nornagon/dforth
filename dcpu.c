#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#define die(fmt, args...) \
	do { fprintf(stderr, fmt "\n", ## args); exit(1); } while (0)

uint16_t ram[0x10000];
uint16_t reg[0x8];
uint16_t PC, SP, O;
uint16_t dummy;

struct op {
	unsigned int opcode : 4;
	unsigned int a : 6;
	unsigned int b : 6;
};

void clear() {
	PC = SP = O = 0;
	memset(reg, 0, 0x8);
	memset(ram, 0, 0x10000);
}

uint16_t *val(unsigned int v, size_t *cycles) {
	if (v <= 0x7)
		return &reg[v];
	if (v <= 0xf)
		return &ram[reg[v-0x7]];
	if (v <= 0x17)
	{ (*cycles)++; return &ram[ram[PC++] + reg[v-0x10]]; }
	if (v == 0x18)
		return &ram[SP++];
	if (v == 0x19)
		return &ram[SP];
	if (v == 0x1a)
		return &ram[--SP];
	if (v == 0x1b)
		return &SP;
	if (v == 0x1c)
		return &PC;
	if (v == 0x1d)
		return &O;
	if (v == 0x1e)
	{ (*cycles)++; return &ram[ram[PC++]]; }
	if (v == 0x1f)
	{ (*cycles)++; return &ram[PC++]; }
	dummy = v - 0x20;
	return &dummy;
}

void read_bin(uint16_t* mem, size_t len, FILE* fd) {
	size_t num = fread(mem, sizeof(uint16_t), len, fd);
	if (!feof(stdin))
		die("The input file was longer than RAM.");
	while (num--) {
		mem[num] = ((uint8_t*)&mem[num])[1] | (((uint8_t*)&mem[num])[0] << 8);
	}
}

int main(int argc, char **argv) {
	clear();
	read_bin(ram, 0x10000, stdin);
	while (1) {
		size_t cycles = 0;
		struct op ins = *(struct op*)(&ram[PC++]);
		//printf("%d %d %d\n", ins.opcode, ins.a, ins.b);
		if (ins.opcode == 0) { // non-basic opcode
			uint16_t *a = val(ins.b, &cycles);
			switch (ins.a) {
				case 0x00: exit(0);
				case 0x01: // JSR
					ram[--SP] = PC;
					PC = *a;
					cycles += 2;
					break;
				case 0x10: // [non-standard] write char
					fputc(*a, stdout);
					cycles += 1;
					break;
				case 0x11: // [non-standard] read char
					{
						int ret = fputc(*a, stdout);
						O = ret == EOF ? 1 : 0;
						*a = ret == EOF ? 0 : ret;
					}
					cycles += 1;
					break;
				default:
					die("Bad non-basic opcode.");
			}
		} else {
			uint16_t *a, *b;
			a = val(ins.a, &cycles);
			b = val(ins.b, &cycles);
			switch (ins.opcode) { // basic opcodes
				case 0x1: // SET
					*a = *b;
					cycles++;
					break;
				case 0x2: // ADD
					if (*a & *b & 0x8000) O = 0x0001;
					*a += *b;
					cycles += 2;
					break;
				case 0x3: // SUB
					// TODO overflow
					*a -= *b;
					cycles += 2;
					break;
				case 0x4: // MUL
					O = (((uint32_t)*a) * ((uint32_t)*b)) >> 16;
					*a *= *b;
					cycles += 2;
					break;
				case 0x5: // DIV
					if (*b == 0) *a = O = 0;
					else {
						O = (((uint32_t)*a) << 16) / *b;
						*a /= *b;
					}
					cycles += 3;
					break;
				case 0x6: // MOD
					if (*b == 0) *a = 0;
					else *a %= *b;
					cycles += 3;
					break;
				case 0x7: // SHL
					O = (((uint32_t)*a) << *b) >> 16;
					*a <<= *b;
					cycles += 2;
					break;
				case 0x8: // SHR
					O = (((uint32_t)*a) << 16) >> *b;
					*a >>= *b;
					cycles += 2;
					break;
				case 0x9: // AND
					*a &= *b;
					cycles++;
					break;
				case 0xa: // BOR
					*a |= *b;
					cycles++;
					break;
				case 0xb: // XOR
					*a ^= *b;
					cycles++;
					break;
				case 0xc: // IFE
					if (!(*a == *b)) PC++;
					else cycles++;
					cycles += 2;
					break;
				case 0xd: // IFN
					if (!(*a != *b)) PC++;
					else cycles++;
					cycles += 2;
					break;
				case 0xe: // IFG
					if (!(*a > *b)) PC++;
					else cycles++;
					cycles += 2;
					break;
				case 0xf: // IFB
					if (!(*a & *b)) PC++;
					else cycles++;
					cycles += 2;
					break;
			}
		}
	}
}
