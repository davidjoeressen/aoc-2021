#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define HEIGHT 150
#define WIDTH 150

void readfile(char board[HEIGHT][WIDTH], size_t *i, size_t *j, const char *filename)
{
	FILE *stream;
	char *c, *line = NULL;
	size_t len;
	stream = fopen(filename, "r");
	if (stream == NULL) {
		printf("Error reading file %s\n", filename);
		exit(EXIT_FAILURE);
	}
	while (getline(&line, &len, stream) != -1) {
		c = line;
		*j = 0;
		while (*c != '\n') {
			board[*i][*j] = *c;
			c++;
			(*j)++;
		}
		(*i)++;
	}
	free(line);
	fclose(stream);
}

bool moveright(char board[HEIGHT][WIDTH], size_t height, size_t width)
{
	bool moved = false;
	for (size_t i = 0; i < height; i++) {
		char first = board[i][0];
		char last = board[i][width - 1];
		for (size_t j = 0; j < (width - 1); j++) {
			if (board[i][j] == '>' && board[i][j+1] == '.') {
				board[i][j] = '.';
				board[i][j+1] = '>';
				moved = true;
				j++;
			}
		}
		if (last == '>' && first == '.') {
			board[i][width - 1] = '.';
			board[i][0] = '>';
			moved = true;
		}
	}
	return moved;
}

bool movedown(char board[HEIGHT][WIDTH], size_t height, size_t width)
{
	bool moved = false;
	for (size_t j = 0; j < width; j++) {
		char first = board[0][j];
		char last = board[height - 1][j];
		for (size_t i = 0; i < (height - 1); i++) {
			if (board[i][j] == 'v' && board[i+1][j] == '.') {
				board[i][j] = '.';
				board[i+1][j] = 'v';
				moved = true;
				i++;
			}
		}
		if (last == 'v' && first == '.') {
			board[height - 1][j] = '.';
			board[0][j] = 'v';
			moved = true;
		}
	}
	return moved;
}

unsigned int solve(char board[HEIGHT][WIDTH], size_t height, size_t width)
{
	unsigned int turns = 0;
	bool right, down;
	do {
		right = moveright(board, height, width);
		down = movedown(board, height, width);
		turns++;
	} while (right || down);
	return turns;
}

int main(int argc, char* argv[])
{
	if (argc != 2) {
		printf("Usage: %s <file>\n", argv[0]);
		exit(EXIT_FAILURE);
	}
	char board[HEIGHT][WIDTH];
	size_t i = 0, j = 0;
	readfile(board, &i, &j, argv[1]);
	unsigned int solution = solve(board, i, j);
	printf("Solution: %u\n", solution);

	exit(EXIT_SUCCESS);
}
