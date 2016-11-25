#!/usr/bin/env python

import csv
from graphviz import Graph


def read_csv(path):
    with open(path, 'r') as f:
        reader = csv.reader(f)
        return list(reader)


def get_count_matrix(vertex_num, paths):
    count_matrix = [[0 for j in range(vertex_num)] for i in range(vertex_num)]

    for path in paths:
        for n in range(vertex_num - 1):
            i, j = int(path[n]), int(path[n + 1])
            count_matrix[i][j] += 1
            count_matrix[j][i] += 1

    return count_matrix


def generate_graph_fig(vertex_num, out_file, ant_paths, generation):

    g = Graph(format='png')

    count_matrix = get_count_matrix(vertex_num, ant_paths)

    for i in range(vertex_num):
        for j in range(i + 1, vertex_num):
            count = count_matrix[i][j]
            if count == 0:
                g.edge(str(i), str(j), color="blue", penwidth="0.2")
            else:
                g.edge(str(i), str(j), penwidth=str(count * 0.4))

    g.body.append('label="Generation: %02d"' % generation)
    g.body.append('fontsize=20')
    g.render(out_file, cleanup=True)


def generate(vertex_num, ant_num, csv_path, out_dir):

    ant_path_list = read_csv(csv_path)
    generation = len(ant_path_list) // ant_num

    for i in range(generation):
        generate_graph_fig(vertex_num,
                           out_dir + '/%02d' % i, ant_path_list[i * 10: (i + 1) * 10], i)


def main():
    vertex_num = 10
    ant_num = 10
    #csv_path = 'result/5_10_05_10_100_0_100.csv'
    csv_path = 'result/5_10_095_10_10_0_100.csv'
    out_dir = 'graph_02'

    generate(vertex_num, ant_num, csv_path, out_dir)


if __name__ == "__main__":
    main()
