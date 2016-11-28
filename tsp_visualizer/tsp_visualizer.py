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


def generate_graph_fig(vertex_num, out_file, ant_paths, graph, generation):

    g = Graph(format='png')

    count_matrix = get_count_matrix(vertex_num, ant_paths)

    for i in range(vertex_num):
        for j in range(i + 1, vertex_num):
            count = count_matrix[i][j]
            if count == 0:
                g.edge(str(i), str(j),
                       label=str(graph[i][j]), color="blue", penwidth="0.2")
            else:
                g.edge(str(i), str(j),
                       label=str(graph[i][j]),  penwidth=str(count * 0.5))

    g.body.append('label="Generation: %03d"' % generation)
    g.body.append('fontsize=20')
    g.render(out_file, cleanup=True)


def generate(vertex_num, ant_num, csv_path, graph_path, out_dir, grange):

    ant_path_list = read_csv(csv_path)
    graph = read_csv(graph_path)

    generation = len(ant_path_list) // ant_num
    grange = int(grange)

    for i in range(generation):
        if i % grange == 0:
            generate_graph_fig(vertex_num,
                               out_dir + '/%03d' % i,
                               ant_path_list[i * 10: (i + 1) * 10],
                               graph,
                               i)


def main():
    import argparse
    import os
    import sys

    p = argparse.ArgumentParser(
        description='This script is for visualization of TSP solver')
    p.add_argument('-v', '--vertex_num', type=int,
                   help='The numeber of vertex of graph', default=10)
    p.add_argument('-a', '--ant_num', type=int,
                   help='The numeber of ant', default=10)
    p.add_argument('-g', '--graph', type=str,
                   help='The path to graph csv file', required=True)
    p.add_argument('-if', '--input_file', type=str,
                   help='The path to input csv file', required=True)
    p.add_argument('-of', '--output_file_dir', type=str,
                   help='The path to output_dir', required=True)
    p.add_argument('-range', '--generation_range', type=str,
                   help='The path to range', required=True)

    option_args, other_args = p.parse_known_args()
    vertex_num = option_args.vertex_num
    ant_num = option_args.ant_num
    csv_path = option_args.input_file
    graph_path = option_args.graph
    out_dir = option_args.output_file_dir
    grange = option_args.generation_range

    #csv_path = 'data/5_10_05_10_100_0_100.csv'
    #csv_path = 'data/5_10_095_10_10_0_100.csv'
    #graph_path = 'data/graph.csv'
    #out_dir = 'graphs/aco_02'

    if not os.path.isdir(out_dir):
        os.makedirs(out_dir)

    generate(vertex_num, ant_num, csv_path, graph_path, out_dir, grange)


if __name__ == "__main__":
    main()
