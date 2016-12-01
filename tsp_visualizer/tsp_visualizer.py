#!/usr/bin/env python

"""
date: 2016/12/01
author: Masato Hashimoto (m5201129@u-aizu.ac.jp)
brief:
details:

Copyright (C) 2016 Masato Hashimoto All Rights Reserved
"""

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

        i, j = int(path[0]), int(path[-1])
        count_matrix[j][i] += 1
        count_matrix[i][j] += 1

    return count_matrix


def get_minimum_cost(vertex_num, graph, ant_paths):
    costs = []
    for path in ant_paths:
        cost = 0
        for n in range(vertex_num - 1):
            i, j = int(path[n]), int(path[n + 1])
            cost += int(graph[i][j])

        i, j = int(path[0]), int(path[-1])
        cost += int(graph[i][j])
        costs.append(cost)

    costs.sort()

    return costs[0]


def generate_graph_fig(vertex_num, out_file, ant_paths, graph, generation):

    g = Graph(format='png')

    count_matrix = get_count_matrix(vertex_num, ant_paths)
    minimum_cost = get_minimum_cost(vertex_num, graph, ant_paths)

    for i in range(vertex_num):
        for j in range(i + 1, vertex_num):
            count = count_matrix[i][j]
            if count == 0:
                g.edge(str(i), str(j),
                       label=str(graph[i][j]), color="blue", penwidth="0.2")
            else:
                g.edge(str(i), str(j),
                       label=str(graph[i][j]),  penwidth=str(count))

    g.body.append('label="Generation: %02d, Cost: %02d"' % (generation, minimum_cost))
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
                               out_dir + '/%02d' % i,
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
    p.add_argument('-range', '--generation_range', type=str,
                   help='The path to range', required=True)

    option_args, other_args = p.parse_known_args()
    vertex_num = option_args.vertex_num
    ant_num = option_args.ant_num
    csv_path = option_args.input_file
    graph_path = option_args.graph
    grange = option_args.generation_range

    gif_name, extension = os.path.splitext(os.path.basename(csv_path))
    out_dir = './graphs/%s' % gif_name

    if not os.path.isdir(out_dir):
        os.makedirs(out_dir)

    print('Generating Figure')
    generate(vertex_num, ant_num, csv_path, graph_path, out_dir, grange)
    print('Figure Generation done!')

    input_png_path = 'graphs/%s/*.png' % gif_name
    output_gif_path = '%s/%s.gif' % (out_dir, gif_name)
    command = 'convert -delay 60 %s %s' % (input_png_path, output_gif_path)

    print('Generating GIF Anime')
    os.system(command)
    print('Done!!')


if __name__ == "__main__":
    main()
