#!/usr/bin/env python3
"""
Generate an SVG visualisation of a 10x10 matrix and a provided path.

Writes output to ../visuals/path.svg (relative to this script).
No external dependencies required.
"""
from pathlib import Path
from collections import deque

# matrix from app/Main.hs (row-major, rows 0..9, cols 0..9)
matrix = [
    [1,1,0,1,1,0,0,0,1,0],
    [1,1,1,1,1,1,0,0,0,1],
    [1,0,0,0,1,1,1,1,0,1],
    [1,0,0,1,0,0,0,1,1,0],
    [1,0,0,1,0,0,0,1,0,0],
    [0,0,1,1,0,0,0,0,1,1],
    [0,0,0,1,0,1,0,0,0,1],
    [0,0,1,0,0,1,1,1,1,0],
    [1,0,0,1,0,0,0,1,0,0],
    [1,1,0,1,0,0,0,1,0,1],
]

# path provided by the user (100 coordinates)
path_str = \
    ">(0,0)>(1,0)>(2,0)>(3,0)>(4,0)>(1,1)>(1,2)>(1,3)>(1,4)>(2,4)>(2,5)>(2,6)>(2,7)>(3,7)>(4,7)>(3,8)>(1,5)>(0,4)>(0,3)>(0,1)>(0,2)>(0,5)>(0,6)>(1,6)>(1,7)>(1,8)>(2,8)>(2,9)>(1,9)>(0,9)>(0,8)>(0,7)>(2,1)>(3,1)>(4,1)>(5,1)>(6,1)>(7,1)>(8,1)>(9,1)>(9,0)>(8,0)>(8,2)>(9,2)>(9,3)>(8,3)>(2,2)>(3,2)>(4,2)>(5,2)>(5,3)>(6,3)>(4,3)>(3,3)>(2,3)>(3,4)>(4,4)>(5,4)>(6,4)>(7,4)>(8,4)>(9,4)>(9,5)>(9,6)>(9,7)>(8,7)>(7,7)>(7,8)>(7,6)>(7,5)>(6,5)>(8,6)>(8,5)>(3,5)>(4,5)>(5,5)>(5,6)>(6,6)>(6,7)>(6,8)>(6,9)>(5,9)>(5,8)>(3,6)>(4,6)>(3,9)>(4,9)>(4,8)>(5,0)>(6,0)>(7,0)>(5,7)>(6,2)>(7,2)>(7,3)>(7,9)>(8,9)>(9,9)>(8,8)>(9,8)"

def parse_path(s):
    pts = []
    for part in s.split('>'):
        if not part:
            continue
        part = part.strip()
        if part.startswith('(') and part.endswith(')'):
            x,y = part[1:-1].split(',')
            pts.append((int(x), int(y)))
    return pts

def make_svg(matrix, path, cell=50, padding=20, out_path='visuals/path.svg'):
    rows = len(matrix)
    cols = len(matrix[0]) if rows > 0 else 0
    width = cols * cell + padding * 2
    height = rows * cell + padding * 2

    def cell_center(r, c):
        x = padding + c * cell + cell / 2
        y = padding + r * cell + cell / 2
        return x, y

    svg_lines = []
    svg_lines.append('<?xml version="1.0" encoding="UTF-8"?>')
    svg_lines.append(f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" viewBox="0 0 {width} {height}">')
    svg_lines.append('<rect width="100%" height="100%" fill="#ffffff"/>')

    # arrow marker for jump connectors (dashed arrows)
    svg_lines.append('''<defs>
  <marker id="arrow" markerWidth="10" markerHeight="10" refX="8" refY="5" orient="auto" markerUnits="strokeWidth">
    <path d="M0,0 L10,5 L0,10 L2,5 z" fill="#666" />
  </marker>
</defs>''')

    # title
    svg_lines.append(f'<text x="{padding}" y="{padding-5}" font-family="sans-serif" font-size="14" fill="#333">counted 10 islands — visited {len(path)} districts</text>')

    # draw cells (land/water)
    for r in range(rows):
        for c in range(cols):
            x = padding + c * cell
            y = padding + r * cell
            if matrix[r][c] == 1:
                fill = '#d4f1c5'  # light green for land
            else:
                fill = '#f0f7ff'  # light blue/white for water
            svg_lines.append(f'<rect x="{x}" y="{y}" width="{cell}" height="{cell}" fill="{fill}" stroke="#bbb" stroke-width="1"/>')

    # compute islands (4-connectivity) and assign colors per island
    def in_bounds(r, c):
        return 0 <= r < rows and 0 <= c < cols

    dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    island_id = [[-1] * cols for _ in range(rows)]
    cur_id = 0
    for r in range(rows):
        for c in range(cols):
            if matrix[r][c] == 1 and island_id[r][c] == -1:
                q = deque()
                q.append((r, c))
                island_id[r][c] = cur_id
                while q:
                    rr, cc = q.popleft()
                    for dr, dc in dirs:
                        nr, nc = rr + dr, cc + dc
                        if in_bounds(nr, nc) and matrix[nr][nc] == 1 and island_id[nr][nc] == -1:
                            island_id[nr][nc] = cur_id
                            q.append((nr, nc))
                cur_id += 1

    # color palette (repeat if not enough colors)
    palette = [
        '#ff7f0e', '#1f77b4', '#2ca02c', '#d62728', '#9467bd',
        '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf'
    ]

    def island_color(r, c):
        if not in_bounds(r, c):
            return '#ffffff'
        iid = island_id[r][c]
        if iid == -1:
            return '#ccccff'  # water fallback
        return palette[iid % len(palette)]

    # draw path segments only between adjacent nodes (no long jumps)
    def manhattan(a, b):
        (r1, c1), (r2, c2) = a, b
        return abs(r1 - r2) + abs(c1 - c2)

    # draw connecting segments where adjacent; color by island if both same island else gray
    for i in range(len(path) - 1):
        a = path[i]
        b = path[i + 1]
        if manhattan(a, b) == 1:
            x1, y1 = cell_center(a[0], a[1])
            x2, y2 = cell_center(b[0], b[1])
            col = '#888888'
            if in_bounds(a[0], a[1]) and in_bounds(b[0], b[1]):
                ida = island_id[a[0]][a[1]]
                idb = island_id[b[0]][b[1]]
                if ida == idb and ida != -1:
                    col = palette[ida % len(palette)]
                else:
                    col = '#888888'
            svg_lines.append(f'<line x1="{x1}" y1="{y1}" x2="{x2}" y2="{y2}" stroke="{col}" stroke-width="4" stroke-linecap="round" opacity="0.9" />')

    # draw dashed arrow connectors for jumps (non-adjacent consecutive visits)
    # shorten lines a bit so arrows don't overlap the node circles
    def shortened_points(x1, y1, x2, y2, offset=12):
        dx = x2 - x1
        dy = y2 - y1
        dist = (dx * dx + dy * dy) ** 0.5
        if dist == 0:
            return x1, y1, x2, y2
        ox = offset * dx / dist
        oy = offset * dy / dist
        return x1 + ox, y1 + oy, x2 - ox, y2 - oy

    for i in range(len(path) - 1):
        a = path[i]
        b = path[i + 1]
        if manhattan(a, b) > 1:
            x1, y1 = cell_center(a[0], a[1])
            x2, y2 = cell_center(b[0], b[1])
            sx1, sy1, sx2, sy2 = shortened_points(x1, y1, x2, y2, offset=12)
            col = '#666666'
            if in_bounds(a[0], a[1]) and in_bounds(b[0], b[1]):
                ida = island_id[a[0]][a[1]]
                idb = island_id[b[0]][b[1]]
                if ida == idb and ida != -1:
                    col = palette[ida % len(palette)]
            svg_lines.append(f'<line x1="{sx1}" y1="{sy1}" x2="{sx2}" y2="{sy2}" stroke="{col}" stroke-width="2" stroke-dasharray="6,4" stroke-linecap="round" marker-end="url(#arrow)" opacity="0.95" />')

    # draw circles and visit order label for every visited point
    for idx, (r, c) in enumerate(path):
        x, y = cell_center(r, c)
        fill = island_color(r, c)
        svg_lines.append(f'<circle cx="{x}" cy="{y}" r="10" fill="{fill}" stroke="#333" stroke-width="1"/>')
        svg_lines.append(f'<circle cx="{x}" cy="{y}" r="6" fill="#ffffff" stroke="{fill}" stroke-width="2"/>')
        label = str(idx + 1)
        svg_lines.append(f'<text x="{x}" y="{y+4}" font-family="monospace" font-size="10" text-anchor="middle" fill="#000">{label}</text>')

    # draw grid lines on top
    for r in range(rows + 1):
        y = padding + r * cell
        svg_lines.append(f'<line x1="{padding}" y1="{y}" x2="{padding+cols*cell}" y2="{y}" stroke="#aaa" stroke-width="1" />')
    for c in range(cols + 1):
        x = padding + c * cell
        svg_lines.append(f'<line x1="{x}" y1="{padding}" x2="{x}" y2="{padding+rows*cell}" stroke="#aaa" stroke-width="1" />')

    svg_lines.append('</svg>')

    out = Path(__file__).parent.parent / out_path
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text('\n'.join(svg_lines), encoding='utf-8')
    print(f'Wrote {out}')

if __name__ == '__main__':
    path = parse_path(path_str)
    make_svg(matrix, path)
