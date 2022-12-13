#!/usr/bin/env python3
from pprint import pprint

class File:
    def __init__(self, name, size):
        self.name = name
        self.size = size

    def updateSize(self):
        return self.size

    def __str__(self):
        return f'File {self.name}, size {self.size}'

    def __repr__(self):
        return self.__str__()


class Dir:
    def __init__(self, fullPath, name, size, contents):
        self.fullPath = fullPath
        self.name = name
        self.size = size
        self.contents = contents

    def updateSize(self):
        self.size = sum(map(lambda x: x.updateSize(), self.contents))
        return self.size

    def __str__(self):
        return f'Dir {self.fullPath} {self.name}, size {self.size}, contents {self.contents}'

    def __repr__(self):
        return self.__str__()


def parse(fs, dir):
    for d in fs[dir]:
        if isinstance(d, Dir):
            d.contents = fs[d.fullPath]
            parse(fs, d.fullPath)
            d.updateSize()
    return fs


def part1(fs):
    s = sum([d.size for d in fs.contents if isinstance(d,Dir) and d.size<100000])
    for d in fs.contents:
        if isinstance(d,Dir):
            s += part1(d)
    return s

def part2(fs, size):
    candidates = []
    for d in fs.contents:
        if isinstance(d, Dir) and d.size>=size:
            candidates.append(d.size)
            p = part2(d,size)
            if p:
                candidates.append(p)
    return min(candidates) if len(candidates) else None


def main():
    with open("input.txt", 'r') as f:
        data = f.readlines()
    filesystem = {} # Dict with dir name as key and list of content as value
    dirHistory = []
    for line in data:
        match line.split():
            case ['$', 'ls']:
                pass
            case ['$', 'cd', dir]:
                dir = dir.strip()
                if dir == '..':
                    dirHistory.pop(-1)
                else:
                    dirHistory.append(dir)
            case ['dir', dir]:
                if len(dirHistory) == 1:
                    d = dirHistory[0]
                    dir = Dir(d+dir, dir, 0, [])
                else:
                    d = '/'.join(dirHistory)[1:] # from 1 to remove double / in begining
                    dir = Dir(d+'/'+dir, dir, 0, [])

                if d in filesystem:
                    filesystem[d].append(dir)
                else:
                    filesystem[d] = [dir]
            case [size, fname]:
                if len(dirHistory) == 1:
                    d = dirHistory[0]
                else:
                    d = '/'.join(dirHistory)[1:] # from 1 to remove double / in begining
                if d in filesystem:
                    filesystem[d].append(File(fname, int(size)))
                else:
                    filesystem[d] = [File(fname, int(size))]
    fs = parse(filesystem, '/')['/']
    fs = Dir('/', '/', 0, fs)
    fs.updateSize()

    print("Answer part 1:\n", part1(fs))

    diskSize = 70000000
    neededFreeSpace = 30000000
    createFreeSpace = fs.size - (diskSize - neededFreeSpace)
    print(part2(fs, createFreeSpace))

if __name__ == '__main__':
    main()
