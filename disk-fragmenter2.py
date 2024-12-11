def parse_disk(disk_map):
   return [int(c) for c in disk_map]

def expand_disk(lengths):
   result = []
   file_id = 0
   for i, length in enumerate(lengths):
       if i % 2 == 0:  # File
           result.extend([file_id] * length)
           file_id += 1
       else:  # Free space
           result.extend(['.'] * length)
   return result

def find_file_spans(disk):
   # Find start and length of each file
   spans = {}
   i = 0
   while i < len(disk):
       if disk[i] != '.':
           file_id = disk[i]
           start = i
           while i < len(disk) and disk[i] == file_id:
               i += 1
           spans[file_id] = (start, i - start)
       else:
           i += 1
   return spans

def find_free_space(disk, start, needed_length):
   # Find leftmost free space that can fit the file
   i = 0
   while i < start:
       if disk[i] == '.':
           space_start = i
           length = 0
           while i < len(disk) and disk[i] == '.' and length < needed_length:
               length += 1
               i += 1
           if length >= needed_length:
               return space_start
       i += 1
   return None

def compact_disk_whole_files(disk):
   spans = find_file_spans(disk)

   # Process files in descending order of ID
   for file_id in sorted(spans.keys(), reverse=True):
       start, length = spans[file_id]
       new_pos = find_free_space(disk, start, length)

       if new_pos is not None:
           # Move the whole file
           file_content = disk[start:start + length]
           disk[start:start + length] = ['.'] * length
           disk[new_pos:new_pos + length] = file_content

def calculate_checksum(disk):
   return sum(pos * file_id
             for pos, file_id in enumerate(disk)
             if file_id != '.')

def solve(disk_map):
   lengths = parse_disk(disk_map)
   disk = expand_disk(lengths)
   compact_disk_whole_files(disk)
   return calculate_checksum(disk)

print(solve(open("disk-fragmenter.input", "r").read().strip()))
