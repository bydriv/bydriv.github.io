struct asset {
  const char *path;
  unsigned int len;
  const char *data;
};

unsigned int assets_len = 9;

struct asset assets[9] = {
{"pixelart/emily/walk.png", 717
, "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a\x00\x00\x00\x0d\x49\x48\x44\x52\x00\x00\x00\x40\x00\x00\x00\x40\x08\x06\x00\x00\x00\xaa\x69\x71\xde\x00\x00\x00\x09\x70\x48\x59\x73\x00\x00\x5c\x46\x00\x00\x5c\x46\x01\x14\x94\x43\x41\x00\x00\x00\x1b\x74\x45\x58\x74\x53\x6f\x66\x74\x77\x61\x72\x65\x00\x43\x65\x6c\x73\x79\x73\x20\x53\x74\x75\x64\x69\x6f\x20\x54\x6f\x6f\x6c\xc1\xa7\xe1\x7c\x00\x00\x02\x58\x49\x44\x41\x54\x78\xda\xed\xd8\xd1\x99\xa2\x30\x14\x05\xe0\x93\x0a\xd6\x0e\x4c\x07\x3b\x1d\x24\x76\x60\x09\x94\x60\x07\xc9\xed\x80\x12\x28\xc1\x0e\x4c\x3a\x98\xe9\x20\x76\xe0\x56\x90\x7d\x10\x32\x22\x38\x30\xe2\xe8\xc3\x1c\x5e\xe4\x13\xbf\x9c\x7b\xff\x04\x0d\xaa\x9c\x33\x7e\xf3\xa1\x08\x40\x00\x02\x10\x80\x00\x04\x20\x00\x01\x08\x40\x00\x02\x10\x80\x00\x04\x20\x00\x01\x08\x40\x00\x02\x10\x80\x00\x04\x20\x00\x01\x08\x40\x00\x02\x10\x80\x00\x04\x20\x00\x01\x08\x40\x00\x02\x10\x80\x00\x04\x20\x00\x01\x08\x40\x00\x02\x10\x80\x00\x04\xf8\x4d\x00\x22\xe5\x82\x78\x0f\x97\xb3\x7a\x6a\x65\x4f\xca\x1f\x07\x10\xc9\x31\x04\x84\x10\x60\xad\x05\x00\x84\x10\xbe\x57\xc4\x92\x06\x9e\x98\x3f\x04\x68\xc3\xbb\xe3\xae\x22\x96\x34\xf0\xe4\xfc\x3e\xc0\x55\xf8\xd8\x31\x59\xc4\x92\x06\x5e\x90\xff\x09\x30\x23\x7c\xb2\x88\x25\x0d\xbc\x28\x7f\x14\x40\xd7\x1e\x69\xe7\x8b\xde\xa5\xe2\xcd\x02\x96\x36\xf0\xa2\xfc\x51\x80\x2e\x50\xd7\x1e\xcd\x9b\x1d\x14\xf1\xd3\x0d\x3c\x33\xbf\x07\x70\xdc\x5a\xa4\x9d\x87\xd9\x37\x50\x95\x46\x6e\xd2\x79\x99\xb4\xe7\x71\x5b\x95\xa2\x7e\xa2\x81\x57\xe4\x0f\x56\x80\xae\x3d\xd6\xfb\x00\xa5\x3d\xf2\xb6\x2d\x60\xaf\x91\x93\x47\x57\xe0\x4f\x36\xf0\xec\xfc\x01\x40\x19\xfc\xdf\xb1\xf7\xfb\x28\x2b\x3d\x6b\x09\x2e\x69\xe0\x72\x06\xdd\x29\xf5\x6f\xdc\x3f\x6b\x25\x4a\xe5\x6e\x36\x1f\x95\x7f\x1b\x00\xc8\xc7\x8f\x08\x00\x65\x19\xcd\xf9\x06\xbe\x05\x38\xd9\xc0\x08\x42\xf5\x7e\x3e\x4f\x27\xc0\x18\x33\x0b\xe0\xbb\xf9\x83\x7d\x00\x9c\x53\xa2\x54\x76\xed\xfb\xa2\x14\x6c\x08\x08\xd6\x4e\x6f\x42\xfa\x45\xa0\x03\x4c\x27\x20\xd8\xcf\x7b\x71\xce\x38\x70\x0e\xa2\x14\x5c\xce\x88\x31\x96\xfc\xb6\x36\xf5\xa8\xfc\xb1\xad\x70\x8e\x31\x9e\x3f\x08\x20\xa6\x84\xa0\x35\x9c\x73\x88\x31\xc2\x18\x73\x57\xf1\x73\x11\x63\x8c\xd9\x18\x03\x11\x81\x4d\x09\x46\x6b\x08\x00\x6b\x2d\x8c\x31\x00\x30\x3d\x09\xd6\x96\x4c\x63\x4c\xa9\xa5\x7d\x55\x5f\x6e\x85\x45\x24\xbb\x5d\x05\x79\x5f\x23\x34\x02\x5b\x39\x84\x46\xd0\xec\x2c\xd6\x7f\xcd\x9c\xbd\xf8\x32\x40\x00\xc7\x8f\x98\xab\x3a\x94\x6c\x5b\x39\xb8\xb7\x23\xa4\x6e\xe0\x9c\x7b\x28\x60\x0f\x40\x2e\x1e\x20\xdc\xae\x82\x5a\x69\x78\xef\x11\x12\x60\x35\x26\xc3\xbb\x31\xaa\xad\x45\x73\x32\x03\xc0\x66\x1f\x66\x8f\xd1\x65\x7a\xef\x91\x4f\x09\x52\x37\xe5\xfa\x57\x63\x74\xf9\xd7\x80\xd5\x2a\x8e\xe6\x4f\xfe\x1f\xb0\xd9\x6c\x32\x00\x1c\x0e\x87\x59\x4f\x62\xdd\x0a\x52\x2b\x0d\xb4\xf2\x00\x70\xd8\x37\xb3\x66\xf0\xde\xdc\xeb\xfc\xcd\xb6\x2a\x5f\xa6\x00\x0a\xe2\x75\xfe\x7f\x61\x79\x18\xbd\xf1\xf2\x99\xf2\x00\x00\x00\x00\x49\x45\x4e\x44\xae\x42\x60\x82"},
{"pixelart/font/alpha.png", 567
, "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a\x00\x00\x00\x0d\x49\x48\x44\x52\x00\x00\x00\x68\x00\x00\x00\x20\x08\x04\x00\x00\x00\x54\x1e\xf5\x2b\x00\x00\x00\x09\x70\x48\x59\x73\x00\x00\x0b\x12\x00\x00\x0b\x12\x01\xd2\xdd\x7e\xfc\x00\x00\x00\x1b\x74\x45\x58\x74\x53\x6f\x66\x74\x77\x61\x72\x65\x00\x43\x65\x6c\x73\x79\x73\x20\x53\x74\x75\x64\x69\x6f\x20\x54\x6f\x6f\x6c\xc1\xa7\xe1\x7c\x00\x00\x01\xc2\x49\x44\x41\x54\x58\xc3\xed\x58\xd1\x92\xc4\x20\x08\x83\xff\xff\xe8\xec\xcb\xf5\xaa\x12\x02\xf6\x76\x67\x7a\x9d\xdd\x97\x6a\xd5\x4a\x20\x06\x59\x87\x3d\xeb\xe7\x30\x33\x3b\x50\xb9\x8d\xbd\xa3\x5f\x8d\x8f\x3e\x71\xd2\x67\xeb\x7d\x78\xae\xe3\xeb\x9b\x71\xde\xdc\xb7\xe1\xed\x4f\xcb\x11\x37\xb8\xfa\x1c\x8d\x61\x7d\xb5\x6e\x5d\xb3\xc2\xd5\xfb\x35\x00\x31\x8f\x2a\x03\x55\x84\xd8\x7a\xc6\x01\x0d\x08\x3d\x7b\x0e\x40\xcc\x80\x2a\x02\x7b\x11\x42\xfa\x7d\xa3\xed\x9c\x90\x2d\x40\x77\xa2\x1c\x07\xe4\xbf\xd4\x62\x94\x3b\xd6\xb4\x28\x77\x45\x14\x22\x7d\x76\x44\x01\x34\x22\x39\xc5\x17\x40\x0f\x94\xed\x87\x01\xaa\x28\xb0\x4b\xc1\x7c\xbc\x3a\x4b\x88\x14\x5a\x65\x99\xf6\x87\xef\x9f\x2a\xe7\x6f\x13\x87\xde\xa1\xdf\x01\xd4\x11\x23\x7c\x0e\x90\xce\x2a\xf9\x7c\x6c\x8a\x06\x81\x33\x52\x0e\x92\x12\x4a\xa5\x22\xe5\x8c\xfa\xef\x3a\xa0\x2a\x35\x0c\xaa\xd8\x05\x54\xcb\x30\x84\x2c\x57\x80\xd6\xf6\xda\xe2\x11\xa1\x22\xdf\x15\x05\x27\x82\xa2\x29\x97\x1b\x50\x1b\x18\x0f\x7d\xe5\xa0\x49\x14\xbe\x79\xe8\x3f\x00\x4a\x0f\x99\x75\xc6\x7b\x2a\xe4\xcb\x85\x1f\xe5\x8e\xb3\x9c\xb3\x82\x61\x3e\x69\x7e\x16\x78\xbd\x04\xa6\xfb\xd9\x86\xf5\x7a\xb6\xa6\xbf\x5f\x0b\x90\x11\x8f\x2a\x03\x75\x84\x20\x55\xb2\x07\x08\xa9\x3d\x93\xca\xb2\x12\xbc\x1b\x81\xbd\x08\x41\x7c\xdf\x02\x01\xa3\xc3\x5c\x50\x8e\x00\xba\x17\xe5\xd8\x7c\x15\x21\x3f\x67\xf4\x28\xb7\x2f\x0a\x9e\xe4\xac\xae\x28\xc4\xf9\x10\x97\xdf\x00\xe8\x9b\x87\xee\xff\xbf\x1c\xa3\xc0\xf5\xbc\x94\x5d\x75\x62\x99\x90\xc9\x33\xa6\xcb\xaa\x33\x81\xe6\x77\xbd\x53\xe5\xfc\xcf\xa2\x70\xf5\xd0\x57\x80\xb8\xc3\x9d\x4b\xcf\x67\x00\xe9\xac\x52\xad\x5f\x6f\xef\x59\x85\x45\xfb\x27\xe5\x20\xf3\x90\x49\x95\x8a\x94\xd3\x52\xfe\x7e\x40\x18\xcb\x87\x0e\xa0\x5a\x86\x21\x65\xd9\x8a\x71\x56\x3e\xec\x52\x78\x4b\x14\x78\x3d\xe4\x2d\x03\xf3\xec\x6e\x2d\x40\xdc\xb1\xe0\x79\xf0\x71\xb2\xfd\x02\x06\x0a\xf6\x10\xf0\xf8\xe4\x68\x00\x00\x00\x00\x49\x45\x4e\x44\xae\x42\x60\x82"},
{"pixelart/font/hiragana.png", 869
, "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a\x00\x00\x00\x0d\x49\x48\x44\x52\x00\x00\x00\x60\x00\x00\x00\x30\x08\x06\x00\x00\x00\xee\x16\x7f\xcf\x00\x00\x00\x09\x70\x48\x59\x73\x00\x00\x0b\x12\x00\x00\x0b\x12\x01\xd2\xdd\x7e\xfc\x00\x00\x00\x1b\x74\x45\x58\x74\x53\x6f\x66\x74\x77\x61\x72\x65\x00\x43\x65\x6c\x73\x79\x73\x20\x53\x74\x75\x64\x69\x6f\x20\x54\x6f\x6f\x6c\xc1\xa7\xe1\x7c\x00\x00\x02\xf0\x49\x44\x41\x54\x78\xda\xed\x5c\xdb\x6e\xc5\x30\x08\x83\xff\xff\x68\xf6\x34\x69\x3a\x6a\x82\x6d\x48\xdb\x6d\xe4\x69\x52\x7b\xd2\x04\x73\x31\x81\xcc\x23\xc2\x66\x3c\x37\xfc\x03\x80\x30\x33\x5f\xbc\xfb\xfd\x0c\x79\xc7\x84\xe7\xcc\xb3\xab\x77\x3f\x35\xc9\x85\xf5\x5b\x71\x7f\x25\x00\x62\xb3\xf8\xcf\x8f\xb3\x02\xb8\x32\xb3\xd5\x73\xdf\x7c\x2f\x13\xf2\x4e\x48\x28\x40\x2b\xc0\x7f\xfe\xae\x4d\x09\x57\x16\x10\x89\x00\x3d\xd9\x68\xb7\x96\x57\x2d\x20\x2e\xd6\x8d\xcc\xb9\x02\xd1\x81\x35\x78\xe2\x45\xcc\xcc\x7c\x65\x01\xac\x86\xa0\x1a\xe8\x17\x1b\x41\xad\xcb\x1b\x41\x56\x85\x67\x80\x0c\x50\x0b\x88\x9f\x00\xc4\x85\x76\x58\x51\x23\x15\xff\x19\x1b\x0d\x76\x40\x50\x21\xba\x27\xe4\x3d\x04\x00\x16\xc4\x6d\x0c\x58\x09\x39\xd0\xc9\x09\x0b\x40\xe6\xd8\x29\x07\xba\x06\x34\x46\x64\x01\x1c\x91\x01\x64\xa5\x3e\x34\xf4\x5d\x34\x74\xc6\xcb\x01\xe8\xe0\xc8\x59\x90\x5d\x51\xd2\x8c\x81\x21\xee\xe7\xb5\x00\x74\x26\x3a\x48\x80\xb5\x62\x10\x57\x19\x48\x75\xfe\x47\x01\xb0\x86\xcd\x57\xa9\x1b\x3a\x8f\x22\x40\x34\x80\x1e\x73\x41\x2c\xcd\x52\xb5\x58\xf9\xed\x2a\x2b\x66\xb2\xed\x3b\x2c\x80\x56\x80\x4f\x00\xaa\x99\xa8\x37\xb8\x14\x65\x6e\x34\x49\xea\x88\x75\xad\x20\x32\x41\x18\xe5\xd8\xea\x3b\x59\x9e\x80\xf2\xff\xa7\x84\x2f\x59\xc1\xd0\xd0\xc9\x03\x06\x00\x96\x47\x57\xcf\xd3\x11\x1a\xab\xf2\xfc\x2a\x13\x53\x03\xf8\x6b\x12\xb1\x15\x0b\xc9\x0e\xf7\x18\x01\xa0\xec\x2c\xab\x27\x9c\x28\xba\xd0\x4a\x7c\x55\x0f\x30\xeb\x3f\x49\x64\xb8\x7d\x76\x64\xcc\xd2\xd5\x55\x90\x7e\x22\x9b\x4e\xeb\x01\xcc\x99\x7c\x45\x03\x10\x6e\x9f\xfd\x1d\x04\x4b\x42\x01\xee\xa0\xa1\x66\xb8\x0b\x8e\x2b\x17\x54\x2d\xb7\x29\x71\x24\xab\x47\xa0\x60\xa1\xc7\x15\x96\x68\xff\x69\x2a\x7b\x69\x01\x5d\x41\x4c\x05\x08\xad\x17\x38\x6a\xde\x8d\x04\xa1\x3b\x57\xf2\xa1\xa1\x93\x07\xcc\x60\x59\x10\x42\x05\x91\xb6\x0e\x03\x73\x81\xe3\xe7\x30\x07\x8e\x21\x8e\x59\x40\xd6\x97\x83\x0a\x18\xe1\xf1\x95\x96\x17\x35\x91\x62\xfa\x7e\xba\x58\xe0\x32\x13\x36\x30\x69\x52\x4e\x43\x19\x16\xa5\x34\x05\xa0\x34\x53\xf9\xfe\x6d\x16\xa0\xf6\xe5\x54\x01\x60\x7a\x85\x10\x70\x42\xb4\x82\x8a\x1b\xa3\x99\x94\x9a\x09\x57\x12\x35\x07\xb5\x8f\xa1\x73\xdd\x00\x74\xc5\x22\xda\x05\x21\x9b\xaf\x72\xe9\xdd\xb9\x50\x10\x00\x74\x06\xe1\x8e\xef\x9b\x15\x2b\x62\x33\x26\x0f\x18\x00\x2a\x67\xf9\x59\x00\xab\xb6\xb7\x23\xed\xe9\x5d\xb5\x0a\xb3\xbc\x7d\xbe\x1d\x80\xae\x1e\x78\x86\xe9\xa0\x2c\xa6\xa3\x6f\x89\x89\x51\x0a\x09\x91\x94\x60\x67\x01\x4c\x20\xab\x66\xba\x28\x00\x27\x2d\x20\x53\x10\x03\xc1\xa3\x48\x08\x73\x3f\xa0\x4a\xc1\xd8\xbe\xfd\x6e\x0b\xe8\xb8\xff\xc0\xd4\x12\xa0\x5c\x68\x75\x3f\x00\xdd\x74\xa7\xff\x65\x0a\x32\xac\x10\x59\x7a\x58\x2d\x48\xc1\x2e\x90\xa9\x88\x49\x1f\x20\x03\x39\x12\xc4\x83\x31\x71\x51\x80\x6a\xd1\x1e\xbd\xc2\x34\xf5\x80\xc9\x03\x66\xbc\x16\x00\xd4\x4d\x3d\xb5\x36\xff\x0f\x00\xdc\x76\x51\xfa\xaf\x00\xc0\xb0\x8b\x8c\xe2\xbe\x15\x80\x23\xdf\x66\xbb\xa3\x99\x1b\x30\x66\xd8\xbd\xe0\xdf\x04\x00\xba\x07\x58\x96\xab\x1b\x32\x4a\x02\x72\x67\xc2\xf6\xa4\xf6\xb3\x95\xb9\xed\xf3\xdd\x05\x8d\x13\x9b\x57\x5d\xd0\xb1\xc3\x30\x51\x61\xda\xfe\xa9\xc9\xd0\xd0\x87\xc7\x17\xbd\xe0\x75\xbe\x28\xf8\xb6\xb4\x00\x00\x00\x00\x49\x45\x4e\x44\xae\x42\x60\x82"},
{"pixelart/font/katakana.png", 786
, "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a\x00\x00\x00\x0d\x49\x48\x44\x52\x00\x00\x00\x60\x00\x00\x00\x30\x08\x06\x00\x00\x00\xee\x16\x7f\xcf\x00\x00\x00\x09\x70\x48\x59\x73\x00\x00\x0b\x12\x00\x00\x0b\x12\x01\xd2\xdd\x7e\xfc\x00\x00\x00\x1b\x74\x45\x58\x74\x53\x6f\x66\x74\x77\x61\x72\x65\x00\x43\x65\x6c\x73\x79\x73\x20\x53\x74\x75\x64\x69\x6f\x20\x54\x6f\x6f\x6c\xc1\xa7\xe1\x7c\x00\x00\x02\x9d\x49\x44\x41\x54\x78\xda\xed\x5b\xd1\x72\xc4\x20\x08\x84\xff\xff\x68\xfa\xd2\x76\xee\xae\x89\xb0\xb0\x18\xa7\x83\x6f\x57\x6b\xa2\x2c\xe8\x2e\x12\x35\x33\x99\xf6\x5c\xd3\x6f\x00\xee\x50\xd0\x8f\xdf\x76\xf1\xb7\xab\x3e\xbb\x19\xbb\x7a\x76\xe4\x1d\xb2\x18\x13\x99\x3f\x32\x5e\x81\xfe\x32\x00\x11\x83\xae\x16\x18\x31\xc0\x0a\xa0\xea\x78\xc4\x21\x2c\x61\x40\x5b\xcc\x85\x0e\xc0\xcf\xc3\xcd\x89\x00\x03\x0d\x54\x8d\x80\xc8\xf3\xd5\xf9\x2d\x37\x6b\x33\x00\x7c\x06\x00\xbf\x73\xb9\x03\x40\x12\x46\x88\xf6\x21\x5b\x99\x80\x11\x60\xce\x16\xa2\x49\x8f\x47\xe6\x28\x8e\xfd\xde\x9c\xe0\x13\x00\xc4\x30\x19\x23\x56\x17\xa7\xcd\xef\xae\xcc\x2f\xea\xc0\x6f\x7d\xaf\x00\x18\xc1\x43\x2c\xb9\xbd\x20\x91\xd7\xe1\x00\x16\xdc\x1a\xa5\x08\xce\x9f\xff\xd3\xa1\xa1\x67\xd0\xd0\x69\x07\x02\xd0\x45\xbd\x32\x34\x10\xe5\xf2\x9a\x1c\xaf\xe4\xb5\xa6\x01\xc8\xd0\x34\x01\x68\xa8\xc7\x20\x3c\x0d\xc0\xda\x8b\x8f\x01\x80\xad\x52\x23\x87\x6d\x56\xa9\x6a\x53\x64\x1d\x13\x01\x16\xf4\x50\x59\x78\xa5\x17\x41\x15\xef\xec\xda\xe2\x58\x00\x40\xef\x47\x00\xe8\xe6\xf9\x5d\xe2\x48\x1a\xde\x45\x03\x9f\x05\x40\x75\x8b\x40\x92\x81\xd1\x73\x64\x8b\x01\xab\xe0\x0d\x0d\x1d\x1d\x30\x00\xb0\xe4\xbd\xb7\x7d\x44\x59\xce\x8e\x83\x7a\x35\x7e\x1b\x03\x5a\xd1\xd0\x15\xd7\x8f\x18\xe8\x4e\xc4\x45\x17\x89\xea\x90\xe8\x21\xed\x9d\x1f\x8f\x02\x90\xcd\xfd\x47\x3c\x31\x02\x00\x62\x80\xca\x78\x6b\x06\x80\x76\x08\x67\x00\xa8\x0a\xb5\xec\x78\xe6\x7d\x43\x05\x08\xe4\x3e\x00\x4a\x45\x30\x3d\xa8\x63\xfc\xeb\xd8\xac\x9a\xff\x7c\x46\x57\x14\xd8\x0a\x00\x2b\x4e\x3e\xab\x13\x18\xe3\x23\xf9\xa3\x95\x92\xdf\xa1\x82\xff\xac\x63\x68\xe8\xe8\x80\x01\x60\x57\x2e\x26\x42\x63\x9f\xcc\xe7\x6c\xa5\x9f\x11\x1d\xe0\x95\x95\x88\x60\x75\x41\x0c\x21\xc6\x2a\x1a\x38\x0e\x80\xea\x21\x1b\xa1\xad\x08\x40\x59\x9e\x5f\xa5\x90\xff\x02\x00\xc6\x56\xa5\x05\xef\x66\xa5\xbb\x3b\x74\x40\x29\x17\xb4\x43\x84\xc9\x26\x35\x8c\x0a\xcf\x76\x25\xcc\xd8\x83\x3d\x0f\x88\xd4\xfd\x64\x8d\xe0\xe9\x80\x16\xef\xed\x88\x80\x69\xa3\x03\x06\x80\x0e\x46\xc0\xac\x29\x8a\xde\x49\x20\xd5\xd9\x99\xef\x03\x84\xb1\xa6\xe8\x7d\x40\xb5\x7c\xbb\xca\xc9\x59\x67\x14\x73\xbe\x14\x96\x84\x96\xa7\x67\x17\xc0\x2a\x0f\xef\x8c\x00\x05\x1d\x00\xd5\x31\x97\xfd\x77\x34\xb4\xca\xd5\x57\x37\x62\x22\xb9\xaa\x8b\x48\xae\xfe\xe9\x08\x40\x9c\x40\xaf\xb6\x20\x45\x11\xdc\x24\xa4\x4e\x03\x20\x5b\x66\xef\x46\xc0\x8a\x53\x47\x0f\x29\xb6\x7e\xc8\x08\xc5\xee\x43\x38\x9b\x0a\x99\xfb\x80\xd1\x01\xd3\x8e\x07\xc0\x4b\x67\xe8\x00\xd0\x0f\x00\x3b\xd3\xc9\x72\x0a\x7a\xf9\x3a\x0b\x80\x5d\x37\x59\xa7\x44\xc0\x23\x00\xa0\x57\x8e\x22\xb5\x02\xad\xff\x0c\x80\x5d\xe9\x80\x08\xfd\xdb\x91\x23\x3a\x1d\x80\xaa\xad\xde\xfa\x91\xef\x84\x19\x8b\xcf\x5e\xcc\x33\x72\xf5\x5d\xc4\x20\xe3\xac\xf3\x9d\xf0\x29\xed\x0b\x99\x46\x1f\xbf\x85\xa6\x7a\xc1\x00\x00\x00\x00\x49\x45\x4e\x44\xae\x42\x60\x82"},
{"pixelart/maptip/archimedes.png", 215
, "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a\x00\x00\x00\x0d\x49\x48\x44\x52\x00\x00\x00\x10\x00\x00\x00\x10\x08\x06\x00\x00\x00\x1f\xf3\xff\x61\x00\x00\x00\x09\x70\x48\x59\x73\x00\x00\x5c\x46\x00\x00\x5c\x46\x01\x14\x94\x43\x41\x00\x00\x00\x1b\x74\x45\x58\x74\x53\x6f\x66\x74\x77\x61\x72\x65\x00\x43\x65\x6c\x73\x79\x73\x20\x53\x74\x75\x64\x69\x6f\x20\x54\x6f\x6f\x6c\xc1\xa7\xe1\x7c\x00\x00\x00\x62\x49\x44\x41\x54\x38\xcb\xc5\x93\x41\x0e\x00\x21\x08\x03\xe5\x65\x7c\x1d\x5e\x56\xaf\x5d\x53\x89\x09\x24\xcb\x4d\x29\xa3\x56\x30\x00\xc8\xcc\xc5\xe1\xee\x4b\x85\xd2\x19\x03\xb8\xb0\x82\xb2\xde\x00\x40\x25\x14\x40\x1d\x64\x11\x81\x97\x82\x5b\x7e\x06\x50\x5d\x51\x99\xc8\xda\x0f\x40\x09\xd5\x9a\xf7\xa4\x89\xa7\xf3\xd5\x8f\xf4\x01\xed\x27\x8c\x98\xf8\x6f\x1f\xb4\x5b\x79\x64\x98\x3a\xe3\xbc\x01\x5a\xd4\xc9\xe9\x37\x05\xe1\x64\x00\x00\x00\x00\x49\x45\x4e\x44\xae\x42\x60\x82"},
{"pixelart/system/window.png", 292
, "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a\x00\x00\x00\x0d\x49\x48\x44\x52\x00\x00\x00\x20\x00\x00\x00\x20\x08\x06\x00\x00\x00\x73\x7a\x7a\xf4\x00\x00\x00\x09\x70\x48\x59\x73\x00\x00\x5c\x46\x00\x00\x5c\x46\x01\x14\x94\x43\x41\x00\x00\x00\x1b\x74\x45\x58\x74\x53\x6f\x66\x74\x77\x61\x72\x65\x00\x43\x65\x6c\x73\x79\x73\x20\x53\x74\x75\x64\x69\x6f\x20\x54\x6f\x6f\x6c\xc1\xa7\xe1\x7c\x00\x00\x00\xaf\x49\x44\x41\x54\x58\xc3\xed\xd7\xdd\x0a\x80\x20\x0c\x05\xe0\x9d\xf7\x7f\xe8\xd3\x45\x08\xa6\x2e\x97\xbf\x83\xdc\x55\x08\xb5\xaf\x89\xe0\x01\x49\x19\x55\x00\x28\x22\x42\x12\xe6\x77\x46\x01\x00\x30\x7c\x0b\x80\x19\x51\x05\x84\xbf\xaa\x15\x49\x01\x50\x7a\x46\x13\x20\x1a\xa7\x75\x02\x19\x28\x5e\xd7\x20\x45\x40\x3a\xce\x9e\xaa\x6d\x4b\x06\x08\xcd\x7b\x1b\x6b\x5b\x94\x22\x1e\x80\x59\xcd\xdf\x10\x7e\x00\xb3\x9b\x6b\x08\x1f\x80\x55\xcd\x8b\x80\x7b\x6d\x4d\x73\x9f\x80\xed\x5b\x70\x4e\xc1\x01\xac\x06\xa4\x88\x2d\x80\x18\xf1\x4f\xc0\xd6\x2d\xf0\x7b\x0a\xce\x8d\xc8\xc5\xa5\xd4\xc5\xb5\x5c\x0b\x26\x5a\xf0\xb0\x06\x96\x4f\xc1\x44\x83\x94\xa2\x97\x61\x02\x6d\xd1\x6c\x54\xf8\x1c\x9a\x8e\x5b\xe2\x77\xad\x2e\xe3\x0c\x81\xd4\xd8\x75\x08\x9d\x00\x00\x00\x00\x49\x45\x4e\x44\xae\x42\x60\x82"},
{"pixelart/teiri/hijack.png", 947
, "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a\x00\x00\x00\x0d\x49\x48\x44\x52\x00\x00\x00\x80\x00\x00\x00\x80\x04\x03\x00\x00\x00\x31\x10\x7c\xf8\x00\x00\x00\x04\x67\x41\x4d\x41\x00\x00\xb1\x8f\x0b\xfc\x61\x05\x00\x00\x00\x20\x63\x48\x52\x4d\x00\x00\x7a\x26\x00\x00\x80\x84\x00\x00\xfa\x00\x00\x00\x80\xe8\x00\x00\x75\x30\x00\x00\xea\x60\x00\x00\x3a\x98\x00\x00\x17\x70\x9c\xba\x51\x3c\x00\x00\x00\x2d\x50\x4c\x54\x45\x00\x00\x00\x00\x00\x00\x40\x40\x40\xc0\xc0\xc0\xff\xff\xff\x80\x80\x80\x80\x60\x60\x40\x30\x30\xff\xe0\xc0\x00\xff\x00\xff\x81\x81\x20\x18\x18\xc0\xa0\x80\x60\x00\x00\xa0\x40\x40\x1e\xcb\x5e\xb4\x00\x00\x00\x01\x74\x52\x4e\x53\x00\x40\xe6\xd8\x66\x00\x00\x00\x01\x62\x4b\x47\x44\x00\x88\x05\x1d\x48\x00\x00\x00\x07\x74\x49\x4d\x45\x07\xe2\x09\x1e\x07\x15\x0f\x88\xcb\xbf\xf4\x00\x00\x02\xd8\x49\x44\x41\x54\x68\xde\xed\x98\xb1\x6f\xd3\x40\x14\xc6\x5f\x8c\xe2\x0e\x5e\xdc\x30\xb4\x62\x72\xdc\x56\xaa\xd4\x05\xea\x84\xec\xf1\x41\x26\x50\xd5\x76\x40\x4c\x16\x0c\x66\x41\xa2\x4a\x64\x47\x72\x41\x6a\xa5\x92\xfc\x0b\x19\x19\xd8\x2d\x96\x2c\xec\x1d\x58\x91\x98\xd2\x09\x89\x01\x35\x7f\x03\x77\x71\x5c\x9f\xdd\xbc\x7b\x95\x2a\xb6\xfb\x96\xe4\xdd\xef\xbe\x37\x78\x79\xdf\x3b\x00\x2d\xad\x1b\xd5\x1a\xae\x73\x1f\x0e\xeb\x2d\xe6\xaa\x0c\x14\xaf\x3d\x64\xac\xeb\xe0\x06\x8a\x43\xad\xc5\x18\x73\x70\x03\xc5\x1f\x78\x1e\xbf\xe1\xa2\x06\x8a\x03\x2f\x9f\xc9\x17\xaa\x06\x8a\x83\xff\x34\x1a\x78\xfb\xad\x03\xcc\x40\x71\x80\xe6\xf9\x78\xb7\x71\xe4\x60\x06\x82\x4f\x26\xd0\xfc\x3a\x7e\xdd\xd8\x71\xf8\xbf\x65\x9d\x19\x8a\x5a\xc5\xc5\xc1\xb7\xcd\xf1\xe8\xc4\x05\xd4\xa0\xe4\xfc\xc0\x8a\xe3\xd3\xcb\x38\x0e\x30\x83\x9a\x8b\x0b\x97\x83\x9f\x2f\x7b\xc3\x08\x33\x4c\x33\x1e\xac\xe6\x5c\xd6\x69\xef\xe3\x9f\xc1\xe7\x28\xff\x48\x4b\x43\x51\x7f\x17\x7c\x14\x60\x1c\xac\x24\xbe\xba\x8a\xa3\xc2\x50\x6d\x38\x15\x3c\x0c\x30\x0e\x90\xa6\x4e\x0d\x82\xa0\xdc\xb0\x30\x58\xd6\x9a\x53\x33\x4d\xc0\xb8\xd0\x7c\x26\x57\x69\x3a\x9f\x49\x86\x3b\x70\x2d\x2d\x2d\xad\x5c\x86\xdb\xb4\xef\xc3\xc1\x65\xfe\xba\xca\x40\x71\x63\x9b\xb1\x27\x36\x6e\xa0\x38\x18\x7c\x5a\xfb\x36\x6e\xa0\x38\x6f\xc8\x4a\x2d\xab\x06\x8a\xd7\xbb\xac\x37\xf0\xb7\x0f\x30\x03\xc5\xa1\xbe\xcf\x7c\xdf\xdb\x3a\xc0\x0c\x14\x07\xe3\x70\x23\x8e\x4f\x76\x1c\xcc\x40\xf0\xc9\xa4\xe6\x8a\x0b\xae\x9d\x0f\xcf\xdc\x90\xd7\x6a\x2e\x86\xf4\x1b\x71\xb0\x07\x98\x41\xcd\xf9\x81\x19\x2f\x94\x62\x06\x35\xe7\x3f\x61\xe5\x82\xb9\xba\x4e\x56\x73\xae\x30\xe4\x2d\xa6\xd3\x34\xff\x48\xcb\x86\x49\x5e\x9b\x51\xc4\xab\x24\xc1\xf8\xed\x06\xa2\x96\x0c\xb7\x1a\x54\x38\x98\xa6\xb9\x61\x59\x16\x94\x0c\x52\x43\x1e\x1d\x02\x23\x4d\x53\x94\x73\xcd\xe6\x72\x15\x04\xef\xff\x4a\x0d\xef\xc0\xb5\xb4\xb4\xb4\x72\xe9\x7c\x20\xc6\xb5\xef\x79\x8f\x51\x03\xc5\xc5\x85\x63\x7e\x84\x1a\x28\x5e\xf7\xb7\xbc\x5e\xf8\xb6\x85\x19\x28\x0e\x46\x7b\xb1\xcf\xa3\x0d\x09\x2e\xe6\xfd\x62\x9f\xbf\x99\xc6\xb9\xa1\xc8\x03\x2a\x2e\x7e\xdc\x8d\xe1\xf8\x6c\x0f\x35\xa8\x79\x36\xae\xc5\x3a\x9f\x62\x06\x35\x17\xf9\x60\x34\xf8\xfd\xa2\x77\x9a\x62\x06\x33\xe3\xc9\x6a\xce\x15\xbe\xeb\x7d\xfa\x35\xb8\x4c\xf3\x8f\xb4\x6c\x58\x8c\xf7\xa1\xe0\x3f\x12\x8c\xf3\x79\x1f\x5f\x5f\xc7\x52\x3e\xc8\x1a\x4a\xf9\x40\x70\x29\x1f\x54\x38\xcf\x07\x60\x1b\xa5\x7c\x50\x6e\xc8\xc7\xb9\x6d\x7c\x90\xf3\x41\x85\xc3\xed\xf9\x3f\x9b\xab\xf3\x41\x95\x6b\x69\x69\x69\xe5\xd2\xf9\x00\x5c\xdf\x6b\x1f\xa9\xde\x0f\x08\x5e\xef\x1e\x33\xbf\x2d\xbf\x0f\x94\x0d\x14\xe7\xeb\xfc\xf3\x28\xea\xcb\xef\x03\x65\x03\xc5\xc1\x38\xdc\xdc\x3d\x3f\x93\xdf\x07\xca\x06\x82\x8b\x75\xfe\xd1\xab\x8b\x2f\xf2\xfb\x40\x66\x28\xd6\x7d\x15\xcf\xd6\xf9\xf1\xb8\xf4\x3e\x50\x36\xa8\xf9\xaa\xf9\x5f\x31\xa8\x79\x9e\x0f\x3a\x44\x3e\xe8\x50\xf9\xa0\x5f\xc9\x07\x9d\x4a\x3e\xe8\x57\xf2\x41\x87\xca\x07\x7d\x22\x1f\xf4\x4b\xf9\x60\x8d\xc8\x07\xa1\x6d\x80\xce\x07\x5a\xff\x51\xff\x00\xa2\xdc\xa8\x6e\x3d\x15\x88\x45\x00\x00\x00\x00\x49\x45\x4e\x44\xae\x42\x60\x82"},
{"pixelart/teiri/walk.png", 1621
, "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a\x00\x00\x00\x0d\x49\x48\x44\x52\x00\x00\x00\x40\x00\x00\x00\x40\x08\x06\x00\x00\x00\xaa\x69\x71\xde\x00\x00\x00\x09\x70\x48\x59\x73\x00\x00\x0b\x12\x00\x00\x0b\x12\x01\xd2\xdd\x7e\xfc\x00\x00\x00\x1b\x74\x45\x58\x74\x53\x6f\x66\x74\x77\x61\x72\x65\x00\x43\x65\x6c\x73\x79\x73\x20\x53\x74\x75\x64\x69\x6f\x20\x54\x6f\x6f\x6c\xc1\xa7\xe1\x7c\x00\x00\x05\xe0\x49\x44\x41\x54\x78\xda\xed\x9b\x3d\x76\xa3\x3c\x14\x86\x5f\xed\xe0\x2b\xed\x6a\xc8\x0e\xec\x6e\x3a\x44\x37\x65\xca\xe9\x02\x3b\xf0\x0e\x10\x3b\xf0\x0e\x20\xdd\x94\xd9\x01\xa2\x9b\xce\x2e\xa7\x0b\xa9\x92\x72\x76\x70\xbf\xc2\x48\x11\x42\xfc\x18\x65\xac\xc6\x9c\x93\xc3\x39\x3e\x48\xcf\xbd\x6f\x64\xa1\xfb\x63\x46\x44\xb0\x2f\xc6\x18\x01\x00\xe7\xbc\xf7\x79\x5d\xd7\x0c\x37\xb8\x6e\xc9\x67\xb6\x00\x0a\x2e\xa5\x44\x1c\xc7\xfa\xf3\x24\x49\x16\x1b\xe1\xe3\xc0\xad\xf9\x3d\x01\xc6\xe0\xd7\x18\xe1\xe3\x40\x08\xfe\x40\x80\x31\xb8\x39\xd1\x98\x01\xbe\x0e\x84\xe0\x6b\x01\xe6\x06\x03\x40\x51\x14\x90\x52\x8e\x2f\x27\x0f\x07\x42\xf1\x99\x94\x92\xe6\xbe\x53\x71\x1c\xab\x81\x3d\x63\xf2\x3c\x67\xbe\x0e\x34\x4d\x13\x94\xcf\xc8\xf5\x1a\xb0\xae\xa6\x69\x20\x84\xe8\xfd\x49\x29\x91\xe7\x39\xfb\x02\x07\x82\xf2\x19\x11\x69\xf5\x00\x20\xda\x6c\xf0\xfa\xfb\xd7\x65\xb9\xa4\x02\x75\x25\xc0\x22\x0e\xce\x79\x6f\x57\x95\x52\x2a\x15\xbd\x1c\x30\xff\x7b\x21\xf8\x83\x3d\x80\x84\x40\x13\x01\x31\xe7\x28\x2a\x89\x1c\x00\x13\x62\xf2\x95\xe2\xe9\x40\x50\x7e\xef\x2d\xf0\xb0\xdd\x52\xfb\xf1\x01\x6a\xe5\xe7\xa6\xf1\x78\xb8\x0c\x38\x9f\x2f\x93\x44\x11\xf2\xb2\x1c\xdd\xc4\xd6\x38\x10\x92\xaf\x05\x78\xd8\x6e\x29\xda\x6c\x7a\x0f\x89\xc3\x23\xc4\xf1\x65\xb0\xa4\xf8\x6e\xe7\x34\xc2\xc7\x81\x50\x7c\xa7\x00\xf5\xe9\x84\x8c\x31\x94\x44\x78\x4e\x12\x3c\xd5\x35\x92\xfd\x7e\xd2\x08\x1f\x07\xec\xb1\xf5\xe9\xa4\xb9\xca\x0e\x93\xef\x12\x60\x2d\x7f\x20\x40\xfb\xf1\x81\x68\xb3\xd1\x46\x54\x7f\xff\xa2\x3e\x9d\xb4\x01\x7c\xb7\xbb\xa8\x69\x4c\xe2\x72\xe0\x1a\x01\x5d\xec\x64\xbf\x47\x74\x3e\xa3\xdd\xed\xf4\x9c\x45\x96\x0d\xd8\x6b\x04\x34\xf9\xa3\x02\x44\xe7\x33\xd4\x79\xfa\xa9\x12\x7a\x29\x4d\x09\x60\x3b\x90\xfe\xf7\xdf\x55\x02\xea\xf1\x2f\x47\x3c\xa7\x42\x6f\x56\xed\x6e\x87\xfa\xe5\x88\x42\x54\x93\x02\xac\xe1\x0f\x36\xc1\xd7\xf7\x77\x86\xb7\x86\x58\xc4\x41\x44\x0c\x6f\x0d\x25\xa9\x40\x5d\xd7\xac\xc8\x32\xca\xcb\x52\xdf\x5d\x4b\xb0\xfd\xf8\xc0\xeb\xfb\x3b\x32\xc6\x06\x02\xba\x9c\x77\xb1\xf1\x2d\x66\x49\x92\x90\xde\x28\xdf\x1a\x2a\x44\x85\x31\xb6\x8b\xff\xdc\x1d\x7b\xa5\x94\x28\x5b\x39\xca\x1f\x44\x83\x59\xb7\x9b\x96\x44\x0c\x00\x9e\x93\x84\x00\xe0\x69\x41\x14\xf6\xb0\xdd\xd2\xeb\xef\x5f\xc8\x22\xde\x73\x1e\xdf\xe2\x49\xf1\xcc\xcb\xe6\xd9\xf6\x2c\xe1\x9b\xab\xa7\x6c\xe5\x24\x9f\xb9\x0e\x82\x49\x67\x84\xf9\xce\x04\x70\x59\x11\x37\xb8\x6e\xc9\x1f\x08\xa0\xe0\xe6\xb1\xb1\x69\x1a\xfd\x1e\x5d\x62\x84\x8f\x03\xb7\xe6\xf7\x04\x70\xc1\xaf\x35\xc2\xc7\x81\x10\xfc\x81\x00\x2e\xb8\x3d\xd1\x98\x01\xbe\x0e\x84\xe0\x6b\x01\xa6\x06\x2f\x75\xc2\xc7\x81\x50\x7c\x46\x44\x28\x8a\x82\xf2\x3c\x1f\x84\x8c\xea\xdd\x59\x9f\x4e\xbd\xa8\xaa\xae\xeb\x41\x3c\xee\xe3\x40\x48\xbe\x16\x80\x73\xae\x27\x37\x07\xc4\x71\x8c\xa6\x69\x3e\x8f\x97\x8e\x70\xd6\xd7\x81\x90\x7c\x46\x44\x48\x92\x84\x38\xe7\x90\x52\x42\x4a\x09\xde\x1d\x3f\xed\x4b\x07\x14\xdd\xb3\xea\xa0\xe2\xeb\x40\x48\xbe\x4e\x88\xa8\xa5\xb1\xc4\x00\xf5\x9c\x5a\x46\xbe\x0e\x84\xe4\xeb\x4d\xb0\xc8\x32\x92\x6d\xbb\x68\x82\xf6\xcf\x1f\xbc\xbe\xbf\xf7\x12\x12\x3e\x0e\x84\xe4\x5f\xf6\x80\x2c\x23\xd9\x05\x3f\x4b\xaf\xee\xcc\xfd\x25\x0e\x84\xe4\x33\x91\xa6\x57\xc3\x6d\x03\x7c\x1d\x58\x33\xde\x9c\xc3\x87\xaf\x05\x30\x23\x25\xd7\x64\x2a\xd4\x34\xef\x6b\xe1\x63\x0e\xac\xb1\x21\xfd\xf1\x03\x3e\xfc\x2f\x13\x60\x6e\xfc\x57\xcc\x31\x25\xc0\x5a\xbe\xde\x03\xcc\x50\xb1\xc8\x32\x6a\xab\xea\x92\x88\x38\x9d\x98\x8a\xd5\xed\xbb\xfd\x1d\x9e\x32\xc2\x05\x07\xd0\xfb\x1a\xd8\x36\xe4\x65\xc9\x92\xfd\x9e\xa2\xf3\x19\x72\xb3\xc1\x98\x0d\x3e\x7c\x36\x57\x17\x59\x9a\x0f\x18\x13\x91\x73\x0e\xd1\x6d\x3a\x63\x02\xfa\x70\xa7\xf8\x51\xb7\x29\x4e\x89\x77\xcf\x07\xdc\xf3\x01\xf7\x7c\xc0\x3d\x1f\xd0\x1b\xcc\x39\x47\x9e\xe7\xc1\xf2\x01\xb7\xe6\xf7\xf2\x01\x56\x13\x41\x2f\x8a\x9a\x28\x3d\x33\x5f\x07\x42\xf2\x07\xf9\x00\xf3\x6f\xaa\xd3\xc2\xfc\x1a\xf9\x3a\x10\x92\xdf\xcb\x07\x98\xbb\xa6\x94\x12\xd4\x4a\x5d\x62\x06\x80\x87\xef\x3f\xf5\x01\xc2\x54\xd1\xd7\x81\x90\xfc\xc1\x1e\x60\x1a\x41\x42\xa0\x00\x90\xa7\x1c\x8d\x94\x88\xdb\x4b\xa9\xd9\x5e\xc2\xbe\x0e\x84\xe4\xf7\xde\x02\xae\x90\xb2\x7e\x39\x7e\xaa\x15\xf1\x4b\x03\x82\xe3\x14\xe7\xe3\x40\x48\x7e\x3f\x21\xe2\x08\x22\x5c\x25\x66\x3b\x16\xff\x0a\x07\x42\xf1\x47\x13\x22\x53\x25\x66\x97\x01\xbe\x0e\xd8\xe3\xe7\x4a\xec\xce\x84\xc8\x0a\x7e\x4f\x00\x33\x8a\x32\x4b\xcc\x0a\x6e\x87\xb1\x53\xf0\xb9\x1a\xfd\xd8\x1c\x5d\xe5\x56\x3f\xab\x7a\x04\x94\x3d\x2e\x1b\xc6\xf8\x4b\x04\x1c\x17\xa0\xab\xd1\xab\x73\x74\xbb\xdb\xcd\x0a\x30\x26\xa0\x59\xa3\x5f\x32\x47\x2e\x52\x24\x8f\x07\x44\x46\x0e\x4f\x95\xd8\xa7\x04\x58\xc3\xef\xed\x01\xbd\x38\x3c\x49\xa8\xee\x4a\xdb\x8c\x31\xa2\xae\xcc\x3c\x16\xca\xda\x46\xd8\x02\x96\x44\x78\xd8\x6e\x9d\xce\x3b\x43\x5a\x91\x02\xdf\x3e\x0f\x39\x75\x5d\xeb\xde\x01\x97\x0d\x2e\x7e\x16\x7d\x76\x86\x3d\xd5\xb5\x93\x3f\x9a\x0f\x58\x53\xa7\x1f\xd4\xe0\xdf\x1a\xea\x89\xd0\x4a\x3c\x7c\xff\x89\xb9\x5c\xc0\xda\x3e\x05\x17\xbf\x27\x42\x25\x06\xfc\x7b\x3e\xe0\x9e\x0f\xb8\xe7\x03\xee\xf9\x80\xc1\x60\x3b\x8a\x52\x4b\xc8\xec\xb7\xfd\x97\xfd\x01\xb7\xe4\x0f\xfa\x03\x5c\xf1\xb4\x32\x48\x05\x19\x73\xf5\xf9\x6b\x1d\x08\xc9\x77\xf6\x07\x98\x1d\x99\xe6\x5d\x1c\x8f\xb3\xf5\xf9\x35\x0e\x84\xe4\x3b\xfb\x03\xec\x30\xd2\xd5\x76\x3e\x55\x9f\xbf\xd6\x81\x90\x7c\x67\x7f\x80\x19\x46\x02\x70\xb6\x9d\x4f\xd5\xe7\xaf\x75\x20\x24\x7f\xb4\x3f\xc0\x0c\x23\x81\x4b\xdb\xf9\xd2\xfa\xfc\xb5\x0e\x84\xe4\x4f\xf6\x07\x88\xc3\xe3\xe5\xee\x68\x39\x9f\xab\xcf\x5f\xe3\x40\x48\xbe\xb3\x3f\xc0\x0c\x65\xbb\x73\x39\x52\x29\x21\x0e\x87\xab\xea\xfb\x4b\x1d\xb0\xc7\x8b\xe3\x11\x15\xe7\x28\xbb\xd5\x39\xd6\x72\x3f\x57\xa2\x5f\xc2\x1f\x94\xc7\xed\x50\x52\xb5\xab\x8b\xe3\x11\xe2\x70\x58\xd4\x1f\xb0\x44\xc0\xa9\xfe\x00\xc5\x52\xe1\xb0\xdd\xf2\x3e\xd7\x1f\x70\x8d\x80\x6e\x01\x8c\x50\x56\x6d\x18\x69\x25\x20\x8e\x2f\xb3\xb5\xfd\x39\x01\x6d\x07\x9c\x02\x1c\x1e\x51\xa5\x42\xb3\xed\xdf\x2c\x4c\xf5\x07\x28\x01\x96\x0a\xe8\xec\x0f\x48\xf6\x7b\x52\xf1\x74\x49\xa4\x7b\xf7\x97\xf6\x07\x88\xc3\x23\xda\x4a\x2e\x12\x70\x2c\x94\x55\xcc\x8c\x31\x52\xfd\xfe\xae\x5e\x85\x31\xbe\x2d\x60\x94\x72\x27\xff\xdf\xf4\x07\x88\xb4\xf7\x9b\x01\x3b\x16\xff\xe7\xfd\x01\x22\xed\xfd\x66\xc0\xce\x45\x98\xfc\xff\x01\xd1\x73\x34\x2e\xce\x49\x9e\xc4\x00\x00\x00\x00\x49\x45\x4e\x44\xae\x42\x60\x82"},
{"pixelart/verity/walk.png", 1338
, "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a\x00\x00\x00\x0d\x49\x48\x44\x52\x00\x00\x00\x40\x00\x00\x00\x40\x08\x06\x00\x00\x00\xaa\x69\x71\xde\x00\x00\x00\x09\x70\x48\x59\x73\x00\x00\x5c\x46\x00\x00\x5c\x46\x01\x14\x94\x43\x41\x00\x00\x00\x1b\x74\x45\x58\x74\x53\x6f\x66\x74\x77\x61\x72\x65\x00\x43\x65\x6c\x73\x79\x73\x20\x53\x74\x75\x64\x69\x6f\x20\x54\x6f\x6f\x6c\xc1\xa7\xe1\x7c\x00\x00\x04\xc5\x49\x44\x41\x54\x78\xda\xed\x9b\xed\x75\xe3\x28\x18\x85\x2f\x15\xcc\x76\xe0\xe9\x60\xb7\x03\x43\x07\x29\x41\xe9\xc0\x1d\x20\x3a\x70\x07\xab\x12\xd2\x81\x50\x07\x93\x0e\x46\x1d\x6c\x2a\x60\x7f\x58\x30\x08\x83\xf8\x92\xd0\x99\x33\xd6\x39\x39\xb1\x93\x88\x87\x7b\xad\x88\xf7\x03\x11\xa5\x14\x7c\xc7\x34\x4d\xe6\x17\x7d\xdf\x63\x1c\x47\x02\x00\x8c\x31\x35\x8e\x23\x61\x8c\xa9\xbe\xef\x71\xbd\x5e\x09\x0e\x38\x5a\xf1\x89\xcf\x80\x69\x9a\x94\x94\x72\xf5\x33\xfd\xbe\xef\x7b\xf3\x25\xa5\x04\xe7\x9c\xec\x2d\xa0\x25\xff\xc9\x00\x1f\xdc\x9e\x04\xa5\x14\x94\x52\xf4\x7d\x0f\x00\x66\xe0\xbd\x04\xb4\xe6\xaf\x0c\xd8\x82\xbb\x93\x90\x52\x26\xc3\x53\x05\x9c\xc1\x37\x06\xb8\x27\xea\x3f\xce\x99\x44\x8d\x80\xb3\xf8\x5e\x03\xba\x37\x8a\xe1\x43\x06\x27\xe1\x9b\x40\xad\x80\xb3\xf8\x41\x03\xba\x5b\x6f\xfe\xc8\x1d\xe8\x68\x01\x2d\xf9\x5e\x03\xf8\xad\x03\x00\xb0\xdb\x80\xf1\xde\x81\xbd\x3d\xde\xeb\xc1\x8e\x16\xe0\xe3\xdb\x63\xec\xc9\x7f\x32\x80\x03\x98\xdf\x28\x2e\xdf\xbf\x43\xfc\xb8\x80\x4b\x01\xb6\x0c\x9c\x6a\x40\x8d\x80\x10\x3f\xc7\x80\x1c\xbe\x77\x15\xd0\x03\x00\x30\x9f\xbe\x6b\x40\xe8\x0e\xbe\xa7\x00\x9b\xbf\x65\x7e\x0d\xdf\x1b\x08\x09\x21\x14\xe7\x9c\xcc\x9f\x93\xba\xfc\xfd\x2b\x50\x11\x42\x28\x9f\xf8\x14\x03\x63\x02\x56\xe7\x2f\xb1\x81\xe6\xdb\x5c\x7d\xec\xc5\x27\x81\x50\x58\x79\x4c\x09\xae\xbd\x21\x11\xb6\x81\xae\x88\xc8\x38\x2a\x97\x5b\xca\x0f\x19\x60\x4f\x82\x2c\xaf\x73\x63\xee\x95\x08\xce\x39\x32\xc7\x68\xc2\x0f\x1a\xc0\xde\x85\x02\x00\xda\x71\xf0\x7f\x66\xe0\xdb\xa5\x78\x02\x25\x22\xc4\xf4\x38\x5f\x0e\x02\xe3\xbd\x3b\x8c\xef\x37\xe0\x6b\x56\x06\x68\xbf\xce\x9c\x00\x7b\x17\x28\x32\x71\x27\xbe\x98\x96\x9b\xde\x86\x89\x4f\x06\x30\xc6\x94\xfd\x3f\x22\xc4\xe3\x4a\x08\x65\x5d\x21\x01\xf6\xcd\x87\xdf\xd2\x3f\x41\x97\xe7\xce\x27\x95\x2f\xee\x83\xb9\xe9\x8d\x1f\x43\x90\x4f\x72\xeb\x01\xad\x8e\x56\xfc\xac\x7a\x40\xce\x24\x6a\x04\xb4\xe4\x7b\xeb\x01\x6e\xde\xac\xbf\x52\x2f\xc5\x1a\x01\xad\xf9\x4f\x91\xa0\x1b\x2c\xf8\xde\x6f\x4d\xa2\x46\xc0\x19\xfc\xa7\x5c\xc0\x4e\x20\x7c\x13\x88\x15\x23\x4a\x05\x9c\xc5\x7f\x32\x40\x4a\x69\xca\x45\xba\x72\x62\x9f\x1c\x0b\x63\x4b\x05\x9c\xc5\x5f\x19\xa0\x2f\x93\x94\xe3\x08\x01\x67\xf0\x37\xaf\x00\x5f\x51\xb1\xa5\x80\x16\x7c\x6f\x41\xc4\x76\xcc\x57\x62\x4a\xbd\x84\x73\x05\x9c\xc1\xf7\x57\x84\x38\x27\x3a\x02\xf3\xd5\x02\x62\xd5\xd8\x12\x01\xee\x18\xbe\xf4\x57\x47\x89\x7b\x1a\xe8\x2f\x88\x58\xf9\x38\x00\x53\x5f\x4b\x4d\x85\x7d\x02\x62\xe2\x7d\xe1\x70\xf7\xf6\x38\xc7\x4d\x69\x43\xe7\x97\xf0\x83\x8d\x91\x25\x7d\x34\xf9\x78\x6a\x1b\xca\xcd\xc7\xb5\x80\xd8\xe4\x43\x42\x74\x1a\x9b\x73\xbe\x5d\xd0\xb1\xf9\xbe\x7c\x66\xab\x20\x42\x00\xa8\x69\x9a\x70\xbd\x5e\xc9\x34\x4d\x2a\xa3\x0f\xa7\xac\x1c\x1c\x42\x08\x50\x4a\x93\xfb\x78\x9a\xb5\x7c\xcf\x4e\xa7\x7d\x1f\x62\x5e\x3d\xe0\x6b\x56\xe2\xc7\x05\x72\x10\xa0\x1d\x87\x1c\x04\x86\x1b\x85\x5d\x1e\x4b\xc8\xc5\x8b\x0d\x9c\x3f\x27\xd5\xdd\xa5\x61\xe7\xa6\xd3\x39\x06\x86\x2b\x42\x4b\x4a\x3b\x8e\x23\x61\xef\x42\x8d\xff\xa6\xa7\xc3\xf3\xe7\xa4\x86\xff\xae\x35\x06\x42\x33\x19\x63\x6a\x2b\x9d\x4d\x35\xb0\xfb\x6b\xf2\xf2\xb7\x4a\x62\xd8\xab\x1e\x60\x2a\xbd\x19\x22\x8a\xb8\x81\x7a\x00\x80\x60\x4d\x80\x48\x29\x95\x4e\x17\x75\xeb\xb8\x65\x3e\xae\x03\x98\xb3\xf8\xa4\xef\x7b\xa5\x43\x46\xb7\x75\x9c\x92\x4e\xd6\x0a\x10\x42\x9c\xca\x27\x94\x52\x93\x3e\xba\x49\x84\xaf\xa7\xe6\x0e\x54\x2b\x40\x07\x5c\x67\xf1\x89\x52\x0a\x8c\x31\xa5\xfb\xe6\x29\xed\x65\x7b\xa0\x5a\x01\x7a\x8c\xb3\xf8\x84\x52\x1a\x85\x4b\x4f\x6b\xc9\x9d\x44\x8d\x80\x33\xf9\x51\x03\x34\x5c\xb7\x9c\xdd\x41\x6a\x05\xc4\xce\x3f\x9a\xbf\x69\x80\x7d\x49\x0d\xf7\xbe\x68\x02\x35\x02\x5a\xf0\xbd\x06\xd8\x6b\xa7\x6e\x31\x03\xc0\xaa\xd6\x7e\xb0\x00\x5d\xcf\x3f\x9a\xbf\x6d\x00\xa5\x10\xf4\x11\x86\xce\x3f\x7f\xe2\xf2\x21\x21\x1a\x09\x90\x52\x36\xe1\x9b\x55\xc0\x5e\x3a\x56\x5d\xd4\x8f\xe1\xd7\x92\x73\x1f\x92\x6f\x40\xa9\x02\x42\x63\xac\x22\x38\x8b\xbf\xb7\x81\x26\x14\xb6\x4d\xb0\xc3\x4f\xb7\x47\x9f\xba\x8c\x85\x04\xc4\xe2\x01\x7b\x49\x0b\xb5\xb8\xdd\xf3\x6b\xf8\xd1\x7a\x80\x1b\x37\x04\x93\x17\x67\x3d\x0e\x6d\x72\x28\xe8\xf3\x1f\xca\x8f\xed\x0f\x20\x58\xf7\xe9\xd3\x3b\xb3\x56\x3d\xa0\xe0\xfc\x66\xfc\xcd\x74\x58\xd7\x04\x00\x20\x27\x1d\xf6\x4c\x3e\xdf\x80\xaf\x59\xb1\xdb\x00\xda\x3d\x44\xf0\x2b\x0e\xe1\x6f\xa7\xc3\xa5\x7d\xfa\x67\xf3\x4a\xae\x80\x2a\xbe\x63\x5e\xbe\x01\xd5\x7d\xfa\x25\x27\x8f\xe5\xe3\xb1\xff\xe9\xe2\x7d\x0a\x4b\x4d\x22\x56\x8f\x78\xed\x0f\x78\xed\x0f\xc8\x7c\x5e\xa0\xc5\xfe\x80\x96\xfc\xa2\xe7\x05\x62\xfd\xf9\x52\x01\x67\xf0\x83\xcf\x0b\x84\x52\xc9\xd4\xd6\x56\xae\x80\xb3\xf8\x9b\xcf\x0b\xa4\x16\x24\xf6\x10\x70\x16\x7f\xf3\x0a\x08\x6d\x3b\x6f\x25\xa0\x05\x3f\xf8\xbc\x80\x9d\x46\xea\xd7\xbe\x6c\xec\x08\x01\x2d\xf9\x7e\x03\x00\x93\x46\x02\x30\xdb\xce\x45\xe2\x27\x58\x2b\xa0\x25\x3f\xb8\x3f\x00\x5f\xf3\x6a\x7d\xdc\x9a\xfc\x1e\x02\x7c\x22\x56\xc7\xb7\x4b\xf2\xfe\x80\x1c\xfe\x21\xfb\x03\x7c\x06\x6a\x01\x2d\xf6\x07\xe4\xf0\x7f\x97\xfd\x01\xc5\xcf\x2b\xc4\xf8\xaf\xfd\x01\xaf\xfd\x01\xaf\xfd\x01\x7f\xf6\xfe\x80\xff\x01\xf2\xe2\x2c\x94\xe2\x1b\xf9\x99\x00\x00\x00\x00\x49\x45\x4e\x44\xae\x42\x60\x82"},
};
