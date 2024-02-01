mac-build:
	arch -x86_64 gcc -std=c99 -c src/Disk/bplustree.c -o src/Disk/bplustree.o
	arch -x86_64 gcc -std=c99 -shared -o src/Disk/libbplustree.so src/Disk/bplustree.o
	dotnet build
build:
	gcc -std=c99 -fPIC -c src/Disk/bplustree.c -o src/Disk/bplustree.o
	gcc -std=c99 -fPIC -shared -o src/Disk/libbplustree.so src/Disk/bplustree.o
	dotnet build
disk:
	dotnet run --project src/Disk
executor:
	dotnet run --project src/Executor
server:
	dotnet run --project src/Server
language:
	dotnet run --project src/Language

