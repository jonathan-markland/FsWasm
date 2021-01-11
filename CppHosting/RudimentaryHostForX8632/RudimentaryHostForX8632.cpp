#include <iostream>
#include <Windows.h>
#include <fstream>
#include <vector>
#include <stdint.h>

#define BASE_ADDRESS 0x40000000
#define REGION_SIZE  0x100000
#define FILE_VERSION 1
#define IMAGE_FLAGS_MASK 1
#define IMAGE_FLAG_USES_STACK_POINTER_AT_ADDRESS_4 1
#define LINEAR_MEMORY_LOW_MASK 0xFFFF

std::string GuestProgramPath = "C:\\Users\\Jonathan\\Documents\\Work\\FsWasm\\3rdParty\\program-7-x8632.bin";

void SaveMemoryToFile(const void* baseAddress, uint32_t size, const char* filename)
{
    std::ofstream file(filename, std::ios::binary);
    file.write((char*)baseAddress, size);
    file.close();
}

std::vector<uint8_t> LoadFileIntoVector(const char* filename)
{
    // open the file:
    std::streampos fileSize;
    std::ifstream file(filename, std::ios::binary);

    // get its size:
    file.seekg(0, std::ios::end);
    fileSize = file.tellg();
    file.seekg(0, std::ios::beg);

    // read the data:
    std::vector<uint8_t> fileData(fileSize);
    file.read((char*)&fileData[0], fileSize);
    return fileData;
}

uint32_t mem32(const std::vector<uint8_t>& vec, size_t index) 
{
    auto sz = vec.size();
    if (sz >= 4 && index < (sz - 3))  // TODO: constants
    {
        auto p = reinterpret_cast<const uint32_t *>(&vec[index]);
        return *p;
    }
    throw std::runtime_error("Invalid index.");
}

uint64_t mem64(const std::vector<uint8_t>& vec, size_t index)
{
    auto sz = vec.size();
    if (sz >= 8 && index < (sz - 7))  // TODO: constants
    {
        auto p = reinterpret_cast<const uint64_t*>(&vec[index]);
        return *p;
    }
    throw std::runtime_error("Invalid index.");
}

int main()
{
    auto imageBlockHandle =
        ::VirtualAlloc(
            (LPVOID)BASE_ADDRESS,
            REGION_SIZE,
            MEM_COMMIT | MEM_RESERVE,
            PAGE_EXECUTE_READWRITE);

    if (imageBlockHandle == nullptr)
    {
        std::cout << "Cannot allocate the session region at the fixed address." << std::endl;
        return 1;
    }

    auto guestExecutableImage = LoadFileIntoVector(GuestProgramPath.c_str());

    if (guestExecutableImage.size() < 32)
    {
        std::cout << "Executable file is too small." << std::endl;
        return 1;
    }

    if (guestExecutableImage[0] != 'F' ||
        guestExecutableImage[1] != '#' ||
        guestExecutableImage[2] != 'F' ||
        guestExecutableImage[3] != 'X')
    {
        std::cout << "Executable file is missing the 4-byte signature." << std::endl;
        return 1;
    }

    if( guestExecutableImage[4] != 'I' ||
        guestExecutableImage[5] != 'A' ||
        guestExecutableImage[6] != '3' ||
        guestExecutableImage[7] != '2')
    {
        std::cout << "Executable file is not for Intel X86/32." << std::endl;
        return 1;
    }

    if (mem64(guestExecutableImage, 0x08) != FILE_VERSION)
    {
        std::cout << "File version stamp doesn't match what this host supports." << std::endl;
        return 1;
    }

    if (mem64(guestExecutableImage, 0x10) != BASE_ADDRESS)
    {
        std::cout << "Base address isn't 1GB.  We don't support anything else at present." << std::endl;
        return 1;
    }

    auto endAddress = mem64(guestExecutableImage, 0x18);
    if (endAddress < BASE_ADDRESS)
    {
        std::cout << "End address before the base address." << std::endl;
        return 1;
    }

    auto lengthOfImage = endAddress - BASE_ADDRESS;
    if (lengthOfImage > REGION_SIZE)
    {
        std::cout << "Image requires too much space to run." << std::endl;
        return 1;
    }

    auto entryAddress = mem32(guestExecutableImage, 0x20);
    if (entryAddress < BASE_ADDRESS)
    {
        std::cout << "Entry address is before the base address." << std::endl;
        return 1;
    }

    auto entryOffset = entryAddress - BASE_ADDRESS;
    if (entryOffset >= guestExecutableImage.size())
    {
        std::cout << "Entry address is beyond the end of the file image." << std::endl;
        return 1;
    }

    auto linearMemoryAddress = mem64(guestExecutableImage, 0x28);
    if (linearMemoryAddress < BASE_ADDRESS)
    {
        std::cout << "Linear memory address is before the base address." << std::endl;
        return 1;
    }

    auto linearMemorySize = endAddress - linearMemoryAddress;
    if ((linearMemorySize & LINEAR_MEMORY_LOW_MASK) != 0)
    {
        std::cout << "Linear memory length is not multiple of 64KB." << std::endl;
        return 1;
    }

    auto imageFlags = mem32(guestExecutableImage, 0x30);
    if ((imageFlags | IMAGE_FLAGS_MASK) ^ IMAGE_FLAGS_MASK)
    {
        std::cout << "Image flags have invalid bits set." << std::endl;
        return 1;
    }

    auto useSp4 = (imageFlags & IMAGE_FLAG_USES_STACK_POINTER_AT_ADDRESS_4) != 0;

    memcpy(
        (void*)BASE_ADDRESS,
        (const void*)(&guestExecutableImage[0]),
        guestExecutableImage.size());

    if (useSp4)
    {
        std::cout << "Setting the stack pointer at address 4" << std::endl;
        auto wasmLinearMemory = (uint32_t*)linearMemoryAddress;
        auto initialSP = linearMemorySize;
        wasmLinearMemory[1] = initialSP;
    }

    void(*f)() = (void(*)()) (uint32_t) entryAddress;
    f();

    SaveMemoryToFile((const void *) linearMemoryAddress, (uint32_t) linearMemorySize, "WASMLinearMemoryDumpPostRun.bin");

    return 0;
}
