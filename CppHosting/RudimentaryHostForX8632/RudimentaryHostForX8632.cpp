#include <iostream>
#include <Windows.h>
#include <fstream>
#include <vector>
#include <stdint.h>

#define BASE_ADDRESS 0x40000000
#define REGION_SIZE  0x100000

std::string GuestProgramPath = "C:\\Users\\Jonathan\\Documents\\Work\\FsWasm\\3rdParty\\program-5.bin";

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
    std::vector<BYTE> fileData(fileSize);
    file.read((char*)&fileData[0], fileSize);
    return fileData;
}

uint32_t mem32(const std::vector<uint8_t>& vec, size_t index) 
{
    auto sz = vec.size();
    if (sz >= 4 && index < (sz - 3))
    {
        auto p = reinterpret_cast<const uint32_t *>(&vec[index]);
        return *p;
    }
    throw std::exception("Invalid index.");
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

    if (mem32(guestExecutableImage, 0x0C) != 0)
    {
        std::cout << "Base address is too large for 32-bit machine." << std::endl;
        return 1;
    }

    if (mem32(guestExecutableImage, 0x08) != BASE_ADDRESS)
    {
        std::cout << "Base address isn't 1GB.  We don't support anything else at present." << std::endl;
        return 1;
    }

    if (mem32(guestExecutableImage, 0x14) != 0)
    {
        std::cout << "End address is too large for 32-bit machine." << std::endl;
        return 1;
    }

    auto endAddress = mem32(guestExecutableImage, 0x10);
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

    if (mem32(guestExecutableImage, 0x1C) != 0)
    {
        std::cout << "Entry address is too large for 32-bit machine." << std::endl;
        return 1;
    }

    auto entryAddress = mem32(guestExecutableImage, 0x18);
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

    memcpy(
        (void*)BASE_ADDRESS,
        (const void*)(&guestExecutableImage[0]),
        guestExecutableImage.size());

    void(*f)() = (void(*)()) entryAddress;

    f();

    return 0;
}