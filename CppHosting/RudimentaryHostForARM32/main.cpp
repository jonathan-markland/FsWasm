#include <iostream>
#include <fstream>
#include <vector>
#include <stdint.h>
#include <sys/mman.h>
#include <cstring>


#define BASE_ADDRESS 0x40000000
#define REGION_SIZE  0x100000

std::string GuestProgramPath = "program-7-Arm32.bin";

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
    if (sz >= 4 && index < (sz - 3))
    {
        auto p = reinterpret_cast<const uint32_t*>(&vec[index]);
        return *p;
    }
    throw std::runtime_error("Invalid index.");
}

int main()
{
    auto imageBlockHandle = mmap((void *)BASE_ADDRESS, REGION_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (imageBlockHandle == (void*)MAP_FAILED)
    {
        std::cout << "Cannot allocate the session region at the fixed address (case 1)." << std::endl;
        return 1;
    }
    if (imageBlockHandle != (void*) BASE_ADDRESS)
    {
        std::cout << "Cannot allocate the session region at the fixed address (case 2)." << std::endl;
        return 1;
    }
    if (mprotect(imageBlockHandle, REGION_SIZE, PROT_READ | PROT_WRITE | PROT_EXEC) != 0)
    {
        std::cout << "Failed to set the EXEC access rights on the session area." << std::endl;
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

    if (guestExecutableImage[4] != 'A' ||
        guestExecutableImage[5] != 'R' ||
        guestExecutableImage[6] != 'v' ||
        guestExecutableImage[7] != '7')
    {
        std::cout << "Executable file is not for 32-bit ARMv7." << std::endl;
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

    // Completely hack the shadow stack pointer, AND rely on non-validation of the WASM Linear Memory limit(!):
    // auto p = (uint32_t *) BASE

    // uint32_t stackPointerInitialAddress = REGION_SIZE - 0x13ec;
    // auto wasmLinearMemory = (uint32_t*)0x400013ec; // TODO! HACK for program-7 only!
    // wasmLinearMemory[1] = stackPointerInitialAddress; // HACK for program-7 only!

    f();

    // SaveMemoryToFile(wasmLinearMemory, 320 * 256, "ARM32WasmCircleDrawingOptimised.bin");

    return 0;
}
