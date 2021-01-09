#include <fstream>

// - - - SHAPES using RasterLR<> array - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

namespace lib80GFX
{
	namespace System
	{
		namespace ToRasters
		{

			template<typename SCALAR, typename RASTER_COLLECTOR>
			void BresenhamFilledEllipse(
				SCALAR x0, SCALAR y0, SCALAR x1, SCALAR y1,     // The extents area for the ellipse.
				RASTER_COLLECTOR& rr)                  // The receiver for the output rasters.  This also knows the viewport & does clipping.
			{
				// Draw an ellipse into a RasterLR array.

				// Inside-out check:
				if (x0 >= x1 || y0 >= y1) return;

				// Check for totally outside viewport:
				if (x0 >= rr.vx1 || x1 <= rr.vx0 || y0 >= rr.vy1 || y1 <= rr.vy0) return;

				// Determine centre:
				auto centerX = (x0 + x1) >> 1;
				auto centerY = (y0 + y1) >> 1;

				// Determine radii:
				auto radiusX = x0 < x1 ? x1 - centerX : x0 - centerX;
				auto radiusY = y0 < y1 ? y1 - centerY : y0 - centerY;

				// If it's a TALL ellipse, swap radii so it becomes WIDE,
				// and flag for coordinate swapping at draw-time:
				bool isWideEllipse = true;
				if (radiusX < radiusY)
				{
					auto temp = radiusX;
					radiusX = radiusY;
					radiusY = temp;
					isWideEllipse = false;
				}

				// This plots a 'wide' ellipse.
				SCALAR d = 3 - (2 * radiusX);
				SCALAR X = 0;         // Circle X and y0 X
				SCALAR Y = radiusX;   // Circle Y and Side X
				SCALAR topY = radiusY;   // y0 Y
				SCALAR sideY = 0;         // Side Y
				SCALAR topDec = radiusX >> 1; // Limit=RadiusX, IncBy=RadiusY
				SCALAR sideDec = radiusX >> 1; // Limit=RadiusX, IncBy=RadiusY
				SCALAR delta1 = 6;
				SCALAR delta2 = -radiusX * 4 + 10;

				while (X <= Y)
				{
					// Call the callback to draw horizontal lines, reflect all four quadrants:
					// Here, we send UNCLIPPED line coordinates:
					if (isWideEllipse)
					{
						// Wide
						rr(centerX - X, centerX + X, centerY + topY);
						rr(centerX - Y, centerX + Y, centerY + sideY);
						rr(centerX - X, centerX + X, centerY - topY);
						rr(centerX - Y, centerX + Y, centerY - sideY);
					}
					else
					{
						// Tall
						rr(centerX - topY, centerX + topY, centerY + X);
						rr(centerX - sideY, centerX + sideY, centerY + Y);
						rr(centerX - topY, centerX + topY, centerY - X);
						rr(centerX - sideY, centerX + sideY, centerY - Y);
					}

					// Conditionally increment SideY:
					sideDec += radiusY;
					if (sideDec > radiusX)
					{
						sideY++;
						sideDec -= radiusX;
					}

					// Bresenham's circle algorithm decisions:
					if (d >= 0)
					{
						d += delta2;
						delta1 += 4;
						delta2 += 8;
						X++;
						Y--;

						// Conditionally decrement TopY:
						topDec += radiusY;
						if (topDec > radiusX)
						{
							topY--;
							topDec -= radiusX;
						}

						continue;
					}

					d += delta1;
					delta1 += 4;
					delta2 += 4;
					X++;
				}
			}

		} /// end namespace

	} /// end namespace

} /// end namespace



// - - - Interfacing - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

struct Surface
{
	char* TopLeft;
	int    Strafe;
	int    Width;
	int    Height;
};

struct PrivateRasterCollector
{
	PrivateRasterCollector(Surface* s, char colour) : colour(colour), target(s), vx1(s->Width), vy1(s->Height) {}
	char const colour;
	enum { vx0 = 0, vy0 = 0 };
	Surface* const target;
	const int vx1;
	const int vy1;
	void operator()(int left, int right, int y)
	{
		if (y >= vy0 && y < vy1 && left < right)
		{
			if (left < vx1 && right > vx0)
			{
				if (left < vx0) left = vx0;
				if (right > vx1) right = vx1;
				auto p = target->TopLeft + (target->Strafe * y) + left;
				auto q = p + (right - left);
				while (p < q) *p++ = colour;
			}
		}
	}
};

void Circle(Surface* surface, int cx, int cy, int r, char colour)
{
	PrivateRasterCollector rasterCollector(surface, colour);
	lib80GFX::System::ToRasters::BresenhamFilledEllipse(
		cx - r, cy - r, cx + r, cy + r, rasterCollector);
}


#define WIDTH 320
#define HEIGHT 256

char DemoScreen[WIDTH * HEIGHT];

void Demo()
{
	for (auto p = DemoScreen; p < (DemoScreen + WIDTH * HEIGHT); p++)
	{
		*p = (char)0x00;
	}

	Surface s;
	s.TopLeft = DemoScreen;
	s.Strafe = WIDTH;
	s.Width = WIDTH;
	s.Height = HEIGHT;

	Circle(&s, 160, 100, 80, (char)0xFF);
	Circle(&s, 160, 100, 40, (char)0xC0);
}

void SaveMemoryToFile(const void *baseAddress, uint32_t size, const char* filename)
{
	std::ofstream file(filename, std::ios::binary);
	file.write((char*)baseAddress, size);
	file.close();
}

int main()
{
	Demo();
	SaveMemoryToFile(DemoScreen, WIDTH * HEIGHT, "DemoScreenBinary.bin");
	return 0;
}










