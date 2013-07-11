/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file tile_map.cpp Global tile accessors. */

#include "stdafx.h"
#include "tile_map.h"

/**
 * Return the slope of a given tile
 * @param tile Tile to compute slope of
 * @param h    If not \c NULL, pointer to storage of z height
 * @return Slope of the tile, except for the HALFTILE part
 */
template <bool Tgeneric>
Slope GetTileSlope(typename TileIndexT<Tgeneric>::T tile, int *h)
{
	assert(IsValidTileIndex(tile));

	uint x = TileX(tile);
	uint y = TileY(tile);

	if (x == MapMaxX(MapOf(tile)) || y == MapMaxY(MapOf(tile)) ||
			((x == 0 || y == 0) && IsMainMapTile(tile) && _settings_game.construction.freeform_edges)) {
		if (h != NULL) *h = TileHeight(tile);
		return SLOPE_FLAT;
	}

	int a = TileHeight(tile); // Height of the N corner
	int min = a; // Minimal height of all corners examined so far
	int b = TileHeight(tile + TileDiffXY(1, 0, MapOf(tile))); // Height of the W corner
	if (min > b) min = b;
	int c = TileHeight(tile + TileDiffXY(0, 1, MapOf(tile))); // Height of the E corner
	if (min > c) min = c;
	int d = TileHeight(tile + TileDiffXY(1, 1, MapOf(tile))); // Height of the S corner
	if (min > d) min = d;

	/* Due to the fact that tiles must connect with each other without leaving gaps, the
	 * biggest difference in height between any corner and 'min' is between 0, 1, or 2.
	 *
	 * Also, there is at most 1 corner with height difference of 2.
	 */

	uint r = SLOPE_FLAT; // Computed slope of the tile

	/* For each corner if not equal to minimum height:
	 *  - set the SLOPE_STEEP flag if the difference is 2
	 *  - add the corresponding SLOPE_X constant to the computed slope
	 */
	if ((a -= min) != 0) r += (--a << 4) + SLOPE_N;
	if ((c -= min) != 0) r += (--c << 4) + SLOPE_E;
	if ((d -= min) != 0) r += (--d << 4) + SLOPE_S;
	if ((b -= min) != 0) r += (--b << 4) + SLOPE_W;

	if (h != NULL) *h = min;

	return (Slope)r;
}
/* instantiate */
template Slope GetTileSlope<false>(TileIndex tile, int *h);
template Slope GetTileSlope<true>(GenericTileIndex tile, int *h);

/**
 * Get bottom height of the tile
 * @param tile Tile to compute height of
 * @return Minimum height of the tile
 */
template <bool Tgeneric>
int GetTileZ(typename TileIndexT<Tgeneric>::T tile)
{
	if (TileX(tile) == MapMaxX(MapOf(tile)) || TileY(tile) == MapMaxY(MapOf(tile))) return 0;

	int h = TileHeight(tile); // N corner
	h = min(h, TileHeight(tile + TileDiffXY(1, 0, MapOf(tile)))); // W corner
	h = min(h, TileHeight(tile + TileDiffXY(0, 1, MapOf(tile)))); // E corner
	h = min(h, TileHeight(tile + TileDiffXY(1, 1, MapOf(tile)))); // S corner

	return h;
}
/* instantiate */
template int GetTileZ<false>(TileIndex tile);
template int GetTileZ<true>(GenericTileIndex tile);

/**
 * Get top height of the tile
 * @param t Tile to compute height of
 * @return Maximum height of the tile
 */
template <bool Tgeneric>
int GetTileMaxZ(typename TileIndexT<Tgeneric>::T t)
{
	if (TileX(t) == MapMaxX(MapOf(t)) || TileY(t) == MapMaxY(MapOf(t))) return 0;

	int h = TileHeight(t); // N corner
	h = max<int>(h, TileHeight(t + TileDiffXY(1, 0, MapOf(t)))); // W corner
	h = max<int>(h, TileHeight(t + TileDiffXY(0, 1, MapOf(t)))); // E corner
	h = max<int>(h, TileHeight(t + TileDiffXY(1, 1, MapOf(t)))); // S corner

	return h;
}
/* instantiate */
template int GetTileMaxZ<false>(TileIndex t);
template int GetTileMaxZ<true>(GenericTileIndex t);
