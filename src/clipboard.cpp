/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file clipboard.cpp Implementaion of clipboard related to both copying and pasting. */

#include "stdafx.h"
#include "core/alloc_func.hpp"
#include "core/geometry_type.hpp"
#include "core/mem_func.hpp"
#include "clipboard_func.h"
#include "map_type.h"
#include "newgrf_airport.h"
#include "station_map.h"
#include "tilearea_type.h"
#include "void_map.h"

Map _clipboard;
ClipboardStationList _clipboard_stations = NULL;

/** Clear all clipbord object (everything from ouside the clipboard tile array) */
void FreeClipboardStationList(ClipboardStationList *list)
{
	for (ClipboardStation *item = *list, *next; item != NULL; item = next) {
		next = item->next;
		free(item);
	}
	*list = NULL;
}

bool IsClipboardEmpty()
{
	return _clipboard.m == NULL;
};

/** Unload clipboard contents. */
void MakeClipboardEmpty()
{
	if (IsClipboardEmpty()) return;

	FreeClipboardStationList(&_clipboard_stations);

	_clipboard.size_x = 0;
	_clipboard.size_y = 0;
	_clipboard.size = 0;

	free(_clipboard.m);
	_clipboard.m = NULL;
	free(_clipboard.me);
	_clipboard.me = NULL;
}

void AllocateClipboard(uint content_size_x, uint content_size_y)
{
	assert(IsInsideMM(content_size_x, 1, INT_MAX - 1));
	assert(IsInsideMM(content_size_y, 1, INT_MAX - 1));

	FreeClipboardStationList(&_clipboard_stations);

	_clipboard.size_x = content_size_x + 1;
	_clipboard.size_y = content_size_y + 1;
	_clipboard.size = _clipboard.size_x * _clipboard.size_y;

	_clipboard.m = ReallocT<Tile>(_clipboard.m, _clipboard.size);
	_clipboard.me = ReallocT<TileExtended>(_clipboard.me, _clipboard.size);

	/* write zeros form the beggining to the last non-void tile (a'la MakeClear) */
	MemSetT(_clipboard.m, 0, _clipboard.size);
	MemSetT(_clipboard.me, 0, _clipboard.size);

	GENERIC_TILE_AREA_LOOP(iter, GenericTileArea(TileXY(_clipboard.size_x - 1, 0, &_clipboard), 1, _clipboard.size_y)) {
		MakeVoid(iter);
	}
	GENERIC_TILE_AREA_LOOP(iter, GenericTileArea(TileXY(0, _clipboard.size_y - 1, &_clipboard), _clipboard.size_x - 1, 1)) {
		MakeVoid(iter);
	}
}

/* static */ ClipboardStation *ClipboardStation::Get(StationID id)
{
	for (ClipboardStation *ret = _clipboard_stations; ret != NULL; ret = ret->next) {
		if (ret->id == id) return ret;
	}
	return NULL;
}

/* static */ ClipboardStation *ClipboardStation::GetByTile(GenericTileIndex tile)
{
	assert(MapOf(tile) == &_clipboard);
	return ClipboardStation::Get(GetStationIndex(tile));
}

/* static */ const ClipboardStation::Spec *ClipboardStation::GetSpecByTile(GenericTileIndex tile)
{
	ClipboardStation *st = ClipboardStation::GetByTile(tile);
	if (st == NULL) return NULL;
	byte specindex = GetCustomStationSpecIndex(tile);
	return (specindex < st->num_specs) ? &st->speclist[specindex] : NULL;
}

ClipboardStation::ClipboardStation()
{
	MemSetT(this, 0);
	this->id = INVALID_STATION;
	this->xy = INVALID_TILE_INDEX;
	for (TransportType tt = TRANSPORT_BEGIN; tt < TRANSPORT_END; tt++) this->area[tt].tile = INVALID_TILE_INDEX;
	this->airport.type = AT_INVALID;
	this->dock.dir = INVALID_DIAGDIR;
}

ClipboardStation::~ClipboardStation()
{
	free(this->speclist);
}

GenericTileArea ClipboardStation::GetTransportArea(TransportType tt) const
{
	return GenericTileArea(this->area[tt], &_clipboard);
}

GenericTileIndex ClipboardStation::GetDockTile() const
{
	GenericTileIndex ret(this->area[TRANSPORT_WATER].tile, &_clipboard);
	if (IsValidTileIndex(ret) && (this->dock.dir == DIAGDIR_NE || this->dock.dir == DIAGDIR_NW)) {
		ret = TileAddByDiagDir(ret, ReverseDiagDir(this->dock.dir));
	}
	return ret;
}

ClipboardStation **ClipboardStationsBuilder::FindStation(StationID sid)
{
	ClipboardStation **ret = &this->stations;
	while (*ret != NULL) {
		if ((*ret)->id == sid) break;
		ret = &((*ret)->next);
	}
	return ret;
}

ClipboardStation *ClipboardStationsBuilder::AddStation(StationID sid, GenericTileIndex xy)
{
	assert(MapOf(xy) == &_clipboard);

	ClipboardStation **st_link = this->FindStation(sid);
	if (*st_link != NULL) return *st_link;

	ClipboardStation *st = *st_link = CallocT<ClipboardStation>(1);
	st->id = sid;
	st->xy = IndexOf(xy);
	st->area[TRANSPORT_RAIL].tile = INVALID_TILE_INDEX;
	st->area[TRANSPORT_ROAD].tile = INVALID_TILE_INDEX;
	st->area[TRANSPORT_AIR].tile = INVALID_TILE_INDEX;
	st->area[TRANSPORT_WATER].tile = INVALID_TILE_INDEX;
	st->airport.type = AT_INVALID;
	st->dock.dir = INVALID_DIAGDIR;
	return st;
}

byte ClipboardStationsBuilder::AddSpecToStation(ClipboardStation *st, StationClassID spec_class, byte spec_index)
{
	uint ret;
	for (ret = 0; ret < st->num_specs; ret++) {
		if (st->speclist[ret].spec_class == spec_class && st->speclist[ret].spec_index == spec_index) return ret;
	}

	st->speclist = ReallocT(st->speclist, ret + 1);
	st->speclist[ret].spec_class = spec_class;
	st->speclist[ret].spec_index = spec_index;
	st->num_specs = ret + 1;
	assert(st->num_specs != 0); // preven from overflow

	return ret;
}

ClipboardStation *ClipboardStationsBuilder::BasicAddTile(GenericTileIndex tile, StationID sid, TransportType tt, StationFacility facility)
{
	ClipboardStation *st = this->AddStation(sid, tile);

	assert(HasExactlyOneBit(facility));
	/* dont't mix waypoints with stations */
	assert((facility != FACIL_WAYPOINT) ? ((st->facilities & FACIL_WAYPOINT) == 0) :
			((st->facilities & ~FACIL_WAYPOINT) == 0) &&
			/* don't mix different waypoints */
			(tt == TRANSPORT_RAIL  || !st->HasTransport(TRANSPORT_RAIL)) &&
			(tt == TRANSPORT_ROAD  || !st->HasTransport(TRANSPORT_ROAD)) &&
			(tt == TRANSPORT_WATER || !st->HasTransport(TRANSPORT_WATER)) &&
			(tt == TRANSPORT_AIR   || !st->HasTransport(TRANSPORT_AIR)));

	st->facilities |= facility;

	GenericTileArea temp_area(st->area[tt], &_clipboard);
	temp_area.Add(tile);
	st->area[tt].tile = IndexOf(temp_area.tile);
	st->area[tt].w    = temp_area.w;
	st->area[tt].h    = temp_area.h;

	return st;
}

byte ClipboardStationsBuilder::AddRailTile(GenericTileIndex tile, StationID sid, StationClassID spec_class, byte spec_index)
{
	ClipboardStation *st = this->BasicAddTile(tile, sid, TRANSPORT_RAIL, FACIL_TRAIN);
	return this->AddSpecToStation(st, spec_class, spec_index);
}

byte ClipboardStationsBuilder::AddWaypointTile(GenericTileIndex tile, StationID sid, StationClassID spec_class, byte spec_index)
{
	ClipboardStation *st = this->BasicAddTile(tile, sid, TRANSPORT_RAIL, FACIL_WAYPOINT);
	return this->AddSpecToStation(st, spec_class, spec_index);
}

void ClipboardStationsBuilder::AddAirportTile(GenericTileIndex tile, StationID sid, AirportTypes type, byte layout)
{
	ClipboardStation *st = this->BasicAddTile(tile, sid, TRANSPORT_AIR, FACIL_AIRPORT);

	if (st->airport.type == AT_INVALID) {
		st->airport.type = type;
		st->airport.layout = layout;
	} else {
		assert(st->airport.type == type);
		assert(st->airport.layout == layout);
	}
}

void ClipboardStationsBuilder::AddDockTile(GenericTileIndex tile, StationID sid, DiagDirection dir)
{
	ClipboardStation *st = this->BasicAddTile(tile, sid, TRANSPORT_WATER, FACIL_DOCK);

	if (st->dock.dir == INVALID_DIAGDIR) {
		st->dock.dir = dir;
	} else {
		assert(st->dock.dir == dir);
	}

	assert(st->area[TRANSPORT_WATER].w * st->area[TRANSPORT_WATER].h <= 2); // signle dock allowed only
}

ClipboardStationList ClipboardStationsBuilder::BuildDone()
{
	ClipboardStationList ret = this->stations;
	this->stations = NULL;
	return ret;
}
