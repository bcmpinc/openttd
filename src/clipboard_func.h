/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file clipboard_func.h Functions related to the clipboad. */

#ifndef CLIPBOARD_FUNC_H
#define CLIPBOARD_FUNC_H

#include "clipboard_type.h"

void FreeClipboardStationList(ClipboardStationList *list);

/** Helper class to build a station list while copying to the clipboard. */
class ClipboardStationsBuilder {
protected:
	ClipboardStationList stations; ///< the list of stations

	ClipboardStation **FindStation(StationID sid);
	ClipboardStation *AddStation(StationID sid);
	void AddSpecToStation(ClipboardStation *st, StationClassID station_class, byte station_type, byte specindex);

public:
	ClipboardStationsBuilder() : stations(NULL)
	{ }

	~ClipboardStationsBuilder()
	{
		FreeClipboardStationList(&this->stations);
	}

	/**
	 * Add a "simple" station part (bus/truck/dock/buoy).
	 * @param sid id of the station
	 */
	inline void AddPart(StationID sid)
	{
		this->AddStation(sid);
	}

	/**
	 * Add a rail station/waypoint part.
	 * @param sid id of the station
	 * @param station_class custom station class
	 * @param station_type type within the custom station class
	 * @param specindex index of the given station spec in the list of specs of this station (aka custom station spec index)
	 */
	inline void AddRailPart(StationID sid, StationClassID station_class, byte station_type, byte specindex)
	{
		this->AddSpecToStation(this->AddStation(sid), station_class, station_type, specindex);
	}

	void AddAirportPart(StationID sid, RawTileIndex tile, AirportTypes type, byte layout);

	void BuildDone(Map *clipboard);
};

static const uint NUM_CLIPBOARD_BUFFERS = 5; ///< Total amount of clipboard buffers

bool IsClipboardBuffer(const Map *map);
Map *GetClipboardBuffer(uint index);
uint GetClipboardBufferIndex(const Map *clipboard);
void AllocateClipboardBuffer(Map *clipboard, uint size_x, uint size_y);
bool IsClipboardBufferEmpty(const Map *clipboard);
void EmptyClipboardBuffer(Map *clipboard);
void ClearClipboard();

#endif /* CLIPBOARD_FUNC_H */
