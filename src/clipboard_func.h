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

extern Map _clipboard;
extern ClipboardStationList _clipboard_stations;

void FreeClipboardStationList(ClipboardStationList *list);

class ClipboardStationsBuilder {
protected:
	ClipboardStationList stations;

	ClipboardStation **FindStation(StationID sid);
	ClipboardStation *AddStation(StationID sid, GenericTileIndex xy);
	void AddTileToTransport(ClipboardStation *st, GenericTileIndex tile, TransportType tt);
	byte AddSpecToStation(ClipboardStation *st, StationClassID spec_class, byte spec_index);

	ClipboardStation *BasicAddTile(GenericTileIndex tile, StationID sid, TransportType tt, StationFacility facility);

public:
	ClipboardStationsBuilder() : stations(NULL) {}
	~ClipboardStationsBuilder() { FreeClipboardStationList(&this->stations); }

	inline void AddRoadStopTile(GenericTileIndex tile, StationID sid, RoadStopType rst)
	{
		this->BasicAddTile(tile, sid, TRANSPORT_ROAD, rst == ROADSTOP_BUS ? FACIL_BUS_STOP : FACIL_TRUCK_STOP);
	}

	inline void AddBuoyTile(GenericTileIndex tile, StationID sid)
	{
		this->BasicAddTile(tile, sid, TRANSPORT_WATER, FACIL_WAYPOINT);
	}

	byte AddRailTile(GenericTileIndex tile, StationID sid, StationClassID spec_class, byte spec_index);
	byte AddWaypointTile(GenericTileIndex tile, StationID sid, StationClassID spec_class, byte spec_index);
	void AddAirportTile(GenericTileIndex tile, StationID sid, AirportTypes type, byte layout);
	void AddDockTile(GenericTileIndex tile, StationID sid, DiagDirection dir);

	ClipboardStationList BuildDone();
};

void AllocateClipboard(uint size_x, uint size_y);
bool IsClipboardEmpty();
void MakeClipboardEmpty();

#endif /* CLIPBOARD_FUNC_H */
