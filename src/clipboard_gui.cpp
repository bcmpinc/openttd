/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file clipboard_gui.cpp GUIs related to the clipboard. */

#include "stdafx.h"
#include "core/geometry_func.hpp"
#include "network/network.h"
#include "widgets/clipboard_widget.h"
#include "clipboard_func.h"
#include "clipboard_gui.h"
#include "command_func.h"
#include "company_func.h"
#include "company_base.h"
#include "copypaste_cmd.h"
#include "direction_func.h"
#include "error.h"
#include "gfx_func.h"
#include "gui.h"
#include "hotkeys.h"
#include "rail.h"
#include "rail_gui.h"
#include "rail_map.h"
#include "road_map.h"
#include "slope_func.h"
#include "sound_func.h"
#include "station_map.h"
#include "strings_func.h"
#include "terraform_gui.h"
#include "tilearea_type.h"
#include "tilehighlight_func.h"
#include "track_func.h"
#include "tunnelbridge_map.h"
#include "viewport_func.h"
#include "window_gui.h"
#include "window_func.h"

#include "table/sprites.h"
#include "table/strings.h"

static const int CLIPBOARD_DRAG_LIMIT = 255; ///< hard limit on width/height of copy area
static const int CLIPBOARD_ADDITIONAL_HEIGHT_MAX = 7;
static const int CLIPBOARD_ADDITIONAL_HEIGHT_MIN = -7;

static TileArea          _clipboard_copy_area       = TileArea(INVALID_TILE, 0, 0); ///< Area on the main map selected as a source of copy operation
static TileArea          _clipboard_paste_area      = TileArea(INVALID_TILE, 0, 0); ///< Area on the main map selected as a destination for paste operation
static CopyPasteMode     _clipboard_copy_paste_mode = CPM_DEFAULT;                  ///< Copy/paste mode selected in the clipboard toolbar
static RailType          _clipboard_railtype        = INVALID_RAILTYPE;             ///< #Railtype to convert to
static DirTransformation _clipboard_transformation  = DTR_IDENTITY;                 ///< Rotation/reflection to apply when pasting, selected in the clipboard toolbar
static int               _clipboard_additional_height_delta = 0;                    ///< Additional amount of tile heights to add

/**
 * Whether the copy/paste operations are performed with the clipboard buffer, or instantantly.
 *
 * If false, clipboard buffer is on. Each "copy" user action moves selected area to the clipboard
 * (to the buffer) and each "paste" tries to reproduce contents of the clipboard on the main map.
 *
 * If true, clipboard buffer is off. "copy" user action just selects area and
 * "paste" makes an instant copy&paste from the selected area to pointed place.
 *
 * @return whether the clipboard buffer is available for local company
 */
static inline bool IsClipboardBufferOn() { return !_networking; }

static inline bool IsClipboardCopyAreaSelected()
{
	return _clipboard_copy_area.tile != INVALID_TILE;
}

static inline bool IsClipboardPasteSourceSet()
{
	return IsClipboardBufferOn() ? !IsClipboardEmpty() : IsClipboardCopyAreaSelected();
}

static void ClipboardRecalcPasteAreaSize()
{
	assert(IsClipboardPasteSourceSet());

	Dimension size;
	if (IsClipboardBufferOn()) {
		size.width = _clipboard.size_x - 1;
		size.height = _clipboard.size_y - 1;
	} else {
		size.width = _clipboard_copy_area.w;
		size.height = _clipboard_copy_area.h;
	}
	size = TransformDimension(size, _clipboard_transformation);

	_clipboard_paste_area.w = size.width;
	_clipboard_paste_area.h = size.height;
}

void CcPaste(const CommandCost &result, TileIndex tile, uint32 p1, uint32 p2)
{
	if (_paste_err_tile != INVALID_TILE) SetRedErrorSquare(_paste_err_tile);

	if (result.Succeeded()) {
		SndPlayTileFx(SND_1F_SPLAT, tile);
		if (!_settings_client.gui.persistent_buildingtools) ResetObjectToPlace();
	}
}

void GetTilePastePreview(TileIndex tile, TilePastePreview *ret)
{
	_clipboard_paste_area.tile = TileVirtXY(_thd.pos.x, _thd.pos.y);

	extern bool TestRailTileCopyability(GenericTileIndex tile, CopyPasteMode mode, CompanyID company, TileContentPastePreview *preview);
	extern bool TestRoadTileCopyability(GenericTileIndex tile, CopyPasteMode mode, CompanyID company, TileContentPastePreview *preview);
	extern bool TestWaterTileCopyability(GenericTileIndex tile, const GenericTileArea &src_area, CopyPasteMode mode, GenericTileArea *object_rect, CompanyID company, TileContentPastePreview *preview);
	extern bool TestTunnelBridgeTileCopyability(GenericTileIndex tile, const GenericTileArea &src_area, CopyPasteMode mode, GenericTileIndex *other_end, CompanyID company, TileContentPastePreview *preview);
	extern bool TestStationTileCopyability(GenericTileIndex tile, const GenericTileArea &src_area, CopyPasteMode mode, GenericTileArea *station_part_area, CompanyID company, TileContentPastePreview *preview);

	/* the area we are copying from */
	GenericTileArea src_area = IsClipboardBufferOn() ?
			GenericTileArea(TileXY(0, 0, &_clipboard), _clipboard.size_x - 1, _clipboard.size_y - 1) :
			GenericTileArea(_clipboard_copy_area);

	DirTransformation inv_dtr = InvertDirTransform(_clipboard_transformation);
	/* area containing all tile corners (also those at SW and SE borders) */
	TileArea paste_area_corners(_clipboard_paste_area.tile, _clipboard_paste_area.w + 1, _clipboard_paste_area.h + 1);
	/* source corner of the most norther corner */
	GenericTileIndex src_of_north_corner = paste_area_corners.TransformedNorth(src_area.tile, inv_dtr);
	/* source corner of the tile corner (source of it's height) */
	GenericTileIndex src_of_tile_corner = paste_area_corners.TransformTile(tile, src_of_north_corner, inv_dtr);
	/* calculate the height difference between areas */
	int height_delta = TileHeight(paste_area_corners.tile) - TileHeight(src_of_north_corner) + _clipboard_additional_height_delta;

	if (_clipboard_paste_area.Contains(tile)) {
		/* source tile of the tile */
		GenericTileIndex src_tile = _clipboard_paste_area.TransformTile(tile, _clipboard_paste_area.TransformedNorth(src_area.tile, inv_dtr), inv_dtr);

		bool has_preview = false;
		switch(GetTileType(src_tile)) {
			case MP_RAILWAY:      has_preview = TestRailTileCopyability(src_tile, _clipboard_copy_paste_mode, _local_company, ret); break;
			case MP_ROAD:         has_preview = TestRoadTileCopyability(src_tile, _clipboard_copy_paste_mode, _local_company, ret); break;
			case MP_STATION:      has_preview = TestStationTileCopyability(src_tile, src_area, _clipboard_copy_paste_mode, NULL, _local_company, ret); break;
			case MP_WATER:        has_preview = TestWaterTileCopyability(src_tile, src_area, _clipboard_copy_paste_mode, NULL, _local_company, ret); break;
			case MP_TUNNELBRIDGE: has_preview = TestTunnelBridgeTileCopyability(src_tile, src_area, _clipboard_copy_paste_mode, NULL, _local_company, ret); break;
			default:              MemSetT(ret, 0); break;
		}

		if (has_preview) ret->highlight_track_bits = TransformTrackBits(ret->highlight_track_bits, _clipboard_transformation);
	} else {
		assert(paste_area_corners.Contains(tile));
		MemSetT(ret, 0);
	}

	ret->tile_height = TileHeight(src_of_tile_corner) + height_delta;
}

struct ClipboardToolbarWindow : Window {
	static HotkeyList hotkeys;

	static CopyPasteMode FlagButtonToFlagBit(int button)
	{
		switch (button) {
			case WID_CT_WITH_RAIL:        return CPM_WITH_RAIL_TRANSPORT;
			case WID_CT_WITH_ROAD:        return CPM_WITH_ROAD_TRANSPORT;
			case WID_CT_WITH_WATER:       return CPM_WITH_WATER_TRANSPORT;
			case WID_CT_WITH_AIR:         return CPM_WITH_AIR_TRANSPORT;
			case WID_CT_MIRROR_SIGNALS:   return CPM_MIRROR_SIGNALS;
			case WID_CT_UPGRADE_BRIDGES:  return CPM_UPGRADE_BRIDGES;
			default: NOT_REACHED(); break;
		};
		return CPM_NONE;
	}

	ClipboardToolbarWindow(WindowDesc *desc) : Window(desc)
	{
		this->InitNested();

		/* select another railtype if the one that was used last time is invalid/unavailable */
		if (!IsInsideMM(_clipboard_railtype, RAILTYPE_BEGIN, RAILTYPE_END)) _clipboard_railtype = RAILTYPE_BEGIN;
		RailType rt = _clipboard_railtype;
		while (!HasRailtypeAvail(_local_company, rt)) {
			rt = (rt < RAILTYPE_END - 1) ? (RailType)(rt + 1) : RAILTYPE_BEGIN;
			if (rt == _clipboard_railtype) { // did we get back to the point where we started?
				rt = INVALID_RAILTYPE;
				_clipboard_copy_paste_mode &= ~CPM_CONVERT_RAILTYPE;
				break;
			}
		}
		_clipboard_railtype = rt;

		/* disable the paste button if there is nothing to paste */
		this->SetWidgetDisabledState(WID_CT_PASTE, !IsClipboardPasteSourceSet());
		/* set the sprite on the railtype button */
		this->GetWidget<NWidgetCore>(WID_CT_CONVERT_RAILTYPE)->widget_data =
				(_clipboard_copy_paste_mode & CPM_CONVERT_RAILTYPE) ?
				GetRailTypeInfo(_clipboard_railtype)->gui_sprites.convert_rail :
				SPR_IMG_CLIPBOARD_NO_RAIL_CONVERTION;
		/* lower on/off buttons */
		for (int widget = WID_CT_PASTE_FLAG_BUTTON_BEGIN; widget < WID_CT_PASTE_FLAG_BUTTON_END; widget++) {
			if (_clipboard_copy_paste_mode & ClipboardToolbarWindow::FlagButtonToFlagBit(widget)) this->LowerWidget(widget);
		}
		this->SetWidgetLoweredState(WID_CT_TERRAFORM, (_clipboard_copy_paste_mode & CPM_TERRAFORM_MASK) != CPM_TERRAFORM_NONE);

		if (_settings_client.gui.link_terraform_toolbar) ShowTerraformToolbar(this);
	}

	~ClipboardToolbarWindow()
	{
		if (_settings_client.gui.link_terraform_toolbar) DeleteWindowById(WC_SCEN_LAND_GEN, 0, false);
	}

	virtual void DrawWidget(const Rect &r, int widget) const
	{
		int offset = this->IsWidgetLowered(widget) ? 2 : 1;
		switch (widget) {
			case WID_CT_WITH_RAIL:
			case WID_CT_WITH_ROAD:
			case WID_CT_WITH_WATER:
			case WID_CT_WITH_AIR: {
				offset++;
				DrawSprite(SPR_BLOT, this->IsWidgetLowered(widget) ? PALETTE_TO_GREEN : PALETTE_TO_RED, r.left + offset, r.top + offset);
				break;
			}

			case WID_CT_TERRAFORM: {
				offset++;
				PaletteID pal;
				switch (_clipboard_copy_paste_mode & CPM_TERRAFORM_MASK) {
					case CPM_TERRAFORM_FULL:    pal = PALETTE_TO_GREEN;  break;
					case CPM_TERRAFORM_MINIMAL: pal = PALETTE_TO_YELLOW; break;
					default:                    pal = PALETTE_TO_RED;    break;
				}
				DrawSprite(SPR_BLOT, pal, r.left + offset, r.top + offset);
				break;
			}

			case WID_CT_TRANSFORMATION:
				DrawSprite(SPR_IMG_TRANFORMATION_IDENTITY + _clipboard_transformation, PAL_NONE, r.left + offset, r.top + offset);
				break;

			case WID_CT_HEIGHT_DIFF_GLYPH:
				DrawSprite(SPR_IMG_CLIPBOARD_HEIGHT_PANEL, PAL_NONE, r.left, r.top);
				break;

			default:
				break;
		}
	}

	virtual void UpdateWidgetSize(int widget, Dimension *size, const Dimension &padding, Dimension *fill, Dimension *resize)
	{
		switch (widget) {
			case WID_CT_HEIGHT_DIFF_GLYPH:
				*size = maxdim(*size, GetSpriteSize(SPR_IMG_CLIPBOARD_HEIGHT_PANEL));
				break;

			case WID_CT_HEIGHT_DIFF: {
				/* Backup the height delta. The variable will be used to calculate the size of the widget. */
				int backup = _clipboard_additional_height_delta;
				/* calculate the size */
				size->width = 0;
				for (_clipboard_additional_height_delta = CLIPBOARD_ADDITIONAL_HEIGHT_MIN;
						_clipboard_additional_height_delta <= CLIPBOARD_ADDITIONAL_HEIGHT_MAX;
						_clipboard_additional_height_delta++) {
					this->SetStringParameters(WID_CT_HEIGHT_DIFF); // _clipboard_additional_height_delta will be used there
					Dimension d = GetStringBoundingBox(this->GetWidget<NWidgetCore>(WID_CT_HEIGHT_DIFF)->widget_data);
					size->width = max(size->width, d.width + 1);
					size->height = max(size->height, d.height);
				}
				/* restore */
				_clipboard_additional_height_delta = backup;
				break;
			}
		}
	}

	virtual void SetStringParameters(int widget) const
	{
		switch (widget) {
			case WID_CT_HEIGHT_DIFF:
				SetDParam(0, (StringID)(STR_CLIPBOARD_HEIGHT_DIFF_NEUTRAL + sgn(_clipboard_additional_height_delta)));
				SetDParam(1, abs(_clipboard_additional_height_delta));
				break;
		}
	}

	virtual void OnClick(Point pt, int widget, int click_count)
	{
		if (this->IsWidgetDisabled(widget)) return;

		DirTransformation add_clipboard_transformation = DTR_IDENTITY; // additional transformation

		switch (widget) {
			case WID_CT_COPY:
				if (HandlePlacePushButton(this, widget, SPR_CURSOR_COPY, HT_RECT)) {
					this->SetWidgetDirty(widget);
				}
				return;

			case WID_CT_PASTE:
				if (HandlePlacePushButton(this, widget, _ctrl_pressed ? SPR_CURSOR_ADJUST_HEIGHT : SPR_CURSOR_PASTE, HT_POINT | HT_PASTE_PREVIEW)) {
					ClipboardRecalcPasteAreaSize();
					SetTileSelectSize(_clipboard_paste_area.w + 1, _clipboard_paste_area.h + 1);
					this->SetWidgetDirty(widget);
				}
				return;

			case WID_CT_TERRAFORM: {
				CopyPasteMode terraform_mode;
				switch (_clipboard_copy_paste_mode & CPM_TERRAFORM_MASK) {
					case CPM_TERRAFORM_NONE:    terraform_mode = CPM_TERRAFORM_FULL;    break;
					case CPM_TERRAFORM_MINIMAL: terraform_mode = CPM_TERRAFORM_NONE;    break;
					case CPM_TERRAFORM_FULL:    terraform_mode = CPM_TERRAFORM_MINIMAL; break;
					default: NOT_REACHED();
				}
				(_clipboard_copy_paste_mode &= ~CPM_TERRAFORM_MASK) |= terraform_mode;
				this->SetWidgetLoweredState(WID_CT_TERRAFORM, terraform_mode != CPM_TERRAFORM_NONE);
				break;
			}

			case WID_CT_TRANSFORMATION:
				/* reset transformation - combined with its inversion will give identity */
				add_clipboard_transformation = InvertDirTransform(_clipboard_transformation);
				break;

			case WID_CT_ROTATE_LEFT:   add_clipboard_transformation = DTR_ROTATE_90_L;   break;
			case WID_CT_ROTATE_RIGHT:  add_clipboard_transformation = DTR_ROTATE_90_R;   break;
			case WID_CT_REFLECT_NE_SW: add_clipboard_transformation = DTR_REFLECT_NE_SW; break;
			case WID_CT_REFLECT_NW_SE: add_clipboard_transformation = DTR_REFLECT_NW_SE; break;

			case WID_CT_WITH_RAIL:
			case WID_CT_WITH_ROAD:
			case WID_CT_WITH_WATER:
			case WID_CT_WITH_AIR:
			case WID_CT_MIRROR_SIGNALS:
			case WID_CT_UPGRADE_BRIDGES: {
				CopyPasteMode flag = ClipboardToolbarWindow::FlagButtonToFlagBit(widget);
				_clipboard_copy_paste_mode ^= flag;
				this->SetWidgetLoweredState(widget, (_clipboard_copy_paste_mode & flag) != 0);
				break;
			}

			case WID_CT_CONVERT_RAILTYPE: {
				ShowDropDownList(this, GetRailTypeDropDownList(),
						(_clipboard_copy_paste_mode & CPM_CONVERT_RAILTYPE) ? INVALID_RAILTYPE : _clipboard_railtype,
						WID_CT_CONVERT_RAILTYPE, 140, true, true);
				break;
			}

			case WID_CT_HEIGHT_DIFF_INCREASE: this->ModifyAdditionalHeightDelta(+1); break;
			case WID_CT_HEIGHT_DIFF_DECREASE: this->ModifyAdditionalHeightDelta(-1); break;

			default:
				return;
		}

		SndPlayFx(SND_15_BEEP);
		this->SetWidgetDirty(widget); // suitable for most cases

		if (add_clipboard_transformation != DTR_IDENTITY) {
			_clipboard_transformation = CombineDirTransform(_clipboard_transformation, add_clipboard_transformation);
			this->SetWidgetDirty(WID_CT_TRANSFORMATION);
			if (this->IsWidgetLowered(WID_CT_PASTE)) {
				ClipboardRecalcPasteAreaSize();
				SetTileSelectSize(_clipboard_paste_area.w + 1, _clipboard_paste_area.h + 1);
			}
		}
	}

	virtual EventState OnHotkey(int hotkey)
	{
		switch (hotkey) {
			case WID_CT_CONVERT_RAILTYPE:
				this->OnDropdownSelect(WID_CT_CONVERT_RAILTYPE, (_clipboard_copy_paste_mode & CPM_CONVERT_RAILTYPE) ? INVALID_RAILTYPE : _clipboard_railtype);
				this->SetWidgetDirty(WID_CT_CONVERT_RAILTYPE);
				SndPlayFx(SND_15_BEEP);
				return ES_HANDLED;

			case WID_CT_WITH_RAIL:
			case WID_CT_WITH_ROAD:
			case WID_CT_WITH_WATER:
			case WID_CT_WITH_AIR:
				if (this->IsWidgetLowered(WID_CT_PASTE)) MarkWholeScreenDirty(); // redraw tile selection
				break;

			default:
				break;
		}

		return this->Window::OnHotkey(hotkey);
	}

	virtual void OnDropdownSelect(int widget, int index)
	{
		assert(widget == WID_CT_CONVERT_RAILTYPE);

		if (index == INVALID_RAILTYPE) {
			_clipboard_copy_paste_mode &= ~CPM_CONVERT_RAILTYPE;
			this->GetWidget<NWidgetCore>(WID_CT_CONVERT_RAILTYPE)->widget_data = SPR_IMG_CLIPBOARD_NO_RAIL_CONVERTION;
		} else {
			_clipboard_copy_paste_mode |= CPM_CONVERT_RAILTYPE;
			_clipboard_railtype = (RailType)index;
			this->GetWidget<NWidgetCore>(WID_CT_CONVERT_RAILTYPE)->widget_data = GetRailTypeInfo(_clipboard_railtype)->gui_sprites.convert_rail;
		}
	}

	virtual EventState OnCTRLStateChange()
	{
		if (this->IsWidgetLowered(WID_CT_PASTE)) SetMouseCursor(_ctrl_pressed ? SPR_CURSOR_ADJUST_HEIGHT : SPR_CURSOR_PASTE, PAL_NONE);

		return ES_NOT_HANDLED;
	}

	virtual void OnPlaceObject(Point pt, TileIndex tile)
	{
		if (this->IsWidgetLowered(WID_CT_COPY)) {
			/* start copy area dragging */
			VpStartPlaceSizing(tile, VPM_X_AND_Y_LIMITED, DDSP_COPY_TO_CLIPBOARD);
			VpSetPlaceSizingLimit(CLIPBOARD_DRAG_LIMIT);
		} else {
			_clipboard_paste_area.tile = tile;

			/* do paste */
			assert(this->IsWidgetLowered(WID_CT_PASTE));
			assert(IsClipboardPasteSourceSet());

			if (IsClipboardBufferOn()) {
				/* copy/paste clipboard-to-map */
				uint32 p1 = 0, p2 = 0;
				SB(p1, 28,  4, _clipboard_railtype);
				SB(p2, 12,  4, _clipboard_additional_height_delta);
				SB(p2, 16,  3, _clipboard_transformation);
				SB(p2, 19, 11, _clipboard_copy_paste_mode);
				DoCommandP(tile, p1, p2, CMD_PASTE_FROM_CLIPBOARD | CMD_MSG(STR_COPY_PASTE_ERROR_SUMMARY), CcPaste);
			} else {
				/* copy/paste map-to-map */
				uint32 p1 = 0, p2 = 0;
				SB(p1,  0, 28, _clipboard_copy_area.tile);
				SB(p1, 28,  4, _clipboard_railtype);
				SB(p2,  0,  6, _clipboard_copy_area.w);
				SB(p2,  6,  6, _clipboard_copy_area.h);
				SB(p2, 12,  4, _clipboard_additional_height_delta);
				SB(p2, 16,  3, _clipboard_transformation);
				SB(p2, 19, 11, _clipboard_copy_paste_mode);
				DoCommandP(tile, p1, p2, CMD_INSTANT_COPY_PASTE | CMD_MSG(STR_COPY_PASTE_ERROR_SUMMARY), CcPaste);
			}
		}
	}

	virtual void OnPlaceDrag(ViewportPlaceMethod select_method, ViewportDragDropSelectionProcess select_proc, Point pt)
	{
		VpSelectTilesWithMethod(pt.x, pt.y, select_method);
	}

	virtual void OnPlaceMouseUp(ViewportPlaceMethod select_method, ViewportDragDropSelectionProcess select_proc, Point pt, TileIndex start_tile, TileIndex end_tile)
	{
		if (pt.x != -1) {
			switch (select_proc) {
				case DDSP_COPY_TO_CLIPBOARD: {
					TileArea ta = TileArea(start_tile, end_tile);

					/* do copy */
					if (IsClipboardBufferOn()) {
						/* copy into the buffer */
						uint32 p1 = 0, p2 = 0;
						SB(p1,  0, 16, ta.w); // source area width
						SB(p1, 16, 16, ta.h); // source area height
						if (!DoCommandP(ta.tile, p1, p2, CMD_COPY_TO_CLIPBOARD) || _shift_pressed) return; // leave copy tool opened
					}
					ResetObjectToPlace();

					/* select copy area */
					_clipboard_copy_area = ta;

					/* reset transformation and update buttons */
					_clipboard_transformation = DTR_IDENTITY;
					this->ModifyAdditionalHeightDelta(-_clipboard_additional_height_delta);
					this->SetWidgetDirty(WID_CT_TRANSFORMATION);
					this->SetWidgetDisabledState(WID_CT_PASTE, false);
					this->SetWidgetDirty(WID_CT_PASTE);
					break;
				}

				default:
					NOT_REACHED();
			}
		}
	}

	virtual void OnPlaceObjectAbort()
	{
		/* Unclick "copy" and "paste" buttons */
		this->RaiseWidget(WID_CT_COPY);
		this->RaiseWidget(WID_CT_PASTE);
		this->SetWidgetDirty(WID_CT_COPY);
		this->SetWidgetDirty(WID_CT_PASTE);
	}

	EventState OnPlaceMouseWheel(Point pt, int mousewheel)
	{
		if (mousewheel == 0 || !_ctrl_pressed || !this->IsWidgetLowered(WID_CT_PASTE)) return ES_NOT_HANDLED;
		this->ModifyAdditionalHeightDelta(-sgn(mousewheel));
		return ES_HANDLED;
	}

	void ModifyAdditionalHeightDelta(int diff)
	{
		_clipboard_additional_height_delta = Clamp(_clipboard_additional_height_delta + diff, CLIPBOARD_ADDITIONAL_HEIGHT_MIN, CLIPBOARD_ADDITIONAL_HEIGHT_MAX);
		this->SetWidgetDirty(WID_CT_HEIGHT_DIFF);
	}
};

EventState ClipboardGlobalHotkeys(int hotkey)
{
	Window *w = ShowClipboardToolbar();
	if (w == NULL) return ES_NOT_HANDLED;
	return w->OnHotkey(hotkey);
}

static const uint16 _clipboard_copy_keys[] = { 'C' | WKC_CTRL | WKC_GLOBAL_HOTKEY, WKC_INSERT | WKC_CTRL | WKC_GLOBAL_HOTKEY, 0 };
static const uint16 _clipboard_paste_keys[] = { 'V' | WKC_CTRL | WKC_GLOBAL_HOTKEY, WKC_INSERT | WKC_SHIFT | WKC_GLOBAL_HOTKEY, 0 };

static Hotkey _clipboard_hotkeys[] = {
	Hotkey(_clipboard_copy_keys,     "copy",            WID_CT_COPY),
	Hotkey(_clipboard_paste_keys,    "paste",           WID_CT_PASTE),
	Hotkey('1',                      "rail",            WID_CT_WITH_RAIL),
	Hotkey('2',                      "road",            WID_CT_WITH_ROAD),
	Hotkey('3',                      "water",           WID_CT_WITH_WATER),
	Hotkey('4',                      "air",             WID_CT_WITH_AIR),
	Hotkey('5',                      "terrain",         WID_CT_TERRAFORM),
	Hotkey('6',                      "convert_rail",    WID_CT_CONVERT_RAILTYPE),
	Hotkey('7',                      "mirror_signals",  WID_CT_MIRROR_SIGNALS),
	Hotkey('8',                      "upgrade_bridges", WID_CT_UPGRADE_BRIDGES),
	Hotkey(WKC_CTRL | WKC_COMMA,     "rotate_l",        WID_CT_ROTATE_LEFT),
	Hotkey(WKC_CTRL | WKC_PERIOD,    "rotate_r",        WID_CT_ROTATE_RIGHT),
	Hotkey(WKC_CTRL | WKC_BACKSLASH, "reflect_ne_sw",   WID_CT_REFLECT_NE_SW),
	Hotkey(WKC_CTRL | WKC_SLASH,     "reflect_nw_se",   WID_CT_REFLECT_NW_SE),
	HOTKEY_LIST_END
};
HotkeyList ClipboardToolbarWindow::hotkeys("clipboard", _clipboard_hotkeys, ClipboardGlobalHotkeys);

static const NWidgetPart _nested_clipboard_toolbar_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_DARK_GREEN),
		NWidget(WWT_CAPTION, COLOUR_DARK_GREEN), SetDataTip(STR_CLIPBOARD_TOOLBAR_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
		NWidget(WWT_STICKYBOX, COLOUR_DARK_GREEN),
	EndContainer(),
	NWidget(NWID_HORIZONTAL),
		/* COPY / PASTE BUTTONS */
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_CT_COPY),
						SetFill(0, 1), SetMinimalSize(22, 22), SetDataTip(SPR_IMG_CLIPBOARD_COPY, STR_CLIPBOARD_TOOLTIP_COPY),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_CT_PASTE),
						SetFill(0, 1), SetMinimalSize(22, 22), SetDataTip(SPR_IMG_CLIPBOARD_PASTE, STR_CLIPBOARD_TOOLTIP_PASTE),
		NWidget(WWT_PANEL, COLOUR_DARK_GREEN),
						SetFill(0, 1), SetMinimalSize(4, 22), EndContainer(),

		/* COPY/PASTE MODE SELECTORS */
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_CT_WITH_RAIL),
						SetFill(0, 1), SetMinimalSize(22, 22), SetDataTip(SPR_IMG_BUILDRAIL, STR_CLIPBOARD_TOOLTIP_COPY_WITH_RAIL_TRANSPORT),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_CT_WITH_ROAD),
						SetFill(0, 1), SetMinimalSize(22, 22), SetDataTip(SPR_IMG_BUILDROAD, STR_CLIPBOARD_TOOLTIP_COPY_WITH_ROAD_TRANSPORT),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_CT_WITH_WATER),
						SetFill(0, 1), SetMinimalSize(22, 22), SetDataTip(SPR_IMG_BUILDWATER, STR_CLIPBOARD_TOOLTIP_COPY_WITH_WATER_TRANSPORT),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_CT_WITH_AIR),
						SetFill(0, 1), SetMinimalSize(22, 22), SetDataTip(SPR_IMG_BUILDAIR, STR_CLIPBOARD_TOOLTIP_COPY_WITH_AIR_TRANSPORT),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_CT_TERRAFORM),
						SetFill(0, 1), SetMinimalSize(22, 22), SetDataTip(SPR_IMG_LANDSCAPING, STR_CLIPBOARD_TOOLTIP_TERRAFORM),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_CT_CONVERT_RAILTYPE),
						SetFill(0, 1), SetMinimalSize(22, 22), SetDataTip(SPR_IMG_CLIPBOARD_NO_RAIL_CONVERTION, STR_CLIPBOARD_TOOLTIP_CONVERT_RAIL),
		NWidget(WWT_IMGBTN_2, COLOUR_DARK_GREEN, WID_CT_MIRROR_SIGNALS),
						SetFill(0, 1), SetMinimalSize(22, 22), SetDataTip(SPR_IMG_CLIPBOARD_MIRROR_SIGNALS_OFF, STR_CLIPBOARD_TOOLTIP_MIRROR_SIGNALS),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_CT_UPGRADE_BRIDGES),
						SetFill(0, 1), SetMinimalSize(22, 22), SetDataTip(SPR_IMG_CLIPBOARD_UPGRADE_BRIDGES, STR_CLIPBOARD_TOOLTIP_UPGRADE_BRIDGES),
		NWidget(WWT_PANEL, COLOUR_DARK_GREEN),
						SetFill(0, 1), SetMinimalSize(4, 22), EndContainer(),

		/* TRANSFORMATIONS */
		NWidget(WWT_PUSHBTN, COLOUR_DARK_GREEN, WID_CT_TRANSFORMATION),
						SetFill(0, 1), SetMinimalSize(23, 22), SetDataTip(0, STR_CLIPBOARD_TOOLTIP_TRANSFORMATION),
		NWidget(WWT_PUSHIMGBTN, COLOUR_DARK_GREEN, WID_CT_ROTATE_LEFT),
						SetFill(0, 1), SetMinimalSize(22, 22), SetDataTip(SPR_IMG_CLIPBOARD_ROTATE_LEFT, STR_CLIPBOARD_TOOLTIP_ROTATE_LEFT),
		NWidget(WWT_PUSHIMGBTN, COLOUR_DARK_GREEN, WID_CT_ROTATE_RIGHT),
						SetFill(0, 1), SetMinimalSize(22, 22), SetDataTip(SPR_IMG_CLIPBOARD_ROTATE_RIGHT, STR_CLIPBOARD_TOOLTIP_ROTATE_RIGHT),
		NWidget(WWT_PUSHIMGBTN, COLOUR_DARK_GREEN, WID_CT_REFLECT_NE_SW),
						SetFill(0, 1), SetMinimalSize(22, 22), SetDataTip(SPR_IMG_CLIPBOARD_REFLECT_NE_SW, STR_CLIPBOARD_TOOLTIP_REFLECT_NE_SW),
		NWidget(WWT_PUSHIMGBTN, COLOUR_DARK_GREEN, WID_CT_REFLECT_NW_SE),
						SetFill(0, 1), SetMinimalSize(22, 22), SetDataTip(SPR_IMG_CLIPBOARD_REFLECT_NW_SE, STR_CLIPBOARD_TOOLTIP_REFLECT_NW_SE),
		NWidget(WWT_PANEL, COLOUR_DARK_GREEN),
						SetFill(0, 1), SetMinimalSize(4, 22), EndContainer(),

		/* HEIGHT MANIPULATOR */
		NWidget(WWT_PANEL, COLOUR_DARK_GREEN), SetMinimalSize(0, 22),
			NWidget(NWID_HORIZONTAL),
				NWidget(WWT_TEXT, COLOUR_DARK_GREEN, WID_CT_HEIGHT_DIFF_GLYPH), SetDataTip(STR_EMPTY, STR_NULL), SetFill(0, 1),
				NWidget(WWT_TEXT, COLOUR_DARK_GREEN, WID_CT_HEIGHT_DIFF), SetDataTip(STR_CLIPBOARD_HEIGHT_DIFF, STR_NULL), SetFill(0, 1),
				NWidget(NWID_VERTICAL), SetPIP(3, 0, 3),
					NWidget(NWID_HORIZONTAL), SetPIP(0, 1, 3),
						NWidget(WWT_PUSHIMGBTN, COLOUR_GREY, WID_CT_HEIGHT_DIFF_INCREASE), SetDataTip(SPR_ARROW_UP, STR_NULL), SetFill(0, 1),
						NWidget(WWT_PUSHIMGBTN, COLOUR_GREY, WID_CT_HEIGHT_DIFF_DECREASE), SetDataTip(SPR_ARROW_DOWN, STR_NULL), SetFill(0, 1),
					EndContainer(),
				EndContainer(),
			EndContainer(),
		EndContainer(),
	EndContainer(),
};

static WindowDesc _clipboard_toolbar_desc(
	WDP_ALIGN_TOOLBAR, "toolbar_clipboard", 0, 0,
	WC_BUILD_TOOLBAR, WC_NONE,
	WDF_CONSTRUCTION,
	_nested_clipboard_toolbar_widgets, lengthof(_nested_clipboard_toolbar_widgets),
	&ClipboardToolbarWindow::hotkeys
);


/**
 * Open the clipboard toolbar to copy and paste map pieces.
 * @return newly opened clipboard toolbar, or NULL if the toolbar could not be opened.
 */
Window *ShowClipboardToolbar()
{
	if (!Company::IsValidID(_local_company)) return NULL;
	DeleteWindowByClass(WC_BUILD_TOOLBAR);
	return new ClipboardToolbarWindow(&_clipboard_toolbar_desc);
}
