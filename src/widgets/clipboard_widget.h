/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file clipboard_widget.h Types related to the clipboard widgets. */

#ifndef WIDGETS_CLIPBOARD_WIDGET_H
#define WIDGETS_CLIPBOARD_WIDGET_H

/** Widgets of the #ClipboardToolbarWindow class. */
enum ClipboardToolbarWidgets {
	WID_CT_COPY,
	WID_CT_PASTE,

	WID_CT_PASTE_FLAG_BUTTON_BEGIN,
	WID_CT_WITH_RAIL = WID_CT_PASTE_FLAG_BUTTON_BEGIN,
	WID_CT_WITH_ROAD,
	WID_CT_WITH_WATER,
	WID_CT_WITH_AIR,
	WID_CT_MIRROR_SIGNALS,
	WID_CT_UPGRADE_BRIDGES,
	WID_CT_PASTE_FLAG_BUTTON_END,

	WID_CT_CONVERT_RAILTYPE = WID_CT_PASTE_FLAG_BUTTON_END,

	WID_CT_TERRAFORM,

	WID_CT_TRANSFORMATION,
	WID_CT_ROTATE_LEFT,
	WID_CT_ROTATE_RIGHT,
	WID_CT_REFLECT_NE_SW,
	WID_CT_REFLECT_NW_SE,

	WID_CT_HEIGHT_DIFF_GLYPH,
	WID_CT_HEIGHT_DIFF,
	WID_CT_HEIGHT_DIFF_INCREASE,
	WID_CT_HEIGHT_DIFF_DECREASE,
};

#endif /* WIDGETS_CLIPBOARD_WIDGET_H */

