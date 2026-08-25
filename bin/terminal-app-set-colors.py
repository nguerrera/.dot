#!/usr/bin/env python3
"""Rewrite the macOS Terminal profile's colors from the Ptyxis palette.

Regenerates the color keys in etc/nguerrera.terminal from
etc/ptyxis-ng.palette so the two stay in sync. Runs anywhere (pure
stdlib, no macOS APIs); import the rewritten profile in Terminal.app to
apply it.

Terminal stores each color as an NSKeyedArchiver blob holding Display P3
components plus a generic-RGB fallback. One existing blob is used as the
structural template (it carries the ICC profile) and only the component
strings are replaced. Keys with no palette counterpart (cursor,
selection, bold text) are left alone.
"""

import argparse
import copy
import plistlib
import struct
import sys
from pathlib import Path

# sRGB -> Display P3, both linear, D65. Display P3 shares sRGB's
# transfer curve.
SRGB_TO_P3 = (
    (0.8224621, 0.1775380, 0.0000000),
    (0.0331941, 0.9668058, 0.0000001),
    (0.0170827, 0.0723974, 0.9105199),
)

# Linear P3 -> linear generic RGB (gamma 1.8), fitted against the
# fallback values macOS itself wrote into the profile. Only legacy
# readers that ignore the ICC ever see this value.
P3_TO_GENERIC = (
    (1.1916996, -0.1933303, -0.0005068),
    (-0.0679681, 1.1062337, -0.0378912),
    (-0.0155968, -0.0734272, 1.0920287),
)

PALETTE_TO_TERMINAL = {
    "Background": "BackgroundColor",
    "Foreground": "TextColor",
    "Color0": "ANSIBlackColor",
    "Color1": "ANSIRedColor",
    "Color2": "ANSIGreenColor",
    "Color3": "ANSIYellowColor",
    "Color4": "ANSIBlueColor",
    "Color5": "ANSIMagentaColor",
    "Color6": "ANSICyanColor",
    "Color7": "ANSIWhiteColor",
    "Color8": "ANSIBrightBlackColor",
    "Color9": "ANSIBrightRedColor",
    "Color10": "ANSIBrightGreenColor",
    "Color11": "ANSIBrightYellowColor",
    "Color12": "ANSIBrightBlueColor",
    "Color13": "ANSIBrightMagentaColor",
    "Color14": "ANSIBrightCyanColor",
    "Color15": "ANSIBrightWhiteColor",
}


def srgb_linear(c):
    return c / 12.92 if c <= 0.04045 else ((c + 0.055) / 1.055) ** 2.4


def srgb_encode(c):
    c = max(0.0, min(1.0, c))
    return 12.92 * c if c <= 0.0031308 else 1.055 * c ** (1 / 2.4) - 0.055


def apply_matrix(m, v):
    return [sum(m[i][j] * v[j] for j in range(3)) for i in range(3)]


def f32(x):
    # macOS stores float32; matching its precision keeps diffs quiet
    return struct.unpack("f", struct.pack("f", x))[0]


def component_strings(hex_color):
    srgb = [int(hex_color[i:i + 2], 16) / 255 for i in (0, 2, 4)]
    lin = [srgb_linear(c) for c in srgb]
    p3 = [srgb_encode(c) for c in apply_matrix(SRGB_TO_P3, lin)]
    generic = [max(0.0, c) ** (1 / 1.8)
               for c in apply_matrix(P3_TO_GENERIC, apply_matrix(SRGB_TO_P3, lin))]
    components = " ".join("%.10g" % f32(c) for c in p3) + " 1"
    fallback = " ".join("%.10g" % f32(c) for c in generic)
    return components.encode(), fallback.encode() + b"\x00"


def parse_palette(path):
    colors = {}
    for line in path.read_text().splitlines():
        if "=" in line:
            key, _, value = line.partition("=")
            if value.startswith("#"):
                colors[key.strip()] = value.strip().lstrip("#").lower()
    return colors


def make_blob(template_archive, hex_color):
    archive = copy.deepcopy(template_archive)
    components, fallback = component_strings(hex_color)
    for obj in archive["$objects"]:
        if isinstance(obj, dict) and "NSComponents" in obj:
            obj["NSComponents"] = components
            obj["NSRGB"] = fallback
            break
    else:
        sys.exit("template blob has no NSComponents")
    return plistlib.dumps(archive, fmt=plistlib.FMT_BINARY)


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    root = Path(__file__).resolve().parent.parent
    parser.add_argument("--palette", type=Path,
                        default=root / "etc/ptyxis-ng.palette")
    parser.add_argument("--terminal", type=Path,
                        default=root / "etc/nguerrera.terminal")
    args = parser.parse_args()

    colors = parse_palette(args.palette)
    profile = plistlib.loads(args.terminal.read_bytes())
    template = plistlib.loads(profile["BackgroundColor"])

    for palette_key, terminal_key in PALETTE_TO_TERMINAL.items():
        profile[terminal_key] = make_blob(template, colors[palette_key])

    args.terminal.write_bytes(plistlib.dumps(profile, fmt=plistlib.FMT_XML))
    print("wrote %d colors from %s to %s"
          % (len(PALETTE_TO_TERMINAL), args.palette.name, args.terminal.name))


if __name__ == "__main__":
    main()
