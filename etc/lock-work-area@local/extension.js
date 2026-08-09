// Keeps the work area's top edge fixed across a lock.
//
// Locking disables every extension without "unlock-dialog" in its
// session-modes. Where a panel extension has moved the panel to the bottom,
// that hands the stock top bar back with its 44 px strut, the work area's
// origin moves from y=0 to y=44, and mutter pushes every window down into it.
// Nothing moves them back, because mutter enforces the work area's origin and
// ignores its bottom edge. So the bottom strut disappearing costs nothing --
// that grows the work area -- and the whole symptom is the top strut appearing.
//
// This extension declares "unlock-dialog" so it survives the lock, and stops
// that strut from being claimed while the session is locked. Suppressing the
// strut is preferred to hiding the bar, which would leave the lock screen
// without one. Hiding is the fallback for a shell whose internals have moved
// on.
//
// Two pieces of timing are load-bearing, and both were arrived at by watching
// the wrong thing happen.
//
// Suppression is applied again after the lock, because locking disables the
// other extensions after this handler runs and a panel extension's teardown
// re-registers the panel box as chrome -- a fresh tracked entry with struts
// back on, and the cleared flag gone with the old one.
//
// Releasing is deliberately late, because the session mode flips to unlocked
// before the panel extension is re-enabled. Releasing on that signal hands the
// still-present top bar its strut back and pushes every window down, which is
// the original bug recreated on the way out.

import GLib from 'gi://GLib'

import * as Main from 'resource:///org/gnome/shell/ui/main.js'
import { Extension } from 'resource:///org/gnome/shell/extensions/extension.js'

const LOG = 'lock-work-area:'

// Applied again at each of these, in milliseconds after locking, to land after
// the other extensions have been torn down. Observed here: still strutted at
// 300, clear from 1000 on.
const REASSERT_MS = [300, 1000, 2500]

// How long to hold after unlocking, covering the panel extension being
// re-enabled and republishing its own strut. Observed at about a second, so
// this is that with room.
const SETTLE_MS = 3000

export default class LockWorkAreaExtension extends Extension {
  enable() {
    this._sessionModeId = 0
    this._settleId = 0
    this._reassertIds = []
    this._strutSuppressed = false
    this._panelBoxHidden = false
    this._visibilityId = 0

    this._sessionModeId = Main.sessionMode.connect('updated', () =>
      this._sync(),
    )
    this._sync()
  }

  disable() {
    if (this._sessionModeId) {
      Main.sessionMode.disconnect(this._sessionModeId)
      this._sessionModeId = 0
    }

    this._cancelSettle()
    this._cancelReasserts()
    this._release()
  }

  _sync() {
    if (Main.sessionMode.isLocked) {
      this._cancelSettle()
      this._suppress()
      this._reassert()
    } else {
      this._scheduleSettle()
    }
  }

  _logWorkArea(when) {
    let index = Main.layoutManager.primaryIndex
    let area = Main.layoutManager.getWorkAreaForMonitor(index)

    if (area)
      console.log(
        `${LOG} ${when}: work area ${area.x} ${area.y} ${area.width}x${area.height}`,
      )
  }

  _reassert() {
    REASSERT_MS.forEach((delay) => {
      let id = GLib.timeout_add(GLib.PRIORITY_DEFAULT, delay, () => {
        this._reassertIds = this._reassertIds.filter((i) => i != id)

        if (!Main.sessionMode.isLocked) return GLib.SOURCE_REMOVE

        this._strutSuppressed = false
        this._suppress()
        this._logWorkArea(`re-asserted after ${delay}ms`)

        return GLib.SOURCE_REMOVE
      })

      this._reassertIds.push(id)
    })
  }

  _cancelReasserts() {
    this._reassertIds.forEach((id) => GLib.Source.remove(id))
    this._reassertIds = []
  }

  _scheduleSettle() {
    this._cancelSettle()
    this._cancelReasserts()

    this._settleId = GLib.timeout_add(GLib.PRIORITY_DEFAULT, SETTLE_MS, () => {
      this._settleId = 0
      this._release()
      this._logWorkArea('released after settling')

      return GLib.SOURCE_REMOVE
    })
  }

  _cancelSettle() {
    if (this._settleId) {
      GLib.Source.remove(this._settleId)
      this._settleId = 0
    }
  }

  _suppress() {
    if (this._strutSuppressed || this._panelBoxHidden) return

    if (this._setAffectsStruts(false)) {
      this._strutSuppressed = true
      return
    }

    let panelBox = Main.layoutManager.panelBox
    if (!panelBox) return

    panelBox.hide()
    this._panelBoxHidden = true

    // Nothing orders this handler against the other extensions being disabled,
    // so a panel extension's teardown can show the bar back up after this has
    // hidden it.
    this._visibilityId = panelBox.connect('notify::visible', () => {
      if (Main.sessionMode.isLocked && panelBox.visible) panelBox.hide()
    })

    console.log(`${LOG} hid the panel box for the lock screen`)
  }

  _release() {
    if (this._strutSuppressed) {
      this._setAffectsStruts(true)
      this._strutSuppressed = false
    }

    if (this._panelBoxHidden) {
      let panelBox = Main.layoutManager.panelBox

      if (this._visibilityId) {
        panelBox?.disconnect(this._visibilityId)
        this._visibilityId = 0
      }

      panelBox?.show()
      this._panelBoxHidden = false
    }
  }

  // Reaches into the layout manager's own bookkeeping, which is private and
  // may be renamed by a shell update. Returning false is what sends the caller
  // to the fallback, so a rename degrades rather than breaks.
  _setAffectsStruts(value) {
    let layoutManager = Main.layoutManager
    let tracked = layoutManager._trackedActors
    let panelBox = layoutManager.panelBox

    if (!Array.isArray(tracked) || !panelBox) return false

    let entry = tracked.find((t) => t.actor == panelBox)
    if (!entry || !('affectsStruts' in entry)) return false

    entry.affectsStruts = value

    if (typeof layoutManager._queueUpdateRegions != 'function') return false
    layoutManager._queueUpdateRegions()

    return true
  }
}
