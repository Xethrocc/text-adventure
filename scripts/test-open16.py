#!/usr/bin/env python3
"""OPEN-16 smoke test: bare verbs with 0/1/N candidate targets."""
import subprocess, sys, os, shutil

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
WB = ["cabal", "run", "-v0", "worldbuilder", "--"]
GAME = ["cabal", "run", "-v0", "text-adventure-cli", "--"]
TMP = "/tmp/zt-open16"
os.makedirs(TMP, exist_ok=True)

def make_yaml(name, extra_items="", lamp_room="living-room", lamp_state="off",
              start_room="west-of-house"):
    s = open(os.path.join(REPO, "examples/zork1.yaml"), encoding="utf-8").read()
    if extra_items:
        anchor = "- id: bag-of-coins"
        assert s.count(anchor) == 1, f"anchor count: {s.count(anchor)}"
        s = s.replace(anchor, extra_items + anchor, 1)
    # light the lamp and put it in the start room
    a = "location: living-room\n  state: off\n  tags_when:\n    lit: [lightsource]\n  portable: true\n- id: emerald"
    assert a in s
    s = s.replace(a, f"location: {lamp_room}\n  state: {lamp_state}\n  tags_when:\n    lit: [lightsource]\n  portable: true\n- id: emerald", 1)
    s = s.replace("start_room: west-of-house", f"start_room: {start_room}", 1)
    path = os.path.join(TMP, name + ".yaml")
    open(path, "w", encoding="utf-8").write(s)
    return path

def compile_and_run(name, commands):
    yaml_path = make_yaml(name, **EXTRAS[name])
    out = os.path.join(TMP, name + "-out")
    r = subprocess.run(WB + ["compile", yaml_path, "-o", out],
                       capture_output=True, text=True, cwd=REPO)
    if r.returncode != 0:
        print(f"  COMPILE FAIL: {r.stdout} {r.stderr}")
        return ""
    saves = os.path.join(TMP, name + "-saves")
    os.makedirs(saves, exist_ok=True)
    r = subprocess.run(
        GAME + ["--world", os.path.join(out, "world.json"),
                "--save", os.path.join(out, "save.json")],
        input=commands, capture_output=True, text=True, cwd=REPO,
        env={**os.environ, "TA_SAVES_DIR": saves})
    return r.stdout + r.stderr

BOX2 = """- id: box
  name: wooden box
  description: There is a wooden box here.
  keys:
  - box
  location: living-room
  capacity: 5
  state: closed
  portable: false
- id: crate
  name: wooden crate
  description: There is a wooden crate here.
  keys:
  - crate
  location: living-room
  capacity: 5
  state: closed
  portable: false
"""

EXTRAS = {
    "attack1": dict(lamp_room="troll-room", lamp_state="lit", start_room="troll-room"),
    "attack0": dict(lamp_state="lit", start_room="living-room"),
    "take2":   dict(lamp_state="lit", start_room="living-room"),
    "open2":   dict(extra_items=BOX2, lamp_state="lit", start_room="living-room"),
    "drop0":   dict(start_room="living-room"),
    "talk1":   dict(lamp_room="troll-room", lamp_state="lit", start_room="troll-room"),
}

EXPECT = {
    "attack1": ("You hit for", "attack auto-targets the troll"),
    "attack0": ("There's nothing to fight here.", "attack with 0 NPCs"),
    "take2":   ("Which do you mean:", "take asks with 2+ items"),
    "open2":   ("Which do you mean:", "open asks with 2 closed containers"),
    "drop0":   ("You're not carrying anything to drop.", "drop with empty inventory"),
    "talk1":   ("Troll has nothing to say.", "talk auto-targets the troll"),
}

COMMANDS = {
    "attack1": "attack\nquit\n",
    "attack0": "attack\nquit\n",
    "take2":   "take\n1\nquit\n",
    "open2":   "open\n1\nquit\n",
    "drop0":   "drop\nquit\n",
    "talk1":   "talk\nquit\n",
}

results = []
for name in ["attack1", "attack0", "take2", "open2", "drop0", "talk1"]:
    print(f"--- {name}")
    out = compile_and_run(name, COMMANDS[name])
    needle, label = EXPECT[name]
    ok = needle in out
    results.append(ok)
    print(f"  {'OK' if ok else 'FAIL'} {label}: expected '{needle}'")
    if not ok:
        # show the relevant lines
        for line in out.splitlines():
            if line.strip() and not line.startswith("=") and "Loaded" not in line and "Type " not in line:
                print(f"    | {line}")

# open2: after answering "1", the box or crate should open
out = compile_and_run("open2", COMMANDS["open2"])
opened_ok = "is open now" in out or "Opened." in out or "reveals" in out
results.append(opened_ok)
print(f"--- open2 answer")
print(f"  {'OK' if opened_ok else 'FAIL'} open answer opens a container")

print(f"\n{sum(results)}/{len(results)} passed")
sys.exit(0 if all(results) else 1)
