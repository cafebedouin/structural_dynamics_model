# Presence-clause blinded adjudication (pinned before calls)

The graduation grammar (PREREGISTRATION.md) requires kernel presence 3/3 name-blind
subject+stance. Runs 1–3 minted kernels under three churned ids
(`role_of_debate_kernel` / `topicality_boundary_kernel` /
`legitimate_ballot_grounding`) — a CLOSE cross-run presence call, which per the
prereg and the KNOWN_STATE 2026-08-06 tripwire may not be made unblinded.

## Protocol

- Packet: `PRESENCE_PACKET.md` (md5 `afbaaa5533cb125f9afd2ba5eabec283`) — the three T
  Framework draws' `kernel_description` texts PLUS one planted different-subject
  control (AT Fiat's kernel description: same broad domain — debate-practice
  meta-layer — genuinely different contested commitment). Ids stripped, order
  randomized (seed 2592, builder `build_presence_packet.py`).
- Withheld mapping md5 pinned: **`e6a26bed1870e2364d570750b3344452`** — the mapping
  file enters the tree only after the calls commit.
- Adjudicator: fresh instance, no tools, packet + rubric inline only.
- **Rubric (pinned):** two items describe the SAME contested kernel iff the SUBJECT
  (which shared commitment/arrangement is being contested) and the CONTEST STRUCTURE
  (what the rival readings disagree about) match; different verbal framings,
  altitudes, or emphases of one commitment count as SAME; a different contested
  commitment counts as DIFFERENT even in the same domain. Output: all pairwise calls
  + groups.
- **Instrument-validity rule (pinned, fires before any verdict is read):** the calls
  are usable ONLY if the planted control is called DIFFERENT from every T Framework
  item. If the plant groups with the others, the instrument failed to discriminate —
  the presence clause is then UNDETERMINED (draw-level language only), not satisfied.
- Verdict rule (pinned): presence 3/3 iff the three T Framework items form one SAME
  group (given a valid instrument). Any split → presence not unanimous → draw-level
  language only per the grammar. Either way, P1 unanimity (3/3, mechanical) is
  unaffected.
