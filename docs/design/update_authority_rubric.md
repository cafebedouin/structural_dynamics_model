# `update_authority` — authoring rubric (institutional revision authority)

**Status: DRAFT, pending operator ruling. Not yet used to author anything.**

This rubric assigns one value of `update_authority` to a constraint's kernel, on **institutional
grounds only**: *who may amend the kernel, and by what procedure.* It records the revision-authority
structure of the kernel and nothing else. It is authored independently of any downstream analysis
that may consume it — do not reason about what the value implies for any other property.

## The decision procedure

Assign the value by answering these questions **from the authority text**, in order:

0. **Is there an identifiable kernel at all — a foundational content that could *have* an owner and
   an amendment procedure?**
   - **No** — there is no kernel to own (an emergent norm, a diffuse practice, a situation where
     "who may amend the kernel" simply does not apply) → **`unauthored`** (the field does not apply;
     this is **not** `absent_diffuse`).
   - **Yes** → go to Q1.

1. **Is the kernel's amending authority LIVE and present — a party whose authority over *this* kernel
   is current, who could legitimately amend it NOW?** (Present tense: read the authority as it stands
   now, not as the founding text once described it.)
   - **No, the instrument is superseded / defunct** — the body that once amended it no longer holds
     jurisdiction over the kernel; any procedure it had is *historical*, not a live authority →
     **`unauthored`** (not `licensed_revisable`: the procedure is not live; not `frozen`: `frozen`
     is reserved for a *live* kernel with foreclosed amendment).
   - **No, authority is distributed or emergent** — a live kernel exists, but no party is in a
     position to decide a change now → **`absent_diffuse`**.
   - **Yes** → go to Q2.

2. **Is there a defined, recognized, LIVE procedure by which that party amends *the kernel itself* —
   the foundational content, not merely its application or interpretation — such that a kernel
   amendment through it is legitimate right now (an ordinary act, not a rupture of the authority)?**
   - **Yes** → **`licensed_revisable`**.
   - **No** — the kernel is live but its amendment is not provided for: declared closed/complete/
     unalterable (even where its *application* stays freely revisable), or the only way to change it
     is to override the authority's own standing → **`frozen`** (the husk-relevant state: a live
     kernel whose amendment is foreclosed).

**The Q2 cut is kernel-amendment legitimacy, not activity or application-revisability.** A system may
develop its *application* vigorously — reinterpret, extend, adjudicate — while declaring the *kernel*
closed; that is `frozen`, because amending the kernel itself is not a recognized legitimate act.
Conversely a system that recognizes a procedure to revise the foundational content itself (binding
reinterpretation treated as amendment, precedent that can overturn precedent) is `licensed_revisable`.
Application-revisability is shared by `frozen` and `licensed_revisable` and is **not** the test.

The discriminator between `frozen` and `absent_diffuse` is **ownership**: `frozen` has an owner but no
kernel-amendment procedure; `absent_diffuse` has no owner at all.

## The three values

- **`licensed_revisable`** — a recognized authority *and* a defined procedure for amending the
  **kernel itself**; kernel amendment is a legitimate, ordinary act. *Examples:* a constitution with
  an amendment article; a standards body with a published revision process for the specification;
  common-law precedent that can overturn precedent; an interpretive authority empowered to issue
  binding revisions to the foundational content; openly-revisable published consensus.

- **`frozen`** — a **live** kernel with a recognized owner/authority, but **no legitimate procedure
  to amend the kernel itself**; the kernel is declared closed/complete/unalterable, even where
  application or interpretation remains freely revisable. Changing the kernel would require overriding
  the authority's own standing (fiat/rupture). The **husk-relevant** state. *Examples:* a canon
  declared closed by a still-authoritative body; a founding charter its own terms hold unamendable
  ("perpetual", "shall not be altered") and still in force; a specification published as final by a
  live standards body. **NOT** a superseded/defunct instrument (that is `unauthored`, case 7).

- **`absent_diffuse`** — **no single party owns** the kernel and no procedure governs its amendment;
  authority is distributed or emergent, and change (if it happens) comes from uncoordinated adoption,
  not a decision. *Examples:* a de-facto layout or format with no governing body; a natural
  language's grammar; a widely-forked convention no one controls.

## Worked boundary cases

1. **Rarely-exercised procedure.** A standards body that *can* revise the kernel but almost never
   does → `licensed_revisable`. The value records whether a legitimate kernel-amendment procedure
   **exists**, not how often it is used.

2. **Declared non-amendment — available-but-unchosen vs. foreclosed (the seam with case 6).** Both a
   body that has resolved never to amend and a canon declared closed are *an owner declaring
   non-amendment*. They split on whether the authority text treats future kernel amendment as
   **procedurally available but unchosen** (→ `licensed_revisable`) or as **illegitimate / foreclosed**
   (→ `frozen`). Test phrase: *"we do not intend to revise"* — a policy over a procedure that still
   exists — versus *"shall not be altered"* — the procedure itself foreclosed. **If the text will not
   decide which, the field is `unauthored`** (do not guess from tone).

3. **Emergent standard vs. proprietary standard.** A convention with no governing body →
   `absent_diffuse`; the same function owned by a company with an internal revision process →
   `licensed_revisable`. Same artifact-type, different authority structure.

4. **Perpetual founding document.** A charter with an amendment article → `licensed_revisable`; one
   declaring itself unalterable → `frozen`. The self-description of the amendment path is the test.

5. **Distributed but coordinated.** A protocol governed by a formal multi-party process (an RFC
   track, a consortium vote) is **not** `absent_diffuse` — a kernel-amendment procedure exists, so
   → `licensed_revisable`. `absent_diffuse` requires the *absence* of a governing process, not merely
   many participants.

6. **Kernel vs. application (the decisive cut).** A canon whose text is declared closed but whose
   interpretation/application is actively developed → **`frozen`**: the discriminating fact is the
   kernel-closing declaration (an institutional act, readable off the authority text), and the lively
   application is shared with the revisable case, so it is not the test. A tradition that recognizes
   a procedure to revise the **foundational content itself** — binding reinterpretation treated as
   amendment of the kernel, precedent empowered to overturn precedent — → **`licensed_revisable`**.

7. **Superseded / defunct instrument — the tense cut (Q1).** An instrument whose amending body once
   had a revision procedure but **no longer holds jurisdiction** over the kernel (a repudiated treaty,
   a monetary order that has ended, a superseded mandate) → **`unauthored`**, **not**
   `licensed_revisable`. The founding text describing a procedure does **not** make the authority
   live; read amendment authority in the **present tense**. This is the case the field must not
   misread as revisable — a dead/ended kernel that still recites its original governance is precisely
   where a husk hides. `frozen` is **not** this case: `frozen` needs a *live* kernel whose amendment
   is foreclosed.

## Evidence rule (for the authoring pass)

Assign the value from the constraint's **authority text**, and record **one quoted sentence** that
establishes the ownership + kernel-amendment procedure (or their absence). If the value cannot be
assigned from who-may-amend-the-kernel-and-how alone — no identifiable kernel (Q0), or the text will
not decide available-but-unchosen vs. foreclosed (case 2) — the constraint is **`unauthored`** for
this field.

**Record `unauthored` as an explicit call with its reason** — *which* question could not be answered
from the text (no kernel / no decidable amendment status) — **not a blank row.** A blank row cannot
be distinguished from a constraint the pass skipped, which destroys the coverage denominator (Kill B
needs it). The field on the constraint stays *absent* in the engine (no `update_authority/2` fact —
absence is never a default), but the authoring pass's record carries the `unauthored` call + reason.

**`unauthored` is not `absent_diffuse`.** `absent_diffuse` is a substantive finding of the authority
text (a kernel exists, but no party owns it). `unauthored` means the question did not apply or was
not resolved from the text. They are different states and must never be conflated — never write
`absent_diffuse` as a stand-in for "not determined," and never impute any value as a default.
