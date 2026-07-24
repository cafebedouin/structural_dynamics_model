You are classifying institutional constraints on ONE dimension: the revision authority of each
constraint's KERNEL (its foundational content), READ IN THE PRESENT TENSE. Assign exactly ONE value
per item using ONLY the rubric. Items are unrelated — judge each only from its own text.

## Decision procedure (answer in order, from the item's text)
- **Q0. Is there an identifiable kernel at all** (a foundational content that could have an owner and
  an amendment procedure)? NO → **`unauthored`**. YES → Q1.
- **Q1. Is the kernel's amending authority LIVE and PRESENT** — a party whose authority over this
  kernel is current, who could legitimately amend it NOW?
    - NO because the instrument is **superseded / defunct** (the body that once amended it no longer
      holds jurisdiction; any procedure it had is HISTORICAL, not live) → **`unauthored`**. The
      founding text reciting a procedure does NOT make the authority live — read it present-tense.
    - NO because authority is **distributed/emergent** (a live kernel, but no party can decide a
      change now) → **`absent_diffuse`**.
    - YES → Q2.
- **Q2. Is there a defined, LIVE procedure by which that party amends THE KERNEL ITSELF** (foundational
  content, not merely its application), legitimately, right now? YES → **`licensed_revisable`**.
  NO (a LIVE kernel whose amendment is foreclosed — declared closed/unalterable, or change requires
  overriding the authority) → **`frozen`**.

## Values
- **licensed_revisable** — live authority + a live procedure to amend the kernel itself.
- **frozen** — a LIVE kernel with a recognized owner but NO live procedure to amend it (closed/foreclosed).
- **absent_diffuse** — a live kernel, but NO single party owns it; change comes from uncoordinated adoption.
- **unauthored** — no identifiable kernel, OR a superseded/defunct instrument whose amending authority
  is not live, OR the text won't decide. Always give the reason.

## Boundary cases
1. Procedure exists but rarely used → licensed_revisable (existence, not frequency).
2. Declared non-amendment: available-but-unchosen → licensed_revisable; illegitimate/foreclosed →
   frozen; text won't decide → unauthored.
3. Emergent standard, no governing body → absent_diffuse; owned with a revision process → licensed_revisable.
4. Charter with amendment article → licensed_revisable; declaring itself unalterable → frozen.
5. Formal multi-party governing process (RFC, consortium vote) → licensed_revisable, not absent_diffuse.
6. Canon closed by a STILL-AUTHORITATIVE body, application developed → frozen; a tradition that revises
   the foundational content itself → licensed_revisable.
7. **Superseded/defunct instrument** whose amending body no longer holds jurisdiction (a repudiated
   treaty, an ended monetary order, a superseded mandate, a lapsed custom) → **`unauthored`**, NOT
   licensed_revisable (procedure is historical) and NOT frozen (which needs a LIVE kernel).
8. **A dead founding problem is NOT a dead amending authority** — judge the amending authority in the
   present tense on its own terms; a lapsed rationale with a live legislature that can amend/repeal now
   is `licensed_revisable`.

## Evidence rule
Give ONE quoted phrase establishing the PRESENT ownership + kernel-amendment procedure (or its absence).
If you must reason from anything other than who-may-amend-the-kernel-now-and-how, mark unauthored + reason.

## Output
One row per item: `item_NN | value | "one quoted phrase" | one-sentence reason (name Q0/Q1/Q2)`.
Valid values: licensed_revisable, frozen, absent_diffuse, unauthored.

---

## item_01

Founding Problem: Israel lacks a single codified constitution; the Basic Laws were enacted piecemeal, several explicitly described as building blocks toward a future constitution, and the 1992 Human Dignity and Liberty and Freedom of Occupation laws were read by the 1995 Bank Mizrahi ruling as furnishing the missing higher-law tier, filling the gap left by the 1950 Harari decision's deferred constitution-writing.
Coordination Function: Provides a check against majoritarian legislation that would otherwise be unconstrained by any codified constitution, protecting rights and minority interests that lack durable electoral majorities, and supplying predictable legal doctrine (proportionality, reasonableness) that other institutions and private actors can plan around.
Transfer Function: Moves effective policy-making power from the elected legislative majority to the unelected judiciary and, derivatively, to the litigants and organizations able to invoke the Court's jurisdiction — at the cost of the enacting coalition's ability to convert electoral mandates into durable law.
Absent Voices: The general electorate that produced the invalidated legislation has no institutionalized voice in the judicial process itself; their remedy is indirect (electing legislators who might eventually alter Basic Law procedure or Court composition), and that remedy has historically been slow and contested. Religious and settler constituencies who experience repeated invalidation describe themselves as structurally locked out of an interpretive process they cannot access on equal terms with litigating NGOs.
Disappearance Verdict: world_rearranges
Disappearance Rationale: If judicial invalidation power over Knesset legislation disappeared overnight, the Knesset would become the unconstrained final word on all legislation; military-exemption laws, settlement-related enactments, and reasonableness-doctrine-constrained executive decisions currently blocked or narrowed by the Court would proceed unmodified. Rights-claimant litigation as a policy lever would collapse, and minority groups without electoral leverage would lose their primary institutional protection. This is not a null structure — an entire body of case law, litigation strategy, and coalition-formation calculation is built on the assumption that the Court can and will exercise this power.
Founding Problem Status: contested


## item_02

Founding Problem: Bitcoin's early development showed that an open-source monetary protocol with no formal governance could be changed by a small group of developers, creating the risk of capture, unexpected inflation, or contentious splits that would destroy confidence in neutral digital money.
Coordination Function: Prevents arbitrary or capture-driven changes to a monetary protocol by requiring changes to meet an extremely high bar of agreement, thereby preserving predictability and resisting governance capture by any single interest.
Transfer Function: Transfers the cost of blocked innovation and higher transaction fees to users and researchers who require base-layer changes, while transferring the benefit of rule-predictability and L2 opportunity to existing holders and layered infrastructure builders.
Absent Voices: Alt-chain developers and base-layer scaling advocates who were driven out during the blocksize wars or silenced as altcoiners; retail users in the Global South who need low-fee on-chain payments but lack representation in the English-language developer and holder discourse.
Disappearance Verdict: world_rearranges
Disappearance Rationale: If the universal-consensus ossification norm vanished, protocol upgrades like block size increases, privacy enhancements, and new scripting capabilities would be debated on technical rather than procedural grounds; the L2 ecosystem would face competition from L1 improvements; existing holders would face uncertainty about monetary rule changes; the social and technical landscape of Bitcoin would reorganize around a more upgrade-permissive governance culture.
Founding Problem Status: contested


## item_03

Founding Problem: Medieval dynasties faced chronic succession crises when multiple claimants of comparable legitimacy could contest a throne; a rigid rule that removed an entire class of otherwise-qualified claimants (women) reduced the number of viable claimants and, in principle, reduced war.
Coordination Function: Provides an unambiguous, non-negotiable succession rule that forecloses the very succession disputes and civil wars that ambiguous or contestable inheritance rules tend to produce — a bright-line test removes bargaining space that could otherwise be exploited by rival claimants.
Transfer Function: Moves the crown, its lands, and its revenues along the male line exclusively, transferring political standing, marriage-market value as consort rather than sovereign, and inheritance rights away from female descendants and toward male collaterals, sometimes quite distant ones, ahead of closer female relations.
Absent Voices: Female heirs themselves are never seated at the interpretive table that declares the exclusion divinely or naturally mandated — the doctrine is elaborated entirely by male jurists and clergy who are also its beneficiaries. Cognatic-claimant territories are heard only through the arbitration of war, not through any recognized legal forum this reading admits.
Disappearance Verdict: world_rearranges
Disappearance Rationale: If the immutable-mandate framing collapsed, female and cognatic claimants would immediately become legally cognizable, succession disputes would be litigated on genealogical proximity rather than sex, and the entire apparatus of preventive war to enforce agnatic priority would lose its legal justification overnight — inheritance patterns, marriage alliances, and the war-making calculus of every neighboring dynasty would reorganize around the changed rule.
Founding Problem Status: contested


## item_04

Founding Problem: Early Bitcoin had no established governance process at all; without some norm for evaluating proposed changes, either developers could push through experimental changes with untested consequences (as nearly happened with the 2010 value-overflow bug and later hard-fork attempts), or a well-resourced minority could rewrite the protocol's core promises out from under long-term holders.
Coordination Function: A near-universal consensus requirement genuinely coordinates trust: it prevents any single faction — including the core developers themselves — from unilaterally rewriting monetary policy or settlement rules, which is the credibility property that makes the asset's scarcity claim durable.
Transfer Function: The arrangement moves the cost of protocol conservatism onto use cases that require base-layer change (cheap payments, high throughput, new feature sets) and transfers a durability/scarcity premium to holders and businesses whose models depend on the base layer staying exactly as it is.
Absent Voices: Proponents of base-layer capacity increases and alternative monetary parameters are structurally present in mailing lists and forums but functionally excluded from the outcome, since the consensus bar lets any determined minority veto change indefinitely; users in low-fee-tolerant markets have essentially no representation in the informal governance process at all.
Disappearance Verdict: contested
Disappearance Rationale: If the near-unanimity norm vanished overnight, long-term holders and layer-two builders would argue the world rearranges catastrophically (monetary credibility destroyed, hard-fork chaos, hash power splits); merchants and remittance users would argue the world mostly stays the same for them except that a base-layer capacity increase becomes newly possible, improving their situation; the parties fundamentally disagree on which counterfactual is the real one, which is itself evidence the norm functions as an identity commitment as much as an engineering choice.
Founding Problem Status: contested


## item_05

Founding Problem: Originally, remonstrance addressed a genuine problem: preventing the Crown from issuing edicts that contradicted existing registered law without any check, and ensuring new law was formally consistent with the body of registered law the parlement administered.
Coordination Function: In principle, remonstrance provides a review step that could catch genuinely arbitrary or legally defective royal acts before they take force — a coordination function analogous to judicial review of legislative regularity.
Transfer Function: The arrangement moves fiscal burden away from the offices, orders, and provinces the parlements are staffed by and sympathetic to, and onto the general taxpaying populace and the Crown's creditors, by blocking or diluting reforms that would have taxed privilege more evenly or restructured venal offices.
Absent Voices: The unrepresented taxpaying populace and the Crown's creditors have no seat in the remonstrance exchange; reforming ministers are structurally routed through the very body whose members' privileges the reforms would curtail, so their case is heard only as filtered through hostile review.
Disappearance Verdict: world_rearranges
Disappearance Rationale: If remonstrance disappeared overnight (as it effectively did after 1789), royal edicts would take immediate legal effect on promulgation; venal office values tied to obstruction capacity would collapse, provincial fiscal exemptions defended through parlementary sympathy would lose their institutional shield, and fiscal reform could proceed without the systematic multi-year delays that characterized late Bourbon finance.
Founding Problem Status: dead


## item_06

Founding Problem: The founding generation sought to avoid dependence on a standing professional army (associated with British occupation and monarchical tyranny) by ensuring the citizenry itself retained the capacity for organized armed defense, rooted in Anglo-American militia tradition and civic-republican political theory.
Coordination Function: Preserves, as founding constitutional commitment, the capacity of the citizenry as a body to constitute an armed common-defense force independent of a standing professional army, understood as both a practical defense mechanism and a check against governmental tyranny.
Transfer Function: Moves interpretive authority and legitimacy toward readings of gun regulation that ask whether a given citizen or class of citizens is being kept within reach of militia-style civic capacity, and away from readings organized purely around individual self-defense utility or organized-state regulatory prerogative.
Absent Voices: Contemporary gun-violence-affected communities and urban residents disconnected from any living militia tradition are not addressed within the reading's own terms; individual-right proponents object that civic-virtue framing subordinates personal self-defense to a communitarian purpose many gun owners do not share; collective-security proponents object that the reading strips the state's regulatory role from a clause whose grammar foregrounds a 'well regulated militia.'
Disappearance Verdict: contested
Disappearance Rationale: If this specific reading vanished from constitutional discourse, the operative constitutional text would remain, and litigation would proceed under whichever sibling reading (individual-right or collective-security) courts adopted instead — the world does not rearrange around this reading uniquely, since it is one interpretive lens among three live contenders rather than an independently operative arrangement. Originalist scholars would say serious historical work on founding intent would be lost from the discourse; other camps would say little would change in practical doctrine, since this reading has never been the controlling one in modern jurisprudence.
Founding Problem Status: contested
