# ERGO-PAPER-MECH-001 — Mechanisms Digest for the Verification-Cost Paper

**Status:** Working digest, non-canon. Drafted 22 July 2026 by extraction from the covenant corpus (all eleven documents then in the project folder), in preparation for the third folder reset. After the reset, this digest and START-003 are the only documents in the folder; the corpus documents leave. This digest is Claude-drafted and unadopted until signed by an accountable member; the adoption is the act.

**Purpose.** The paper cites no unpublished corpus and none of the member's own writing. Every mechanism the paper's reference design needs is therefore restated here in genericized, paper-ready language — stripped of corpus document IDs, internal doctrine names, staging machinery, and lore — with each mechanism tagged to the paper section that consumes it and to the substrate property it demands. This digest is the raw material for the paper's reference-design section and the grounding for all future drafting discussions after the corpus leaves the folder.

---

## 1 · Rules of extraction

1. **Genericized register.** Mechanisms are stated as *this paper's reference design*, in the voice of a systems paper specifying a design. No corpus IDs, no weld numbers, no internal doctrine names ("the musket clause," "apophasis," etc.) appear in paper-ready text. Where a corpus term is also a natural English term ("cell," "surety," "witness"), it survives as ordinary vocabulary.
2. **Two status labels only.** *Specified in this paper, unimplemented* — the reference design's own constructions. *Shipped, external* — properties of public systems (the chain, published protocols) citable to their own public sources. Nothing is cited to the corpus.
3. **External cites permitted where the thing is public and not the member's.** The mutual-credit note protocol (ChainCash, BetterMoneyLabs, public Ergo project) and the chain's own shipped properties are external public artifacts and citable normally. The member's own prior public writing is not cited (see §8 quarantine).
4. **The paper carries the full burden.** Because nothing defers to a specification elsewhere, every mechanism below must be defensible in-text. Each entry therefore includes the *defense core* — the one or two structural arguments the paper must make for it — so that no defense is lost with the corpus.
5. **Constants stay unset.** Where the corpus deliberately left a constant open (issuance caps, cadence floors, band boundaries, bond amounts), the paper states the constant's *shape and role* and marks the value as a deployment parameter. Inventing numbers here would be false precision; the corpus's own discipline is preserved by naming the parameter, not the number.

---

## 2 · Mechanisms for Role 1 — Neutral Time

**M-1 · Content-free commitment chains (the record head).**
Each governance unit ("cell") keeps an append-only local log of its institutional acts, folded into a running cryptographic commitment (hash chain or Merkle accumulator). At a fixed cadence the cell signs and publishes only the current head: a hash, a signature, a sequence number. The head proves the log exists, is ordered, and is unaltered — and reveals nothing beneath it. There is no expansion path from head to content except an inclusion proof plus voluntary disclosure by the record's subject and the cell jointly; no administrator, mode, or escrow with read access exists in the design.
*Defense core:* privacy is a property of the format, not a policy — there is nothing in a hash to leak; and provability without legibility is exactly the reconciliation a self-governing community needs between "records must convince strangers" and "records must not become a surveillance file."
*Substrate demand:* none yet — this layer is off-chain. It sets up M-2.
*Consumed by:* Role 1 (§4); reference-design section.

**M-2 · Two-axis attestation: peers attest meaning, the ledger attests time.**
A head is attested along two independent axes because either alone is forgeable by whoever controls it. *Social axis:* the head is deposited with at least two other cells, which countersign that head N was received at a stated time and extends the head N−1 they already hold — defeating the lone rewriter, who would have to reissue every subsequent head against witnesses holding the originals. *Ledger axis:* the head is committed to a public proof-of-work ledger as a content-free commitment, fixing when it existed on a clock no cell, ring, or coordinator controls — defeating the colluding ring, which can countersign each other's fictions but cannot move the ledger's time. The design needs no coordinator and no closed club: meaning is vouched by peers who actually know the depositor; time is fixed by infrastructure that adjudicates for no one.
*Defense core:* this is the paper's "neutral third party that is not a party" made mechanical, and it is the entire reason Role 1 exists — the ledger's *only* job here is time, which is why a content-free anchor is target-poor (the reorg-economics payoff-asymmetry argument attaches here).
*Substrate demand:* stranger-verifiable time-ordering from genesis (PoW; the categorical argument of the paper's Part III), at member-cost verification.
*Consumed by:* Role 1 (§4); the equilibrium argument; §6.5's hosting claim.

**M-3 · Non-enumerable anchoring.**
Ledger anchors must be unlinkable commitments in unremarkable transactions — no shared script template, no browsable set, jittered cadence — so that neither the anchors nor the deposit relationships can be walked to enumerate cells, members, or acts. The rent mechanics of a bounded-state chain *assist* this: old anchor outputs are eventually collected out of live state, so anchors do not persist as an enumerable population; the time guarantee is unaffected because the proof is historical block inclusion, not continued state membership.
*Defense core:* an anchor set that can be enumerated is a registry rebuilt on the honest path; the design must make the aggregate structurally unbuildable, not merely disallowed. (Candidate publishable technical result — companion note per the outline decision.)
*Substrate demand:* expressive-enough script layer to make anchors indistinguishable from ordinary traffic; bounded state with rent (the alignment noted above).
*Consumed by:* Role 1 (§4); the anti-compilation thread of the reference design; open item 3.

**M-4 · Cadence, rotation, and legible staleness.**
Deposits are periodic (a cadence floor I, a deployment parameter derived from reorg-depth economics — the paper's depth-D analysis supplies the floor's derivation). Witness sets rotate over time so no fixed pair ossifies into a permanent mutual alibi. A cell that stops depositing fails *loud*: its last head sits at time T on the ledger, and every interval past T + I is visible staleness a counterparty and an adjudicator can read. The mechanism distinguishes "no record" from "no witness," and the absence of proof is itself a readable signal rather than a hiding place.
*Defense core:* liveness is legibility — the failure posture is designed, not hoped for; under attack the system degrades toward *delay*, a named state, never toward forgery.
*Substrate demand:* predictable block cadence; the rollback ceiling and rental economics that bound D.
*Consumed by:* Role 1 (§4); the resilience property cluster; objection 6.

**M-5 · Four-step verification of a proffered record.**
A stranger or adjudicating panel verifies a disclosed record by exactly four checks: (V1) inclusion — the record hashes into head N via its inclusion proof; (V2) continuity — head N extends an unbroken, socially witnessed chain back to a trusted head; (V3) time — head N's ledger commitment exists at block height T; (V4) diversity — the witnesses of head N span independent regions of the deposit topology rather than forming a clique. On success the verifier knows the record existed by T and is unaltered — and learns nothing else. Demanding the whole log to check one record is defined as a design violation; the single inclusion proof is sufficient by construction.
*Defense core:* verification consumes only disclosed material plus public data — no registry, no directory, no trusted office — which is precisely what member-cost verification of the *ledger* makes possible one layer up: the member checking the chain themselves is the base of this whole tower.
*Substrate demand:* member-cost full verification (the paper's central thesis); light-client bootstrap for constrained members.
*Consumed by:* Role 1 (§4); the equilibrium argument's payoff.

---

## 3 · Mechanisms for Role 2 — Public Randomness and Collective Decision

**M-6 · Adjudication panels drawn by lot from the uninvolved.**
Contested matters are decided by ad-hoc panels drawn by verifiable public randomness from cells not party to the matter, convening for one matter and dissolving. Any venue with a stake in the outcome is disqualified automatically (interest, not accusation, triggers the move). Panel service is a mutual duty denominated in service, not money — it can be neither bought out of nor bought into. The permanent institution is a published instruction set (procedure, proof standards, the judgment ceiling), never a standing chamber: rules stand; judges do not. Prior judgments are published and persuasive, never binding — binding precedent applied across ad-hoc panels would ossify into the standing will of whoever curates it.
*Defense core:* an appointed judge can be captured through the appointer; a randomly drawn panel has no appointer — *provided the draw itself cannot be the rigged step*, which is exactly why Role 2's beacon analysis is load-bearing and why grinding-resistance has an economic floor the paper must derive.
*Substrate demand:* verifiable public randomness with priced grinding resistance (the withhold-cost analysis); auditable draw without a published roster of who served.
*Consumed by:* Role 2 (§4); Role 3 (the panel that attests surety release); §6.6(c).

**M-7 · The receipt-free assembled ballot (end-to-end verifiable, coercion-resistant).**
Mass decisions are taken in simultaneous, bodily-present, mixed-witness sessions inside a declared census window. The required tally property: anyone — participant or public — can verify that every counted ballot was valid, every credential spent at most once, every session batch was attested, and the tally equals the aggregate, while no one, including any officer, can decrypt an individual ballot or link a ballot to a person. Architecture shape: encrypted ballot + zero-knowledge validity proof + per-decision nullifier; each session commits its ballots under a batch root signed by the session's cross-cell attestation (the seam where the social layer certifies the mathematical layer without reading it); batch roots anchor to the ledger; the tally is produced by threshold decryption of *aggregates only*. The nullifier doubles as the one safe receipt: inclusion-verifiable, content-unprovable. Receipt-freeness is a design requirement, not a feature: a pressured voter must be *unable* to prove compliance to a principal, so a purchased ballot is undeliverable.
*Defense core:* this is the enforcement half of the paper's answer to the exchange-rate school (§6.7) — vote-buying fails on its enforcement side even where attempted — and the assembly-has-no-venue property (no hall to pack, no chair to seize, no floor to filibuster; only hardened sessions and a tally no one administers) removes an entire adversary class.
*Substrate demand:* composable zero-knowledge proof system native to the script layer (threshold proofs, validity proofs, nullifiers); anchoring for batch roots.
*Consumed by:* Role 2 (§4); §6.7; the property bundle.

**M-8 · The two-timestamp offline doctrine.**
Sessions assemble and sign fully offline (mesh-floor operation) and anchor when connectivity returns. Two timestamps with distinct jobs: the *session attestation* fixes time-of-cast (inside the window, sworn by mixed-cell witnesses against the cells' own record chains); the *ledger anchor* fixes time-of-anchor, an upper bound. Window compliance is judged on attested cast, with anchoring allowed a stated grace period. A session that never anchors fails loud — absent at tally close, legibly stale. Nullifiers deduplicate globally at tally, so partition confusion cannot double-count.
*Defense core:* peers attest meaning, the ledger attests time — extended one notch to voting; this is the partition-tolerance answer that makes the governance layer survivable on degraded connectivity, and it is the concrete instance of "partition reconciliation as a governed process" the paper claims as (verify-at-writing-time) novel.
*Substrate demand:* same as M-2; light anchoring cost so deferred batch anchoring is affordable.
*Consumed by:* Role 2 (§4); resilience cluster; the novelty claim (open item 8).

**M-9 · The self-claiming lot (compelled consideration).**
A maturity-gated agenda mechanism: public randomness selects one census credential per decision cycle; the draw is *self-claiming* — only the winner can prove she won, by zero-knowledge claim against her own credential, so the eligible pool is never enumerated and declining is invisible; an unclaimed slot lapses and redraws. The slot confers compelled consideration, never outcome: one proposal the body must deliberate and vote, bypassing every agenda gate. Single-use, non-renewable, undelegable, unpurchasable.
*Defense core:* in a large deliberating body the scarce resource is not votes but contemplation; elections are metabolized by oligarchy, randomness starves it. The lot is also the design's honest answer to the residual it concedes in §6.7 — organized interests still out-attend and out-propose; the lot redistributes *attention* without creating anything wealth can buy.
*Substrate demand:* the same beacon as M-6 (with the same grinding economics); ZK self-claim against an unenumerated credential set.
*Consumed by:* Role 2 (§4); §6.7's concession paragraph; objection 8.

---

## 4 · Mechanisms for Role 3 — Surety, Clearing, and Treasury

**M-10 · The surety box (stateless enforcement by pre-pledged collateral).**
Enforcement without police: at deal time, by consent, a party posts bounded collateral into a contract-controlled output spendable only on adjudication-attested conditions — a threshold signature of a panel verifiably drawn by the public beacon, with conformance to the published panel procedure checkable in the contract itself. The cap is published at issuance; liability expires with the relationship. Awards execute against what the loser pledged in advance; coercion is replaced by collateral, and the enforcement power was donated by the defendant, bounded, at the moment of agreement. The clearing device doubles as the judgment fund: judiciary and money are one mechanism viewed from two sides.
*Defense core:* this is the paper's first enumerated money-to-governance touchpoint (money may *fund judgments*) and the hinge from the three roles into the coupling theorem; the judgment ceiling of M-21 bounds what any panel can reach, so the surety box cannot become an instrument against the person.
*Substrate demand:* expressive contract layer (spend conditions referencing threshold signatures and beacon-derived panel identity); eUTXO isolation so surety boxes share no mutable state with anything else.
*Consumed by:* Role 3 (§4); §6.6(a); the property bundle.

**M-11 · Mutual credit with per-signature capped liability (non-fungible by construction).**
Money in the reference design is credit one member extends to another on their own signature, backed by their own reserve or standing — never a deposit an institution owes back. Each note carries its signer history as it circulates; any holder may redeem against any prior signer's reserve; liability is per-signature and capped by published formula, so no chain of endorsement compounds a small promise into an unbounded one. Because each unit carries its makers, weak credit cannot silently debase strong credit the way one issuer's inflation debases every holder of a fungible currency: bad paper is quarantined by its own signature. A public note protocol with exactly this shape ships on the chain today (gold-denominated, spender-signed notes; off-chain transfer, on-chain at redemption; issuable on trust or reserve-backed) and is citable externally. The design's conformance line: a thousand members each their own issuer conforms; one member becoming everyone's clearing desk — pooling others' reserves, issuing against balances held for others — does not.
*Defense core:* the anti-leverage property (mutual credit that cannot become a pyramid) and the anti-debasement property (non-fungibility as capture resistance) are both structural, not regulatory; and a "run" decomposes into many bounded bilateral redemptions because there is no pool to run on.
*Substrate demand:* contract support for accumulating-signature notes; oracle inputs as data (see M-14); the settlement layer of the two-layer money architecture.
*Consumed by:* Role 3 (§4); §6.5 (a reserve is value and nothing else); property cluster 5.1.
*Status:* protocol — shipped, external; the design's conformance profile — specified in this paper.

**M-12 · The six structural refusals (the not-a-bank floor).**
The reference design's clearing layer is defined by what it structurally cannot do: (1) no deposits or custody — nothing pooled sits in the middle to seize, freeze, or run; (2) no fractional reserve — issuance is backed per formula, never multiplied against deposits; (3) no central counterparty — clearing is peer-to-peer; the institution signs nothing and owes nothing; (4) no discretionary supply — only supermajority-amendable formulas govern issuance; no office holds a lever; (5) no seigniorage — the governing body earns nothing from money existing and is funded separately (M-16); (6) no unlicensed intermediation at the boundary with external regulated money — crossings are individual regulated acts at the edge, never a function of the core.
*Defense core:* a bank is dangerous because it custodies (seizable), multiplies (collapsible), and decides (capturable); the design refuses all three at the root by making every member their own issuer — the way to not be a bank is not to promise restraint but to hold nothing and decide nothing. This is "money inside the law as rules" stated as mechanism, and it is what makes the adjacency setting *specifiable* rather than aspirational.
*Substrate demand:* none beyond M-11; the refusals are design constraints the paper's certification-by-negative posture can test.
*Consumed by:* Role 3 (§4); §6.5 (money inside the law); the exile branch's corrective.

**M-13 · Issuance by formula, and the crisis valve written in peacetime.**
How much credit any member may issue, against what surety, at what caps, is fixed by published formula amendable only by slow supermajority — never by an office's judgment. The money supply emerges from members' own bounded issuance; there is no monetary authority because there is no monetary discretion for one to hold. Whatever emergency adjustment the system might ever need — e.g., a temporary widening of issuance in a genuine shortage — is defined in advance with its objective trigger, its bound, and its automatic expiry: a rule that fires itself, not a discretion someone seizes when everyone is afraid. Crisis machinery also pre-writes: the automatic stay on redemption cascades, the orderly workout of a failed issuer, and loss allocation among holders and sureties.
*Defense core:* the danger is the lever, not the hand on it — the historical rules-without-valves failure (a rule whose lawful exception was never written, so the exception was improvised by whoever held power the day it broke) is answered by writing the exception while everyone is still friends. This is the exile branch's direct corrective and the paper's answer to objection 7 ("the alternative is not no flexibility but flexibility written in peacetime, with trigger, bound, and expiry").
*Substrate demand:* none new; formulas live in contract or constitution-text, auditable.
*Consumed by:* §6.4 (exile corrective); Role 3; objection 7.

**M-14 · The price-feed-as-office rule, and the redemption-in-kind preference.**
Any instrument requiring an external price at redemption names its feed as an accountable office: published methodology, accountable holder, auditable record, amendable mandate — never an anonymous oracle whose capture silently mis-prices everyone's money. The strongest money needs no feed at all: commodity money redeemed in kind (metal for metal) references nothing external and cannot be de-pegged, front-run, or captured through a feed. The design prefers redemption-in-kind where possible and names the office where not, so every external dependency the money has carries a face and a published method — and every removable dependency is removed. The substrate's shipped oracle architecture fits the rule natively: oracle pools read as read-only data inputs — a scrutinized office, not a mutable authority contracts depend on.
*Defense core:* a hidden oracle is a hidden authority; the rule generalizes the paper's "offices not oracles" property and gives Role 3's adversarial pass (collateral capture via feed capture) its bounded answer — a captured feed mis-prices only the money that depends on it, and the dependency is visible.
*Substrate demand:* data-input oracle reads (shipped, external); contract-checkable feed identity.
*Consumed by:* Role 3 (§4); property cluster 5.5.

**M-15 · Standing-gated acceptance without a credit bureau.**
Whether to accept a note is the holder's pairwise decision, made on the *provable* standing of its signers: a selective-disclosure proof establishes "these signers are members in good standing whose reserves are attested," and the M-5 verification establishes the attestation is real — no registry queried, none existing to compile. Standing may gate acceptance; it may never be compiled into a cross-cell credit score, and the history of who accepted whose paper may never be assembled into a file. Acceptance is a present-tense judgment about specific signers for a specific note, proven and then discarded.
*Defense core:* "I can verify your issuers are sound" is permitted and "here is everyone's credit record" is structurally refused — the difference is the whole of financial privacy, and the mechanism shows gate-without-compile is buildable, which the paper needs against the objection that any credit system regrows the bureau.
*Substrate demand:* composable ZK predicates (prove standing above threshold without revealing the ledger behind it).
*Consumed by:* Role 3 (§4); the anti-compilation thread.

**M-16 · Treasury independence (the funded separation).**
The body that stewards the rules is funded so it cannot be bought by those it certifies or serves: broad dues on a published formula rather than any patron's renewable goodwill; a hard published cap on the fraction any single interested funder may contribute, enforced in aggregate across all paths; every gift above a de-minimis threshold on a public ledger; the rule-stewarding path and any certification-revenue path structurally separate; and rare expensive obligations paid from a reserve funded *in advance*, so the moment of need is never the moment of capture. The treasury itself is the most transparent object in the design — and holds no member deposits (M-12 binds it).
*Defense core:* a separation that cannot pay its own bills is a separation already for sale (the credit-rating-agency failure: elaborate methodologies, paid by the issuers they rated); capping the source and publishing the gift together make influence visible *and* survivable. Per the member's confirmation, treasury custody folds into Role 3's framing — surety and treasury are both "value spendable on attested conditions" — and this mechanism is the treasury half.
*Substrate demand:* public auditable books (the M-1/M-5 machinery applied to the body's own funds); nothing new.
*Consumed by:* Role 3 (§4, one paragraph); §6.6's "written, bounded, audited" standard applied to the body's own purse.

---

## 5 · Mechanisms for the Adjacency Setting (the coupling theorem's third branch)

**M-17 · Non-convertibility (money never buys franchise).**
The design's constitutional rule, proposed unamendable in the reference design: no mechanism shall ever exist by which money, token, or stake purchases franchise or franchise weight; the currencies of standing are non-convertible with each other and with wealth, permanently. Money may fund judgments (M-10), money may price attacks (the beacon's grinding economics), money may never buy voice. Connected, never convertible — two organs sharing one skeleton, not sharing blood.
*Defense core:* this is the substrate iff-claim's institutional half; the design refuses the *market* between wealth and voice rather than taxing it, which is the paper's entire disagreement with the exchange-rate school — a tariff on plutocracy versus an embargo. The enforcement is M-7 + M-19, not exhortation.
*Consumed by:* §6.5–§6.7; the title's third element.

**M-18 · The flat, non-scaling proposal bond.**
Agenda access is gated two ways: a seconding threshold from cells of independent lineage (a clique cannot self-second), and a modest *flat* bond that prices summoning the body — compelling others to deliberate at your petition — never speech (the discussion surface is open, to the public included) and never voting. The bond is refunded at a stated support floor, forfeited to the treasury below it, and is never scaled to the proposal's stakes or the proposer's wealth. The design records its own fence: the moment the bond scales with stakes or wealth it has become a toll booth on the floor and the design has failed its own non-convertibility rule. The lot (M-9) bypasses both gates by construction; bypassing them is its purpose.
*Defense core:* the second enumerated touchpoint (money prices *summoning*, flat, refundable) — the paper's demonstration that "enumerated, capped, audited contact" is a real design register, not a euphemism for leakage; the fence sentence is the testable commitment.
*Consumed by:* §6.6(b); objection 9.

**M-19 · No delegation primitive.**
The ballot exists only inside the census-window session; there is no proxy instrument in the design at all. Delegation whales — institutions accumulating signed-over franchise until the floor is a handful of custodians voting other people's ballots — have no mechanism to attack, because the instrument they would accumulate was never built. Combined with receipt-freeness (M-7), the market in franchise is closed on both sides: nothing to buy in bulk, and a purchased ballot undeliverable.
*Defense core:* liquid democracy's known terminal state is answered by absence, not regulation — the general theorem in action (make the un-intermediated path the only path, rather than forbidding intermediation and breeding its black market).
*Consumed by:* §6.7; objection 8.

**M-20 · The dual franchise and the gate-change escalation.**
Decision classes carry different electorates: constitutional text is decided by the universal electorate, one cell one vote, consent-binding (a treaty conference, not a legislature — the vote fixes canonical text; each cell adopts or diverges); technical content is decided by cells holding current certification against the specification touched — heads are the franchise where persons are at stake; running code is the franchise where code is at stake; neither converts to the other, nor to money. The anti-guild escalation: what lives *behind* a competence gate is voted by the gated electorate, but anything that *defines* the gate — raising the cost of becoming an implementer — escalates to the universal electorate. The guarded may not vote themselves the guardhouse.
*Defense core:* the classical iron-law objection (any competence-gated electorate votes the gate higher behind itself, converting meritocracy into aristocracy) is met structurally, and the taxonomy shows non-convertibility is a *system* of scoped franchises, not one rule.
*Consumed by:* §6.5's "outside the ballot as structure"; the Ostrom-audience bridge (polycentric electorates).

**M-21 · The judgment ceiling (bounded, prospective adjudication).**
No judgment may reach a person's standing already vested, bar their identity recovery, suspend the mutual-assistance floor, or be compiled into any cross-context record. A judgment finds facts and gates *future* association and credit — nothing else. Exit is always available under judgment, with keys, personhood, and vested history intact; the design's only native sanction is membership's future. Acts beyond an office's enumerated powers are void, not voidable — no ratification by usage.
*Defense core:* the courts are made safe not by trusting judges but by capping what any judge can reach — the worst outcome of even a captured panel is a bounded, appealable, exitable association-gate. This is what allows Role 3's enforcement to be real without the design ever holding power over persons, and it is why the surety box (M-10) cannot be weaponized.
*Consumed by:* Role 3 (§4); Role 2 (panel stakes bounded above — feeds the withhold-cost calculation's stakes side); the reference-design section.

**M-22 · Weight without a census (banded ZK cardinality).**
If collective decisions weight by community size at all, weight is proven as a zero-knowledge *band* — "at least N distinct session credentials attested" — never a roster. Proportionality without enumeration; the fallback if the predicate proves impractical is flat one-cell-one-vote, with a recorded prohibition on any implementer restoring proportionality by building the registry. Cell-hood itself is structural (one certified trust root, real co-presence edges), so splitting into paper cells to multiply franchise is expensive, visible, and legible as clique peripherality in the deposit topology.
*Defense core:* proportional weighting classically demands a count, and a queryable count is the registry every police state begins with; the banded predicate shows the dilemma is escapable — and honestly flags its own open feasibility question as exactly the kind of gap the paper's discipline names first.
*Substrate demand:* threshold/cardinality ZK predicates over attestations (the design's hardest open cryptographic ask; state as open).
*Consumed by:* Role 2 (§4); the anti-compilation thread; gaps-named-first posture.

---

## 6 · Cross-Cutting Mechanisms — Identity, Verification, Subsidiarity

**M-23 · The identity bundle and cross-cell recovery plurality.**
An identity is a signed, append-mostly bundle of attestations of plural provenance — self-signed, community-attested, optionally intersected one-way with external identifiers — addressed by an identifier the person controls, with no single root and no recovery authority above the self. Recovery of lost keys is by a quorum of attesters chosen in advance, and the recovery ceremony *structurally refuses* to complete from a quorum drawn entirely within one community: the mathematics verifies each share's cross-community provenance and produces nothing otherwise. A captured community is not punished for trying to hold a member's existence hostage; it is unable to succeed. Recovery preserves standing; identity death is not a sanction and cannot be engineered as one. External identifiers federate one-way: they intersect the identity and never root it — losing one removes a provable fact and touches nothing else.
*Defense core:* the threat is never the stranger stealing keys but the trusted community converting recovery into discipline; and the design enforces its limits by refusal-to-complete rather than forbid-and-audit — where a wrong can be made impossible instead of illegal, it is. The threshold cryptography here is also the paper's hidden-subset requirement: the chain records that k-of-n signed, never which k (contrast: script systems that reveal the executed path's signers).
*Substrate demand:* native threshold sigma proofs with signer-set hiding — a load-bearing element of the property bundle and a named comparative discriminator.
*Consumed by:* property clusters 5.2/5.5; the comparative placements; §7.1's Bitcoin concession-and-answer.

**M-24 · Selective disclosure, and the no-global-resolver rule.**
A member proves a specific predicate — "member in good standing," "attested by a qualified elder," "above the age of contract" — revealing nothing else in the bundle and no linkable identifier a counterparty could use to compile them across encounters. The member brings the proof to the counterparty; the counterparty never looks the member up, because the identifier system is *required* to have no global queryable directory — a resolver everyone can query is a registry everyone can compile. Demanding the full bundle for ordinary dealing is itself a design violation with a named remedy.
*Defense core:* minimal disclosure is enforceable only if the cryptography replaces the directory; the older constitutional tradition could only forbid unreasonable searches — this design makes them unanswerable. (The paper's surveillance-ratchet motivation cashes out here.)
*Substrate demand:* composable ZK predicate proofs (same primitive family as M-7/M-15/M-22 — the paper should present these as one requirement, instantiated four ways).
*Consumed by:* property bundle; Role 3 (via M-15); the equilibrium argument's stakes.

**M-25 · Mutual co-presence (the Sybil floor).**
The design's attestation primitive for embodied uniqueness: members of more than one community mutually mint and cross-sign presence proofs at a single gathering, anchored as content-free commitments, held encrypted in the member's own custody, and disclosed selectively. Solo device attestation is rejected on the record — a lone device attesting its own location is defeated by the modified app, the spoofed sensor, the surrogate device — so trust is moved from hardware to society: forging attendance is not a sensor hack but a cross-community conspiracy, and uniqueness is temporal (one census window) rather than biometric. Honest caveats stated: relay/wormhole attacks and commodity distance-bounding remain unsolved; the claim is "hard to forge at scale," never "unforgeable." Presence proofs must never compile into a movement graph — an edge is a relationship fact disclosed for one verification, never a location trace.
*Defense core:* this is the Sybil resistance the exchange-rate school's own proponents concede their mechanisms require — and the design possesses it *and still refuses* vote markets, which isolates the disagreement cleanly (§6.7's hinge). Root of trust is a co-signing member's staked, sanctionable credibility — social, not sensor.
*Prior art (Addendum A, adopted 22 July 2026; narrows the novelty claim):* simultaneous physical meetups with mutual attestation and temporal uniqueness are shipped prior art — the pseudonym-party lineage and a live proof-of-personhood system running global simultaneous ceremonies — and the paper cites them first and generously. What remains the design's own after that survey: cross-community plurality as the trust root (versus randomly assigned strangers plus a trusted-hardware enclave); the no-compilation architecture (no attendance registry, no region leakage, content-free anchors); the structural severance of presence from issuance (presence gates franchise and never mints money — the shipped system converts attendance into currency issuance at the primitive layer, the exact convertibility this design welds shut); and subsidiarity (presence rides gatherings that happen anyway). The device-attestation rejection cites the public secure-positioning and proof-of-location literatures on merits; the quarantine of §8.2 holds with room to spare.
*Substrate demand:* content-free anchoring (M-2/M-3); ZK selective disclosure (M-24).
*Consumed by:* §6.7 (load-bearing); Role 2's electorate integrity; quarantine note §8 (lineage not citable).

**M-26 · The exit invariant.**
Every mechanism above is tested against departure: a member can prove membership and standing to a stranger, exercise their keys, and leave — using only what they custody, with no one's permission, no fee, no exit interview, no hostage data, and nothing vested clawed back. Exit is the appeal of last resort against any judgment (withdrawn consent is the one ruling no panel overrules) and the design's deepest safety property: capture of the body is capture of nothing, because everything worth having walks out the door with the members.
*Defense core:* the design's answer to the iron law of oligarchy is not to out-organize it but to devalue its prize — enumerate in advance what the body may never hold, so the oligarchy that eventually administers it (one eventually will) administers something incapable of the historical harms. Exit is what makes every other guarantee testable: the certification posture throughout is *attempt the forbidden state and fail structurally*.
*Substrate demand:* technically unobstructed exit at the chain layer (no one-way bridges in load-bearing walls — the seam-architecture rule); member-custodied keys.
*Consumed by:* property cluster 5.8; the certification-by-negative posture; comparative placements (bridge critique).

**M-27 · Substrate subsidiarity (the chain only where strangers are).**
The reference design imports the ledger only where verification among strangers demands it, and refuses it where the room already trusts itself. Community-interior ballots run on paper or on a sealed, air-gapped tabulator whose entire security model is that anyone present can check it against the retained physical record — verified boot against a published hash, a tally engine small enough to read in an evening, no network stack at runtime, no per-entry attribution structurally possible, and the hand recount authoritative over the machine on any discrepancy. The machine's job is to be checked, not believed. Inter-community witness, mass tallies, sortition, and clearing ride the ledger, because there the counterparty is a stranger and the alternative to cryptographic verification is a trusted office. One further discipline recurs at every scale: attribution and anonymity never share a device — an instrument built to attribute every entry (books of record) is never repurposed for a function requiring the absence of attribution (ballots); hardware may be shared, function never.
*Defense core:* this is the paper's strongest anti-maximalism exhibit (objection 4): the design treats the chain as a scarce instrument for a specific trust problem, not a totem — the strongest possible posture from which to argue the chain is *necessary* where it is used. Boring is the security model.
*Consumed by:* the one-paragraph subsidiarity exhibit (per the outline's cut of the fuller cluster); objection 4; conclusion material.

---

## 7 · The Substrate Requirements Harvest

The property bundle of the paper's Part IV is *derived* from the mechanisms above, not asserted in parallel. The derivation table:

| Substrate property (the bundle) | Demanded by | Paper's comparative discriminator |
|---|---|---|
| Member-cost verification (ongoing, bootstrap, bounded state) | M-5 (all verification bottoms out in members checking the chain themselves); M-8 (offline-capable members) | The central thesis; the equilibrium argument |
| PoW stranger-verifiable time-ordering from genesis | M-2 (the ledger axis must have no coordinator); M-8 | Weak subjectivity is a coordinator (fusion prong 1) |
| Bounded state with rent | M-3 (anchors leave live state — non-enumerability assist); member-cost verification's durability | The wealth-at-rest sign argument (idle state pays the chain; idle stake pays the holder) |
| Composable native ZK proofs (threshold with hidden signer-set; predicate proofs; nullifiers; cardinality bands) | M-7, M-15, M-22, M-23, M-24, M-25 — one primitive family, five instantiations | Hidden-subset threshold vs. executed-path revelation; the bundle's sharpest single discriminator |
| Expressive contract layer, eUTXO-isolated | M-10 (surety spend conditions); M-11 (accumulating-signature notes); M-3 (unremarkable anchors) | Structural non-interference: the economy's boxes and the governance tooling's boxes share no mutable state |
| Verifiable public randomness with priced grinding | M-6, M-9 (and M-10's panel attestation depends on M-6's draw) | The withhold-cost floor; §6.6(c) |
| Data-input oracle reads (offices, not authorities) | M-14 | Scrutinized-office oracles as the substrate default |
| Scoped protocol governance | The whole design assumes the substrate governs its own parameters by its cost-bearers and nothing else with them | The fused/exiled/adjacent triad at the protocol layer |
| Technically unobstructed exit; no load-bearing one-way seams | M-26 | The seam-architecture rule |
| Wealth on the chain confers no weight over the chain | Every mechanism in §5; the design is unbuildable on a substrate that violates it | The iff-claim (§6.5) |

---

## 8 · Quarantine — what never enters the paper

Recorded so the boundary survives the folder reset. None of the following appears in paper-ready text, drafts, or citations:

1. **Corpus identifiers and internal doctrine names.** All COV-/ASP-/NET-/PPC-/OS-/MON- IDs; weld numbers and the apophasis framing; "Sovereign Cellular Accord," "Constitution 2.0," "the covenant" as a proper noun; stage machinery (Stage I–III); the certification-module lettering; internal mottoes and liturgical lore. The *ideas* enter as this paper's reference design; the *names* do not.
2. **The member's own prior writing.** Including the 2024 proof-of-presence forum post and its pseudonymous byline. M-25 states the device-attestation rejection on its technical merits with no lineage citation. No pseudonym, handle, or forum locus appears anywhere.
3. **Unsourced anecdotes.** The mesh-radio field-transmission story (third-party report, unsourced); the ~500-member congregation origin story behind M-27's tabulator. M-27's mechanism stands on its design logic alone.
4. **Persons.** No claims about any builder's or founder's interior motives (the paper's discipline rule); the tabulator's and the primitive's design histories enter as anonymous design rationale.
5. **Out-of-scope corpus machinery.** The AI-instrument clause and machine-act adoption rule (governs this project's process, not the paper's content); onboarding/preparedness hardware doctrine; continuity/succession and legal-wrapper planning; the arbitration seam's per-jurisdiction legal analysis and the fiat-boundary compliance doctrine (the paper may note in one sentence that external enforceability of collateral-backed awards and any fiat interface raise per-jurisdiction legal questions outside its scope — no more);
6. **Constants.** No corpus-internal constant candidates are presented as chosen values (§1 rule 5).
7. **The relationship between this paper and the two unpublished successor works.** The paper stands alone; no forward references to unpublished projects, by name or description.

---

## 9 · Consumption Map (mechanism → paper section)

| Paper section (per the settled outline) | Mechanisms consumed |
|---|---|
| Part I — thesis, equilibrium argument | M-2, M-5 (as the payoff of member-cost verification) |
| Reference-design précis (new early section) | M-1, M-2, M-23, M-26 as the skeleton; one-paragraph forward pointers to the rest |
| Part II Role 1 — neutral time | M-1 – M-5 |
| Part II Role 2 — randomness & collective decision | M-6 – M-9, M-22, M-25 |
| Part II Role 3 — surety, clearing, treasury | M-10 – M-16, M-21 |
| Part III §6.4 — exile branch corrective | M-12, M-13 |
| Part III §6.5–6.7 — adjacency, iff-claim, exchange-rate school | M-17 – M-20, M-7, M-19, M-25 |
| Part IV — property bundle (derived) | §7 harvest table |
| Part IV — comparative placements | M-23 (threshold discriminator), M-26 (seam rule), M-14 (oracle shape) |
| Part V — objections | Obj. 4 ← M-27; Obj. 7 ← M-13; Obj. 8 ← M-19/M-7/M-18; Obj. 9 ← M-10/M-18 + §6.6 framing |
| Conclusion | M-27 (subsidiarity coda material) |

---

## 10 · Provenance

Extracted 22 July 2026 from the full corpus then in the project folder: the constitutional head document, its five companions (identity, witness, tribunal, clearing, funding), the two aspirational governance exams (assembly machinery; negative space), and the cell-interior tabulator specification with founder notes. Extraction performed against the settled START-003 decisions of the same date: no self-citation; corpus as scaffolding; mechanisms internalized as this paper's reference design; treasury folded into Role 3 (member-confirmed). This digest is Claude-drafted and unadopted until signed by an accountable member; the adoption is the act. Upon adoption and folder reset, this document and START-003 constitute the project folder.
