# Sovereign AI for Law Firms: The Real Problem, the Transitional Solution, and the Two Decisions That Set Its Lifespan

You asked me to be skeptical, so I'm going to be — but I want to be skeptical about the right thing. The technical spec is the strongest part of this and the part you least need me for; it reads like someone who has actually built the thing, not specced it on a whiteboard. The argument below isn't about whether the box works. It's about what the box is *for*, who will pay for it, and how long the reason they pay lasts. That's the part a friend with no stake should pressure-test before you point this at a paying market.

## The instinct is correct, and you're early

The problem is real and on the record. In February 2026, Judge Rakoff in the Southern District of New York (*United States v. Heppner*) held that a defendant's written exchanges with a public, consumer-grade AI tool were not protected by attorney-client privilege or work-product doctrine — the AI is a third party, and disclosure to it can waive protection. A firm that drops privileged matter into a consumer chatbot may be manufacturing discoverable evidence against its own client. That is no longer a hypothetical risk to wave at; it is a published holding.

Most of the market's answer to that anxiety is "trust our cloud, we don't train on your data." That answer is exactly the one *Heppner* makes a cautious litigator nervous about, because it still routes privileged material through a third party. Building something the firm physically controls is a correct read of where the fear actually lives. You have paying clients and a lawyer in the household telling you the fear is real. Believe that signal. The instinct is right, and being early to it is an asset.

So nothing below is "this is a bad idea." It's "here is the shape of the idea that survives, and here is the one that doesn't."

## The word "sovereignty" is promising more than the architecture can hold

Here's the load-bearing concern, and it has nothing to do with intent. "Sovereignty" sells the value of this product as something *permanent and principled* — data control as an end in itself, a foundation you build on for decades. But the thing customers will actually pay a premium for is *transitional*: a bridge across a specific, closing window of legal and technical uncertainty. When the word and the value diverge like that, the market eventually notices, and the gap becomes the liability. Two clocks are running on the "permanent" framing.

**Clock one — the technical necessity is eroding.** The reason physical sovereignty feels mandatory today is that "the provider can't see your data" currently requires "the hardware is in our building." That equivalence is dissolving. Confidential computing — hardware enclaves and remote attestation (AWS Nitro Enclaves, Azure confidential VMs, and the broader trusted-execution ecosystem) — is shipping now and moves "the provider can't read your data" from a physical fact about a closet to a cryptographic guarantee about a chip. A math guarantee doesn't need an integrator, a monthly drive rotation, or an on-site service event. As that matures, "we keep it in your building" stops being *the* answer and becomes *an expensive* answer.

**Clock two — you're horizontal in a market organized vertically.** Lawyers don't adopt infrastructure; they adopt features inside tools they already use. The incumbents already own the workflow and the documents: the legal-research duopoly (Thomson Reuters bought Casetext specifically to own AI-assisted research; Westlaw and Lexis are racing the same direction), and the e-discovery stack that already ingests, classifies, and *holds* the privileged corpus because it has to. Your own spec contains the tell — it states it is "additive, not disruptive," that it does not replace email, billing, or calendar. A product that routes alongside everything and lives inside nothing is in the hardest possible position to plug into a legal workflow. When I ask "how does this plug into how a firm actually works day to day," the honest answer is that it mostly doesn't, and whether that's fatal or strategic depends entirely on the next two sections.

## The strongest version of your case — which I think you should lean into

Give the optimistic reading its best form, because two pieces of it survive both clocks.

First, **legibility is a moat the technology curve does not touch.** "The server is in our building" is explicable to a malpractice-averse partner, a skeptical judge, an ethics board, and a frightened client in a way that homomorphic encryption and remote attestation will *never* be. The math can win on the merits and still lose in the conference room. A security story a non-technical decision-maker can understand and defend can outlive the technical necessity of that story by a decade.

Second — and this is your real edge, using your own observation — **the giants won't fully enter because nobody large wants to hold other people's privileged records.** That's a balance-sheet liability the incumbents will structure *around*, not *into*. The very thing that makes this a poor venture-scale company (it doesn't generalize, the records are radioactive) is the thing that keeps Microsoft and Thomson Reuters out of the niche. A defensible business is hiding in that asymmetry: sell boxes the firm owns and operates, so the *firm* holds the records and the liability, and let "nobody big wants this" be the moat.

That is a different company than the one I think you're currently excited about, which brings us to the decision.

## Decision one: are you a box or a service? (This one decides whether your core claim is even true.)

This is the fork everything hinges on, and I don't think it's been made explicitly.

If you sell **a box the firm owns and runs**, the privilege story holds: no third party touches the data, so there's nothing for a court to find a waiver in. The "no outside access" claim is true.

The moment you operate this as **software-as-a-service** — recurring revenue, you managing it, you with operational access — *you become the third party*, and you have reintroduced the exact *Heppner* problem this product exists to eliminate. The recurring-revenue instinct every founder has points straight at the version of the business that invalidates its own value proposition.

I don't think you've seen that the SaaS economics and the privilege claim are in direct tension. Name it before an investor or a customer's GC names it for you. (What would change this: a sufficiently airtight architecture plus a Business Associate / privilege-preserving agreement structure where you genuinely never have access — but that's a legal-engineering problem to solve deliberately, not a default to drift into.)

## Decision two: the lead fix — your audit trail is a discovery target you built on purpose

This is the most concrete and most receivable item, and it's the one to put in front of your wife first.

The spec's immutable, attributed, un-deletable audit trail is sold as privilege *protection*. To a litigator it reads as the opposite: a permanent record the firm manufactured about itself and is contractually unable to purge. A per-query log of which associate asked the machine what — never reviewed by which partner — is a malpractice-and-competence exposure machine in any future fee dispute, malpractice claim, or under-service allegation. The "e" in email is for "evidence"; the more you document to *prove* privilege, the more discoverable surface you create. The product's central premise (immutability equals trust) runs directly against a litigator's bone-deep instinct that the safest document is the one a legitimate retention policy already destroyed. Your threat model is entirely outward-facing — ransomware, cloud compromise, the integrator. It has no row for *the trail itself becoming adverse evidence from the inside.* That's a gift dressed as a fix: it's specific, it's fixable (retention policy, partner-review gates, scoped logging), and finding it makes you look careful rather than naive.

## Dependencies, if you scale beyond your current clients

Briefly, because these are scoping decisions, not flaws: if you move toward medical (PI, med-mal, workers' comp), the records live in Epic or Cerner, and "works with the EHR of record" becomes a hard dependency, not a nice-to-have — otherwise carve an explicit niche and say no to the rest. And the segment most likely to pay for owned hardware is the paranoid end of the distribution: criminal defense, IP, high-net-worth, small firms too small for the incumbents to court. That's a real market; it is probably not a venture-scale one. Decide which you're optimizing for.

---

## Evidence framework (so you can see which claims are solid and which are bets)

**Documented (Tier 1):**
- *Heppner* holding and its third-party-waiver logic — SDNY, Feb. 2026, Judge Rakoff (verify exact cite before quoting: oral ruling ~Feb. 10, written ~Feb. 17, No. 25-cr-00503-JSR).
- Confidential-computing / trusted-execution products exist and are shipping today (Nitro Enclaves, Azure confidential VMs).
- Thomson Reuters' acquisition of Casetext to own AI-assisted legal research (2023 — verify figure before citing).
- The spec's own "additive, not disruptive," "integrator-maintained," and "non-functional without Keystone" language; and the absence of any SaaS/operational-access or audit-retention provision.

**Reasonable inferences (Tier 2):**
- A horizontal infrastructure product faces structurally harder workflow adoption than a feature inside an incumbent tool. (Follows from how legal tools are adopted + the spec's own positioning.)
- Operating as SaaS reintroduces a third party and therefore the *Heppner* waiver risk. (Follows from the holding's logic applied to the operating model.)
- The immutable audit trail is a discovery and malpractice surface, not only a protection. (Follows from standard discovery practice.)

**Structural hypotheses requiring more evidence (Tier 3):**
- *The product has a limited lifecycle.* This is a prediction, not a fact. **It is true for the venture-scale, SaaS, sovereignty-as-permanent-foundation version. It may be false for an owned-hardware niche tool**, where the legibility moat and the liability moat can sustain it long after the technical necessity fades. **What would falsify the pessimistic read:** if the legibility-to-decision-makers advantage proves durable, or if you become the de facto records-of-record standard in a niche before incumbents move, the "obsolescence" thesis fails and this is simply a stable small business.

## Unresolved questions worth resolving before you scale

- At what point does owned-hardware sovereignty stop being legible-enough-to-be-preferred over a cheaper attested-cloud guarantee? (This is the real lifespan clock; it's empirical and watchable.)
- Does an immutable internal AI-research trail have precedent for being turned adverse in a fee or malpractice dispute? (Ask a litigator; this sizes the audit-trail risk.)
- Priced against solo/small-firm revenue, where does the standard-of-care ratchet — once "you *could* have kept this in-house" becomes a competence expectation — actually start cutting, and for whom? (This tells you who your buyer is and isn't.)

---

## METADATA — for you, not for him

**Mode:** B (invisible scaffolding). No engine vocabulary in the draft above; every translated finding is anchored to an independent fact in the Evidence Framework. The one place to double-check me: the Tier 1 items marked "verify" are from memory, not a fresh source pull — confirm the Casetext figure and the exact *Heppner* cite before this leaves your hands.

**Engine-finding → section map (so you can diff the draft against what the corpus actually said):**
- integrator/operational-dependency reading → "box vs. SaaS" fork + dependencies section.
- the privilege-architecture-as-mixed-coordination reading → "the word sovereignty is promising more than the architecture can hold."
- sovereignty-cost-premium GREEN, with watch-level theater drift → why I did *not* call the premium itself extractive; it's framed as genuine cost in a closing window, which matches the clean classification, not a hardened one.
- the audit-trail / paper-trail finding (new in our discussion) → "decision two," led because it's the most receivable.

**Confidence gradient (tracks the report purity, not cited):** strong language on the documented problem and the box/SaaS tension; deliberately cautious language on lifecycle (Tier 3) because that's the boundary case and the claim of mine most prone to seat-drift.

**Where I may still be distorting, since you have no reviewer to diff me against here:** I softened "cover story" into "the word is doing more work than the architecture can hold." That may be the palatability move you've been catching — it's gentler than the engine's framing. If you think the harder version is the true one, restore it; I chose receivability over force on the judgment that a founder rejects an accusation and accepts a clock. That's a seat-choice, not a finding, and it's yours to overrule.

**Adversarial pass — weakest link:** the lifecycle prediction. A smart founder will say "legibility and liability protect me indefinitely," and on the niche-tool version he's right. The draft concedes this on purpose so the concession can't be used to discredit the parts that don't depend on it (the SaaS tension and the audit-trail fix stand on their own regardless of how the lifespan question resolves).
