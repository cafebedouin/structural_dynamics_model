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

## Evidence rule
Give ONE quoted phrase establishing the PRESENT ownership + kernel-amendment procedure (or its absence).
If you must reason from anything other than who-may-amend-the-kernel-now-and-how, mark unauthored + reason.

## Output
One row per item: `item_NN | value | "one quoted phrase" | one-sentence reason (name Q0/Q1/Q2)`.
Valid values: licensed_revisable, frozen, absent_diffuse, unauthored.

---

## item_01

Founding Problem: The postwar Article 9 settlement was built to permanently foreclose Japanese remilitarization after imperial aggression; the narrower inherent-right reading later addressed the practical need for territorial self-defense capacity, but by the 2010s policymakers argued that a rising security environment (North Korean missiles, Chinese naval expansion) created gaps that the narrow individual-self-defense-only reading could not address.
Coordination Function: Allows Japan to participate in collective security arrangements and respond to regional threats (e.g., to sea lanes, allied forces, or missile trajectories) that fall short of direct attack on Japanese territory, coordinating defense posture with allies without requiring Japan to wait for a first strike.
Transfer Function: Moves interpretive authority over the constitutional pacifism settlement from the amendment process (Article 96, requiring supermajority and referendum) to the executive's cabinet resolution power; moves military risk exposure from a purely territorial-defense SDF to personnel engaged in overseas joint operations; moves alliance burden-sharing costs from the US to Japan incrementally.
Absent Voices: The public was not offered a referendum on this specific constitutional question despite Article 96's amendment procedure existing precisely for such changes; large protest movements in 2015 registered objection but had no formal veto point. Neighboring states with historical grievances about Japanese militarism were not party to the domestic reinterpretation process at all.
Disappearance Verdict: world_rearranges
Disappearance Rationale: If the collective self-defense reading were reversed and Japan reverted to the narrower inherent-right posture, the 2015 Peace and Security Legislation would need to be repealed or radically narrowed, joint operational planning with the US and Australia would contract, and SDF mission scope would shrink back to territorial and individual self-defense — a substantial rearrangement of alliance commitments and defense procurement already underway.
Founding Problem Status: contested


## item_02

Founding Problem: Early typewriters jammed when adjacent-key strikes occurred in rapid succession; the layout was arranged partly to separate common letter pairs and slow the collision-prone striking pattern (the actual QWERTY design history is itself contested and less deliberately anti-speed than folklore holds).
Coordination Function: A shared keyboard layout solves a genuine coordination problem: every typist, manufacturer, typing-instruction system, and piece of software benefits from a single common standard rather than fragmentation across incompatible layouts.
Transfer Function: No deliberate transfer of value from one party to another occurs. What moves is a diffuse efficiency cost — foregone typing speed and ergonomic comfort — spread thinly across the entire population of typists, with no corresponding concentrated gain captured by any single actor.
Absent Voices: Alternative-layout advocates and ergonomics researchers are not silenced by any authority, but they lack a coordination mechanism to organize a mass switch; their technically-grounded objections circulate in niche communities without a lever to move the standard.
Disappearance Verdict: contested
Disappearance Rationale: If QWERTY vanished overnight and had to be re-selected from scratch, the coordination problem would resolve to whatever layout achieved critical mass fastest — plausibly QWERTY again, given how close current empirical estimates place the ergonomic gap. The lock-in reading holds the world would NOT dramatically improve if QWERTY disappeared (the switching cost of relearning would likely exceed captured gains for most individuals), but it WOULD rearrange in the sense that a coordination failure — persistence of a locally-suboptimal-but-not-clearly-inferior standard absent any mechanism for collective re-optimization — would be resolved rather than perpetually latent.
Founding Problem Status: dead


## item_03

Founding Problem: The interwar gold standard collapsed due to deflationary rigidity and chronic shortage of international liquidity; the Bretton Woods system was designed to combine the stability of a gold anchor with enough flexibility for sovereign macroeconomic management.
Coordination Function: Provides a nominal anchor and liquidity mechanism for international trade and finance by maintaining the dollar as a reserve currency with a conditional gold convertibility backstop, reducing transaction costs and exchange-rate uncertainty relative to a purely multipolar or metallic system.
Transfer Function: Transfers devaluation and suspension risk from the U.S. monetary authority to foreign dollar creditors, while transferring monetary policy autonomy and seigniorage benefits to the United States. The asymmetry intensifies as dollar liabilities outgrow gold coverage.
Absent Voices: Foreign commercial banks and private non-sovereign dollar holders are excluded from the convertibility conversation; their holdings lack even the conditional diplomatic leverage of central banks. Hard-money advocates demanding strict convertibility are marginalized in U.S. policy discourse.
Disappearance Verdict: world_rearranges
Disappearance Rationale: If the policy-flexible convertibility obligation vanished overnight, the dollar's role as a conditional reserve asset would collapse; foreign creditors would demand higher interest premia or abandon dollar holdings, international trade settlement would fragment into competing currency blocs, and the U.S. would lose the exorbitant privilege of seigniorage-financed deficits.
Founding Problem Status: dead


## item_04

Founding Problem: The Balfour Declaration and subsequent mandate text were built to reconcile a British wartime commitment to Zionist organizations with continued British control over Palestine's strategic position, while nominally preserving the civil and religious rights of the existing population.
Coordination Function: Coordinates the influx, settlement, and institutional self-organization of a specific immigrant population under a single legal-administrative umbrella, solving genuine problems of land registry, agricultural development, and municipal governance for that population.
Transfer Function: Moves land tenure, political representation capacity, and demographic weight from the existing Arab majority population to an incoming Jewish minority population, operating through immigration quotas, land transfer facilitation, and asymmetric grants of quasi-governmental standing.
Absent Voices: Palestinian Arab political leadership repeatedly petitioned for representative institutions proportional to population and for restriction of land transfers; their objections appear in the historical record (Shaw Commission, Peel Commission testimony) but were not treated as co-equal claims on the mandate's meaning under this reading.
Disappearance Verdict: world_rearranges
Disappearance Rationale: If this reading of the mandate instruments were withdrawn overnight, immigration facilitation and systematic land-transfer administration would lose their legal warrant, the Jewish Agency's quasi-governmental standing would require renegotiation from scratch, and Arab political leadership would regain a plausible claim to proportional representative institutions — the demographic and territorial trajectory of Mandatory Palestine would be structurally altered, not merely relabeled.
Founding Problem Status: contested


## item_05

Founding Problem: The 1951 Convention was built to prevent the return of people to death or persecution after states had already demonstrated, through the Holocaust and postwar displacement, that individual states could not be trusted to define 'refugee' narrowly enough to serve their own interests when lives were at stake.
Coordination Function: Coordinates a shared international standard for who must be protected from return to danger, so that protection does not depend solely on each state's unilateral and potentially self-serving definition of persecution.
Transfer Function: Moves the burden of protection and processing from countries of origin (which have failed or actively caused the harm) to destination states and international institutions, and moves adjudicative discretion from state immigration agencies to a body of evolving international doctrine and jurisprudence.
Absent Voices: Domestic electorates in destination states and legislatures that never explicitly voted to expand eligibility to generalized violence, gender, and non-state persecution claims are not parties to the treaty-interpretation process; restrictive_sovereignty_states object but operate at a doctrinal disadvantage in international fora and case law.
Disappearance Verdict: world_rearranges
Disappearance Rationale: If this reading were displaced by the restrictive_sovereignty_reading, a substantial share of currently-protected claimants — those fleeing generalized violence, non-state persecution, and social-group-based harms not tied to state action — would lose eligibility overnight; interdiction and offshore processing regimes currently contested as refoulement would become permissible; the asylum system's caseload composition and adjudicative standards would shift materially.
Founding Problem Status: contested


## item_06

Founding Problem: In a territory without centralized administrative capacity to levy troops, collect revenue, or adjudicate disputes directly, some mechanism was needed to let a lord raise a reliable, predictable military and administrative capacity from landholders without renegotiating terms with each individually and continuously, while giving those landholders enough certainty about the ceiling on demands to invest in the land and pass it to heirs.
Coordination Function: Converts an otherwise unbounded, renegotiable relationship of protection-for-service into a fixed, mutually legible schedule of obligations — a specific number of knight's-fee service days, specified aids on specified occasions, specified counsel duties — recorded or attested in charter form so that neither party can unilaterally escalate demands without breaching a text both can invoke.
Transfer Function: Land, jurisdiction, and protection flow from lord to vassal; military service, counsel, and specified aids flow from vassal to lord — both flows are capped by the charter's enumerated terms rather than open to continuous renegotiation.
Absent Voices: The unlanded peasantry working the vassal's land are not party to the oath and have no forum to contest obligations passed down to them; the sibling ecclesiastical_mediation_reading would also note the Church's independent claim to bound the relationship by sacramental obligation, a claim this reading treats as external to the charter mechanism itself.
Disappearance Verdict: world_rearranges
Disappearance Rationale: If the fixed, charter-enforced schedule vanished and reverted to open-ended personal fealty with no textual ceiling, both lords and vassals would lose the predictability that lets them plan military musters, successions, and inheritances years in advance; peer courts would lose their reference standard for adjudicating disputes, and the region would revert to case-by-case negotiation backed only by relative force — a materially different and less stable arrangement.
Founding Problem Status: dead


## item_07

Founding Problem: The interwar collapse of fixed exchange rates, competitive devaluation, and capital flight crises (1930s) that deepened the Depression and were widely diagnosed as caused by unregulated hot-money flows and beggar-thy-neighbor currency policy.
Coordination Function: Solves a genuine coordination problem: without some shared convertibility norm, competitive currency manipulation and capital flight crises recur, and international trade and investment require confidence that currency can be exchanged and repatriated.
Transfer Function: Moves policy autonomy and adjustment burden from capital to states, and within states from capital owners to labor and domestic constituencies who cannot exit; moves stability and return-on-capital benefits to internationally mobile finance and to the reserve-currency issuer.
Absent Voices: The IMF's original framers who intended capital controls as a permanent feature (not an emergency exception) are read out of the institution's later self-understanding. Developing states and organized labor movements affected by conditionality have no formal seat in the interpretive process that hardened convertibility into the operative norm.
Disappearance Verdict: world_rearranges
Disappearance Rationale: If the convertibility-discipline reading of Bretton Woods institutions vanished overnight, states would regain formally uncontested legitimacy to deploy capital controls without IMF sanction risk; capital would face repricing for policy-autonomy risk it currently does not bear; the seigniorage advantage of the reserve currency issuer would face renewed challenge; adjustment burdens could shift back toward capital rather than resting on domestic labor and fiscal policy.
Founding Problem Status: contested


## item_08

Founding Problem: The New Deal-era doctrine was built to prevent a formalist 'production vs. commerce' distinction from disabling federal response to an integrated national economy in crisis — Congress needed to reach conduct that was locally structured but nationally consequential (wage deflation, agricultural overproduction, labor unrest with interstate spillover).
Coordination Function: Enables uniform national policy on matters where local economic decisions, taken individually, are trivial but collectively determine national market conditions (commodity prices, labor standards, environmental externalities) — solving a genuine collective-action problem that fifty independent state regimes cannot solve alone.
Transfer Function: Moves regulatory authority and enforcement discretion from state legislatures and local actors to federal agencies and national interest coalitions; moves compliance costs from a negotiated state-level baseline to a federally set floor or ceiling that individual local actors did not vote on and cannot exit.
Absent Voices: Individual intrastate producers whose conduct is aggregated into a national statistic never appear as parties — they are represented, if at all, by trade associations with their own agendas. The abstract federalism interest (diversity of policy as a check on error) has no institutional advocate; it appears only in dissenting opinions and academic commentary.
Disappearance Verdict: world_rearranges
Disappearance Rationale: If the aggregation/substantial-effects doctrine were narrowed to channels-and-instrumentalities only, wide swaths of federal labor, environmental, healthcare, and civil-rights legislation would lose their jurisdictional basis overnight, forcing either constitutional amendment, a patchwork of fifty state regimes, or a scramble to re-ground existing statutes in alternative enumerated powers (taxing/spending, treaty power).
Founding Problem Status: contested


## item_09

Founding Problem: Early Christian communities faced genuine interpretive chaos and competing claims to authentic apostolic teaching; a mechanism was needed to distinguish authentic transmission of apostolic faith from heterodox innovation and to preserve unity against schism.
Coordination Function: Provides a single, stable, centrally adjudicated reading of scripture across a vast and doctrinally diverse population, preventing the fragmentation that arises when many independent interpreters claim equal authority — genuinely useful for maintaining unity in belief and practice across languages, regions, and centuries.
Transfer Function: Moves interpretive authority and sacramental gatekeeping from individual believers to an ordained hierarchy; in material terms, moves tithes, sacramental fees, and institutional deference toward clergy and curial offices in exchange for mediated access to grace and doctrine.
Absent Voices: Vernacular reform advocates, dissenting theologians, and lay readers who might argue scripture is sufficiently clear for direct engagement are structurally positioned outside the adjudicating body; their objections historically surface as heresy trials or schisms rather than as votes within the magisterium itself.
Disappearance Verdict: world_rearranges
Disappearance Rationale: If magisterial authority over interpretation vanished, lay access to vernacular scripture and independent theological reasoning would expand rapidly, sacramental mediation would lose its exclusive claim on grace, clerical economic and social structures dependent on interpretive gatekeeping would restructure substantially, and doctrinal plurality would likely increase sharply — much as occurred historically wherever this authority structure weakened or was rejected.
Founding Problem Status: contested


## item_10

Founding Problem: State execution was historically built to provide a maximal, final sanction for the most severe crimes, understood as necessary for public order, retribution, and deterrence where lesser punishments were seen as insufficient.
Coordination Function: None recognized on this reading. Where retributive and deterrence readings claim execution coordinates a collective demand for proportionate justice or a public-safety deterrent effect, the abolition reading holds there is no coordination problem that execution — as opposed to permanent incarceration — actually solves; whatever public-safety or moral-closure function exists is achievable by non-lethal means.
Transfer Function: The arrangement transfers life itself from the condemned (guilty and wrongfully convicted alike) to the state's exercise of authority, with no compensating flow back to any legitimate party — no restored victim, no measurably deterred future crime attributable to execution specifically versus incarceration.
Absent Voices: Wrongfully convicted persons who were executed before exoneration evidence emerged cannot testify to the apparatus's error rate. Victims'-rights advocates who reject execution as inadequate or beside the point of their own healing are marginalized within a public discourse that frames execution as inherently victim-serving.
Disappearance Verdict: world_rearranges
Disappearance Rationale: If state execution authority disappeared overnight, capital sentencing dockets would convert to maximum-incarceration outcomes, death-row populations would be resentenced, and the entire evidentiary and procedural machinery built around capital cases (bifurcated trials, special appellate tracks, execution protocols) would be dismantled or repurposed — a substantial institutional rearrangement, not a null change.
Founding Problem Status: dead


## item_11

Founding Problem: Allied states needed a legal and moral basis to compel compensation for extensive war damage from Germany, and to coordinate collection among multiple claimant nations rather than negotiate separately.
Coordination Function: Provides Allied creditor states a single treaty-grounded mechanism to collect compensation for war destruction and to coordinate collection among multiple claimant states without each negotiating bilaterally with Germany.
Transfer Function: Moves wealth — cash, coal, industrial capital, labor obligations — from German taxpayers, workers, and currency holders to French and Belgian state treasuries and reconstruction contractors, under a liability schedule set and revised by the Reparations Commission.
Absent Voices: German workers and taxpayers who bear the transfer have no seat on the Commission. International bondholders whose war-debt claims are entangled with reparations flows are not party to the schedule-setting. Economists arguing for a capacity-bounded settlement (Keynes chief among them) published outside the treaty process with no binding effect on it.
Disappearance Verdict: world_rearranges
Disappearance Rationale: If the punitive liability grounding were removed, the entire schedule of open-ended assessments, the occupation sanctions used to enforce them, and the war-guilt clause's role in domestic German politics would collapse; French and Belgian reconstruction financing would have to be renegotiated on a capacity basis, and the political fuel Article 231 supplied to German revanchism would be substantially altered.
Founding Problem Status: contested


## item_12

Founding Problem: The early Christian community faced diverse interpretations of apostolic teachings and writings, leading to theological disputes and the potential for schism. The constraint was established to preserve unity and orthodoxy by centralizing interpretive authority.
Coordination Function: Provides a single, unified, and stable interpretation of divine revelation (Scripture and Tradition) across centuries and diverse cultures, preventing doctrinal fragmentation and ensuring continuity of faith.
Transfer Function: Transfers interpretive authority and the power to define truth from individual believers and independent theologians to the centralized magisterium, in exchange for doctrinal certainty and institutional stability.
Absent Voices: Protestant theologians and independent biblical scholars, who would argue for the sufficiency of Scripture alone or for a more decentralized, community-based interpretive authority. They are excluded by the very definition of the magisterium's exclusive role.
Disappearance Verdict: world_rearranges
Disappearance Rationale: If the magisterium's exclusive interpretive authority vanished, the Church would likely experience immediate doctrinal fragmentation, diverse theological schools would emerge, and the institutional hierarchy's power would significantly diminish, leading to a profound reorganization of its structure and function.
Founding Problem Status: live


## item_13

Founding Problem: Post-war reconstruction required a stable, credible international payments system to avoid the competitive devaluations and trade collapse of the 1930s; convertibility to gold at a fixed dollar price was meant to anchor confidence in the system without requiring a literal gold standard for every currency.
Coordination Function: Provided a nominal anchor for post-war exchange rates, letting trading partners price currencies against a gold-backed dollar rather than negotiate bilateral pegs from scratch — genuine coordination value in the early Bretton Woods years.
Transfer Function: Moves monetary policy autonomy and adjustment costs from the United States to dollar-holding foreign central banks: the U.S. retains freedom to run domestic policy as it sees fit, and when that policy conflicts with the gold peg, the resulting devaluation risk and reserve losses land on those holding dollar claims rather than on U.S. domestic constituencies.
Absent Voices: Ordinary savers and firms in dollar-reserve countries whose national reserves lose value are never party to the U.S. domestic policy debates that determine whether convertibility will be honored; they experience the consequences of decisions made entirely inside U.S. institutions.
Disappearance Verdict: world_rearranges
Disappearance Rationale: The August 1971 suspension is precisely this disappearance event: when the conditional character of the obligation was finally exercised in full, the entire post-war exchange rate architecture reorganized into floating rates within two years (Smithsonian Agreement failing by 1973), central banks restructured reserve management, and the IMF's Article IV was rewritten to reflect a non-gold-backed system.
Founding Problem Status: dead


## item_14

Founding Problem: Archaic Sparta faced land disputes and stasis risk among its warrior elite (Second Messenian War era); the rhetra attributed to Lycurgus fixed land allotment, military training (agoge), and communal messes (syssitia) as an unchangeable constitutional settlement to prevent factional collapse and guarantee a permanent hoplite class.
Coordination Function: The original kleros allotment and rhetra system solved a genuine problem: preventing land concentration and internal factionalism among the Spartiate warrior class by guaranteeing every citizen-soldier a subsistence estate worked by helots, freeing him for military training. This coordinated a stable, materially-equal hoplite citizenry.
Transfer Function: Over time the arrangement's rigidity transferred political status away from Spartiates whose kleroi failed (through debt, partition, or battlefield loss of heirs) toward the shrinking core who retained land, and transferred survival risk from the present incumbent generation onto the future polity, which inherited a hollowed-out citizen army it could not reconstitute.
Absent Voices: Female heirs holding accumulating land, hypomeiones stripped of citizenship, and above all the future citizen-generations who would need the reform are structurally absent from the assembly that could revise the rhetra — by the time the shortfall was undeniable (post-Leuctra, 371 BCE), reformers like Agis IV faced land-holding elites for whom the frozen system was still, individually, advantageous.
Disappearance Verdict: world_rearranges
Disappearance Rationale: Had the unrevisability norm not held — had kleros reallocation, debt cancellation, or citizenship broadening been legally available reform paths as pressures mounted in the 5th-4th centuries BCE — the Spartiate citizen body's contraction from roughly 8,000-10,000 (479 BCE) to under 1,000 (371 BCE) was not the only possible outcome; the polity's military and political capacity, and its eventual eclipse by Thebes, plausibly follow a materially different path.
Founding Problem Status: dead


## item_15

Founding Problem: The early movement needed a theological answer to accusations of licentiousness surrounding unofficial plural relationships already occurring among leadership, and needed to establish a durable kinship/priesthood order that could outlast any single leader's authority — D&C 132 supplied a textual, revelatory anchor claimed to resolve both at once.
Coordination Function: Provides a unified, textually anchored answer to competing revelatory claims within a young and factionalizing movement, and coordinates a patriarchal kinship and inheritance structure around a single authoritative sealing hierarchy rather than ad hoc arrangements.
Transfer Function: Moves reproductive capacity, domestic labor, family loyalty, and theological standing from plural wives and their families to existing husbands and the senior hierarchy that administers sealing authority; moves guaranteed access to the highest eternal reward away from monogamous members and toward those admitted into plural unions.
Absent Voices: Plural wives, pledged daughters, and dissenting male members would object to the doctrine's immutable status if given a forum with real institutional standing, but the interpretive authority to declare or revise the doctrine rests entirely with the senior male hierarchy; federal authorities press from outside but are excluded from the doctrine's own legitimacy criteria entirely.
Disappearance Verdict: world_rearranges
Disappearance Rationale: If the immutable-commandment reading vanished overnight, the entire theological architecture predicated on plural marriage as a precondition for the highest exaltation would collapse: existing plural households would lose their doctrinal warrant, monogamous members would no longer face a structural exaltation ceiling, and the sealing hierarchy's authority to administer this specific practice would dissolve along with it — which is precisely what happened when the prophetic_override_reading and temporal_accommodation_reading displaced this reading's practical force after 1890.
Founding Problem Status: dead


## item_16

Founding Problem: Post-war monetary disorder, competitive devaluations, and the collapse of international trade and investment in the 1930s required a cooperative framework to stabilize exchange rates and rebuild liquidity.
Coordination Function: Establishes a global monetary order with currency convertibility and reduced capital barriers, ostensibly to lower transaction costs, enable cross-border price discovery, and channel savings toward productive investment.
Transfer Function: Moves fiscal and monetary policy autonomy from national governments to international financial markets and multilateral institutions, converting domestic policy space into market access rights for mobile capital.
Absent Voices: Domestic populations in debtor nations, Keynesian economists defending capital controls, and import-substituting industrialists are structurally excluded; their policy preferences are coded as violations of convertibility rather than legitimate alternatives.
Disappearance Verdict: world_rearranges
Disappearance Rationale: If the constraint vanished, debtor nations would reimpose capital controls, exchange rates would disconnect from market-liberalization benchmarks, the IMF would lose conditionality leverage, and global capital flows would fragment into regional or national circuits â the post-war monetary architecture would reorganize around policy autonomy rather than convertibility.
Founding Problem Status: dead
