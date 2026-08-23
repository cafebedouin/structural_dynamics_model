% ============================================================================
% CONSTRAINT STORY: zero_as_number_entry__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_as_number_entry__hybrid_scaffolding_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: zero_as_number_entry__hybrid_scaffolding_reading
 *   human_readable: Zero-as-Number Entry — Hybrid Scaffolding Reading
 *   domain: history/philosophy of mathematics / conceptual history
 *
 * SUMMARY:
 *   The standing arrangement under evaluation is a convention, not an
 *   artifact: the rule-set admitting an empty-place marker into the number
 *   system as a quantity with defined behavior (additive identity,
 *   multiplicative annihilator, undefined or special-cased divisor). On the
 *   hybrid scaffolding reading, that rule-set was latent in positional
 *   notation from its first use — any sufficiently long positional system
 *   confronts empty columns — but became operationally thinkable only where
 *   existing conceptual vocabulary could host it. Sanskrit mathematical
 *   lineages, working where shunya (the void) was already a lived
 *   metaphysical category, converted placeholder into number early and wrote
 *   its arithmetic down (Brahmagupta, 628 CE). Baghdad's translation and
 *   administrative circles industrialized the result into algorithms and
 *   carried it west. Latin Europe encountered the structure twice: as
 *   Babylonian sexagesimal tables its astronomers copied for centuries
 *   without ever promoting the empty slot, and later as Italian commercial
 *   arithmetic that triggered recognition rather than delivering a novelty.
 *   Traditions whose scaffolding could not host an operable nothing —
 *   magnitude-based geometry and Aristotelian category discipline — paid for
 *   the convention's success in obsolescence and doctrinal strain. KEY AGENTS
 *   (by structural relationship): - hindu_mathematical_schools: primary
 *   beneficiary with codifying role (organized/mobile) -
 *   islamic_transmission_centers: secondary beneficiary and industrializer
 *   (institutional/mobile) - italian_merchant_reckoners: adopter beneficiary
 *   with transitional cost exposure (organized/mobile) -
 *   greek_geometric_lineage: primary cost-bearing tradition
 *   (institutional/constrained) - aristotelian_scholastics: cost-bearing
 *   institutional seat (institutional/identity_locked) -
 *   babylonian_placeholder_scribes: excluded evidentiary seat
 *   (powerless/trapped) - historians_of_mathematics: analytical observer
 *   (analytical/global)
 *
 * KEY AGENTS:
 *   - - hindu_mathematical_schools: primary beneficiary with agenda-setting codification (organized/mobile) — hosted the latent structure in existing void-vocabulary and wrote the operative rules
 *   - - islamic_transmission_centers: secondary beneficiary and industrializer (institutional/mobile) — algorithmized the convention and carried it across three continents
 *   - - italian_merchant_reckoners: adopter beneficiary bearing transitional enforcement friction (organized/mobile) — gained double-entry arithmetic, paid fraud-suspicion bans
 *   - - greek_geometric_lineage: primary cost-bearing tradition (institutional/constrained) — magnitude-based method left no operational slot for an empty quantity; declined into applied status
 *   - - aristotelian_scholastics: cost-bearing institutional seat (institutional/identity_locked) — category commitments made the convention thinkable only at the price of framework abandonment
 *   - - babylonian_placeholder_scribes: excluded evidentiary seat (powerless/trapped) — held the latent structure for roughly two millennia without recognition; demonstrates that structure alone forces no entry
 *   - - historians_of_mathematics: analytical observer (analytical/global) — sees all seats simultaneously; adjudicates mechanism claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__hybrid_scaffolding_reading, 0.38).
domain_priors:suppression_score(zero_as_number_entry__hybrid_scaffolding_reading, 0.3).
domain_priors:theater_ratio(zero_as_number_entry__hybrid_scaffolding_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__hybrid_scaffolding_reading, rope).
narrative_ontology:human_readable(zero_as_number_entry__hybrid_scaffolding_reading, "Zero-as-Number Entry — Hybrid Scaffolding Reading").
narrative_ontology:topic_domain(zero_as_number_entry__hybrid_scaffolding_reading, "history/philosophy of mathematics / conceptual history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__hybrid_scaffolding_reading, 'a0d369ef-9cfb-484d-930b-6bf584cfd4e9').
narrative_ontology:cs_kernel_codification('a0d369ef-9cfb-484d-930b-6bf584cfd4e9', distributed).
narrative_ontology:cs_authority_grounding('a0d369ef-9cfb-484d-930b-6bf584cfd4e9', expertise).
narrative_ontology:cs_interpretation_layer_present('a0d369ef-9cfb-484d-930b-6bf584cfd4e9').
narrative_ontology:cs_reading_relation('a0d369ef-9cfb-484d-930b-6bf584cfd4e9', zero_as_number_entry__contingent_thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0d369ef-9cfb-484d-930b-6bf584cfd4e9', zero_as_number_entry__universal_discovery_reading, influences).
narrative_ontology:cs_axiom('a0d369ef-9cfb-484d-930b-6bf584cfd4e9', foundational, operational_thinkability_requires_compatible_scaffolding).
narrative_ontology:cs_axiom_status(operational_thinkability_requires_compatible_scaffolding, holdable).
narrative_ontology:cs_axiom_grounding('a0d369ef-9cfb-484d-930b-6bf584cfd4e9', operational_thinkability_requires_compatible_scaffolding, empirically_contingent).
narrative_ontology:cs_axiom('a0d369ef-9cfb-484d-930b-6bf584cfd4e9', foundational, contact_triggers_latent_recognition_not_concept_transfer).
narrative_ontology:cs_axiom_status(contact_triggers_latent_recognition_not_concept_transfer, holdable).
narrative_ontology:cs_axiom_grounding('a0d369ef-9cfb-484d-930b-6bf584cfd4e9', contact_triggers_latent_recognition_not_concept_transfer, empirically_contingent).
narrative_ontology:cs_reference_frame('a0d369ef-9cfb-484d-930b-6bf584cfd4e9', latent_structure_gated_by_compatible_scaffolding).
narrative_ontology:cs_drift_state('a0d369ef-9cfb-484d-930b-6bf584cfd4e9', contemporary_post_bakhshali_redating, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a0d369ef-9cfb-484d-930b-6bf584cfd4e9', '').
narrative_ontology:cs_kernel_id(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, hindu_mathematical_schools).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, islamic_transmission_centers).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, italian_merchant_reckoners).
narrative_ontology:constraint_victim(zero_as_number_entry__hybrid_scaffolding_reading, greek_geometric_lineage).
narrative_ontology:constraint_victim(zero_as_number_entry__hybrid_scaffolding_reading, aristotelian_scholastics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(zero_as_number_entry__hybrid_scaffolding_reading, italian_merchant_reckoners).
narrative_ontology:constraint_vindicates(zero_as_number_entry__hybrid_scaffolding_reading, positional_placeholder_pressure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sanskrit mathematical-astronomical lineages (the Aryabhata and Brahmagupta traditions and their commentator chains) working where the void was already a metaphysical category with scriptural and devotional currency. For them the empty-place marker slid into number-status with little friction: Brahmagupta could write down rules for calculating with zero and negative quantities in 628 CE that other traditions treated as paradoxes. Codifying those rules gave the lineages a durable export — astronomical computation and the numerals themselves traveled outward along trade and observatory networks. They could have continued verse-form mnemonic computation without material loss, so participation was a choice that paid.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, hindu_mathematical_schools, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__hybrid_scaffolding_reading, hindu_mathematical_schools, agenda_setter).

% Baghdad's translation and administrative circles and their successors — al-Khwarizmi's algebra and Hindu-reckoning manuals, later Maghribi and Andalusian reckoning teachers — absorbed the imported positional arithmetic, standardized its algorithms for paper-based tax, inheritance-share, and astronomical work, and carried the package across the caliphates to Iberia. Fractional inheritance division made capable arithmetic administratively urgent. Nothing trapped them into the import; paper administration predated it. They kept it because it outperformed what it replaced, and they became the channel through which everyone west of Persia eventually met it.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, islamic_transmission_centers, beneficiary,
    institutional, generational, mobile, continental).

% Abaco-school masters and Mediterranean merchants after Liber Abaci gained double-entry bookkeeping and fast pen-reckoning, and made positional numerals the commercial norm. Early adopters also paid a specific price: municipal authorities, judging the new numerals fraud-prone (a single stroke turns a naught into a six or nine), barred them from official account books — Florence's 1299 provision is the famous case — forcing decades of mixed Roman-and-Arabic record keeping. Many houses ran counting board and pen side by side throughout, so the method was always a choice rather than a condition.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, italian_merchant_reckoners, beneficiary,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__hybrid_scaffolding_reading, italian_merchant_reckoners, payer).

% Custodians of magnitude-and-ratio mathematics — the Euclidean-Archimedean corpus and its medieval Latin and Byzantine continuators — for whom quantity is always a magnitude measured against a unit and handled through proportional theory. An operable nothing has no slot in that machinery; even incommensurables are managed without one. Adopting the new arithmetic meant demoting geometric magnitude from foundation to application, a restructuring the lineage attempted only centuries later (Viete, Descartes). Until then it paid continuously: practical computation migrated to positional reckoning, students and patronage followed the utility, and the lineage's center of gravity slid from operative method to classical curriculum.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, greek_geometric_lineage, payer,
    institutional, generational, constrained, continental).

% University arts faculties whose categories define number as plurality counted from a unity — a quantity of nothing is not a poor number but a category violation — and whose natural philosophy denies the void outright. The incoming convention collided with commitments braided through curriculum, degree requirements, and doctrinal oversight. Accommodation meant loosening the framework that constituted their office, so resistance ran through polemic (the cipher denounced as a sign of nothing, folklorized as foreign sorcery) and softened only as humanist curricula and print-era practical arithmetic loosened the tie between office and framework. Leaving the framework and leaving the office were the same act, which is why the tie held so long.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, aristotelian_scholastics, payer,
    institutional, generational, identity_locked, continental).

% Cuneiform bureaucratic and astronomical scribes who used sexagesimal positional notation with empty-slot (later wedged separation) markers for roughly two millennia without ever promoting the marker to a calculable quantity. Their practice is the controlled case the entire dispute turns on: the structure sat in their hands longer than in anyone's, and it stayed inert. Excluded from this conversation by twenty centuries; they testify only through tablets.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, babylonian_placeholder_scribes, excluded,
    powerless, civilizational, trapped, regional).

% Modern historiography reconstructing the entry from manuscripts, inscriptions, and coinage (Bakhshali fragment datings, the Gwalior inscription, Latin translations of al-Khwarizmi), testing mechanism claims — transmission versus independent recognition — against diffusion patterns, and holding every seated interest in view at once. The seat from which the full structure, and the dispute over its reading, is simultaneously visible.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, historians_of_mathematics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_as_number_entry__hybrid_scaffolding_reading, diffuse).
narrative_ontology:fixing_cost_class(zero_as_number_entry__hybrid_scaffolding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives every practitioner of written positional calculation the same answers to what an empty column means and how it behaves under addition (leaves the total unchanged), multiplication (annihilates), and division (undefined or special-cased), so that a computation can be checked by a stranger in another language a generation later. Without that shared rule-set, positional notation remains a local scribal shorthand that cannot aggregate into a cumulative computational literature.
% TRANSFER_FUNCTION: Moves computational capability toward whoever acquires the host vocabulary, and moves authority and patronage away from traditions whose methods the new arithmetic renders slow: from magnitude-based geometry toward algorithmic reckoning, from scholastic category-discipline toward mercantile and administrative practice. It also moves risk — early adopters absorbed fraud suspicion and conversion costs that later adopters never faced.
% ABSENT_VOICES: Babylonian placeholder scribes are unreachable — two millennia dead, they stand as proof that the structure alone persuades no one. Abacus-guild reckoners spoke through guild petitions and municipal lobbying while the decisive conversations ran among mathematicians, translators, and patrons; their livelihood argument entered late and lost. Women were excluded from the instruction lines of both formal traditions. And the scaffold-locked agents themselves — the scholastics above all — could not articulate their own lock-in from inside it, so their objection reached the record mainly as polemic rather than testimony.
% DISAPPEARANCE_RATIONALE: Remove the convention overnight and positional arithmetic loses its pivot: subtraction and multiplication algorithms fail on empty columns, ledgers and ephemerides revert to board reckoning, and every stored binary digit — which encodes its naughts — becomes unreadable. Commerce, navigation, astronomy, and computing all reorganize around whatever slower substitutes remain; the rearrangement reaches deeper than for almost any other convention on record.
% FOUNDING_PROBLEM: Written positional calculation produced ambiguous empty columns that broke verification between hands and generations: a blank could mean naught, omitted context, or damage, and astronomical cycle counts and mercantile ledgers needed a fixed rule for 'nothing in this place' that any trained stranger would apply identically.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: working practitioners today compute with binary and decimal zero in complete indifference to the genealogy dispute — the solved convention's universality is the attestation. Economic historians document counting-board survival into the nineteenth century, showing alternatives stayed viable and lost on merit rather than by fiat. Recurring proposals for zero-free positional schemes keep failing, which only makes sense if the underlying problem was the empty-column rule and not factional politics. No testimony from Hindu, Islamic, or merchant beneficiary lineages is relied upon for the status judgment.
narrative_ontology:disappearance_verdict(zero_as_number_entry__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_as_number_entry__hybrid_scaffolding_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_as_number_entry__hybrid_scaffolding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_as_number_entry__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_as_number_entry__hybrid_scaffolding_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_as_number_entry__hybrid_scaffolding_reading_tests).
:- end_tests(zero_as_number_entry__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claim is rope: the arrangement solves a genuine collective problem (strangers verifying each other's written calculations across language and generation) with minimal coercive overhead — adoption spread by demonstrated utility, enforcement appeared only as local friction and then decayed, and no participant pays a sustained net cost once accommodated. The metrics describe observed operation. Extractiveness 0.38 is concentrated in a historical window (roughly 1200-1350) when scaffold-incompatible traditions bore restructuring-or-obsolescence costs, then subsides as accommodation paths open. Suppression 0.30 is structural in the main (municipal and guild enforcement — Florence's 1299 ban on Arabic numerals in account books is the emblematic case) with an internalized component (doctrinal aversion among scholastics that outlasted the bans); the suppression_requirement series is authored deliberately because this story traces an enforcement-capacity arc — rising under transmission shock, decaying under utility consolidation — not merely shifting extraction. Theater_ratio 0.10 stays low because the convention is load-bearing everywhere; ceremonial Roman-numeral persistence on monuments and anti-cipher polemic are its only performative residue. Accessibility_collapse 0.52 leaves real alternatives (counting boards, Roman numerals) workable for centuries in niche registers while collapsing them decisively in commerce and science; tally sticks surviving into the nineteenth century mark the incompleteness. Resistance 0.45 records genuine scholastic polemic and guild defense that never coordinated into durable opposition. All three measurement series share one seven-point grid (500, 650, 800, 1000, 1200, 1350, 1500) so no metric borrows another's endpoints; each series terminates at its base_properties scalar.
 *
 * PERSPECTIVAL GAP:
 *   The same rule-set computes differently by seat. From the Hindu and Islamic beneficiary seats it is nearly free coordination — existing vocabulary made adoption cheap, so effective extraction damps toward subsidy. From the Greek geometric seat the identical convention arrives as forced obsolescence of a millennium-scale toolkit: same nominal standing (elite mathematical tradition, institutional power), opposite structural relationship, differentiated purely by scaffolding compatibility rather than by any difference in global standing — a clean same-level lateral case. From the scholastic seat it arrives as a metaphysical demand for framework apostasy, and the identity-lock amplifier (curriculum, office, and doctrine fused with Aristotelian categories) pushes that seat toward maximal target-directionality. Inter-institutionally, universities, abaco schools, and observatory astronomy sat at comparable formal status yet experienced opposite valences because their exits differed: merchants could run board and pen side by side, geometers could convert only by demoting their foundation, scholastics could convert only by dissolving the framework that constituted their office. The observer seat sees the full structure and no valence at all.
 *
 * DIRECTIONALITY LOGIC:
 *   Hindu mathematical schools and Islamic transmission centers declare as beneficiaries with mobile exits: the convention subsidized capabilities they already sought, placing them near the beneficiary pole. Italian merchant reckoners declare beneficiary with a secondary payer registration — early adoption carried fraud-suspicion bans and record-keeping conversion costs — landing them slightly off the pure-beneficiary pole. Greek geometric lineage and Aristotelian scholastics declare as victims with constrained and identity_locked exits respectively; locked or costly exit amplifies their effective directionality toward full target, and the exit difference is what separates two payer seats at identical institutional power. Babylonian placeholder scribes register as an excluded evidentiary seat rather than a cost-bearer: they predate the convention's enforcement and have no directional relation to it. Historians of mathematics take the analytical seat. No directionality_overrides are authored: the beneficiary/victim declarations plus the exit atoms already differentiate the two same-power payer seats, which is exactly what an override would otherwise exist to express. Receipt check supporting gain_flow 'diffuse': walking each named seat, no agent captures the convention's residual costs — transition costs dissipate into historical restructuring rather than accruing to any seat, and benefits accrue along the whole adopter gradient — so the affirmative diffuse declaration rests on an explicit seat-by-seat check, not a default.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two symmetrical mislabels. Against snare: real cost-bearers exist, but no seat harvests their costs — enforcement decayed rather than ratcheted, exits widened rather than closed, and the arrangement persists because every accommodated participant nets positive. Victimhood here is transition cost, not collected rent. Against mountain: the convention presents as mathematical inevitability (the universal_discovery temptation — surely the placeholder simply becomes a number), yet the Babylonian two-millennium non-recognition and the Greek refusal under direct data-contact show the structure alone compels nothing; entry was scaffold-gated, which is construction, not nature. Had this been authored as a mountain claiming naturality while carrying beneficiaries, FSM evaluation would be triggered — the omegas here carry the equivalent documentation anyway. Mandatrophy-wise, the founding problem (ambiguous empty columns breaking cross-handed verification) is dead — solved so completely that its solution is invisible infrastructure — and a dead founding problem paired with a world_rearranges verdict should raise the capture/zombie mismatch flag; here the flag should fire and then be dismissed on the low theater ratio (0.10) and the universal living functional load. This is a rope at full function, not a piton performing coordination's gestures: nothing about its maintenance is theater, and the residual extraction is the fading tail of a transition, not a mandate outliving its purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates one reading of kernel zero_as_number_entry (reading: hybrid_scaffolding_reading). Sibling readings authorize different structures: universal_discovery_reading removes the scaffolding-victim set entirely (no tradition is locked out if availability is unconditional) and drives epsilon toward negligible; contingent_thinkability_reading hardens victims into permanent metaphysical exclusions and raises epsilon. Which reading''s structure governs a given classification query?',
    'Corpus-level comparison across the three sibling stories sharing the kernel_id, keyed on identical evidence (the Babylonian non-recognition span, Maya independent arrival, Greek refusal under direct data-contact, Fibonacci-era adaptation patterns); no single story adjudicates the kernel.',
    'Adopting universal_discovery_reading deletes greek_geometric_lineage and aristotelian_scholastics from the victim set and pushes epsilon below coordination overhead; adopting contingent_thinkability_reading freezes victim status as permanent and raises epsilon toward the enforced-suppression range. This file''s moderate profile (0.38) is conditional on the scaffolding-gated structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Indexical uncertainty: classification is relative to one reading of a contested kernel; siblings change victim sets and epsilon.').

omega_variable(
    scaffolding_necessity_weight,
    'How much of zero-as-number thinkability was carried by the latent positional structure (available to any tradition given time) versus gated by compatible conceptual scaffolding (available only where existing metaphysical vocabulary admitted an operable nothing)?',
    'Comparative analysis of placeholder-without-number cases (Seleucid-Babylonian empty-slot marks sustained roughly two millennia without acquiring number status; Greek astronomers copied Babylonian sexagesimal tables for centuries while refusing a numeric zero) against independent-recognition cases (Maya Long Count zero arising under calendrical, not commercial, scaffolding).',
    'Necessity-heavy weighting relaxes this reading toward universal_discovery_reading (epsilon falls, victims soften); contingency-heavy weighting tightens it toward contingent_thinkability_reading (epsilon rises, victims harden). The authored 0.38 assumes both weights are substantial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffolding_necessity_weight, empirical, 'Balance between latent mathematical availability and scaffolding-gated recognition.').

omega_variable(
    transmission_trigger_mechanism,
    'Did twelfth-to-fourteenth-century contact move a finished concept from Islamic to Latin practice, or did it trigger local recognition of a structure Latin practitioners were already positioned to complete?',
    'Textual-diffusion analysis: compare abaco-school practice against the Latin translations of al-Khwarizmi (error signatures, notation hybrids, terminology calques distinguish copying from reconstruction); track adoption speed against local commercial problem-fit rather than manuscript availability.',
    'Trigger-pattern evidence supports this reading''s recognition model and its moderate epsilon; faithful-transfer evidence shifts evidentiary support to contingent_thinkability_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_trigger_mechanism, empirical, 'Whether cross-cultural contact transferred a concept or triggered recognition of a latent structure.').

omega_variable(
    greek_lineage_victim_status,
    'Is the cost borne by the Greek geometric lineage (obsolescence of magnitude-based method, loss of computational relevance and patronage) attributable to the zero convention as extraction, or is it ordinary disciplinary supersession that no convention caused?',
    'Counterfactual analysis: would magnitude-based mathematics have retained computational primacy absent the positional-zero alternative? Compare regions where board reckoning persisted without stigma; if the lineage''s decline tracks zero-adoption specifically, victim attribution stands.',
    'Removing victim status lowers effective extraction for payer seats and softens classification toward unconstrained coordination; confirming it keeps asymmetric cost-bearing in the structure and sustains the moderate epsilon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(greek_lineage_victim_status, conceptual, 'Whether scaffold-incompatible traditions count as victims or merely as superseded parallels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__hybrid_scaffolding_reading, 500, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t500, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 500, 0.04).
narrative_ontology:measurement(zero_tr_t650, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 650, 0.05).
narrative_ontology:measurement(zero_tr_t800, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 800, 0.06).
narrative_ontology:measurement(zero_tr_t1000, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1000, 0.07).
narrative_ontology:measurement(zero_tr_t1200, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1200, 0.11).
narrative_ontology:measurement(zero_tr_t1350, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1350, 0.15).
narrative_ontology:measurement(zero_tr_t1500, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1500, 0.1).

% Extraction over time
narrative_ontology:measurement(zero_be_t500, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 500, 0.2).
narrative_ontology:measurement(zero_be_t650, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 650, 0.24).
narrative_ontology:measurement(zero_be_t800, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 800, 0.26).
narrative_ontology:measurement(zero_be_t1000, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1000, 0.29).
narrative_ontology:measurement(zero_be_t1200, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1200, 0.36).
narrative_ontology:measurement(zero_be_t1350, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1350, 0.42).
narrative_ontology:measurement(zero_be_t1500, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1500, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(zero_su_t500, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 500, 0.06).
narrative_ontology:measurement(zero_su_t650, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 650, 0.08).
narrative_ontology:measurement(zero_su_t800, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 800, 0.12).
narrative_ontology:measurement(zero_su_t1000, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 1000, 0.16).
narrative_ontology:measurement(zero_su_t1200, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 1200, 0.34).
narrative_ontology:measurement(zero_su_t1350, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 1350, 0.4).
narrative_ontology:measurement(zero_su_t1500, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 1500, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_as_number_entry__hybrid_scaffolding_reading, information_standard).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, contingent_thinkability_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, universal_discovery_reading).

% DUAL FORMULATION NOTE:
% Family decomposition per the epsilon-invariance principle: the colloquial label 'the entry of zero as a number' covers three structurally distinct claims and splits into three linked stories sharing kernel zero_as_number_entry. This member instantiates hybrid_scaffolding_reading; its epsilon referent is the scaffolding-mediated entry arrangement itself, authored moderate (0.38) because the referent mixes latent necessity (which pulls extraction down — no one is prevented from reaching the structure) with scaffold contingency (which concentrates transition costs on incompatible traditions — pulling up). contingent_thinkability_reading authors a higher epsilon over a transmission-necessity arrangement with hardened permanent victims; universal_discovery_reading authors near-zero epsilon with an empty victim set. Each file keeps a single stable epsilon; upstream-downstream citation flows run from the availability claim toward the mechanism claims, so this file links both siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
