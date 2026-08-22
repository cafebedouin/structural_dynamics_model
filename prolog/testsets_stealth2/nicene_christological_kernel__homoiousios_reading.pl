% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoiousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_christological_kernel__homoiousios_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: nicene_christological_kernel__homoiousios_reading
 *   human_readable: Homoiousian Settlement: Christ of Similar Substance with the Father, Ontological Distinction Preserved
 *   domain: historical_theology/christology/ecclesiastical_authority
 *
 * SUMMARY:
 *   Between the Sirmium crisis of 357 and the Council of Constantinople in
 *   381, the moderate Eastern episcopal majority governed the churches under
 *   a christological settlement: the Son is homoiousios, of similar substance
 *   with the Father, preserving a real ontological distinction in service of
 *   monotheistic clarity. This story instantiates ONE reading of the
 *   contested substance kernel (nicene_christological_kernel); the sibling
 *   reading (homoousios_reading, same substance, full equality of essence) is
 *   a separate constraint with its own epsilon, beneficiary/victim structure,
 *   and classification, linked through the network edge. The epsilon referent
 *   here is the standing homoiousian arrangement itself as it operated,
 *   assessed by its own lights: a formula that genuinely coordinated the
 *   broad middle while excluding both extremes and taxing institutional
 *   unity. The colloquial label 'the substance dispute' covers two
 *   structurally distinct settlements with different extraction profiles; per
 *   the epsilon-invariance principle they are authored as separate stories in
 *   one family, with this file documenting the decomposition in the
 *   dual-formulation note.
 *
 * KEY AGENTS:
 *   - - homoiousian_episcopal_leadership: Agenda-setting collector (organized/constrained) — administers the similar-substance boundary, collects sees and synodal standing, exposed to deposition when imperial preference shifts
 *   - - eastern_regional_churches: Primary beneficiary (organized/mobile) — hold exegetical and liturgical autonomy under the pluralist band
 *   - - homoousian_party: Primary payer (powerful/constrained) — deposed and exiled; supplies the principal documented resistance
 *   - - anomoean_radicals: Payer (moderate/constrained) — excluded by the anti-Arian floor the settlement exists to maintain
 *   - - imperial_court: Agenda-setting beneficiary (institutional/arbitrage) — enforces the settlement for unity and pays for it in fragmentation; alone among the seats can exit by rewriting the formula
 *   - - parochial_clergy_and_laity: Payer (powerless/trapped) — absorb deposition cycles and liturgical whiplash with no seat anywhere
 *   - - cappadocian_theologians: Analytical observer (analytical/analytical) — retrospective full-structure view from inside the settlement's aftermath
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoiousios_reading, 0.48).
domain_priors:suppression_score(nicene_christological_kernel__homoiousios_reading, 0.58).
domain_priors:theater_ratio(nicene_christological_kernel__homoiousios_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoiousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoiousios_reading, "Homoiousian Settlement: Christ of Similar Substance with the Father, Ontological Distinction Preserved").
narrative_ontology:topic_domain(nicene_christological_kernel__homoiousios_reading, "historical_theology/christology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoiousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoiousios_reading, '12f52066-e032-430d-b6d6-bf6e36508e0e').
narrative_ontology:cs_kernel_codification('12f52066-e032-430d-b6d6-bf6e36508e0e', formalized).
narrative_ontology:cs_authority_grounding('12f52066-e032-430d-b6d6-bf6e36508e0e', lineage).
narrative_ontology:cs_interpretation_layer_present('12f52066-e032-430d-b6d6-bf6e36508e0e').
narrative_ontology:cs_reading_relation('12f52066-e032-430d-b6d6-bf6e36508e0e', nicene_christological_kernel__homoousios_reading, forecloses).
narrative_ontology:cs_axiom('12f52066-e032-430d-b6d6-bf6e36508e0e', foundational, substance_similarity_not_identity).
narrative_ontology:cs_axiom_status(substance_similarity_not_identity, holdable).
narrative_ontology:cs_axiom_grounding('12f52066-e032-430d-b6d6-bf6e36508e0e', substance_similarity_not_identity, theological).
narrative_ontology:cs_axiom('12f52066-e032-430d-b6d6-bf6e36508e0e', foundational, monarchy_of_the_father_preserved).
narrative_ontology:cs_axiom_status(monarchy_of_the_father_preserved, holdable).
narrative_ontology:cs_axiom_grounding('12f52066-e032-430d-b6d6-bf6e36508e0e', monarchy_of_the_father_preserved, theological).
narrative_ontology:cs_axiom('12f52066-e032-430d-b6d6-bf6e36508e0e', secondary, homoousios_conflates_persons_or_divides_essence).
narrative_ontology:cs_axiom_status(homoousios_conflates_persons_or_divides_essence, holdable).
narrative_ontology:cs_axiom_grounding('12f52066-e032-430d-b6d6-bf6e36508e0e', homoousios_conflates_persons_or_divides_essence, theological).
narrative_ontology:cs_reference_frame('12f52066-e032-430d-b6d6-bf6e36508e0e', apostolic_monarchy_real_distinction).
narrative_ontology:cs_drift_state('12f52066-e032-430d-b6d6-bf6e36508e0e', eve_of_constantinople_381, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('12f52066-e032-430d-b6d6-bf6e36508e0e', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoiousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, eastern_regional_churches).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, homoiousian_episcopal_leadership).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, homoousian_party).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, anomoean_radicals).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, parochial_clergy_and_laity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, imperial_court).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, homoiousian_episcopal_leadership).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoiousios_reading, monarchy_of_the_father).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoiousios_reading, real_distinction_father_son).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Basil of Ancyra, Eustathius of Sebaste, George of Laodicea and allied bishops drafted the Ancyra manifesto of 358, convened synods, negotiated with Constantius II, and administered the similar-substance boundary across the Greek East. They collected sees, synodal presidencies, and imperial audiences while the formula held. When imperial preference moved to homoian wording after 360 they became casualties of the machinery they had built: deposed, exiled, and forced to restate their position in each new council. Leaving the episcopal system entirely meant schism and the loss of their office, so their exit ran through formula-revision rather than departure.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, homoiousian_episcopal_leadership, agenda_setter,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoiousios_reading, homoiousian_episcopal_leadership, payer).

% Provincial churches of Asia Minor, Syria, Pontus, and adjacent regions kept local liturgical customs, exegetical schools, and theological emphases under a formula that fixed a floor (the Son is not a creature) and a ceiling (the Son is not the Father renamed) without micromanaging local teaching. Communal continuity cushioned formula transitions, so shifting allegiance to a rival wording carried lower cost for a corporate body than for an individual bishop.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, eastern_regional_churches, beneficiary,
    organized, generational, mobile, regional).

% Athanasius, Hilary of Poitiers, the Roman see, and the Western episcopate refused similar-substance wording as a dilution of the Son's full deity. Whenever the settlement's enforcement reached them they suffered deposition, exile, and broken communion; Athanasius spent much of the period in flight or banishment. They would not leave the church, so their exit consisted of exile-and-return cycles, polemical writing, and appeals to Rome and to sympathetic emperors.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, homoousian_party, payer,
    powerful, generational, constrained, continental).

% Aetius, Eunomius, and their circles taught that the Son is unlike the Father in essence. The similar-substance floor condemned their position by definition. They faced synodal condemnation, exile, and the suppression of their writings; their exclusion was the credential by which the settlement proved its anti-Arian seriousness. Their exit options were acceptance of the floor or persistence as a hunted minority.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, anomoean_radicals, payer,
    moderate, biographical, constrained, regional).

% Constantius II and his successors convoked councils, ratified or discarded formulas, and exiled opponents, seeking a single creed as an instrument of imperial cohesion. The similar-substance plurality was useful when it delivered manageable majorities and obstructive when its built-in pluralism fragmented the uniformity the court wanted. Unlike every ecclesiastical actor, the court could abandon a formula at will: convene a new council, rewrite the wording, redirect enforcement.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, imperial_court, agenda_setter,
    institutional, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoiousios_reading, imperial_court, beneficiary).

% Congregations in contested sees endured deposed pastors, rotating replacement bishops, abrupt liturgical changes, and communal division as formulas turned over. Baptismal belonging and the absence of any rival religious institution left them no exit; they had no seat in any council and their costs arrived on the timescale of a bishop's deposition.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, parochial_clergy_and_laity, payer,
    powerless, immediate, trapped, local).

% Basil of Caesarea, Gregory of Nazianzus, and Gregory of Nyssa were formed inside the homoiousian milieu, watched the similar-substance settlement fail to secure either distinction or unity, and produced the ousia/hypostasis vocabulary that dissolved the dilemma the older formula had frozen. Writing from inside its aftermath, they see the full structure: what the settlement coordinated, whom it excluded, and why its wording could not hold.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, cappadocian_theologians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_christological_kernel__homoiousios_reading, homoiousian_episcopal_leadership).
narrative_ontology:fixing_cost_class(nicene_christological_kernel__homoiousios_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared middle formula that holds the broad moderate episcopal majority in communion: it affirms a real distinction between Father and Son (blocking collapse of the persons into one) while fixing a floor beneath which no confession may fall (the Son is not a creature), so regional churches with differing theologies can commune without conceding either extreme.
% TRANSFER_FUNCTION: Moves doctrinal legitimacy and episcopal office toward the moderate Eastern synodal network; moves exclusion, deposition, and broken communion onto parties outside the similarity band; moves liturgical stability away from congregations in contested sees; and moves creedal uniformity away from the imperial center.
% ABSENT_VOICES: Lay Christians, and women and non-elite Christians most completely, had no voice in any council that set or revised the formula; the homoousian and anomoean parties appear in the record only as objects of procedure, their objections entering as heresy charges rather than as negotiating positions. They stood outside the synodal process, appealing to emperors, to Rome, or to popular support in their own cities.
% DISAPPEARANCE_RATIONALE: If the similar-substance settlement vanished overnight in, say, 359, the mid-century church would immediately reorganize: the episcopal alliance network built around Ancyra would dissolve into homoousian and homoian blocs, the sequence of councils, depositions, and exiles would run on a different schedule, and imperial religious policy would lose the majority instrument it was actually using. Episcopal careers, exile patterns, and the content of the eventual Constantinopolitan settlement all presuppose this arrangement having existed.
% FOUNDING_PROBLEM: After Nicaea 325 the churches faced a triple bind: confess the Son's true divinity against Arian subordinationism, avoid collapsing Father and Son into one person (the modalist fear that made homoousios read as Sabellian to many Eastern bishops), and preserve the Father's monarchy as the sole source of deity. The similar-substance formula was built to hold all three at once.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Athanasius and the Western bishops, the settlement's principal opponents, treated the distinction problem as real even while rejecting the similar-substance answer (De Synodis engages the homoiousian arguments on their merits); the Cappadocian theologians who inherited the wreckage attested that the underlying problem of stating distinction-without-division was genuine while judging the formula insufficient to it; and imperial correspondence attests the unity problem the arrangement was meant to serve. No source inside or outside the beneficiary set claims the problem was already solved in 357.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoiousios_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoiousios_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoiousios_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoiousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoiousios_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_christological_kernel__homoiousios_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_christological_kernel__homoiousios_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_christological_kernel__homoiousios_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.48, anchored at the operative phase 357-360 when the arrangement actually governed): the formula taxed out-of-band parties with deposition and exile and taxed the whole body with fragmentation, but it also delivered a real coordination good, so it does not approach the extraction levels of arrangements whose coordination story is pure cover. Suppression (0.58) is authored as a raw structural property and is NOT scaled by power or scope — the engine owns that arithmetic; the raw figure reflects the enforcement machinery the boundary required: synodal condemnation, imperial banishment, communion-breaking. Theater (0.42) is moderate: the exegetical work behind Ancyra 358 was real, but conciliar activity increasingly ratified formulas decided in advance, and the theater series climbs monotonically as the arrangement loses function. Accessibility collapse is 0.50: inside the framework, alternatives collapse substantially (creature-readings fall below the floor, identity-readings exceed the ceiling), yet exit to rival frameworks stayed live throughout — bishops crossed formulas repeatedly, which is why collapse stops at half. Resistance is 0.66: the homoousian party's polemic and the anomoean underground were persistent, organized, and partly successful. The claim (tangled_rope) and the metrics are independent authored facts: the claim states what I believe is structurally true of the arrangement at its operative core; the metric series additionally documents its lifecycle, including decay toward theatrical persistence after 361 — a rising theater_ratio alongside falling extraction is the drift signature of a function dying, not evidence against the claim. The suppression_requirement series is authored because enforcement capacity is precisely the dynamic this story traces: it peaks at Constantinople 360, collapses under Julian's universal toleration in 361-363, partially revives under Valens, and terminates after Adrianople. The resulting oscillation is driven by imperial succession — an external factor, not intermittent reinforcement; the scalars are therefore anchored at the operative phase rather than at any single cycle phase. Coordination type is identity_coordination, and the known gaming risk applies in reverse here: the settlement's boundary-maintenance function was genuine (communion required shared confession), but the same boundary concentrated exclusion on out-of-band minorities, which is exactly the coupling pattern the identity_coordination offset is not permitted to excuse. All three tracked series share one nine-point grid (357-381 at three-year steps) so the engine never substitutes an end-state scalar into an earlier row.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the administering seat (homoiousian_episcopal_leadership) the arrangement is a hard-won coalition instrument that kept the churches confessible and themselves in office; from the homoousian seat the same structure is a machine for diluting the Son's deity and exiling anyone who says otherwise; from the anomoean seat it is a ceiling that criminalizes their teaching; from the regional-church seat it is doctrinal home rule; from the parish seat it is a sequence of disruptions arriving from nowhere. Inter-institutional dynamics: the imperial court and the episcopal network occupy the same nominal governance tier with opposite exit profiles — the court holds arbitrage-grade exit (rewrite the formula, reconvene the council) while the bishops hold constrained exit (revise wording from inside, at the cost of their office if the wind shifts), which is why identical imperial pressure produced compliance in one seat and martyrdom-adjacent endurance in the other. Same-level lateral dynamics: the homoousian party and the homoiousian leadership are both powerful-or-organized episcopal actors of equal global standing, differentiated entirely by constraint-specific factors — relationship to the formula (administered versus excluded-by-it) and the shape of constrained exit (formula-revision versus exile-and-return). Coalition potential among the weaker payers was real but unrealized: the laity's trapped position and the anomoeans' pariah status prevented the cross-victim coalition that might have raised their joint bargaining power.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: eastern_regional_churches sit near the subsidized end (the formula protects their autonomy at others' expense), and the episcopal leadership derives low d from its beneficiary listing despite its agenda-setting role. Victim declarations drive high d: homoousian_party, anomoean_radicals, and parochial_clergy_and_laity derive target-side values, amplified for the trapped laity and damped slightly for the powerful, well-networked homoousian party. One override is declared: the imperial court holds the institutional power atom but appears in neither the beneficiary nor the victim arrays, so the derivation chain would fall back to a canonical default that misses its actual position — the settlement fragments the uniformity project the court exists to advance (target-side pressure) while supplying a governable majority instrument (benefit-side relief), netting to a mild target lean, hence d = 0.55 for the institutional atom. Effective extraction is computed by the engine from these directionalities and the continental scope (large scope, harder verification, modest amplification); nothing here pre-computes chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against both symmetrical mislabels. Reading the arrangement as pure extraction erases the genuine coordination achievement: it solved a real collective-action problem (holding a fractious episcopal majority in communion against two extremes) that no participant would have solved as well individually, and its beneficiaries were numerous and not merely rent-collecting. Reading it as pure coordination erases the identifiable payers: two excluded parties and a voiceless parish layer who bore deposition, exile, and disruption through the same structure that coordinated everyone else, sustained by active enforcement. Tangled_rope holds both halves. On obsolescence: the founding problem (stating distinction-without-division) remained live for the entire interval and beyond, so this is not a mandate outliving its function — the arrangement was displaced by a better answer (the Cappadocian ousia/hypostasis resolution ratified at Constantinople 381), not left running after its problem died. mandatrophy_resolved is accordingly not declared, and the status-times-verdict pairing (live, world_rearranges) raises no capture flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is ONE reading (homoiousios_reading) of the kernel nicene_christological_kernel; what structurally changes if the sibling reading homoousios_reading is instantiated instead?',
    'Comparative instantiation of the sibling story: re-derive beneficiary/victim sets, exit profiles, and epsilon under the same-substance commitment and diff the two classifications.',
    'Under the sibling reading, the payer set shifts: regional exegetical autonomy and the moderate episcopal network become the coordinated-and-extracted parties, imperial religious uniformity moves toward the beneficiary side, and epsilon plausibly rises because enforced essence-identity demands heavier suppression of pluralism than a similarity-band does.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: which kernel, which reading, what the sibling would change.').

omega_variable(
    semantic_stability_of_homoiousios,
    'Was ''of similar substance'' a determinate metaphysical thesis or a deliberately elastic formula serving coalition maintenance among bishops who agreed on little else?',
    'Philological analysis of 4th-century usage: compare Basil of Ancyra''s De Fide and the Ancyra 358 encyclical with later homoian appropriations of similar-substance language; measure whether the term carried a fixed intension across authors.',
    'If elastic, a measurable share of the arrangement''s coordination achievement is coalition management rather than doctrine, raising effective theater above the authored 0.42 and weakening the coordination-function gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semantic_stability_of_homoiousios, empirical, 'Whether the similar-substance formula was semantically stable or strategically vague.').

omega_variable(
    epsilon_anchor_phase,
    'The scalar epsilon is anchored at the arrangement''s operative phase (357-360), when it actually governed; would a lifecycle-weighted epsilon over the full 357-381 interval yield a different classification?',
    'Specify a lifecycle-weighting convention (operative-phase weight vs. uniform-interval weight) and recompute; the authored series supports a lifecycle-weighted epsilon near 0.25.',
    'A lifecycle-weighted epsilon near 0.25 with the same suppression profile could move the computed classification toward rope or transitional-support territory; the operative-phase anchor keeps it in the hybrid coordination/extraction band.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_anchor_phase, conceptual, 'Sensitivity of classification to the choice of epsilon anchoring phase.').

omega_variable(
    counterfactual_settlement_durability,
    'Would ANY mid-fourth-century formula have held the churches together, or was ecclesiastical fragmentation overdetermined by episcopal competition and repeated imperial intervention regardless of the substance wording?',
    'Compare formula-churn rates and schism incidence across successive settlements (Nicaea 325, the Sirmium series, Constantinople 360, Constantinople 381) controlling for imperial enforcement intensity.',
    'If fragmentation was overdetermined, the cohesion cost currently attributed to this arrangement as extraction is partly misattributed systemic cost, lowering effective extraction for the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_settlement_durability, empirical, 'Whether the fragmenting effect is attributable to this formula or to the environment.').

omega_variable(
    cohesion_cost_attribution,
    'The structural delta names ''institutional cohesion and imperial religious uniformity'' as the victim, but cohesion is not an agent; which concrete actors bear that cost, and is the authored payer set complete?',
    'Trace concrete incidence: deposed and exiled clergy, congregations enduring liturgical whiplash, imperial policy failures traceable to creedal plurality; audit whether any bearing seat is missing from the stakeholder surface.',
    'If a materially burdened seat is missing, derived directionality for that population is absent and the engine understates target-side extraction; completion would raise aggregate effective extraction modestly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohesion_cost_attribution, empirical, 'Who concretely pays the fragmentation cost the delta attributes to cohesion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoiousios_reading, 357, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t357, nicene_christological_kernel__homoiousios_reading, theater_ratio, 357, 0.2).
narrative_ontology:measurement(nice_tr_t360, nicene_christological_kernel__homoiousios_reading, theater_ratio, 360, 0.4).
narrative_ontology:measurement(nice_tr_t363, nicene_christological_kernel__homoiousios_reading, theater_ratio, 363, 0.55).
narrative_ontology:measurement(nice_tr_t366, nicene_christological_kernel__homoiousios_reading, theater_ratio, 366, 0.62).
narrative_ontology:measurement(nice_tr_t369, nicene_christological_kernel__homoiousios_reading, theater_ratio, 369, 0.68).
narrative_ontology:measurement(nice_tr_t372, nicene_christological_kernel__homoiousios_reading, theater_ratio, 372, 0.72).
narrative_ontology:measurement(nice_tr_t375, nicene_christological_kernel__homoiousios_reading, theater_ratio, 375, 0.75).
narrative_ontology:measurement(nice_tr_t378, nicene_christological_kernel__homoiousios_reading, theater_ratio, 378, 0.78).
narrative_ontology:measurement(nice_tr_t381, nicene_christological_kernel__homoiousios_reading, theater_ratio, 381, 0.8).

% Extraction over time
narrative_ontology:measurement(nice_be_t357, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 357, 0.32).
narrative_ontology:measurement(nice_be_t360, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 360, 0.46).
narrative_ontology:measurement(nice_be_t363, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 363, 0.26).
narrative_ontology:measurement(nice_be_t366, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 366, 0.2).
narrative_ontology:measurement(nice_be_t369, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 369, 0.17).
narrative_ontology:measurement(nice_be_t372, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 372, 0.14).
narrative_ontology:measurement(nice_be_t375, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 375, 0.12).
narrative_ontology:measurement(nice_be_t378, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 378, 0.1).
narrative_ontology:measurement(nice_be_t381, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 381, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t357, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 357, 0.35).
narrative_ontology:measurement(nice_su_t360, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 360, 0.6).
narrative_ontology:measurement(nice_su_t363, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 363, 0.15).
narrative_ontology:measurement(nice_su_t366, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 366, 0.3).
narrative_ontology:measurement(nice_su_t369, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 369, 0.33).
narrative_ontology:measurement(nice_su_t372, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 372, 0.35).
narrative_ontology:measurement(nice_su_t375, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 375, 0.3).
narrative_ontology:measurement(nice_su_t378, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 378, 0.15).
narrative_ontology:measurement(nice_su_t381, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 381, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoiousios_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoiousios_reading, homoousios_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Nicene substance dispute' decomposes into two structurally distinct settlements per the epsilon-invariance principle. This story (homoiousios_reading) authors the similar-substance arrangement: moderate epsilon (~0.48), beneficiaries in the regional churches and the administering episcopal network, payers among the excluded extremes and the parish layer, fragmentation of imperial uniformity as the diffuse cost. The sibling story (homoousios_reading) authors the same-substance arrangement: different beneficiary/victim geometry (regional exegetical autonomy becomes the coordinated-and-taxed party; enforced uniformity moves toward the beneficiary side) and plausibly higher epsilon given the heavier suppression essence-identity enforcement demanded. The upstream/downstream structure runs from this reading to the sibling: the homoiousian settlement's failure modes (elastic wording, coalition fragility) are cited in the historical record as part of the case for the homoousian resolution, so this constraint structurally influences its sibling's legitimacy conditions without either file averaging over the other. Each file links the other via affects_constraints; neither contains the other's contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_christological_kernel__homoiousios_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
