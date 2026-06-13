% ============================================================================
% CONSTRAINT STORY: reformation_composite__political_realignment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__political_realignment_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reformation_composite__political_realignment_reading
 *   human_readable: Territorial Sovereignty via Religious Differentiation (Political Reading)
 *   domain: political/historical/institutional
 *
 * SUMMARY:
 *   The Reformation (1517–1648, with long-tail effects) is read here as
 *   fundamentally a political event: emerging territorial rulers—Henry VIII
 *   in England, Frederick the Wise in Saxony, Gustavus Adolphus in Sweden,
 *   and others—use religious differentiation (reformed theology, break with
 *   Rome, state-controlled churches) to consolidate territorial sovereignty
 *   and escape tributary relationships to the papacy and Holy Roman Empire.
 *   The observable is cuius regio eius religio: each territorial ruler sets
 *   the religion for their realm, formalizing religious fragmentation as a
 *   consequence of political decentralization. Beneficiaries: territorial
 *   rulers and the state apparatus they build. Victims: papal and imperial
 *   transnational authority. This reading does NOT claim theology is false or
 *   secondary—it claims theology is the legitimacy FRAME that makes political
 *   realignment intelligible and durable. The theological claims may be
 *   sincere, but their structural function is to authorize political
 *   consolidation.
 *
 * KEY AGENTS:
 *   - Emerging territorial rulers (England, Saxony, Scandinavia, France): agenda-setters breaking with Rome; consolidate sovereignty; establish state churches
 *   - Papal and imperial authority: victims losing tributary control, doctrinal reach, and transnational legitimacy; trapped because they cannot restore the medieval universal-church framework
 *   - Reforming theologians and clergy: beneficiaries of state patronage within reformed state churches; identity-locked to reformation theology; secondary to the rulers' political agenda
 *   - Merchant networks: beneficiaries of reformed territorial governance that removes ecclesiastical restrictions on capital accumulation
 *   - Peasantry: payers; trapped between papal extraction and reformed-state extraction; initially mobilized by reformation rhetoric but suppressed when their demands diverge from rulers' interests
 *   - Theological dissenters and radical reformers: excluded from the coordination frame because territorial sovereignty takes precedence over theological consistency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.68).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.72).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Territorial Sovereignty via Religious Differentiation (Political Reading)").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "political/historical/institutional").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, '621e31bd-078f-4335-8be1-a71b457c7a1a').
narrative_ontology:cs_kernel_codification('621e31bd-078f-4335-8be1-a71b457c7a1a', formalized).
narrative_ontology:cs_authority_grounding('621e31bd-078f-4335-8be1-a71b457c7a1a', extraction).
narrative_ontology:cs_interpretation_layer_present('621e31bd-078f-4335-8be1-a71b457c7a1a').
narrative_ontology:cs_reading_relation('621e31bd-078f-4335-8be1-a71b457c7a1a', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('621e31bd-078f-4335-8be1-a71b457c7a1a', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('621e31bd-078f-4335-8be1-a71b457c7a1a', foundational, territorial_sovereignty_over_transnational_authority).
narrative_ontology:cs_axiom_status(territorial_sovereignty_over_transnational_authority, holdable).
narrative_ontology:cs_axiom_grounding('621e31bd-078f-4335-8be1-a71b457c7a1a', territorial_sovereignty_over_transnational_authority, deontological).
narrative_ontology:cs_axiom('621e31bd-078f-4335-8be1-a71b457c7a1a', foundational, reformation_theology_as_legitimacy_frame_not_prime_cause).
narrative_ontology:cs_axiom_status(reformation_theology_as_legitimacy_frame_not_prime_cause, holdable).
narrative_ontology:cs_axiom_grounding('621e31bd-078f-4335-8be1-a71b457c7a1a', reformation_theology_as_legitimacy_frame_not_prime_cause, instrumental).
narrative_ontology:cs_reference_frame('621e31bd-078f-4335-8be1-a71b457c7a1a', medieval_universal_christendom_papacy).
narrative_ontology:cs_drift_state('621e31bd-078f-4335-8be1-a71b457c7a1a', post_westphalian_settlement_1648, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('621e31bd-078f-4335-8be1-a71b457c7a1a', '2026-06-12T14:33:27Z').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, emerging_territorial_rulers).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, imperial_papal_authority).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, transnational_catholic_church_apparatus).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__political_realignment_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__political_realignment_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__political_realignment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__political_realignment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because territorial rulers consolidate not only religious authority but also the revenue streams formerly controlled by the papacy—tithes, church lands, indulgences—and they exclude rival payment/authority systems (transnational Catholicism) from their territories. Suppression is correspondingly high (0.72) because maintaining the territorial-religious boundary requires active enforcement: enforcement of the break with Rome, suppression of radical reformation movements that challenge state authority, enforcement of religious conformity within the realm, and counter-reformation Catholic resistance that triggers religious wars. Theater ratio climbs over time (from 0.08 to 0.41): early reformation has substantial genuine religious and political content; by the late 16th and early 17th centuries, as territorial churches become established and the political realignment is locked in, more energy goes to ceremonial maintenance of the reformed churches and theological legitimation rather than active struggle against papal authority. The time-grid shows the constraint's maturation: early phase is high struggle/low theater, late phase is high enforcement/moderate theater. The temporal trajectory shows extractiveness plateauing by T=150 (post-Westphalia), indicating the constraint has stabilized into its final configuration.
 *
 * PERSPECTIVAL GAP:
 *   From a territorial ruler's seat, the reformation is a genuine solution to the problem of universal-church interference in territorial affairs—the coordination function is real, the extraction is the cost of that coordination, and the theological claims are sincere or at least instrumentally justified. From a papal/imperial seat, the same structure is pure extraction and heresy: the reformation theology is the cover for political theft, the extraction is not a coordination cost but a grabbing of what rightfully belongs to the Church, and the entire enterprise is an illegitimate revolt. From the theological dissenters' seat, both positions are inadequate: they see the reformation theology being corrupted by territorial power-seeking just as it was by papal corruption, and they articulate a third reading (theological_fragmentation_reading) where theological truth, not political consolidation, is the measure. The engine should compute divergent type classifications across these seats: agenda-setter seat sees coordination with incidental extraction; payer/victim seat sees enforced extraction wearing coordination cover; theological dissenter seat (excluded) sees a different constraint entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial rulers: beneficiary d-value near 0.1–0.2 (they collect the coordination benefit and the extraction; they set the rules and enforce them). Papal/imperial apparatus: victim d-value near 0.85–0.95 (they lose authority, revenue, and have no exit except capitulation or violent counter-reformation). Reforming theologians: moderate d-value near 0.3–0.4 (they gain institutional authority but lose autonomy; they are beneficiaries of state patronage but constrained by state interests; their exit is identity_locked, so they cannot leave without abandoning their professional identity). Merchant networks: low d-value near 0.15–0.25 (they benefit from removed restrictions; they have mobile exit so they could exit but choose arbitrage instead). Peasantry: victim d-value near 0.75–0.85 (they are trapped and bear costs; they have no meaningful exit and receive no beneficiary position). The directionality reflects the structural asymmetry: some parties use this constraint to consolidate power; others have it imposed on them.
 *
 * MANDATROPHY ANALYSIS:
 *   The question is whether the founding problem (papal/imperial interference in territorial governance) remains live or has become dead, and whether the reformation constraint is still serving its founding function or has become inertial. At T=0–100 (through the Peace of Westphalia, 1648), the founding problem is live: territorial rulers actively consolidate sovereignty against papal claims, the reformation theology is actively deployed to justify that consolidation, and the constraint's entire structure is designed to solve the problem of transnational ecclesiastical authority. By T=150–200, the founding problem is substantially dead: the papacy has accepted territorial church fragmentation, the Peace of Westphalia has codified it into international law, and territorial rulers face no serious challenge to their religious authority from Rome. But the constraint persists—territorial religious authority remains enforced, reformed state churches persist, the theological differentiation remains vivid in social identity. The rising theater_ratio (0.08→0.41) is evidence of mandatrophy drift: as the original political struggle fades, more of the constraint's operation goes to ceremonial maintenance (religious ritual, theological apologetics, enforcement of conformity as tradition rather than emergency). The constraint is not yet a full piton (there is still real institutional investment in the territorial church apparatus), but it is showing piton symptoms: the original function (breaking with Rome, consolidating sovereignty against transnational authority) is solved, and the system now persists through institutional inertia and identity commitment rather than ongoing political necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_order_theological_vs_political,
    'Is the reformation fundamentally a theological event (theological innovation drives political realignment) or a political event (political realignment deploys theology as legitimacy cover)? Which came first causally—the reformation theology or the territorial rulers'' consolidation agenda?',
    'Chronological analysis of theological development (when did the key theological claims emerge—1450s or 1500s?) versus political realignment timing; examination of whether reformation theology was independently derived from scripture or strategically borrowed to justify rulers'' pre-existing goals; counterfactual: would the same political realignment have occurred with different theology or no reformation theology at all?',
    'If theology is primary, the constraint is better read as theological_fragmentation_reading with political consequences. If politics is primary, the political_realignment_reading is correct and theology is the coordination/extraction cover. The reading instantiated here asserts politics is primary; this omega documents the irreducible uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causality_order_theological_vs_political, conceptual, 'Whether the reformation is fundamentally theological or fundamentally political; which structural element is explanatorily prior.').

omega_variable(
    printing_press_enabling_condition,
    'Could the political realignment have occurred without the printing press''s capacity to distribute reformation theology widely? Is the printing press a necessary enabling condition (mediating technology) or incidental to the political dynamic?',
    'Examination of how reformation theology spread before mass printing (oral tradition, manuscript circulation); analysis of whether earlier reformation movements failed due to lack of technological reach or due to lack of political patronage; counterfactual: if the printing press existed but reformation theology was different (or papal opposition to printing had succeeded), would the political realignment still occur?',
    'If printing press is necessary, the constraint is better read as technological_mediation_reading. If political realignment would occur regardless of print distribution (through other patronage and institutional mechanisms), the political reading is supported and printing is secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printing_press_enabling_condition, empirical, 'Whether the printing press is a necessary enabling technology for the political realignment or incidental to it.').

omega_variable(
    beneficiary_extraction_vs_coordination_function_balance,
    'What proportion of the extractiveness measured (0.68) is the necessary cost of coordinating a federated territorial-church system, and what proportion is excess extraction from the papal apparatus (tribute rent-seeking)?',
    'Comparison of the actual territorial church revenue extraction rates with the revenue needed to fund comparable state ecclesiastical administration in a coordinate-but-not-extractive arrangement; analysis of whether territorial rulers could have achieved territorial ecclesiastical autonomy with lower extraction from Rome.',
    'If the coordination cost is high (say, 0.40+), the constraint is closer to tangled_rope with genuine coordination benefits. If coordination cost is low (0.20 or below) and most extraction is above-cost rent, the constraint is closer to snare-like extraction wearing a coordination cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_extraction_vs_coordination_function_balance, empirical, 'The balance between genuine coordination costs and extractive overhead.').

omega_variable(
    sibling_reading_kernel_contest,
    'This constraint is one of three readings of the reformation_composite kernel. What is the structural relationship between this political_realignment_reading and the competing theological_fragmentation_reading and technological_mediation_reading?',
    'This is a conceptual uncertainty about how to classify the relationships between incommensurable readings of the same kernel. The omega documents the choice made: this reading treats theological_fragmentation as coexisting (different parties hold both as live hypotheses) and technological_mediation as influences (the printing press creates structural conditions for political realignment but does not foreclose the political reading). An alternative framing might classify differently.',
    'Different relation choices (forecloses vs. coexists_with vs. influences) produce different compression/contamination outcomes in the engine''s cross-constraint analysis. This omega documents the authored choice and flags it as a reading-internal decision vulnerable to disagreement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_kernel_contest, conceptual, 'The structural relationship classification between this reading and its sibling readings in the contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_composite__political_realignment_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(refo_tr_t25, reformation_composite__political_realignment_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(refo_tr_t50, reformation_composite__political_realignment_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(refo_tr_t100, reformation_composite__political_realignment_reading, theater_ratio, 100, 0.38).
narrative_ontology:measurement(refo_tr_t150, reformation_composite__political_realignment_reading, theater_ratio, 150, 0.41).
narrative_ontology:measurement(refo_tr_t200, reformation_composite__political_realignment_reading, theater_ratio, 200, 0.41).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_composite__political_realignment_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(refo_be_t25, reformation_composite__political_realignment_reading, base_extractiveness, 25, 0.51).
narrative_ontology:measurement(refo_be_t50, reformation_composite__political_realignment_reading, base_extractiveness, 50, 0.59).
narrative_ontology:measurement(refo_be_t100, reformation_composite__political_realignment_reading, base_extractiveness, 100, 0.66).
narrative_ontology:measurement(refo_be_t150, reformation_composite__political_realignment_reading, base_extractiveness, 150, 0.68).
narrative_ontology:measurement(refo_be_t200, reformation_composite__political_realignment_reading, base_extractiveness, 200, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t0, reformation_composite__political_realignment_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(refo_su_t25, reformation_composite__political_realignment_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(refo_su_t50, reformation_composite__political_realignment_reading, suppression_requirement, 50, 0.65).
narrative_ontology:measurement(refo_su_t100, reformation_composite__political_realignment_reading, suppression_requirement, 100, 0.71).
narrative_ontology:measurement(refo_su_t150, reformation_composite__political_realignment_reading, suppression_requirement, 150, 0.72).
narrative_ontology:measurement(refo_su_t200, reformation_composite__political_realignment_reading, suppression_requirement, 200, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__political_realignment_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reformation_composite__political_realignment_reading, 0.18).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__technological_mediation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, westphalian_sovereignty_doctrine).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, cuius_regio_eius_religio_principle).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the reformation_composite kernel. It is linked to theological_fragmentation_reading and technological_mediation_reading, which instantiate competing structural claims about the same historical kernel. The three stories should be compared on (a) epsilon—do they have different extractiveness values?—and (b) beneficiary/victim structure—do they identify different extraction seats? Decomposition rationale: the ε-invariance principle requires that if measuring the Reformation through theological lenses versus political lenses produces different extractiveness values, they are different constraints. This reading measures extractiveness through political consolidation (what rulers gain/lose relative to papal authority = 0.68); the theological reading measures it through doctrinal incompatibility (how much extraction does theological pluralism itself create = different ε); the technological reading measures it through distribution-cost changes (how much extraction does the printing press's scalability create = different ε).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_composite__political_realignment_reading, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
