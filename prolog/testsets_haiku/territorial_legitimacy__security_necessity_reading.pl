% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__security_necessity_reading, []).

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
 *   constraint_id: territorial_legitimacy__security_necessity_reading
 *   human_readable: Territorial Legitimacy via Security Necessity and Defensive Control
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel
 *   'territorial_legitimacy' — specifically the security-necessity reading,
 *   which claims that Israeli control of the West Bank, Golan Heights, and
 *   (historically) Gaza is legitimate as a defensive buffer against
 *   existential security threats. Under this reading, Palestinian sovereignty
 *   is conditional on demilitarization and security coordination; settlements
 *   provide early-warning presence; territorial control is justified by the
 *   indefensibility of pre-1967 borders. The sibling readings
 *   (indigenous-continuity and partition) offer different legitimacy claims
 *   rooted in anti-colonial self-determination and international legal
 *   partition. This story instantiates ONLY the security-necessity reading as
 *   a structurally coherent constraint, with its own ε, beneficiary/victim
 *   structure, and institutional commitments. The rivalry with siblings is
 *   documented via omega variables and cs_structure, not embedded in the
 *   constraint itself.
 *
 * KEY AGENTS:
 *   - Israeli state security apparatus: administers territorial control, justifies it as security buffer, accrues control benefits, bears no extraction cost
 *   - Palestinian population (West Bank): subject to military law and resource control, subordinated under security framing, constrained exit (no effective voice)
 *   - Palestinian population (Gaza): subject to blockade justified as weapons containment, trapped exit (enclosed territory)
 *   - Displaced Palestinian refugees: held outside territory by right-of-return denial framed as security threat, identity-locked exclusion
 *   - Israeli settler population: benefits from land, subsidies, security narrative; identity-locked to settlement ideology; constrained exit
 *   - Israeli public security constituency: benefits from security narrative and early-warning presence; constrained exit (cognitive and political cost of abandoning security frame)
 *   - International legal/human-rights bodies: observers only, contest the necessity framing, document asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.68).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.76).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Territorial Legitimacy via Security Necessity and Defensive Control").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, 'd51e478c-ab99-46dd-a215-84bc9bb8b2fc').
narrative_ontology:cs_kernel_codification('d51e478c-ab99-46dd-a215-84bc9bb8b2fc', formalized).
narrative_ontology:cs_authority_grounding('d51e478c-ab99-46dd-a215-84bc9bb8b2fc', extraction).
narrative_ontology:cs_interpretation_layer_present('d51e478c-ab99-46dd-a215-84bc9bb8b2fc').
narrative_ontology:cs_reading_relation('d51e478c-ab99-46dd-a215-84bc9bb8b2fc', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('d51e478c-ab99-46dd-a215-84bc9bb8b2fc', territorial_legitimacy__partition_reading, influences).
narrative_ontology:cs_axiom('d51e478c-ab99-46dd-a215-84bc9bb8b2fc', foundational, security_necessity_territorial_buffer).
narrative_ontology:cs_axiom_status(security_necessity_territorial_buffer, holdable).
narrative_ontology:cs_axiom_grounding('d51e478c-ab99-46dd-a215-84bc9bb8b2fc', security_necessity_territorial_buffer, empirically_contingent).
narrative_ontology:cs_axiom('d51e478c-ab99-46dd-a215-84bc9bb8b2fc', secondary, conditional_palestinian_sovereignty_demilitarization).
narrative_ontology:cs_axiom_status(conditional_palestinian_sovereignty_demilitarization, holdable).
narrative_ontology:cs_axiom_grounding('d51e478c-ab99-46dd-a215-84bc9bb8b2fc', conditional_palestinian_sovereignty_demilitarization, instrumental).
narrative_ontology:cs_reference_frame('d51e478c-ab99-46dd-a215-84bc9bb8b2fc', defensive_security_state_1967).
narrative_ontology:cs_drift_state('d51e478c-ab99-46dd-a215-84bc9bb8b2fc', contemporary_2020s, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d51e478c-ab99-46dd-a215-84bc9bb8b2fc', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_state_security_apparatus).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_population_west_bank).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_population_gaza).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, displaced_palestinian_refugees).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__security_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__security_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because territorial control, resource allocation (water, movement, building permits), and political subordination transfer asymmetrically from Palestinians to the Israeli state and settler population. The security-necessity framing is the legitimating cover; without it, the transfers would be transparently colonial. Suppression (0.76) is high because the constraint's persistence depends on actively preventing Palestinian self-governance, blocking alternative security arrangements, and enforcing checkpoints and settlement expansion. Theater ratio (0.52) is moderate-high because security justification is partially genuine (existential threat was real in 1967-1973) but increasingly performs political functions it originally did not serve (settlement expansion, resource extraction, political control). Accessibility collapse (0.41) is moderate-low: Palestinians have formulated alternatives (demilitarized zones, international guarantees, asymmetric military balancing) that are theoretically available but politically suppressed. Resistance (0.78) is high: sustained Palestinian armed and civil resistance to occupation demonstrates the constraint is not accepted as legitimate from the subordinated side. The measurement series shows: (1) extractiveness rose steeply from 1967-1995 (occupation consolidation), plateaued after 2000 (negotiation failures, barrier construction), suggesting the original security problem was displaced by political extraction; (2) theater ratio climbed from low (genuine security focus) to moderate-high (security narrative increasingly justifying land claims and political control); (3) suppression requirement rose steeply and remains high (actively preventing alternatives), indicating the constraint is held by enforcement, not by acceptance.
 *
 * PERSPECTIVAL GAP:
 *   The Israeli security apparatus and settler population perceive the constraint as genuine coordination against existential threat — they experience it as costly but necessary defense. Palestinians (West Bank, Gaza, refugees) perceive the same constraint as extraction and colonization justified by inflated threat claims. The engine should compute these seats as having radically different type classifications from the same structural data: agenda-setter (institutional power, trapped exit, no extraction cost) sits near rope or scaffold; targets (powerless, constrained/trapped exit, high asymmetric cost) sit near snare or tangled-rope-as-extraction. The divergence is not an error — it is the measurement the dual-reading apparatus exists to capture. The security-necessity reading privileges the beneficiary's framing; the indigenous-continuity reading privileges the victim's frame; the partition reading aims for neutral international law. This constraint, authored as written, is the Israeli security establishment's coherent articulation of its own position.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli security apparatus: d ~ 0.15 (beneficiary, agenda-setter, trappped exit means they are locked to the constraint, not free to exit, but they are not targets — they benefit and control). Settler population: d ~ 0.20 (beneficiary, organized power, beneficiary role, but constrained exit by ideology and community ties reduces freedom and raises extraction intensity slightly). Israeli public security constituency: d ~ 0.25 (beneficiary, organized power, receives security narrative benefit, but constrained exit by internalized security frame). Palestinian West Bank population: d ~ 0.85 (target, powerless, constrained exit, victim of resource control and political subordination). Palestinian Gaza population: d ~ 0.95 (target, powerless, trapped exit, blockaded). Displaced refugees: d ~ 0.90 (target, powerless, identity-locked exit, excluded by right-of-return denial). International observers: d ~ 0.50 (analytical, no stakeholder benefit or cost, symmetric observation). Directionality is not symmetric because the constraint extracts from the powerless to benefit the institutional and settler seats. The constraint is claimed as coordination (security buffer) but the extraction asymmetry is substantial.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (indefensible 1967 borders, existential threat from hostile state armies) was genuine in 1967-1973. However: (1) Israeli military capability evolved (air force dominance, antimissile systems, cyberwarfare) reducing dependence on territorial buffer; (2) Palestinian state threat remained limited (no independent military, constrained governance); (3) regional adversary threat shifted (Egypt-Israel peace 1979, Arabia-Israel normalization 2020s, Syria weakened post-civil war); (4) alternative security arrangements were proposed (Camp David, Taba, 2008 talks) but rejected, suggesting political preference for unilateral control over security necessity. The theater ratio's rise from 0.22 to 0.52 indicates the original security function is being displaced by political extraction (settlement expansion, resource control, political subordination). If the founding problem is dead (threat no longer existential, alternatives available, regional alignment changed), the constraint exhibits mandatrophy: the founding problem's resolution condition has been met or the problem has become obsolete, but the arrangement persists for reasons other than addressing the founding problem (political control, settler expansion, military habitus). This does NOT automatic-reclassify the type (engine computes from metrics and structure), but it flags the constraint as a candidate for piton or zombie-snare (extractive constraint maintained theatrically after its original function atrophied).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_necessity_threshold,
    'What objective measure would falsify the claim that 1967 borders are militarily indefensible and that territorial buffer is necessary for security?',
    'Military capability assessment: can Israeli defensive systems (air force, antimissile architecture, cyberdefense) provide security equivalent to territorial depth? Comparative analysis with other states at similar threat levels (e.g., Taiwan, Singapore, Gulf states) that lack territorial buffers but maintain defensive capability through alternative means.',
    'If defensive capability can be maintained without territorial buffer, the security-necessity framing becomes optional (not foundational), and the constraint reclassifies from coordination-with-extraction to pure extraction. If buffer is genuinely necessary, the constraint''s coordination component is real, though asymmetric extraction may persist alongside it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_necessity_threshold, empirical, 'Whether territorial buffer is structurally necessary for Israeli security or alternative sufficient mechanisms exist.').

omega_variable(
    alternative_security_arrangements,
    'Have demilitarized-zone arrangements, international peacekeeping guarantees, or technology-based monitoring been seriously proposed and rejected, or are they categorically unavailable?',
    'Historical record of Israeli-Palestinian negotiations: Camp David (2000), Taba (2001), 2008 Abbas-Olmert talks, 2010-2014 Kerry mediation. Documentary analysis of what security proposals were offered, why they were rejected, and whether rejection was driven by genuine security insufficiency or by political preference for unilateral control.',
    'If genuine alternatives were available and rejected, the security-necessity framing conceals a political choice for unilateral control (extraction via security discourse). If alternatives were categorically insufficient, the framing is honest about constraint structure. This determines whether the constraint is a false-summit mountain or genuine tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_security_arrangements, empirical, 'Whether the constraint rests on necessity or on choice among available options.').

omega_variable(
    settlement_security_function_vs_political_expansion,
    'Do Israeli settlements in the West Bank serve a genuine security early-warning function, or are they primarily vehicles for territorial expansion and political control disguised as security presence?',
    'Analysis of settlement distribution relative to vulnerability corridors and threat vectors: are settlements placed at military-strategic chokepoints (early warning), or scattered across the territory in patterns that maximize land acquisition and population presence? Assessment of settlement governance: do they respond to military chain of command, or do they operate under civilian authority pursuing expansion? Comparison of settlement growth trajectory to threat assessment and security doctrine evolution.',
    'If settlements are primarily security infrastructure, the extraction is justified as security necessity and the tangled_rope framing is honest. If settlements are primarily expansion using security as cover story, the constraint reclassifies as snare and the security-necessity reading becomes a false legitimacy claim. The core distinction is whether the beneficiary (settler population) serves the security function or exploits it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_security_function_vs_political_expansion, empirical, 'Whether settlements are security infrastructure or territorial expansion justified through security discourse.').

omega_variable(
    suppression_asymmetry_structural_vs_tactical,
    'Is the suppression of Palestinian resistance (checkpoint networks, resource control, movement restrictions) structurally necessary for security, or is it tactical control enabling political subjugation?',
    'Comparative analysis: security checkpoints whose location and operation respond to threat assessment vs. those whose location enables resource control and political leverage. Assessment of whether suppression intensity varies with actual threat level or tracks political pressure (settlement expansion, negotiation stalling). Analysis of Palestinian security forces'' capability: are they prevented from operating as an alternative security provider, or supported to provide equivalent security with less direct Israeli suppression?',
    'If suppression is structurally necessary for security, the high suppression metric (0.76) reflects genuine coordination cost. If suppression is tactical and enables extraction, the constraint reclassifies toward snare and theater-ratio climbs (performative security justifying opportunistic control). This omega determines whether extraction is ineliminable or optional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_asymmetry_structural_vs_tactical, empirical, 'Whether measured suppression is structurally necessary for security or enables extraction.').

omega_variable(
    kernel_reading_contest_structure,
    'Is the security-necessity reading a defensible position within a single international-law framework, or does commitment to this reading logically foreclose the indigenous-continuity and partition readings within the same framework?',
    'Jurisprudential analysis: can a single state''s commitment to security-based territorial legitimacy coexist with recognition of Palestinian self-determination and indigenous rights within one legal order, or does the security reading require denying the premises of the rival readings? Case-law examination: how have courts and international bodies handled conflicts between security and territorial integrity/self-determination claims?',
    'If the readings coexist (can be held simultaneously by different parties), the constraint description is accurate and the rivalry is political, not logical. If the security reading forecloses the others within a single framework, the constraint''s legitimacy in that framework is mutually exclusive with the rivals''. This determines whether the kernel is truly contested or whether one reading possesses structural dominance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Whether this reading logically forecloses, coexists with, or influences its sibling readings.').

omega_variable(
    mandate_obsolescence_occupation_decades,
    'Has the security-necessity frame outlived its founding problem? If the threat landscape has changed structurally (e.g., Palestinian state capacity remains limited, regional adversary states have normalized relations with Israel), does the original security justification still hold?',
    'Threat assessment over time: Israeli intelligence estimates, military doctrine evolution, statement of strategic objectives. Comparison of threat vectors in 1967-1973 (existential military threat from state armies) vs. 2000-present (non-state actors, asymmetric threats, changed regional alignment with Abraham Accords). If threat has structurally changed but territorial control persists unchanged, the founding problem is dead and the constraint exhibits mandatrophy.',
    'If founding problem is dead (threat no longer existential, regional alignment shifted, alternative security arrangements now viable), the constraint reclassifies as piton — maintained theatrically for political control after security function atrophied. This shifts the type from tangled_rope toward piton and the verdict from coordination-with-extraction to pure inertial extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_obsolescence_occupation_decades, empirical, 'Whether the security-necessity founding problem remains live or has become obsolete.').

omega_variable(
    indigenous_continuity_reading_foreclosure,
    'Does the security-necessity reading logically foreclose the indigenous-continuity reading, or are they genuinely coexisting positions held by different parties?',
    'Doctrinal analysis of security-based and indigenous-rights-based frameworks: can a state simultaneously recognize indigenous self-determination and claim security-based territorial legitimacy, or does one reading''s core premise logically exclude the other? Examination of precedent (e.g., international attitudes toward indigenous land claims vs. security claims in other contexts).',
    'If they foreclose each other (mutually exclusive in one framework), this is a true kernel contest where the readings cannot coexist. If they coexist as genuinely different readings held by different parties, the kernel is truly contested and the rivalry is political, not logical. This determines the reading_relations type: forecloses vs. coexists_with.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_continuity_reading_foreclosure, conceptual, 'Logical relationship between security-necessity and indigenous-continuity readings within a single framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy__security_necessity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(terr_tr_t8, territorial_legitimacy__security_necessity_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(terr_tr_t16, territorial_legitimacy__security_necessity_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(terr_tr_t24, territorial_legitimacy__security_necessity_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement(terr_tr_t32, territorial_legitimacy__security_necessity_reading, theater_ratio, 32, 0.49).
narrative_ontology:measurement(terr_tr_t40, territorial_legitimacy__security_necessity_reading, theater_ratio, 40, 0.51).
narrative_ontology:measurement(terr_tr_t48, territorial_legitimacy__security_necessity_reading, theater_ratio, 48, 0.52).
narrative_ontology:measurement(terr_tr_t56, territorial_legitimacy__security_necessity_reading, theater_ratio, 56, 0.52).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy__security_necessity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(terr_be_t8, territorial_legitimacy__security_necessity_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(terr_be_t16, territorial_legitimacy__security_necessity_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(terr_be_t24, territorial_legitimacy__security_necessity_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(terr_be_t32, territorial_legitimacy__security_necessity_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(terr_be_t40, territorial_legitimacy__security_necessity_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(terr_be_t48, territorial_legitimacy__security_necessity_reading, base_extractiveness, 48, 0.68).
narrative_ontology:measurement(terr_be_t56, territorial_legitimacy__security_necessity_reading, base_extractiveness, 56, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy__security_necessity_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(terr_su_t8, territorial_legitimacy__security_necessity_reading, suppression_requirement, 8, 0.59).
narrative_ontology:measurement(terr_su_t16, territorial_legitimacy__security_necessity_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(terr_su_t24, territorial_legitimacy__security_necessity_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(terr_su_t32, territorial_legitimacy__security_necessity_reading, suppression_requirement, 32, 0.74).
narrative_ontology:measurement(terr_su_t40, territorial_legitimacy__security_necessity_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(terr_su_t48, territorial_legitimacy__security_necessity_reading, suppression_requirement, 48, 0.76).
narrative_ontology:measurement(terr_su_t56, territorial_legitimacy__security_necessity_reading, suppression_requirement, 56, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__security_necessity_reading, 0.18).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__indigenous_continuity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, settlement_expansion_legitimacy).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, palestinian_statehood_conditionality).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, right_of_return_denial).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way kernel contest over territorial legitimacy. The security-necessity reading instantiates one coherent position within that contest; the indigenous-continuity and partition readings are separate constraint files, each with its own ε, stakeholder structure, and classification. All three are linked via network.affects_constraints because they are rival claims over the same territorial domain and beneficiary/victim sets. The ε-invariance principle requires separate files because the three readings would compute radically different ε values (security reading: high extraction justified by necessity; indigenous-continuity reading: extraction as colonial dispossession; partition reading: extraction as illegal settlement expansion). Readers should examine all three constraint files together to see how different readings of the same kernel instantiate different constraint types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy__security_necessity_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
