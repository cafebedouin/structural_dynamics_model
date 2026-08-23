% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__expansive_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__expansive_construction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: unclos_maritime_sovereignty__expansive_construction_reading
 *   human_readable: Expansive Construction Reading of UNCLOS Maritime Sovereignty
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint story captures the expansive construction reading of
 *   UNCLOS maritime sovereignty: the claim that artificial islands built on
 *   submerged features or low-tide elevations generate full territorial seas
 *   (12nm) and potentially EEZs through effective occupation and
 *   administrative control. The reading is advanced primarily by states
 *   engaged in large-scale island construction (notably in the South China
 *   Sea) to convert maritime space into sovereign territory. The constraint
 *   coordinates by offering a clear rule (build and administer -> get waters)
 *   but extracts asymmetrically: constructing states gain maritime zones,
 *   while neighboring claimants lose overlapping claims and
 *   freedom-of-navigation states face restricted passage. Enforcement is
 *   active (coast guard patrols, administrative agencies, military backing).
 *   The claimed type is tangled_rope because the reading presents as a
 *   coordination mechanism (clarifying status of constructed features) while
 *   functioning as extraction. The metrics show rising extractiveness and
 *   suppression over the 2010-2026 period, with increasing theater as legal
 *   justifications elaborate while substantive coordination declines.
 *
 * KEY AGENTS:
 *   - island_constructing_states: Primary agenda_setter and beneficiary (institutional/biographical/trapped) — constructs islands, administers them, claims maritime zones
 *   - neighboring_claimant_states: Primary payer/victim (powerful/biographical/constrained) — lose traditional fishing grounds, EEZ claims, and strategic space
 *   - freedom_of_navigation_states: Payer/victim (institutional/biographical/mobile) — face restricted passage, must conduct FONOPs to challenge excessive claims
 *   - international_tribunals: Observer (analytical/generational/analytical) — interpret UNCLOS, issue rulings that may validate or reject the reading
 *   - commercial_shipping: Payer (organized/biographical/constrained) — bears increased transit costs, routing uncertainty, insurance premiums
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.72).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.78).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "Expansive Construction Reading of UNCLOS Maritime Sovereignty").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, 'f9ac3d29-66d2-47b3-84e0-bf2b376d9404').
narrative_ontology:cs_kernel_codification('f9ac3d29-66d2-47b3-84e0-bf2b376d9404', formalized).
narrative_ontology:cs_authority_grounding('f9ac3d29-66d2-47b3-84e0-bf2b376d9404', extraction).
narrative_ontology:cs_interpretation_layer_present('f9ac3d29-66d2-47b3-84e0-bf2b376d9404').
narrative_ontology:cs_reading_relation('f9ac3d29-66d2-47b3-84e0-bf2b376d9404', unclos_maritime_sovereignty__strict_geographic_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9ac3d29-66d2-47b3-84e0-bf2b376d9404', unclos_maritime_sovereignty__hybrid_effective_control_reading, coexists_with).
narrative_ontology:cs_axiom('f9ac3d29-66d2-47b3-84e0-bf2b376d9404', foundational, constructed_features_generate_territorial_sea).
narrative_ontology:cs_axiom_status(constructed_features_generate_territorial_sea, holdable).
narrative_ontology:cs_axiom_grounding('f9ac3d29-66d2-47b3-84e0-bf2b376d9404', constructed_features_generate_territorial_sea, conventional).
narrative_ontology:cs_axiom('f9ac3d29-66d2-47b3-84e0-bf2b376d9404', foundational, effective_occupation_creates_sovereignty).
narrative_ontology:cs_axiom_status(effective_occupation_creates_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('f9ac3d29-66d2-47b3-84e0-bf2b376d9404', effective_occupation_creates_sovereignty, conventional).
narrative_ontology:cs_reference_frame('f9ac3d29-66d2-47b3-84e0-bf2b376d9404', historic_state_practice).
narrative_ontology:cs_drift_state('f9ac3d29-66d2-47b3-84e0-bf2b376d9404', contemporary_artificial_island_construction, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f9ac3d29-66d2-47b3-84e0-bf2b376d9404', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, commercial_shipping).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__expansive_construction_reading, effective_occupation_creates_sovereignty).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__expansive_construction_reading, state_practice_modifies_treaty_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invest heavily in dredging, construction, and administration of artificial islands. They enact domestic laws claiming territorial seas, deploy coast guard and militia to enforce, and integrate features into military logistics. Exit would mean abandoning massive sunk costs and strategic position; they are trapped by their own investment.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, agenda_setter,
    institutional, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, beneficiary).

% Lose access to traditional fishing grounds, hydrocarbon resources, and strategic maritime space. They can protest, arbitrate, or confront, but each option carries high cost: arbitration is slow and unenforceable, confrontation risks escalation, acquiescence loses rights permanently. Exit from the dispute is constrained by domestic politics and alliance commitments.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states, payer,
    powerful, biographical, constrained, regional).

% Conduct Freedom of Navigation Operations (FONOPs) to challenge excessive claims. They have global reach and can route around contested areas, but at increased cost and risk. Their mobile exit option is real but imperfect: rerouting affects global trade efficiency, and FONOPs require sustained political will.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states, payer,
    institutional, biographical, mobile, global).

% Interpret UNCLOS provisions on artificial islands (Articles 60, 80, 121). Their rulings (e.g., Philippines v. China 2016) have rejected expansive readings but lack enforcement power. They neither collect benefits nor bear costs; their authority depends on state compliance.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, international_tribunals, observer,
    analytical, generational, analytical, universal).

% Face increased insurance premiums, routing delays, and compliance costs from competing regulatory demands. They cannot exit the maritime domain but can adjust routes. Their constrained exit reflects dependence on major trade lanes that pass through contested waters.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, commercial_shipping, payer,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__expansive_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, actionable rule for converting constructed features into recognized maritime zones, reducing ambiguity in contested waters where natural features are scarce.
% TRANSFER_FUNCTION: Transfers maritime space (territorial sea, EEZ, continental shelf rights) and associated resources (fish, hydrocarbons, seabed minerals) from the global commons and neighboring claimants to the island-constructing states.
% ABSENT_VOICES: Small island developing states not party to the dispute but affected by precedent; indigenous fishing communities displaced by construction; future generations who inherit a partitioned ocean. They are absent from the negotiation table because they lack standing in the current state-centric framework.
% DISAPPEARANCE_RATIONALE: If the expansive reading vanished overnight, constructing states would lose legal basis for claimed zones, neighboring states would reassert overlapping claims, FONOP states would cease challenges, and commercial shipping would revert to UNCLOS baseline rules. The maritime order would reorganize around natural features and agreed boundaries.
% FOUNDING_PROBLEM: Post-WWII decolonization and resource competition created overlapping claims in feature-poor seas; states needed a way to secure maritime space without waiting for natural island formation.
% FOUNDING_PROBLEM_CORROBORATION: Constructing states attest the problem is live (ongoing resource competition, security needs). Neighboring claimants and the 2016 Philippines v. China arbitral tribunal attest the problem was solved by UNCLOS's comprehensive regime and the reading is now extraction. Independent legal scholars (e.g., Rothwell, Schofield) corroborate that UNCLOS intended Article 121 to limit, not expand, artificial feature effects.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__expansive_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__expansive_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__expansive_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the reading converts global commons (high seas, international seabed) into exclusive state control without compensation. Suppression (0.78) is high because the constraint's persistence depends on continuous naval/coast guard presence and administrative enforcement that physically prevents other states' access. Theater ratio (0.38) is moderate: the legal apparatus (domestic laws, white papers, arbitration submissions) performs coordination but increasingly serves to legitimize extraction. Accessibility collapse (0.62) reflects that alternative interpretations (strict, hybrid) remain legally available but are practically foreclosed by facts on the ground. Resistance (0.55) is moderate: FONOPs, arbitral proceedings, and diplomatic protests exist but have not rolled back established facts.
 *
 * PERSPECTIVAL GAP:
 *   From the constructing states' seat, the constraint is a rope: they built it, they administer it, it provides order. From neighboring claimants' seat, it is a snare: their rights are extinguished by force majeure. From freedom-of-navigation states' seat, it is a tangled_rope: some coordination (safety zones) is real but overwhelmed by excessive claims. The engine computes these per-seat types from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Island-constructing states are structural beneficiaries (d ~ 0.15): they initiate the constraint, control enforcement, and capture the maritime space. Neighboring claimants are targets (d ~ 0.85): they lose pre-existing claims and face coercion if they resist. Freedom-of-navigation states are near-target (d ~ 0.75): they retain high-seas rights in theory but face practical obstruction. Commercial shipping is target (d ~ 0.7): they pay increased costs. International tribunals are analytical (d = 0.5): they observe but do not bear costs or collect benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (orderly maritime claims in contested waters) is contested: constructing states argue it remains live; neighbors and tribunals argue it has been solved by UNCLOS and the reading is now pure extraction. The reading's persistence despite adverse rulings (e.g., Philippines v. China) suggests mandatrophy is unresolved — the constraint continues because the constructing states have the power to maintain it, not because the coordination need persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the expansive construction reading a legitimate interpretation of UNCLOS Article 60 and 80, or a strategic reinterpretation that serves the extracting states?',
    'Authoritative interpretation by an international tribunal (ITLOS or ICJ) addressing artificial islands on submerged features, or subsequent state practice crystallizing into customary law.',
    'If legitimate interpretation, the constraint is a rope (coordination of maritime claims). If strategic reinterpretation, it is a snare/tangled_rope (extraction masked as law).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the reading reflects law or power.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of neighboring claims and navigation rights structural (naval enforcement, administrative control) or internalized (other states acquiescing to avoid conflict)?',
    'Track compliance behavior of victim states: if they challenge but are blocked by force, suppression is structural; if they cease challenging without direct coercion, internalized component exists.',
    'If internalized, effective suppression is higher than measurable enforcement; the constraint persists even with reduced naval presence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in maritime disputes.').

omega_variable(
    mandatrophy_status,
    'Does the expansive reading still serve its claimed coordination function (orderly maritime claims), or has it become pure extraction of maritime space?',
    'Assess whether new artificial island construction correlates with genuine resource/security needs or with strategic expansion beyond need.',
    'If coordination function atrophied, constraint drifts toward snare/piton; if live, remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_status, conceptual, 'Whether the constraint''s founding problem remains live.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(uncl_tr_t4, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(uncl_tr_t8, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(uncl_tr_t12, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(uncl_tr_t16, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 16, 0.38).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(uncl_be_t4, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(uncl_be_t8, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(uncl_be_t12, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(uncl_be_t16, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 16, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(uncl_su_t4, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(uncl_su_t8, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(uncl_su_t12, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 12, 0.74).
narrative_ontology:measurement(uncl_su_t16, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 16, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__expansive_construction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__expansive_construction_reading, 0.12).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, south_china_sea_nine_dash_line).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, arctic_continental_shelf_claims).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, mediterranean_eez_delimitation).

% DUAL FORMULATION NOTE:
% Part of the UNCLOS maritime sovereignty kernel family. The expansive reading claims artificial islands generate full territorial sea; strict reading denies this; hybrid reading allows maturation through prolonged effective control. The three readings form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__expansive_construction_reading, institutional, 0.15).
constraint_indexing:directionality_override(unclos_maritime_sovereignty__expansive_construction_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
