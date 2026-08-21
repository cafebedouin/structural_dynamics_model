% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__expansive_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Expansive Sovereignty via Artificial Island Construction (UNCLOS Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint represents an 'expansive construction' reading of UNCLOS
 *   maritime sovereignty, where artificial island construction on submerged
 *   features or low-tide elevations is asserted to generate de facto
 *   territorial waters and other maritime zones through effective occupation
 *   and administrative control. This reading is highly contested and leads to
 *   significant geopolitical tension, particularly in regions with
 *   overlapping claims. The constraint is framed as a snare due to its high
 *   extractiveness and reliance on active enforcement to suppress alternative
 *   interpretations and challenges.
 *
 * KEY AGENTS:
 *   - island_constructing_states: Primary beneficiary/agenda-setter (institutional/constrained)
 *   - neighboring_claimant_states: Primary payer (powerful/constrained)
 *   - freedom_of_navigation_states: Payer (institutional/mobile)
 *   - international_maritime_community: Excluded (organized/constrained)
 *   - unclos_arbitration_tribunals: Observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.85).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.78).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, snare).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "Expansive Sovereignty via Artificial Island Construction (UNCLOS Reading)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, '441e5717-2e8a-4c66-b404-28eafee6872c').
narrative_ontology:cs_kernel_codification('441e5717-2e8a-4c66-b404-28eafee6872c', formalized).
narrative_ontology:cs_authority_grounding('441e5717-2e8a-4c66-b404-28eafee6872c', extraction).
narrative_ontology:cs_interpretation_layer_present('441e5717-2e8a-4c66-b404-28eafee6872c').
narrative_ontology:cs_reading_relation('441e5717-2e8a-4c66-b404-28eafee6872c', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('441e5717-2e8a-4c66-b404-28eafee6872c', unclos_maritime_sovereignty__hybrid_effective_control_reading, coexists_with).
narrative_ontology:cs_axiom('441e5717-2e8a-4c66-b404-28eafee6872c', foundational, effective_occupation_generates_sovereignty).
narrative_ontology:cs_axiom_status(effective_occupation_generates_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('441e5717-2e8a-4c66-b404-28eafee6872c', effective_occupation_generates_sovereignty, conventional).
narrative_ontology:cs_axiom('441e5717-2e8a-4c66-b404-28eafee6872c', foundational, artificial_features_can_be_islands).
narrative_ontology:cs_axiom_status(artificial_features_can_be_islands, holdable).
narrative_ontology:cs_axiom_grounding('441e5717-2e8a-4c66-b404-28eafee6872c', artificial_features_can_be_islands, conventional).
narrative_ontology:cs_reference_frame('441e5717-2e8a-4c66-b404-28eafee6872c', state_centric_territorial_expansion).
narrative_ontology:cs_drift_state('441e5717-2e8a-4c66-b404-28eafee6872c', contemporary_geopolitical_competition, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('441e5717-2e8a-4c66-b404-28eafee6872c', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, international_maritime_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively constructs artificial islands on submerged features or low-tide elevations, asserting de facto territorial waters and exclusive economic zones around them. Benefits from expanded maritime claims and resource access. Faces international diplomatic pressure but continues construction and control.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, agenda_setter,
    institutional, generational, constrained, regional).

% Have overlapping maritime claims or are directly impacted by the expanded territorial claims of the island-constructing states. Bear the cost of lost maritime space, resources, and increased geopolitical tension. Their options are diplomatic protest, legal challenge, or military deterrence, all with high costs.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states, payer,
    powerful, generational, constrained, regional).

% Advocate for unimpeded passage through international waters and airspace. Bear the cost of increased restrictions on navigation and overflight in areas now claimed as territorial waters. Their options include 'freedom of navigation operations' (FONOPs) to challenge claims, diplomatic protests, or re-routing, which incurs economic costs.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states, payer,
    institutional, civilizational, mobile, global).

% Represents the collective interest in a stable, rules-based maritime order. Excluded from direct decision-making on these claims but bears the systemic cost of eroded international law and increased conflict risk. Can only exert pressure through international bodies and diplomatic channels.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, international_maritime_community, excluded,
    organized, generational, constrained, global).

% Are tasked with interpreting UNCLOS and adjudicating maritime disputes. Observe the unfolding claims and counter-claims, providing legal opinions and rulings when cases are brought before them. Their authority is often challenged by states that reject their jurisdiction or findings.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, unclos_arbitration_tribunals, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading attempts to coordinate maritime claims by establishing a clear, if controversial, mechanism for expanding national sovereignty through physical presence and control, reducing ambiguity for the claiming state.
% TRANSFER_FUNCTION: Transfers control over vast maritime areas, including potential resources and strategic chokepoints, from the international commons (or other claimant states) to the island-constructing state.
% ABSENT_VOICES: The international maritime community, particularly states without the capacity or intent for such construction, would object to the unilateral expansion of sovereignty and the erosion of established UNCLOS principles. They are present in international forums but lack direct enforcement power against powerful states.
% DISAPPEARANCE_RATIONALE: If this expansive reading of sovereignty via artificial construction vanished overnight, the geopolitical landscape of contested maritime regions would immediately shift. Island-constructing states would lose their asserted territorial waters, neighboring states would reclaim disputed areas, and freedom of navigation would be restored, leading to a significant re-ordering of maritime control and resource access.
% FOUNDING_PROBLEM: The problem this reading implicitly 'solves' is the desire of states to expand their sovereign control and resource access in strategically important maritime areas where natural features are scarce or insufficient to generate desired claims.
% FOUNDING_PROBLEM_CORROBORATION: The 'problem' is attested as live by the continued geopolitical competition and resource scarcity in maritime regions, driving states to seek new avenues for asserting control. This is corroborated by geopolitical analysts and international relations scholars, not just the benefiting states.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__expansive_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__expansive_construction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__expansive_construction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unclos_maritime_sovereignty__expansive_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.85) is high because this reading allows states to unilaterally claim vast areas of the global commons, denying access and resources to others. Suppression (0.78) is also high, as these claims are maintained through active military presence, administrative control, and diplomatic pressure, effectively suppressing challenges and alternative interpretations. The theater ratio (0.20) is relatively low, as the construction and control are genuinely functional for asserting sovereignty, though the legal justification is highly performative. Accessibility collapse (0.60) is moderate, as legal and diplomatic avenues for challenge exist but are costly and often ineffective against powerful states. Resistance (0.70) is high, reflecting ongoing diplomatic protests, legal challenges, and freedom of navigation operations.
 *
 * PERSPECTIVAL GAP:
 *   Island-constructing states perceive this as a legitimate exercise of sovereignty and a solution to resource and security needs, viewing the constraint as a 'rope' for national development. Neighboring claimant states and freedom-of-navigation states perceive it as a 'snare' designed for unilateral extraction and a violation of international law. The engine's classification will reflect the latter due to the high extractiveness and suppression metrics, despite the claiming state's narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   Island-constructing states are clear beneficiaries, gaining expanded maritime territory and resources. Neighboring claimant states and freedom-of-navigation states are targets, losing access and facing increased costs. The international maritime community is broadly a target, as the rules-based order is undermined. UNCLOS tribunals are analytical observers, attempting to apply existing law to a novel and contested situation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its 'mandate' (expanding sovereignty) is actively pursued by its beneficiaries. The classification as a snare prevents mislabeling it as a legitimate coordination mechanism (rope) or a temporary support (scaffold), which would obscure its extractive and coercive nature. The rising extractiveness and suppression over time indicate an enforcement ratchet, not a decaying function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_status_of_artificial_features,
    'Does international law, specifically UNCLOS, permit artificial constructions on submerged features or low-tide elevations to generate territorial waters or other maritime zones?',
    'A definitive ruling by the International Court of Justice or an UNCLOS arbitration tribunal that is universally accepted and enforced, or a new international treaty explicitly clarifying the status of such features.',
    'If ruled impermissible, this reading''s claims would be legally invalid, significantly reducing its extractiveness and suppression. If ruled permissible, it would legitimize the claims, potentially reducing resistance but increasing effective extraction for non-claiming states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_status_of_artificial_features, conceptual, 'Ambiguity in UNCLOS regarding the legal status of artificial islands for generating maritime zones.').

omega_variable(
    effective_control_threshold,
    'What level of ''effective occupation and administrative control'' is required for an artificial feature to generate de facto territorial waters, and is this threshold met by current practices?',
    'Development of clear, internationally recognized criteria for ''effective control'' in this context, and independent verification of whether claiming states meet these criteria.',
    'If the threshold is high and not met, the claims would be weakened. If the threshold is low or easily met, it would further legitimize the expansive reading, increasing its perceived naturalness for claiming states.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(effective_control_threshold, empirical, 'Uncertainty regarding the criteria and verification of ''effective control'' for artificial features.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (military presence, legal threats) or internalized (fear of retaliation, diplomatic isolation)?',
    'Post-challenge trajectory: if challenges persist and escalate despite structural barriers, reclassify as partially internalized. If challenges are effectively deterred by structural means, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — targets carry the suppression with them after exit attempts. If purely structural, removing the external barriers would immediately reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for challenging maritime claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(uncl_tr_t1995, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(uncl_tr_t2005, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(uncl_tr_t2015, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(uncl_tr_t2024, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 1982, 0.4).
narrative_ontology:measurement(uncl_be_t1995, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(uncl_be_t2005, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 2005, 0.75).
narrative_ontology:measurement(uncl_be_t2015, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 2015, 0.82).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 1982, 0.3).
narrative_ontology:measurement(uncl_su_t1995, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(uncl_su_t2005, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement(uncl_su_t2015, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__expansive_construction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the UNCLOS maritime sovereignty kernel. Its expansive interpretation directly influences and is in tension with the strict geographic and hybrid effective control readings, as well as the broader freedom of navigation doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
