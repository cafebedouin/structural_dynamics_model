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
 *   territorial waters and broader maritime zones through effective
 *   occupation and administrative control. This reading is highly contested
 *   but actively pursued by certain states. The claimed type is 'snare'
 *   because it involves significant extraction from other states and requires
 *   active suppression of alternative interpretations and challenges to its
 *   legitimacy. The metrics reflect a high degree of extraction and
 *   suppression, with low theater, as the construction and assertion are
 *   concrete and militarily backed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.85).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.9).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, snare).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "Expansive Sovereignty via Artificial Island Construction (UNCLOS Reading)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, '9075cae0-fb04-4d0c-904e-960107e46610').
narrative_ontology:cs_kernel_codification('9075cae0-fb04-4d0c-904e-960107e46610', fixed_text).
narrative_ontology:cs_authority_grounding('9075cae0-fb04-4d0c-904e-960107e46610', extraction).
narrative_ontology:cs_interpretation_layer_present('9075cae0-fb04-4d0c-904e-960107e46610').
narrative_ontology:cs_reading_relation('9075cae0-fb04-4d0c-904e-960107e46610', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('9075cae0-fb04-4d0c-904e-960107e46610', unclos_maritime_sovereignty__hybrid_effective_control_reading, influences).
narrative_ontology:cs_axiom('9075cae0-fb04-4d0c-904e-960107e46610', foundational, effective_occupation_generates_sovereignty).
narrative_ontology:cs_axiom_status(effective_occupation_generates_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('9075cae0-fb04-4d0c-904e-960107e46610', effective_occupation_generates_sovereignty, conventional).
narrative_ontology:cs_axiom('9075cae0-fb04-4d0c-904e-960107e46610', foundational, artificial_features_equate_to_natural_islands).
narrative_ontology:cs_axiom_status(artificial_features_equate_to_natural_islands, holdable).
narrative_ontology:cs_axiom_grounding('9075cae0-fb04-4d0c-904e-960107e46610', artificial_features_equate_to_natural_islands, conventional).
narrative_ontology:cs_reference_frame('9075cae0-fb04-4d0c-904e-960107e46610', post_unclos_ratification_era).
narrative_ontology:cs_drift_state('9075cae0-fb04-4d0c-904e-960107e46610', contemporary_south_china_sea_disputes, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9075cae0-fb04-4d0c-904e-960107e46610', '').
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

% Actively constructs artificial islands on submerged features or low-tide elevations, asserting these constructions generate full territorial waters and exclusive economic zones. Benefits from expanded maritime claims and resource control. Faces international diplomatic pressure but continues construction and assertion.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, agenda_setter,
    institutional, generational, constrained, regional).

% Have pre-existing or overlapping maritime claims that are directly infringed upon by the expansive claims of the constructing states. Bear the cost of lost maritime space, resources, and increased geopolitical tension. Their options are diplomatic protest, legal challenge, or military deterrence, all with high costs.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states, payer,
    organized, generational, constrained, regional).

% Advocate for unimpeded passage through international waters and airspace. Bear the cost of increased restrictions on navigation and overflight in areas claimed by constructing states. Their options include diplomatic demarches, freedom of navigation operations (FONOPs), or accepting de facto restrictions.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states, payer,
    institutional, civilizational, mobile, global).

% Relies on a stable and predictable international legal order for maritime activities. Bears the cost of legal uncertainty, increased risk of conflict, and erosion of UNCLOS principles. Their options are collective diplomatic pressure or adaptation to new de facto norms.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, international_maritime_community, payer,
    moderate, generational, constrained, global).

% Are tasked with interpreting UNCLOS provisions and adjudicating disputes. Their rulings can clarify legal status but may be ignored by non-compliant states, leading to enforcement challenges. They provide an analytical seat on the legal contest.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, unclos_arbitral_tribunals, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading attempts to coordinate the expansion of maritime claims by establishing a precedent for effective occupation of constructed features, thereby reducing ambiguity for the constructing state regarding its asserted rights.
% TRANSFER_FUNCTION: Transfers maritime space, resource control, and strategic advantage from neighboring claimant states and the international community to island-constructing states.
% ABSENT_VOICES: Future generations and non-state actors dependent on open maritime access, who lack direct representation in state-centric international law, would object to the erosion of common heritage principles and the militarization of international waters.
% DISAPPEARANCE_RATIONALE: If this interpretation of UNCLOS vanished, constructing states would lose their legal justification for expansive claims, leading to a retraction of asserted territorial waters, a reduction in geopolitical tension, and a re-establishment of traditional freedom of navigation. The maritime legal order would revert to a more restrictive interpretation of island status.
% FOUNDING_PROBLEM: The problem of establishing clear and enforceable sovereignty over newly created or enhanced maritime features, particularly in contested areas, to secure national interests and resources.
% FOUNDING_PROBLEM_CORROBORATION: Island-constructing states assert the problem is live, citing national security and resource needs. Neighboring states and international legal scholars, from outside the benefiting parties, corroborate that the problem of defining maritime features is indeed live, but dispute whether this expansive construction reading is a legitimate solution or an opportunistic reinterpretation.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__expansive_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__expansive_construction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__expansive_construction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because this reading allows states to unilaterally claim vast maritime areas and resources that would otherwise be international waters or subject to other claims. Suppression is also very high (0.90) as these claims are often enforced through military presence, exclusion zones, and diplomatic pressure, actively suppressing freedom of navigation and other states' rights. Theater ratio is low (0.10) because the construction and subsequent control are real, physical acts, not merely performative. Resistance is high (0.80) due to strong opposition from neighboring states and international powers.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the island-constructing states, this reading is a legitimate exercise of sovereignty and a necessary measure for national security and resource acquisition. From the perspective of victim states and the international community, it is an aggressive, extractive reinterpretation of international law that destabilizes regional security. The engine's classification as a snare reflects the latter, more objective structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Island-constructing states are clear beneficiaries and agenda-setters, gaining territory and resources. Neighboring claimant states and freedom-of-navigation states are victims, losing access and facing increased restrictions. The international maritime community is a diffuse victim, suffering from the erosion of international law. UNCLOS arbitral tribunals act as observers, providing legal analysis but lacking direct enforcement power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_status_of_artificial_features,
    'Does international law, specifically UNCLOS, permit artificial constructions on submerged features to generate full territorial waters and EEZs, or only limited safety zones?',
    'A definitive ruling by the International Court of Justice or a universally accepted amendment to UNCLOS, or a consistent pattern of state practice and opinio juris that explicitly rejects or accepts such claims.',
    'If full territorial waters are legally rejected, this reading''s claims become pure extraction without legal cover, reclassifying it as a more severe snare or even a pure coercive mechanism. If accepted, its extractiveness might be re-evaluated as a legitimate (though still costly) coordination mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_status_of_artificial_features, conceptual, 'Ambiguity regarding the legal status of artificial islands under UNCLOS.').

omega_variable(
    effective_occupation_threshold,
    'What constitutes ''effective occupation and administrative control'' sufficient to generate de facto territorial waters, and is this threshold met by current construction activities?',
    'Independent expert assessment of the level of civilian presence, administrative functions, and military control on the artificial features, compared against historical precedents for territorial acquisition.',
    'If the threshold is not met, the claims are purely performative and lack even de facto legitimacy, increasing the theater ratio and potentially reclassifying the constraint as a piton or a more theatrical snare. If met, it strengthens the factual basis for the claims, even if their legal basis remains contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_occupation_threshold, empirical, 'Uncertainty about the factual basis for ''effective occupation'' claims.').

omega_variable(
    geopolitical_stability_vs_resource_access,
    'Is the primary driver for these expansive claims geopolitical dominance and strategic control, or legitimate access to resources (e.g., fishing, hydrocarbons)?',
    'Analysis of state policy documents, military deployments, and economic activity in the claimed areas. Comparison of resource value to strategic value.',
    'If primarily geopolitical, the extractiveness is more severe as it targets fundamental state security and international order. If primarily resource-driven, it might be framed as a more conventional (though still contested) resource allocation problem, potentially shifting the perceived balance of coordination vs. extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_stability_vs_resource_access, preference, 'Ambiguity about the underlying motivation for expansive maritime claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 1982, 0.05).
narrative_ontology:measurement(uncl_tr_t1995, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(uncl_tr_t2005, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(uncl_tr_t2015, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(uncl_tr_t2024, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 1982, 0.3).
narrative_ontology:measurement(uncl_be_t1995, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(uncl_be_t2005, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 2005, 0.7).
narrative_ontology:measurement(uncl_be_t2015, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 2015, 0.8).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 1982, 0.4).
narrative_ontology:measurement(uncl_su_t1995, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(uncl_su_t2005, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(uncl_su_t2015, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 2015, 0.85).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
