% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__exogenous_override_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: State Decree as Sufficient for Practice Displacement (Exogenous Override Reading)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint is the 'exogenous_override_reading' of the
 *   'legitimacy_of_imposed_practice' kernel. It asserts that state decree is
 *   sufficient to displace prior practice, with compliance following from
 *   legal mandate regardless of internalization. Sibling readings include
 *   'endogenous_climb_reading' (requiring internalization) and
 *   'hybrid_scaffolding_reading' (requiring reinforcement). This reading
 *   emphasizes the state's sovereign right to impose its will, viewing
 *   resistance as illegitimate and compliance as a matter of legal
 *   obligation, not social acceptance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.85).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.92).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, snare).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "State Decree as Sufficient for Practice Displacement (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, 'c08a68a3-1d76-49b3-ae12-d38ac979d954').
narrative_ontology:cs_kernel_codification('c08a68a3-1d76-49b3-ae12-d38ac979d954', formalized).
narrative_ontology:cs_authority_grounding('c08a68a3-1d76-49b3-ae12-d38ac979d954', extraction).
narrative_ontology:cs_interpretation_layer_present('c08a68a3-1d76-49b3-ae12-d38ac979d954').
narrative_ontology:cs_reading_relation('c08a68a3-1d76-49b3-ae12-d38ac979d954', legitimacy_of_imposed_practice__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('c08a68a3-1d76-49b3-ae12-d38ac979d954', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, forecloses).
narrative_ontology:cs_axiom('c08a68a3-1d76-49b3-ae12-d38ac979d954', foundational, state_sovereignty_is_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('c08a68a3-1d76-49b3-ae12-d38ac979d954', state_sovereignty_is_absolute, deontological).
narrative_ontology:cs_axiom('c08a68a3-1d76-49b3-ae12-d38ac979d954', foundational, legal_mandate_compels_compliance).
narrative_ontology:cs_axiom_status(legal_mandate_compels_compliance, holdable).
narrative_ontology:cs_axiom_grounding('c08a68a3-1d76-49b3-ae12-d38ac979d954', legal_mandate_compels_compliance, conventional).
narrative_ontology:cs_reference_frame('c08a68a3-1d76-49b3-ae12-d38ac979d954', state_sovereignty_and_modernization).
narrative_ontology:cs_drift_state('c08a68a3-1d76-49b3-ae12-d38ac979d954', post_decree_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c08a68a3-1d76-49b3-ae12-d38ac979d954', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_apparatus).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central government and its administrative bodies. It issues decrees, enforces legal mandates, and benefits from the consolidation of power, standardization, and the resources freed by displacing prior practices. It views its authority as absolute and necessary for modernization.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Local communities whose traditional practices (e.g., agricultural calendars, dress codes, social customs) are directly targeted by state decrees. They bear the adjustment costs, face coercive enforcement, and often resort to non-compliance or practical workarounds due to lack of internalization.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations, payer,
    powerless, biographical, trapped, local).

% Local leaders, religious figures, or landholders who previously held authority and influence through the traditional practices now being displaced. They lose status, power, and sometimes economic benefits. Their options are to resist, adapt performatively, or find ways to circumvent the decrees.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_elites, payer,
    powerful, generational, constrained, regional).

% The abstract goal of national development, integration, and progress that the state apparatus claims to serve. It is a non-agent entity that benefits conceptually from the displacement of 'backward' practices.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda).

% Academics, NGOs, or other states that analyze the state's actions, often from a human rights or development perspective. They document the impact of decrees and enforcement on local populations but have limited direct power to alter the constraint.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, international_observers, observer,
    analytical, immediate, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__exogenous_override_reading, state_apparatus).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To centralize authority, standardize practices across a diverse territory, and align local activities with a national modernization agenda, thereby creating a unified state identity and administrative efficiency.
% TRANSFER_FUNCTION: Transfers authority, legitimacy, and resources from traditional local structures to the central state. It imposes significant adjustment costs, cultural disruption, and loss of autonomy on rural populations and traditional elites.
% ABSENT_VOICES: Local community representatives, traditional leaders, and cultural preservationists whose practices are being forcibly displaced. Their perspectives on the value and function of traditional ways are ignored in favor of the state's top-down vision.
% DISAPPEARANCE_RATIONALE: If the state's authority to impose practices vanished, its legitimacy and control would collapse. Traditional practices would likely re-emerge, local power structures might reassert themselves, and the national modernization agenda would fragment, leading to a fundamental reorganization of social and political order.
% FOUNDING_PROBLEM: Fragmented authority, diverse local practices, and perceived 'backwardness' hindering national integration, economic development, and the establishment of a strong, unified state.
% FOUNDING_PROBLEM_CORROBORATION: The state apparatus and its official historians consistently corroborate the founding problem as live and critical. However, independent anthropologists, local historians, and the affected populations themselves often contest this, viewing the 'problem' as a pretext for control and resource extraction, with corroboration from external academic studies and historical records.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very high (0.85) because the state directly benefits from the displacement of prior practices (e.g., land reform, resource control, labor mobilization) and imposes significant costs on affected populations without their consent. Suppression is extremely high (0.92) as compliance is enforced through legal mandates backed by coercive state power, actively suppressing alternatives and resistance. Theater ratio is moderate (0.40) because while the state claims full compliance, there is often performative adherence and widespread practical workarounds at the local level, which the state either ignores or cannot fully eradicate. Accessibility collapse is high (0.75) as legal mandates aim to eliminate alternatives, but resistance remains substantial (0.68) due to the deep-seated nature of traditional practices.
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus's perspective, this constraint is a necessary tool for modernization and national unity, a legitimate exercise of sovereignty. From the perspective of rural populations and traditional elites, it is an illegitimate imposition, a source of extraction and cultural destruction. The engine's classification as a Snare captures this divergence, highlighting the coercive and extractive nature despite the state's claims of coordination for progress.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus is the clear beneficiary (d near 0.0) as it consolidates power and resources. Rural populations and traditional elites are the primary targets (d near 1.0), bearing the direct costs of displacement and coercive enforcement. The 'state_modernization_agenda' is a conceptual beneficiary, representing the ideological justification for the state's actions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (national modernization, unity) is still claimed as live by the state, but its implementation relies on high extraction and suppression. The classification as a Snare prevents mislabeling this as a legitimate coordination mechanism (Rope or Tangled Rope) by highlighting the coercive displacement and identifiable victims, even if the state frames it as a necessary step for progress. The high theater ratio also indicates that the claimed function is not fully realized in practice, with performative compliance masking underlying resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_vs_compliance,
    'To what extent does observed compliance reflect genuine internalization of new practices versus mere performative adherence or fear of reprisal?',
    'Longitudinal ethnographic studies, analysis of informal social structures, and post-enforcement surveys to distinguish between behavioral change driven by conviction and that driven by coercion.',
    'If compliance is largely performative, the constraint''s effective suppression is higher and its long-term stability lower than official reports suggest, potentially leading to reclassification towards a more brittle Snare or Piton if enforcement costs become unsustainable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_vs_compliance, empirical, 'Distinguishing genuine adoption from coerced behavior.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the state''s authority to displace prior practice derived from an inherent right to rule (deontological) or from its capacity to enforce its will (conventional/instrumental)?',
    'Analysis of state foundational documents, legal philosophy, and historical narratives. If the state''s legitimacy is primarily asserted through force, it supports a more extractive classification.',
    'If authority is primarily grounded in coercive capacity, the constraint''s legitimacy is more fragile and dependent on continuous high suppression, reinforcing its Snare classification. If it genuinely rests on a widely accepted deontological claim, it might lean towards a more stable, albeit still extractive, Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'The philosophical grounding of state authority.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, physical force) or internalized (fear, self-censorship, identity fusion with the state)?',
    'Post-decree-relaxation studies: if non-compliance or prior practices re-emerge quickly, suppression was largely structural. If new practices persist, some internalization occurred.',
    'If suppression is significantly internalized, the constraint''s effective suppression is higher and more resilient than structural measures alone suggest, making exit harder even if formal enforcement weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 1900, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1900, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(legi_tr_t1910, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1910, 0.28).
narrative_ontology:measurement(legi_tr_t1920, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1920, 0.35).
narrative_ontology:measurement(legi_tr_t1930, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1930, 0.38).
narrative_ontology:measurement(legi_tr_t1940, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1940, 0.39).
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1950, 0.4).

% Extraction over time
narrative_ontology:measurement(legi_be_t1900, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1900, 0.7).
narrative_ontology:measurement(legi_be_t1910, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1910, 0.75).
narrative_ontology:measurement(legi_be_t1920, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1920, 0.8).
narrative_ontology:measurement(legi_be_t1930, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1930, 0.83).
narrative_ontology:measurement(legi_be_t1940, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1940, 0.84).
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1950, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1900, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1900, 0.75).
narrative_ontology:measurement(legi_su_t1910, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1910, 0.8).
narrative_ontology:measurement(legi_su_t1920, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1920, 0.85).
narrative_ontology:measurement(legi_su_t1930, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1930, 0.89).
narrative_ontology:measurement(legi_su_t1940, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1940, 0.91).
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1950, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
