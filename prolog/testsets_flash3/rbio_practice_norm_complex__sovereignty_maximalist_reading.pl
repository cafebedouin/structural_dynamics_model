% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__sovereignty_maximalist_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: RBIO Practice Norm Complex: Sovereignty Maximalist Reading
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty maximalist' reading of the
 *   RBIO (Rules-Based International Order) practice norm complex. It asserts
 *   that state sovereignty is absolute, and any international norms,
 *   particularly those related to humanitarian intervention, are legitimate
 *   only if they protect this absolute sovereignty against external
 *   interference. Humanitarian exceptions are viewed as pretexts for regime
 *   change. This reading is a snare, as it primarily serves to protect
 *   authoritarian regimes from accountability, extracting the right to
 *   self-determination from their populations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.85).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.9).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, snare).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "RBIO Practice Norm Complex: Sovereignty Maximalist Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, '5dffdc40-48e3-4d59-9986-26daa5668040').
narrative_ontology:cs_kernel_codification('5dffdc40-48e3-4d59-9986-26daa5668040', formalized).
narrative_ontology:cs_authority_grounding('5dffdc40-48e3-4d59-9986-26daa5668040', extraction).
narrative_ontology:cs_interpretation_layer_present('5dffdc40-48e3-4d59-9986-26daa5668040').
narrative_ontology:cs_reading_relation('5dffdc40-48e3-4d59-9986-26daa5668040', rbio_practice_norm_complex__liberal_institutional_reading, forecloses).
narrative_ontology:cs_reading_relation('5dffdc40-48e3-4d59-9986-26daa5668040', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('5dffdc40-48e3-4d59-9986-26daa5668040', foundational, absolute_state_sovereignty).
narrative_ontology:cs_axiom_status(absolute_state_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('5dffdc40-48e3-4d59-9986-26daa5668040', absolute_state_sovereignty, conventional).
narrative_ontology:cs_axiom('5dffdc40-48e3-4d59-9986-26daa5668040', foundational, humanitarian_intervention_as_regime_change_pretext).
narrative_ontology:cs_axiom_status(humanitarian_intervention_as_regime_change_pretext, holdable).
narrative_ontology:cs_axiom_grounding('5dffdc40-48e3-4d59-9986-26daa5668040', humanitarian_intervention_as_regime_change_pretext, instrumental).
narrative_ontology:cs_reference_frame('5dffdc40-48e3-4d59-9986-26daa5668040', westphalian_sovereignty_model).
narrative_ontology:cs_drift_state('5dffdc40-48e3-4d59-9986-26daa5668040', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5dffdc40-48e3-4d59-9986-26daa5668040', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, state_elites).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repression).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These regimes benefit from the absolute sovereignty principle, which shields them from external interference regarding internal human rights abuses. They actively invoke this reading to deflect criticism and prevent intervention, ensuring their survival and control.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes, beneficiary,
    institutional, generational, arbitrage, national).

% The ruling elites within authoritarian states directly benefit from the non-interference norm, which protects their power and wealth from international accountability. Their personal security and continued governance are tied to this interpretation of sovereignty.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, state_elites, beneficiary,
    powerful, biographical, constrained, national).

% These populations bear the direct costs of this constraint, as their human rights are violated without external recourse. They are trapped within their states, with no legitimate international mechanism for protection against their own governments.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repression, payer,
    powerless, immediate, trapped, national).

% Advocates for human rights find their efforts to protect vulnerable populations severely hampered by this maximalist interpretation of sovereignty. Their calls for intervention or accountability are dismissed as illegitimate interference, making their work largely performative without real-world impact.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, human_rights_advocates, payer,
    moderate, generational, constrained, global).

% States that champion human rights and multilateral intervention find their normative framework undermined by this reading. Their attempts to establish humanitarian exceptions are framed as pretexts for regime change, eroding their moral authority and limiting their policy options.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_democracies, excluded,
    institutional, generational, constrained, global).

% Scholars analyze the historical evolution and contemporary application of sovereignty norms, documenting how different readings serve various state interests. They observe the strategic deployment of the maximalist reading to protect state power.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It coordinates state behavior by establishing a clear, albeit rigid, boundary against external interference in internal affairs, theoretically preventing endless conflicts over domestic governance.
% TRANSFER_FUNCTION: It transfers absolute authority over internal affairs from international norms or external actors to the sovereign state, effectively transferring the right to self-determination from populations to their ruling regimes.
% ABSENT_VOICES: Populations suffering under repressive regimes are the primary absent voices; they would argue for a more conditional sovereignty that prioritizes human security. Their voices are suppressed by the very state structures that benefit from this reading.
% DISAPPEARANCE_RATIONALE: If this maximalist reading of sovereignty vanished, it would fundamentally alter international relations. Authoritarian regimes would lose their primary shield against intervention, leading to increased international pressure and potentially direct action. The global human rights landscape would be dramatically reshaped, with new mechanisms for accountability emerging.
% FOUNDING_PROBLEM: The original problem was to prevent endless wars and interventions by establishing clear boundaries of state jurisdiction and mutual non-interference after the Treaty of Westphalia.
% FOUNDING_PROBLEM_CORROBORATION: Authoritarian regimes and their allies attest that the problem of external interference remains live, citing historical examples of colonialism and neo-colonialism. Liberal democracies and human rights organizations argue that the original problem has been superseded by the need to protect human rights, and that the maximalist reading now serves to perpetuate repression; this is corroborated by UN reports and NGO documentation of human rights abuses.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__sovereignty_maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__sovereignty_maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the cost borne by populations trapped under repressive governments, who are denied external recourse. Suppression (0.90) is extremely high because the norm actively shields regimes from intervention, effectively suppressing any challenge to their internal authority. The theater ratio (0.60) indicates that while the rhetoric of non-interference is presented as a principle of international stability, a significant portion of its function is to provide cover for internal repression. The rising trend in extractiveness and suppression over time reflects the increasing strategic deployment of this reading by states seeking to avoid accountability.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of authoritarian regimes, this reading is a legitimate defense of national self-determination and stability. From the perspective of repressed populations and human rights advocates, it is a coercive mechanism that enables severe human rights abuses. The engine's classification as a snare captures this fundamental divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes and state elites are clear beneficiaries, as the constraint directly protects their power and allows them to act with impunity internally. Populations under repression and human rights advocates are the primary victims, bearing the costs of unchecked state power. Liberal democracies are excluded, as their attempts to promote alternative readings are delegitimized. International law scholars act as observers, analyzing the structural dynamics.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_humanitarian_intervention,
    'Is humanitarian intervention a legitimate exception to state sovereignty, or is it inherently a pretext for regime change?',
    'Empirical analysis of past interventions: assessing whether interventions genuinely improved human rights without ulterior motives, or consistently led to destabilization and regime change.',
    'If proven to be consistently a pretext, it strengthens the maximalist reading. If proven to genuinely protect populations, it weakens this reading and strengthens alternative interpretations of RBIO norms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_humanitarian_intervention, empirical, 'The core dispute over the nature and intent of humanitarian intervention.').

omega_variable(
    state_vs_population_sovereignty,
    'Does sovereignty reside primarily with the state apparatus or with the population it governs?',
    'Conceptual clarification and normative debate within international law and political philosophy, potentially influenced by evolving global consensus on human rights.',
    'If sovereignty is understood to reside with the population, the maximalist reading''s foundation is undermined, as the state''s legitimacy would derive from its protection of its people, not its absolute power over them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_vs_population_sovereignty, conceptual, 'The foundational conceptual ambiguity of where sovereignty ultimately resides.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external barriers) or internalized (cognitive patterns)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., a regime falls but populations remain unable to assert rights due to ingrained fear or lack of civic infrastructure), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making genuine liberation harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for populations under repression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(rbio_tr_t1970, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(rbio_tr_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1990, 0.45).
narrative_ontology:measurement(rbio_tr_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2005, 0.55).
narrative_ontology:measurement(rbio_tr_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2015, 0.58).
narrative_ontology:measurement(rbio_tr_t2024, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1945, 0.6).
narrative_ontology:measurement(rbio_be_t1970, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(rbio_be_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(rbio_be_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2005, 0.82).
narrative_ontology:measurement(rbio_be_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2015, 0.84).
narrative_ontology:measurement(rbio_be_t2024, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(rbio_su_t1970, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(rbio_su_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(rbio_su_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2005, 0.85).
narrative_ontology:measurement(rbio_su_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2015, 0.88).
narrative_ontology:measurement(rbio_su_t2024, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the RBIO practice norm complex. It represents the sovereignty maximalist interpretation, which directly influences and is influenced by the liberal institutional and hegemonic extraction readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
