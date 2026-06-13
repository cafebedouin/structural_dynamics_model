% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__performative_tool_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__performative_tool_reading, []).

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
 *   constraint_id: doomsday_clock_metric__performative_tool_reading
 *   human_readable: Doomsday Clock as Performative Policy Tool
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   This constraint story models the Doomsday Clock as a performative tool,
 *   where its setting is strategically chosen to maximize policy impact and
 *   mobilize collective action, rather than solely reflecting an objective
 *   risk assessment. The Bulletin of the Atomic Scientists, as the
 *   agenda-setter, uses the Clock to drive advocacy, benefiting policy
 *   activists and risk advocacy organizations. The cost is borne by the
 *   epistemic credibility of science and public trust in expert assessments,
 *   which are victims of this strategic manipulation. The constraint is
 *   claimed as a Tangled Rope because it serves a genuine coordination
 *   function (mobilizing action) but does so through an extractive mechanism
 *   (sacrificing epistemic rigor for impact).
 *
 * KEY AGENTS:
 *   - bulletin_of_atomic_scientists: Agenda-setter (institutional/constrained) — administers the Clock, balances scientific input with communication strategy.
 *   - policy_activists: Beneficiary (organized/mobile) — leverage the Clock for advocacy and mobilization.
 *   - risk_advocacy_organizations: Beneficiary (organized/mobile) — use the Clock to validate warnings and secure resources.
 *   - epistemic_credibility_of_science: Victim (analytical/identity_locked) — suffers erosion when the Clock's setting is perceived as non-objective.
 *   - public_trust_in_expert_assessments: Victim (powerless/constrained) — diminishes with perceived political motivation of the Clock.
 *   - scientific_community: Payer (institutional/constrained) — bears diffuse costs of reduced public trust in science.
 *   - skeptical_public: Excluded (powerless/mobile) — dismisses warnings due to perceived bias, not part of the setting process.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, 0.65).
domain_priors:suppression_score(doomsday_clock_metric__performative_tool_reading, 0.4).
domain_priors:theater_ratio(doomsday_clock_metric__performative_tool_reading, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Doomsday Clock as Performative Policy Tool").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__performative_tool_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, '17bc641b-b3e0-4ab3-9774-c43f7b55c94a').
narrative_ontology:cs_kernel_codification('17bc641b-b3e0-4ab3-9774-c43f7b55c94a', formalized).
narrative_ontology:cs_authority_grounding('17bc641b-b3e0-4ab3-9774-c43f7b55c94a', lineage).
narrative_ontology:cs_interpretation_layer_present('17bc641b-b3e0-4ab3-9774-c43f7b55c94a').
narrative_ontology:cs_reading_relation('17bc641b-b3e0-4ab3-9774-c43f7b55c94a', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('17bc641b-b3e0-4ab3-9774-c43f7b55c94a', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('17bc641b-b3e0-4ab3-9774-c43f7b55c94a', foundational, policy_impact_trumps_pure_objectivity).
narrative_ontology:cs_axiom_status(policy_impact_trumps_pure_objectivity, holdable).
narrative_ontology:cs_axiom_grounding('17bc641b-b3e0-4ab3-9774-c43f7b55c94a', policy_impact_trumps_pure_objectivity, instrumental).
narrative_ontology:cs_axiom('17bc641b-b3e0-4ab3-9774-c43f7b55c94a', secondary, symbolic_action_mobilizes_collective_will).
narrative_ontology:cs_axiom_status(symbolic_action_mobilizes_collective_will, holdable).
narrative_ontology:cs_axiom_grounding('17bc641b-b3e0-4ab3-9774-c43f7b55c94a', symbolic_action_mobilizes_collective_will, empirically_contingent).
narrative_ontology:cs_reference_frame('17bc641b-b3e0-4ab3-9774-c43f7b55c94a', strategic_communication_for_impact).
narrative_ontology:cs_drift_state('17bc641b-b3e0-4ab3-9774-c43f7b55c94a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('17bc641b-b3e0-4ab3-9774-c43f7b55c94a', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, policy_activists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, risk_advocacy_organizations).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, epistemic_credibility_of_science).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, public_trust_in_expert_assessments).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(doomsday_clock_metric__performative_tool_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__performative_tool_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__performative_tool_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the cost to epistemic credibility and public trust, which is 'extracted' to achieve policy impact. Suppression (0.4) is moderate; while there's no direct coercion, the narrative power of the Clock can suppress alternative, more nuanced risk assessments. Theater ratio (0.75) is high because the performative aspect (dramatic pronouncements, symbolic 'minutes to midnight') often outweighs the transparent, objective reporting of scientific data. Accessibility collapse (0.2) is low as alternative risk assessment frameworks exist, but the Clock's prominence can overshadow them. Resistance (0.3) is also low, primarily from academic critics rather than organized opposition.
 *
 * PERSPECTIVAL GAP:
 *   The Bulletin of the Atomic Scientists (agenda-setter) experiences this as a necessary, if imperfect, tool for global coordination and impact. Policy activists and advocacy organizations (beneficiaries) see it as a highly effective instrument. However, the scientific community and the abstract 'epistemic credibility of science' (victims/payers) experience it as a drain on their long-term standing, even if individual scientists support its goals. The engine should compute a beneficial classification for the agenda-setter and activists, but an extractive one for the victims.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin, policy activists, and advocacy organizations are beneficiaries (low d) because the Clock's strategic use directly serves their goals of influence and mobilization. Epistemic credibility and public trust are victims (high d) because their value is diminished by the strategic, non-objective framing. The scientific community is a payer (moderate d) due to the diffuse costs of potential reputational damage. The skeptical public is excluded, bearing costs without direct engagement.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the Clock as a pure 'Rope' (genuine coordination) by highlighting the asymmetric extraction of epistemic credibility. It also avoids mislabeling it as a pure 'Snare' by acknowledging the genuine, albeit strategically achieved, coordination function of raising awareness and mobilizing action. The 'Tangled Rope' classification captures the hybrid nature where a legitimate coordination problem is addressed through a mechanism that also extracts a cost from other values.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_cost_quantification,
    'How can the erosion of epistemic credibility and public trust be quantitatively measured to assess the true cost of the Clock''s performative function?',
    'Longitudinal studies on public perception of science, expert surveys on trust in risk communication, and analysis of media framing of scientific consensus versus advocacy.',
    'If the epistemic cost is found to be severe, it would strengthen the ''Snare'' aspect of the constraint, suggesting the extraction outweighs the coordination benefit. If minimal, it would push towards a ''Rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_cost_quantification, empirical, 'Quantifying the long-term damage to scientific credibility from strategic communication.').

omega_variable(
    kernel_reading_identification,
    'Is the Doomsday Clock primarily an objective index, a performative tool, or an irreducible entanglement of both scientific judgment and normative stakes?',
    'Analysis of the Bulletin''s internal decision-making documents, public statements, and the reception of the Clock by different stakeholder groups (scientific, policy, public). This story instantiates the ''performative_tool_reading''.',
    'If resolved towards ''objective_index_reading'', the constraint would be reclassified closer to a ''Mountain'' or ''Rope'' with lower extractiveness. If resolved towards ''hybrid_legitimacy_reading'', it would remain a ''Tangled Rope'' but with a different justification for its hybridity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''doomsday_clock_metric'' kernel. This reading emphasizes strategic impact over pure objectivity. Sibling readings (''objective_index_reading'', ''hybrid_legitimacy_reading'') would shift the balance of coordination vs. extraction and the perceived legitimacy of the Clock''s operation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t1947, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1947, 0.3).
narrative_ontology:measurement(doom_tr_t1960, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1960, 0.45).
narrative_ontology:measurement(doom_tr_t1980, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1980, 0.6).
narrative_ontology:measurement(doom_tr_t2000, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2000, 0.7).
narrative_ontology:measurement(doom_tr_t2024, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2024, 0.75).

% Extraction over time
narrative_ontology:measurement(doom_be_t1947, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1947, 0.4).
narrative_ontology:measurement(doom_be_t1960, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1960, 0.5).
narrative_ontology:measurement(doom_be_t1980, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(doom_be_t2000, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(doom_be_t2024, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t1947, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1947, 0.2).
narrative_ontology:measurement(doom_su_t1960, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1960, 0.25).
narrative_ontology:measurement(doom_su_t1980, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(doom_su_t2000, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(doom_su_t2024, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__performative_tool_reading, identity_coordination).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'doomsday_clock_metric' kernel. This 'performative_tool_reading' emphasizes strategic communication for policy impact, contrasting with the 'objective_index_reading' (focus on empirical indicators) and the 'hybrid_legitimacy_reading' (focus on irreducible entanglement of science and values). Each reading has a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
