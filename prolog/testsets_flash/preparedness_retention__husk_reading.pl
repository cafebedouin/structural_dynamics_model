% ============================================================================
% CONSTRAINT STORY: preparedness_retention__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__husk_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_retention__husk_reading
 *   human_readable: Preparedness as Memorial Performance (Husk Reading)
 *   domain: disaster_preparedness/institutional_memory/governance
 *
 * SUMMARY:
 *   This constraint describes preparedness as a system of memorial
 *   performance, where drills and inspections function as rituals that create
 *   the *feeling* of retention and competence, but lack the live, adaptive
 *   capacity required for actual disaster response. It's a 'husk' of
 *   preparedness, prioritizing visible compliance and institutional
 *   legitimacy over genuine operational readiness. This is one reading of the
 *   'preparedness_retention' kernel, focusing on the performative decay.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__husk_reading, 0.65).
domain_priors:suppression_score(preparedness_retention__husk_reading, 0.4).
domain_priors:theater_ratio(preparedness_retention__husk_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__husk_reading, piton).
narrative_ontology:human_readable(preparedness_retention__husk_reading, "Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_retention__husk_reading, "disaster_preparedness/institutional_memory/governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__husk_reading, 'b339c187-916a-41c2-acec-acfeb9c92e88').
narrative_ontology:cs_kernel_codification('b339c187-916a-41c2-acec-acfeb9c92e88', formalized).
narrative_ontology:cs_authority_grounding('b339c187-916a-41c2-acec-acfeb9c92e88', lineage).
narrative_ontology:cs_interpretation_layer_present('b339c187-916a-41c2-acec-acfeb9c92e88').
narrative_ontology:cs_reading_relation('b339c187-916a-41c2-acec-acfeb9c92e88', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('b339c187-916a-41c2-acec-acfeb9c92e88', preparedness_retention__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('b339c187-916a-41c2-acec-acfeb9c92e88', foundational, appearance_suffices_for_legitimacy).
narrative_ontology:cs_axiom_status(appearance_suffices_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b339c187-916a-41c2-acec-acfeb9c92e88', appearance_suffices_for_legitimacy, conventional).
narrative_ontology:cs_axiom('b339c187-916a-41c2-acec-acfeb9c92e88', secondary, tacit_knowledge_is_unmeasurable).
narrative_ontology:cs_axiom_status(tacit_knowledge_is_unmeasurable, holdable).
narrative_ontology:cs_axiom_grounding('b339c187-916a-41c2-acec-acfeb9c92e88', tacit_knowledge_is_unmeasurable, empirically_contingent).
narrative_ontology:cs_reference_frame('b339c187-916a-41c2-acec-acfeb9c92e88', ceremonial_compliance_framework).
narrative_ontology:cs_drift_state('b339c187-916a-41c2-acec-acfeb9c92e88', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('b339c187-916a-41c2-acec-acfeb9c92e88', '').
narrative_ontology:cs_kernel_id(preparedness_retention__husk_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, governing_institutions).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, political_leaders).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, general_public).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, frontline_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers preparedness programs, conducts drills, and performs inspections. Benefits from the appearance of readiness and compliance, which confers legitimacy and avoids public scrutiny, without necessarily investing in deep, live competence. Resource allocation favors visible compliance metrics.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, governing_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from public perception of safety and institutional competence, especially after a crisis. They can point to drills and reports as evidence of action, deflecting blame when actual response capacity is found wanting. Their political horizon often incentivizes short-term, visible 'performance' over long-term, deep investment.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, political_leaders, beneficiary,
    powerful, immediate, mobile, national).

% Pays for preparedness systems through taxes and bears the ultimate cost of inadequate response during actual disasters. They are largely unaware of the gap between performative readiness and live competence, relying on institutional assurances.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, general_public, payer,
    powerless, biographical, trapped, local).

% Participate in drills and inspections, often recognizing their performative nature and the gap in actual competence. They bear the direct burden of operational failures during crises due to insufficient training, resources, or systemic knowledge retention. Their professional identity often binds them to the system despite its flaws.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, frontline_responders, payer,
    moderate, biographical, constrained, local).

% Observe and document the discrepancy between declared preparedness and actual response capability. They analyze the institutional incentives that lead to performative rituals over genuine competence, but often lack direct power to change the system.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, critical_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_retention__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates institutional activity around a shared narrative of readiness, ensuring compliance with procedural requirements and maintaining public confidence in the face of potential threats.
% TRANSFER_FUNCTION: Transfers resources (time, budget, attention) from genuine competence-building to performative compliance, and transfers the risk of actual disaster response failure from governing institutions to the general public and frontline responders.
% ABSENT_VOICES: The victims of future, unmitigated disasters are the most absent voices; they would demand genuine, live competence over ceremonial performance. Also, independent experts whose assessments challenge the official narrative of readiness are often marginalized.
% DISAPPEARANCE_RATIONALE: If the performative aspect of preparedness vanished, the underlying lack of competence would be immediately exposed. Institutions would lose their legitimacy cover, public trust would erode, and there would be immense pressure to rebuild genuine capacity or face direct accountability for disaster failures. The political and institutional landscape would be forced to re-evaluate its approach to risk.
% FOUNDING_PROBLEM: The need to manage and mitigate the impact of predictable and unpredictable disasters, ensuring public safety and rapid recovery.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem of disaster risk is undeniably live, corroborated by scientific consensus on climate change, seismic activity, and public health threats. However, the effectiveness of the current 'solution' (performative preparedness) in addressing this live problem is contested by critical analysts and often exposed by actual disaster events.
narrative_ontology:disappearance_verdict(preparedness_retention__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__husk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__husk_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_retention__husk_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high theater_ratio (0.85) is central: most activity is performative, aimed at demonstrating compliance rather than building competence. Extractiveness (0.65) is substantial, as resources are diverted from effective solutions to ceremonial ones, and the public pays for a false sense of security. Suppression (0.40) is moderate, as critical voices are often ignored or dismissed, but not actively silenced through overt coercion. The claimed type is piton because the primary function (actual preparedness) has atrophied, but the constraint persists due to institutional inertia and the diffuse benefit of appearing prepared, without any single party benefiting enough to actively maintain the *husk* itself, only the appearance.
 *
 * PERSPECTIVAL GAP:
 *   Governing institutions perceive this as a necessary, if imperfect, system for managing public expectations and demonstrating action. The public, largely unaware of the performance-competence gap, perceives it as genuine safety. Frontline responders, however, experience the gap directly during crises, leading to a divergence in their perceived reality versus the institutional narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   Governing institutions and political leaders are beneficiaries, gaining legitimacy and deflecting scrutiny (low d). The general public and frontline responders are victims, bearing the costs of ineffective preparedness (high d). Critical analysts are observers, neither directly benefiting nor paying, but analyzing the system's flaws (analytical d).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (genuine preparedness) has atrophied, replaced by a performative mandate (appearing prepared). The classification as a Piton prevents mislabeling it as a Rope (which would imply genuine coordination benefits) or a Snare (which would imply concentrated extraction actively maintained by a beneficiary). Instead, it highlights the inertial persistence of a degraded function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ceremony_competence_ratio,
    'What is the actual ratio of resources allocated to performative compliance versus genuine competence-building and tacit skill retention?',
    'Detailed audit of preparedness budgets, training outcomes, and post-disaster performance reviews, distinguishing between ''check-the-box'' activities and demonstrable skill acquisition.',
    'A higher ratio of ceremonial spending would further solidify the ''husk_reading'' and increase the measured extractiveness, potentially reclassifying to a Snare if a clear beneficiary of the misallocation is identified. A lower ratio would lend more credence to the ''competence_reading''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ceremony_competence_ratio, empirical, 'Quantifying the allocation of resources between performative and functional preparedness.').

omega_variable(
    institutional_legitimacy_dependency,
    'To what extent does the legitimacy of governing institutions depend on maintaining the *appearance* of preparedness, even if actual competence is low?',
    'Comparative analysis of public trust and political stability in jurisdictions with transparently low preparedness versus those with high performative preparedness but low actual competence.',
    'If legitimacy is highly dependent on appearance, it reinforces the ''husk_reading'' by identifying institutional legitimacy as a key, diffuse benefit that sustains the Piton. If not, the persistence of the husk is even more purely inertial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_legitimacy_dependency, empirical, 'Assessing the link between performative preparedness and institutional legitimacy.').

omega_variable(
    reading_framing_choice,
    'Is this ''husk_reading'' the most accurate framing, or is the ''competence_reading'' or ''hybrid_reading'' more structurally descriptive?',
    'Empirical evidence from multiple disaster responses: consistent, widespread operational failures would corroborate the ''husk_reading''; consistent, effective responses would support ''competence_reading''; mixed results with clear institutional stratification would support ''hybrid_reading''.',
    'Adopting a different reading would fundamentally alter the constraint''s classification, extractiveness, and beneficiary/victim structure. For example, the ''competence_reading'' would likely be a Rope or Mountain, with low extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_choice, conceptual, 'The choice between different structural interpretations of preparedness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__husk_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1980, preparedness_retention__husk_reading, theater_ratio, 1980, 0.6).
narrative_ontology:measurement(prep_tr_t1990, preparedness_retention__husk_reading, theater_ratio, 1990, 0.7).
narrative_ontology:measurement(prep_tr_t2000, preparedness_retention__husk_reading, theater_ratio, 2000, 0.78).
narrative_ontology:measurement(prep_tr_t2010, preparedness_retention__husk_reading, theater_ratio, 2010, 0.82).
narrative_ontology:measurement(prep_tr_t2024, preparedness_retention__husk_reading, theater_ratio, 2024, 0.85).

% Extraction over time
narrative_ontology:measurement(prep_be_t1980, preparedness_retention__husk_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(prep_be_t1990, preparedness_retention__husk_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(prep_be_t2000, preparedness_retention__husk_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(prep_be_t2010, preparedness_retention__husk_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(prep_be_t2024, preparedness_retention__husk_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1980, preparedness_retention__husk_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(prep_su_t1990, preparedness_retention__husk_reading, suppression_requirement, 1990, 0.33).
narrative_ontology:measurement(prep_su_t2000, preparedness_retention__husk_reading, suppression_requirement, 2000, 0.36).
narrative_ontology:measurement(prep_su_t2010, preparedness_retention__husk_reading, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement(prep_su_t2024, preparedness_retention__husk_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__husk_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'husk_reading' of the 'preparedness_retention' kernel, which also includes 'competence_reading' and 'hybrid_reading'. Each reading represents a distinct structural claim about the nature of preparedness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
