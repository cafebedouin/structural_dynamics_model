% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__instrumentalist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__instrumentalist_reading
 *   human_readable: Orthographic Reform via Literacy Maximization (Instrumentalist Reading)
 *   domain: political_linguistics/state_formation
 *
 * SUMMARY:
 *   An orthographic reform justified as maximizing literacy rates and
 *   administrative efficiency: a state apparatus adopts a new script,
 *   allocates educational resources toward it, and gradually phases out or
 *   marginalizes an older orthography from institutional use. The newly
 *   literate population gains access and social mobility through the new
 *   script. The Arabic-literate elite suffers skill devaluation and
 *   institutional marginalization. This is ONE READING of a contested kernel
 *   about orthographic legitimacy. The instrumentalist reading frames script
 *   choice as a pragmatic tool for solving measurable coordination problems
 *   (literacy, efficiency, administrative throughput); competing readings
 *   (continuity, modernist) dispute whether efficiency really justifies the
 *   cost or whether other motives (cultural continuity, Western alignment)
 *   are the true drivers. This constraint story generates ONLY the
 *   instrumentalist reading, treating it as a clean epistemic claim with its
 *   own ε, beneficiary/victim structure, and temporal trajectory.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, 0.58).
domain_priors:suppression_score(orthographic_legitimacy_kernel__instrumentalist_reading, 0.62).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__instrumentalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__instrumentalist_reading, rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__instrumentalist_reading, "Orthographic Reform via Literacy Maximization (Instrumentalist Reading)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__instrumentalist_reading, "political_linguistics/state_formation").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__instrumentalist_reading, 'f7dc7d91-5b5f-4f7f-b71a-74250c1b1bed').
narrative_ontology:cs_kernel_codification('f7dc7d91-5b5f-4f7f-b71a-74250c1b1bed', fixed_text).
narrative_ontology:cs_authority_grounding('f7dc7d91-5b5f-4f7f-b71a-74250c1b1bed', extraction).
narrative_ontology:cs_interpretation_layer_present('f7dc7d91-5b5f-4f7f-b71a-74250c1b1bed').
narrative_ontology:cs_reading_relation('f7dc7d91-5b5f-4f7f-b71a-74250c1b1bed', orthographic_legitimacy_kernel__continuity_reading, influences).
narrative_ontology:cs_reading_relation('f7dc7d91-5b5f-4f7f-b71a-74250c1b1bed', orthographic_legitimacy_kernel__modernist_reading, coexists_with).
narrative_ontology:cs_axiom('f7dc7d91-5b5f-4f7f-b71a-74250c1b1bed', foundational, orthographic_choice_is_technical_solution).
narrative_ontology:cs_axiom_status(orthographic_choice_is_technical_solution, holdable).
narrative_ontology:cs_axiom_grounding('f7dc7d91-5b5f-4f7f-b71a-74250c1b1bed', orthographic_choice_is_technical_solution, instrumental).
narrative_ontology:cs_axiom('f7dc7d91-5b5f-4f7f-b71a-74250c1b1bed', foundational, efficiency_gains_justify_elite_skill_devaluation).
narrative_ontology:cs_axiom_status(efficiency_gains_justify_elite_skill_devaluation, holdable).
narrative_ontology:cs_axiom_grounding('f7dc7d91-5b5f-4f7f-b71a-74250c1b1bed', efficiency_gains_justify_elite_skill_devaluation, empirically_contingent).
narrative_ontology:cs_reference_frame('f7dc7d91-5b5f-4f7f-b71a-74250c1b1bed', pre_reform_orthographic_tradition).
narrative_ontology:cs_drift_state('f7dc7d91-5b5f-4f7f-b71a-74250c1b1bed', post_reform_administrative_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f7dc7d91-5b5f-4f7f-b71a-74250c1b1bed', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_elite).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__instrumentalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises moderately (0.35→0.58 over the interval) as the reform beds in: initially, literacy gains are genuine and broadly welcomed, suppression is low. By mid-interval, resistance from the elite hardens and enforcement machinery (examination systems, resource reallocation, prestige withdrawal) must intensify to maintain the reform. At interval end (~generation mark), extractiveness plateaus: the constraint has become normalized, the new script is entrenched, and the elite has either adapted or withdrawn from administrative competition. Theater ratio stays low-moderate (0.10→0.28): the literacy-improvement justification remains real throughout, but a growing share of late-stage enforcement activity defends orthographic exclusivity rather than furthering literacy (e.g., examination gatekeeping that could be neutral becomes script-policing). Suppression rises parallel to enforcement intensity — the constraint requires active effort to prevent the old script's re-emergence in informal communication, religious practice, and nostalgic pedagogy. The temporal pattern reflects a coordinate-then-enforce dynamic: early years are genuine coordination (both elite and masses benefit from clarity), middle years are enforcement (elite resistance to skill devaluation requires suppression), late years are maintenance (normalized suppression, diminishing new literacy gains, constraint becomes inertial if foundational problem is solved). Measurements author the same time grid (t∈{0,5,10,15,20,25}) for all three metrics so temporal alignment is explicit.
 *
 * DIRECTIONALITY LOGIC:
 *   State administrative apparatus: agenda_setter, institutional power, high d (benefits from control, devalues alternatives, sets the rules) — computed as near-beneficiary end, ~d=0.15. Newly literate population: genuinely coordinate (gain access without having to master complex script), mobile exit (can choose whether to learn), organized power (though operationally dependent on the reform's continuation) — computed ~d=0.50, symmetric, beneficiary role without high extraction. Arabic-literate elite: powerful, initially trapped (administrative exit is costly but available through emigration or withdrawal), constrained by resource reallocation and prestige loss — computed high d (victims), ~d=0.75, even though not powerless, because the constraint structurally targets them. Religious scholars: excluded from agenda-setting but not compelled to adopt new script immediately; constrained exit (can continue practice but lose institutional support). International advisors: benefit from symbolic alignment with their own standards, arbitrage exit — low d, beneficiary framing though not primary to the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (low literacy, administrative inefficiency due to complex orthography) is LIVE at t=0 and plausibly LIVE at t=25 if the reform is incomplete or if international comparisons show persistent efficiency gaps. However, the omega documents the committer ambiguity: is the state's actual justification instrumentalist (efficiency) or is it modernist (Western alignment/rupture), and is that justification post-hoc? If the foundational problem (efficiency) is genuinely solved by the reform, but the state's justification shifts toward identity/rupture framing (modernist reading), then the constraint's mandate has partially atrophied — the reform persists not because literacy is still expanding but because the identity narrative (we are modern, we are not Ottoman) is now the legitimacy hook. That would be a mandatrophy signal, though the constraint's type (rope, not piton) reflects that genuine coordination persists even if the foundational problem is solved. Mandatrophy in this case would not be certification-changing but would be a drift-warning: the constraint's original justification has softened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    orthographic_necessity_vs_policy_choice,
    'Was orthographic reform structurally necessary to achieve literacy expansion and administrative efficiency, or was it a policy choice that conflated script change with modernization?',
    'Counterfactual historical analysis: comparative literacy trajectories in contexts where administrative efficiency was pursued without orthographic rupture (e.g., script simplification without replacement, pedagogical innovation within existing scripts). Contemporary literacy research on phonetic transparency vs. pedagogical familiarity.',
    'If necessity: the constraint is genuine coordination cost — efficiency gains justify the elite''s devaluation. If choice: the constraint is partially extractive — the elite''s losses subsidize a policy preference, not a technical requirement. The computed type hinges on whether the coordination function (literacy expansion) could have been achieved without the extraction component (script devaluation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orthographic_necessity_vs_policy_choice, empirical, 'Whether orthographic reform was technically necessary or a policy choice framed as necessity.').

omega_variable(
    committer_kernel_ambiguity,
    'Which reading of the orthographic legitimacy kernel is institutionally dominant at the time of evaluation? Does the state''s actual justification rest on instrumentalist (efficiency/literacy), modernist (Western alignment/rupture), or continuity (access-to-tradition) framing?',
    'Analysis of state policy documents, educational curricula, official histories, and public addresses justifying the reform. Cross-check with scholars'' retrospective interpretations and how the constraint is actually taught/legitimized in schools.',
    'If instrumentalist framing dominates: the constraint operates as described (rope-like coordination via literacy gains). If modernist framing dominates: the constraint shifts toward snare-like operation (the literacy gain is rhetorical cover for cultural rupture and Western alignment, with extractive cost to continuity). If continuity framing dominates: the constraint is legitimized as non-coercive (the reform is framed as expanding, not destroying, access — victims'' framing differs from agenda-setter''s).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_ambiguity, conceptual, 'Kernel reading ambiguity: which normative framing is institutionally primary?').

omega_variable(
    elite_deskilling_vs_voluntary_transition,
    'To what extent did the Arabic-literate elite voluntarily adopt and teach the new script (demonstrating adaptive capacity) versus resist and suffer displacement (demonstrating coercion)?',
    'Historical record of whether elite scribes, teachers, and administrators transitioned to new-script training or faced pressure, replacement, or marginalization. Whether continuity of elite social position depended on learning the new script (tight coupling = coercion) or remained independent (loose coupling = voluntary adaptation).',
    'If voluntary: the elite''s devaluation is market-driven (their skills simply became less valuable), not coercive — extraction measure should be lower. If coercive: the state deliberately displaced the elite to break their monopoly — extraction measure should reflect enforced skill obsolescence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_deskilling_vs_voluntary_transition, empirical, 'Whether elite displacement was coerced or market-driven.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__instrumentalist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(orth_tr_t5, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(orth_tr_t10, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(orth_tr_t15, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(orth_tr_t20, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(orth_tr_t25, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(orth_be_t5, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(orth_be_t10, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(orth_be_t15, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(orth_be_t20, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(orth_be_t25, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(orth_su_t5, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(orth_su_t10, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(orth_su_t15, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(orth_su_t20, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(orth_su_t25, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__instrumentalist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__instrumentalist_reading, 0.12).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__modernist_reading).

% DUAL FORMULATION NOTE:
% The orthographic_legitimacy_kernel is composed of three structurally distinct readings, each with its own ε and beneficiary/victim structure. The instrumentalist reading (this story) claims ε~0.58 (rope-like coordination with moderate extraction cost to the elite). The continuity reading claims that script abandonment destroys access to tradition (likely snare-like, with religious scholars and continuity authorities as victims). The modernist reading claims that alignment with Western scripts is a civilizational imperative (likely snare-like or tangled-rope, with indigenous identity and continuity as victims, but Western institutions and domestic modernizers as beneficiaries). The three stories share the same kernel (orthographic legitimacy) but diverge on what legitimates it and who benefits/pays. They are linked by affects_constraints to show that success of one reading constrains the others: if instrumentalist framing dominates institutional discourse, modernist framing is deflated (becomes unnecessary — we can justify reform by efficiency alone), and continuity framing is suppressed (becomes nostalgic rather than normative).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
