% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__sovereignty_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: P5 Veto as Westphalian Sovereignty (Mountain Reading)
 *   domain: international_relations/institutional_design
 *
 * SUMMARY:
 *   The UN Charter Article 27 grants five permanent Security Council members
 *   a veto over substantive resolutions. This constraint story instantiates
 *   the sovereignty reading of that kernel: the veto is not a negotiable
 *   privilege but an institutional recognition of the Westphalian principle
 *   that no state can be bound by international law without its consent,
 *   combined with the physical reality that nuclear-armed great powers
 *   possess global enforcement capacity and cannot be compelled by any
 *   institutional mechanism. From this reading, the veto is a Mountainâa
 *   structural feature of the international system that would persist
 *   regardless of the UN Charter's specific wording, because any attempt to
 *   override great-power will would face the same coordination failure
 *   (defiance or institutional collapse). No beneficiary or victim is
 *   declared; the near-zero extractiveness metric reflects that no party
 *   collects rents from the constraint's operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.05).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.05).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "P5 Veto as Westphalian Sovereignty (Mountain Reading)").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "international_relations/institutional_design").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, 'ff4997e7-186b-4302-b7d1-5f88f34a380a').
narrative_ontology:cs_kernel_codification('ff4997e7-186b-4302-b7d1-5f88f34a380a', formalized).
narrative_ontology:cs_authority_grounding('ff4997e7-186b-4302-b7d1-5f88f34a380a', self_enforcing).
narrative_ontology:cs_reading_relation('ff4997e7-186b-4302-b7d1-5f88f34a380a', article_27_veto_power__coordination_reading, influences).
narrative_ontology:cs_reading_relation('ff4997e7-186b-4302-b7d1-5f88f34a380a', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_axiom('ff4997e7-186b-4302-b7d1-5f88f34a380a', foundational, consent_reflects_enforcement_asymmetry).
narrative_ontology:cs_axiom_status(consent_reflects_enforcement_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('ff4997e7-186b-4302-b7d1-5f88f34a380a', consent_reflects_enforcement_asymmetry, empirically_contingent).
narrative_ontology:cs_axiom('ff4997e7-186b-4302-b7d1-5f88f34a380a', secondary, global_institutions_cannot_transcend_power).
narrative_ontology:cs_axiom_status(global_institutions_cannot_transcend_power, holdable).
narrative_ontology:cs_axiom_grounding('ff4997e7-186b-4302-b7d1-5f88f34a380a', global_institutions_cannot_transcend_power, empirically_contingent).
narrative_ontology:cs_reference_frame('ff4997e7-186b-4302-b7d1-5f88f34a380a', westphalian_sovereignty).
narrative_ontology:cs_drift_state('ff4997e7-186b-4302-b7d1-5f88f34a380a', contemporary_multipolarity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ff4997e7-186b-4302-b7d1-5f88f34a380a', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Noneâthe constraint does not solve a collective-action problem; it formalizes the physical impossibility of compelling nuclear-armed great powers against their will.
% TRANSFER_FUNCTION: No extractive transfer; the constraint records the distribution of coercive capacity among great powers without moving resources between agents.
% ABSENT_VOICES: Non-nuclear states and reform coalitions (e.g., the Accountability, Coherence and Transparency group) argue for veto limitation or abolition; they are present in UN forums but structurally excluded from Security Council decision-making.
% DISAPPEARANCE_RATIONALE: The constraint reflects the physical distribution of nuclear and conventional enforcement capacity among great powers. Removing the legal veto would not remove the underlying incapacity to compel those powers; they would simply ignore or defy resolutions, producing identical behavioral outcomes.
% FOUNDING_PROBLEM: How to sustain a collective security institution after the failure of the League of Nations, given that no enforcement mechanism can compel nuclear-armed great powers against their will.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the 1945 San Francisco Conference and structural realist scholars in international relations attest that the veto was inserted as a recognition of power reality; corroboration comes from archival records and academic theory outside the P5 states' own institutional narratives.
narrative_ontology:disappearance_verdict(article_27_veto_power__sovereignty_reading, world_unchanged).
narrative_ontology:founding_problem_status(article_27_veto_power__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_27_veto_power__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__sovereignty_reading, 0.05, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_27_veto_power__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.05) because the veto does not transfer resources from one party to another; it records an incapacity to compel. Suppression is similarly minimal (0.05) because the constraint persists without active enforcementâgreat powers need not suppress alternatives, since no binding alternative is structurally viable. Accessibility collapse is very high (0.92): once the enforcement asymmetry is understood, proposals to abolish the veto or bind great powers against their will collapse as non-viable. Resistance is low (0.08) because even states that oppose the veto accept its underlying physical reality, channeling opposition into procedural complaints rather than genuine attempts at compulsion. Theater ratio is negligible (0.05) because maintenance of the constraint requires no performanceâit is self-enforcing through power distribution.
 *
 * PERSPECTIVAL GAP:
 *   From the sovereignty reading, all seats experience the constraint as Mountain. A great-power seat sees the veto as the institutional recognition of its autonomous capacity; a non-great-power seat sees the same constraint as the registration of its relative incapacity. The directionalities diverge symmetrically around the power axis, but because there is no extractive transfer, both seats compute as Mountain. The divergence is in welfare implications, not in constraint type. This distinguishes the sovereignty reading from the oligopoly reading, where the same structural positions would compute as radically different types (beneficiary versus victim).
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim is declared because the constraint derives from physical reality, not from an extractive arrangement. The P5 states are not structural beneficiaries of the constraintâthey are the entities whose material capacity makes the constraint necessary. Non-P5 states are not victimsâthey are subject to the same natural law of power distribution that governs all international relations. The directionality derivation is intentionally left empty to signal that this constraint has no parties in the DR sense; it is a natural feature of the system.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereignty reading prevents mandatrophy mislabeling by refusing to treat the veto as an atrophied coordination mechanism (Scaffold/Piton) or as a disguised extraction mechanism (Snare/Tangled Rope). The founding problemâpreventing institutional collapse when great powers disagreeâremains live because the underlying power asymmetry persists. The constraint has not outlived its function because its 'function' is to register reality, not to solve a problem that could be solved another way. This reading would only face mandatrophy if nuclear weapons were abolished and enforcement capacity equalized globallyâan event that would transform the constraint's physical foundation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nuclear_abolition_boundary,
    'If nuclear weapons were universally abolished and enforcement capacity equalized, would the veto remain structurally inevitable or would it become a contingent oligopoly?',
    'Counterfactual analysis of historical cases where great-power military asymmetry was reduced (pre-nuclear eras, conventional power transitions) to assess whether veto-like mechanisms emerged from power reality or institutional choice.',
    'If the constraint depends on nuclear asymmetry, it is empirically contingent and potentially a False Summit Mountain; if it persists regardless, it is a deeper structural feature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_abolition_boundary, empirical, 'Whether the veto''s mountain status depends on nuclear weapons or is deeper.').

omega_variable(
    p5_membership_fossilization,
    'Does the current P5 membership still map to the states with greatest global enforcement capacity, or has the formal membership fossilized while actual power shifted?',
    'Comparative military-economic analysis of current P5 versus non-P5 powers on global force projection and strategic-deterrence metrics.',
    'If the formal P5 no longer tracks actual enforcement capacity, the sovereignty reading weakens and the oligopoly reading gains explanatory power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(p5_membership_fossilization, empirical, 'Whether P5 membership tracks current power distribution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__sovereignty_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(arti_tr_t6, article_27_veto_power__sovereignty_reading, theater_ratio, 6, 0.05).
narrative_ontology:measurement(arti_tr_t12, article_27_veto_power__sovereignty_reading, theater_ratio, 12, 0.05).
narrative_ontology:measurement(arti_tr_t18, article_27_veto_power__sovereignty_reading, theater_ratio, 18, 0.05).
narrative_ontology:measurement(arti_tr_t24, article_27_veto_power__sovereignty_reading, theater_ratio, 24, 0.05).
narrative_ontology:measurement(arti_tr_t30, article_27_veto_power__sovereignty_reading, theater_ratio, 30, 0.05).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__sovereignty_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(arti_be_t6, article_27_veto_power__sovereignty_reading, base_extractiveness, 6, 0.05).
narrative_ontology:measurement(arti_be_t12, article_27_veto_power__sovereignty_reading, base_extractiveness, 12, 0.05).
narrative_ontology:measurement(arti_be_t18, article_27_veto_power__sovereignty_reading, base_extractiveness, 18, 0.05).
narrative_ontology:measurement(arti_be_t24, article_27_veto_power__sovereignty_reading, base_extractiveness, 24, 0.05).
narrative_ontology:measurement(arti_be_t30, article_27_veto_power__sovereignty_reading, base_extractiveness, 30, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(article_27_veto_power__sovereignty_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__oligopoly_reading).

% DUAL FORMULATION NOTE:
% The article_27_veto_power kernel decomposes into three structurally distinct constraints per the epsilon-invariance principle: sovereignty_reading (Mountain, near-zero epsilon), coordination_reading (Rope/Tangled Rope, moderate epsilon), and oligopoly_reading (Snare/Tangled Rope, high epsilon). They share the same institutional kernel (UN Charter Article 27) but instantiate different structural claims about the veto's origin, function, and persistence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
