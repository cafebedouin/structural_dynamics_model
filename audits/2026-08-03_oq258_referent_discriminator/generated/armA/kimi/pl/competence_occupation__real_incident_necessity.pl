% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__real_incident_necessity, []).

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
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Real Catastrophic Incident Necessity for Competence Occupation
 *   domain: organizational/safety/competence
 *
 * SUMMARY:
 *   This constraint instantiates the real_incident_necessity reading of the
 *   competence_occupation kernel: the claim that only actual catastrophic
 *   incidents provide the authentic conditions necessary to occupy the
 *   competence kernel in high-reliability organizations. The reading treats
 *   this as a structural feature of human cognition under existential
 *   threat—a mountain—rather than as a negotiable training preference.
 *   Because catastrophes are definitionally unacceptable as a policy tool,
 *   this creates what the reading describes as an unresolvable competence
 *   maintenance problem: organizations need catastrophes to stay competent
 *   but must prevent catastrophes to exist at all. No viable beneficiary
 *   structure exists; the constraint extracts from no one and benefits no
 *   one. It simply imposes a tragic limit.
 *
 * KEY AGENTS:
 *   - hro_operators: Primary observers (moderate/constrained) — bear the epistemic cost of the constraint; their competence is valid only to the extent they have survived or witnessed catastrophe.
 *   - simulation_researchers: Contesting observers (powerful/analytical) — argue the mountain is a false summit and high-fidelity simulation can replicate catastrophic fidelity.
 *   - safety_systems: Institutional context (institutional/biographical) — the organizational arrangements that have made catastrophes rare, thereby making the constraint visible and costly.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.16).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.14).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.16).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.14).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, mountain).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real Catastrophic Incident Necessity for Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "organizational/safety/competence").

domain_priors:emerges_naturally(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, '791e929a-5e47-4f24-8ffe-0c8d0ca0793e').
narrative_ontology:cs_kernel_codification('791e929a-5e47-4f24-8ffe-0c8d0ca0793e', distributed).
narrative_ontology:cs_authority_grounding('791e929a-5e47-4f24-8ffe-0c8d0ca0793e', practice).
narrative_ontology:cs_reading_relation('791e929a-5e47-4f24-8ffe-0c8d0ca0793e', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('791e929a-5e47-4f24-8ffe-0c8d0ca0793e', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('791e929a-5e47-4f24-8ffe-0c8d0ca0793e', foundational, authentic_competence_requires_existential_risk).
narrative_ontology:cs_axiom_status(authentic_competence_requires_existential_risk, holdable).
narrative_ontology:cs_axiom_grounding('791e929a-5e47-4f24-8ffe-0c8d0ca0793e', authentic_competence_requires_existential_risk, empirically_contingent).
narrative_ontology:cs_axiom('791e929a-5e47-4f24-8ffe-0c8d0ca0793e', foundational, simulation_lacks_catastrophic_fidelity).
narrative_ontology:cs_axiom_status(simulation_lacks_catastrophic_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('791e929a-5e47-4f24-8ffe-0c8d0ca0793e', simulation_lacks_catastrophic_fidelity, empirically_contingent).
narrative_ontology:cs_reference_frame('791e929a-5e47-4f24-8ffe-0c8d0ca0793e', catastrophic_authenticity_pedagogy).
narrative_ontology:cs_drift_state('791e929a-5e47-4f24-8ffe-0c8d0ca0793e', high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('791e929a-5e47-4f24-8ffe-0c8d0ca0793e', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

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
% COORDINATION_FUNCTION: None; the constraint operates as a claimed natural law of competence maintenance under existential threat, imposing a necessary epistemic condition rather than coordinating agents.
% TRANSFER_FUNCTION: No transfer of resources between agents; the constraint imposes a one-way epistemic boundary condition: competence validity flows only through catastrophic exposure.
% ABSENT_VOICES: Simulation researchers and safety scientists who argue stress inoculation is achievable without catastrophe are present in the literature but structurally discounted in operational tradecultures that privilege lived near-death experience.
% DISAPPEARANCE_RATIONALE: If the necessity of real catastrophic incidents for competence were lifted, simulation advocates hold that safer competence maintenance would follow; real-incident-necessity holds that competence would decay and catastrophic risk would rise. The parties dispute which world emerges.
% FOUNDING_PROBLEM: High-reliability operations provide insufficient high-stakes feedback for edge competence maintenance; routine success produces feedback thinness that erodes mindfulness and skill.
% FOUNDING_PROBLEM_CORROBORATION: HRO researchers (Weick, Sutcliffe, LaPorte) attest to feedback thinness, though they increasingly argue mindfulness and rich information substitute for catastrophe. Simulation researchers attest the problem is solvable without catastrophic exposure. No outside corroboration exists for catastrophe-exclusive necessity.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, contested).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.16, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, ExtMetricName, E),
    domain_priors:suppression_score(competence_occupation__real_incident_necessity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(competence_occupation__real_incident_necessity),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(competence_occupation__real_incident_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.16) because a genuine mountain does not extract; it imposes necessary conditions. Suppression is low (0.14) because the constraint does not actively suppress alternatives—simulation simply fails to reach catastrophic fidelity if the mountain claim is true. Accessibility collapse is very high (0.89): once the constraint is understood, the alternative (simulation-based competence) collapses as inauthentic. Resistance is low (0.18) because the constraint, if real, is not defeated by opposition; simulation advocacy represents wishful thinking against a natural limit, not structural resistance. Theater ratio is low (0.10): the constraint is not performative, though organizations may theatricalize around it. The flat measurement series indicates stability consistent with a natural law.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is between operators who have experienced catastrophe and read the constraint as obvious natural law, and researchers who have built high-fidelity simulations and read the constraint as tradeculture mythology. The engine computes this gap from the same structural data: the operator seat has exit_options constrained by the belief that no simulation can substitute; the researcher seat has analytical exit and contests the mountain. The divergence is not resolved by the claim; the metrics author the operator-facing structure, and the contested status is captured in the omegas.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary and no victim declarations are authored because the constraint, as a claimed mountain, shapes situations without transferring resources. Directionality is moot: the constraint does not subsidize or extract from specific agents; it sets a boundary condition for all competence claims. If the classification is mistaken and the constraint is actually a piton or false summit, directionality would flow from the tradeculture that enforces the belief to the operators who pay the cost of competence anxiety and over-preparation.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy risk here is misclassifying a tradeculture belief as a mountain. The classification protects against this by requiring emerges_naturally, low theater, low suppression, and high accessibility collapse. If the constraint were a snare or piton, we would expect a beneficiary structure (training vendors, old-guard operators collecting status) or high theater (ritualized catastrophe narratives). The absence of both, combined with the unresolvable paradox the constraint creates, supports the mountain reading: a genuine natural limit produces exactly this kind of tragic, beneficiary-free bind.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_tradeculture,
    'Is the necessity of real catastrophic incidents for authentic competence a genuine structural limit of human stress cognition, or a constructed feature of safety tradeculture resisting simulation legitimacy?',
    'Controlled longitudinal studies comparing team decision quality and physiological stress markers under real catastrophic exposure versus validated high-fidelity simulation.',
    'If simulation achieves equivalent inoculation, the constraint downgrades from mountain to piton or institutional snare; if real incidents remain irreplaceable, mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_tradeculture, empirical, 'Whether real incident necessity is cognitive natural law or institutional construct').

omega_variable(
    competence_decay_inevitability,
    'Does the increasing rarity of catastrophic incidents in ultra-safe systems produce inevitable competence decay, or can hybrid mechanisms arrest it?',
    'Track near-miss recognition, recovery performance, and drift detection across organizations with varying incident exposure and simulation depth.',
    'If decay is inevitable without catastrophes, the constraint is a tragic mountain imposing an unresolvable trade-off; if hybrid mechanisms suffice, the constraint is a false summit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_decay_inevitability, empirical, 'Whether catastrophe scarcity guarantees competence erosion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corin_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.08).
narrative_ontology:measurement(corin_tr_t8, competence_occupation__real_incident_necessity, theater_ratio, 8, 0.09).
narrative_ontology:measurement(corin_tr_t16, competence_occupation__real_incident_necessity, theater_ratio, 16, 0.09).
narrative_ontology:measurement(corin_tr_t24, competence_occupation__real_incident_necessity, theater_ratio, 24, 0.1).
narrative_ontology:measurement(corin_tr_t32, competence_occupation__real_incident_necessity, theater_ratio, 32, 0.1).
narrative_ontology:measurement(corin_tr_t40, competence_occupation__real_incident_necessity, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(corin_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(corin_be_t8, competence_occupation__real_incident_necessity, base_extractiveness, 8, 0.15).
narrative_ontology:measurement(corin_be_t16, competence_occupation__real_incident_necessity, base_extractiveness, 16, 0.15).
narrative_ontology:measurement(corin_be_t24, competence_occupation__real_incident_necessity, base_extractiveness, 24, 0.16).
narrative_ontology:measurement(corin_be_t32, competence_occupation__real_incident_necessity, base_extractiveness, 32, 0.16).
narrative_ontology:measurement(corin_be_t40, competence_occupation__real_incident_necessity, base_extractiveness, 40, 0.16).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(competence_occupation__real_incident_necessity, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the competence_occupation kernel. The kernel decomposes because the epsilon values and structural relationships differ across readings: real_incident_necessity claims negligible extraction (mountain), simulation_sufficiency claims coordination (rope), and hybrid_occupation claims contested multi-mechanism extraction (tangled_rope or scaffold). They are not the same constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
