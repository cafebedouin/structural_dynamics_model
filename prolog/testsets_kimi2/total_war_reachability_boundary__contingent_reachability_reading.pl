% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contingent_reachability_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Total War Reachability Boundary â Contingent Reachability Reading
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint is the contingent_reachability_reading of the
 *   total_war_reachability_boundary kernel. It holds that total war's current
 *   unreachability is not a permanent contraction (as the contraction_reading
 *   claims) nor a stable coordination equilibrium (as the dropping_reading
 *   claims), but a temporary scaffold dependent on the current technology
 *   equilibrium. The current contraction is treated as an atrophied
 *   capability that could reverse with destabilizing technological change.
 *   Under this reading, states investing in emerging military technologies
 *   are the primary beneficiaries of the temporary stability, while civilian
 *   populations bear the deferred existential risk. The constraint requires
 *   active enforcement through nuclear deterrence postures and arms-control
 *   architectures, and carries an implicit sunset clause in the form of
 *   technological obsolescence.
 *
 * KEY AGENTS:
 *   - states_investing_in_destabilizing_technologies (beneficiary â receives modernization window)
 *   - civilian_populations (target â bears deferred existential risk)
 *   - nuclear_armed_states (agenda-setter â maintains deterrence boundary)
 *   - strategic_studies_community (observer â debates temporality of equilibrium)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.48).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.61).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, scaffold).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Total War Reachability Boundary â Contingent Reachability Reading").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:has_sunset_clause(total_war_reachability_boundary__contingent_reachability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, '8a619141-aed8-4fc6-b5cf-ab3e8d5e37e3').
narrative_ontology:cs_kernel_codification('8a619141-aed8-4fc6-b5cf-ab3e8d5e37e3', implicit).
narrative_ontology:cs_authority_grounding('8a619141-aed8-4fc6-b5cf-ab3e8d5e37e3', practice).
narrative_ontology:cs_interpretation_layer_present('8a619141-aed8-4fc6-b5cf-ab3e8d5e37e3').
narrative_ontology:cs_reading_relation('8a619141-aed8-4fc6-b5cf-ab3e8d5e37e3', total_war_reachability_boundary__contraction_reading, influences).
narrative_ontology:cs_reading_relation('8a619141-aed8-4fc6-b5cf-ab3e8d5e37e3', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_axiom('8a619141-aed8-4fc6-b5cf-ab3e8d5e37e3', foundational, total_war_reachability_is_tech_contingent).
narrative_ontology:cs_axiom_status(total_war_reachability_is_tech_contingent, holdable).
narrative_ontology:cs_axiom_grounding('8a619141-aed8-4fc6-b5cf-ab3e8d5e37e3', total_war_reachability_is_tech_contingent, empirically_contingent).
narrative_ontology:cs_axiom('8a619141-aed8-4fc6-b5cf-ab3e8d5e37e3', foundational, deterrence_equilibrium_is_transitory).
narrative_ontology:cs_axiom_status(deterrence_equilibrium_is_transitory, holdable).
narrative_ontology:cs_axiom_grounding('8a619141-aed8-4fc6-b5cf-ab3e8d5e37e3', deterrence_equilibrium_is_transitory, empirically_contingent).
narrative_ontology:cs_reference_frame('8a619141-aed8-4fc6-b5cf-ab3e8d5e37e3', nuclear_revolution_equilibrium).
narrative_ontology:cs_drift_state('8a619141-aed8-4fc6-b5cf-ab3e8d5e37e3', emerging_tech_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8a619141-aed8-4fc6-b5cf-ab3e8d5e37e3', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_destabilizing_technologies).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and deploy emerging military technologiesâhypersonic delivery vehicles, AI-enabled command and control, counter-space and cyber capabilitiesâthat could erode the current deterrence equilibrium. They benefit from the temporary stability provided by the existing reachability scaffold while modernizing capabilities that may eventually render mutual-vulnerability assumptions obsolete.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_destabilizing_technologies, beneficiary,
    powerful, generational, mobile, global).

% Bear the latent existential risk of total war should the deterrence scaffold collapse or fail catastrophically. They have no institutional voice in strategic doctrine, no exit from the deterrence regime, and no capacity to hedge against state-level technology investments.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations, payer,
    powerless, civilizational, trapped, global).

% Maintain nuclear arsenals, early-warning systems, and deterrence doctrines that constitute the current reachability boundary. They enforce the constraint through mutual vulnerability, strategic ambiguity, and arms-control diplomacy, while also funding some destabilizing technologies themselves.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, nuclear_armed_states, agenda_setter,
    institutional, generational, constrained, global).

% Produces assessments of deterrence stability and technology trajectories. Debates whether the current contraction is permanent, contingent, or illusory, but does not directly control arsenals or doctrine.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, strategic_studies_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_destabilizing_technologies).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__contingent_reachability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents immediate great-power total war by making it technically unfeasible or disproportionately costly under the current technology equilibrium, creating a bounded strategic space for competition short of total war.
% TRANSFER_FUNCTION: Moves existential risk forward in time from the present to a future where technology may restore reachability; moves strategic stability and modernization windows to states investing in next-generation capabilities.
% ABSENT_VOICES: Future generations who will inherit the post-scaffold environment; non-nuclear states whose security is determined by deterrence dynamics they do not shape; civilian populations in potential escalation corridors who are not represented in strategic doctrine.
% DISAPPEARANCE_RATIONALE: If the reachability boundary vanished overnightâmeaning total war became immediately feasible and rational under current technologyâthe global strategic order would reorganize around preventive war incentives, accelerated arms racing, or desperate hedging. The current equilibrium depends on the boundary's existence.
% FOUNDING_PROBLEM: The problem of catastrophic great-power war in an era of rapid technological change; the need to manage the transition between strategic equilibria without immediate total war.
% FOUNDING_PROBLEM_CORROBORATION: Strategic studies scholars and defense planners outside the immediate beneficiary set attest that the technology transition problem is ongoing; historical records of deterrence crises corroborate that the boundary is actively managed, not self-enforcing.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contingent_reachability_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contingent_reachability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contingent_reachability_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate: the scaffold extracts security from civilian populations by deferring existential risk into a tech-dependent future, while concentrating strategic advantage on states modernizing their arsenals. Suppression (0.61) reflects the active enforcement required to maintain deterrence and suppress first-strike incentives during the transition. Theater_ratio (0.42) captures the growing performative element of deterrence signaling as the underlying tech equilibrium erodes and the scaffold's functional basis weakens. Accessibility_collapse (0.62) indicates that alternatives such as comprehensive disarmament or alternative security architectures have been largely marginalized in strategic discourse. Resistance (0.28) is low because anti-nuclear movements and non-aligned states lack leverage over great-power technology investments. The measurement series share one time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat, the constraint is a modernization windowâa temporary scaffold that permits capability development without immediate war. From the payer seat, it is an existential deferral mechanism that concentrates catastrophic risk on populations without representation. The agenda-setter seat sees necessary crisis management; the observer seat sees an unresolved theoretical dispute. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   States_investing_in_destabilizing_technologies are declared beneficiaries and have mobile exit options, placing them near the beneficiary end of the directionality spectrum. Civilian_populations are declared victims with trapped exit, placing them near the full-target end. Nuclear_armed_states are agenda-setters with constrained exit; their directionality is intermediate because they both maintain the constraint and are exposed to its failure modes.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the constraint as scaffold prevents the mandatrophy error of treating a temporary tech equilibrium as a permanent natural law (the contraction_reading's mountain-ward error) or as a self-sustaining rope (the dropping_reading's error). It captures the transitional intent: the arrangement exists to manage a technology gap, not to persist indefinitely. If the founding problem (managing tech transition without war) is resolvedâeither by successful arms control or by the collapse of deterrenceâthe scaffold should sunset. The absence of a formal sunset clause is the institutional gap; the implicit sunset in technological change is what makes the scaffold claim structurally appropriate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffold_vs_piton_nature,
    'Is the current contraction of total war reachability a scaffold (temporary support that will sunset with technology change) or a piton (atrophied capability maintained by institutional inertia and theatrical deterrence posturing)?',
    'Track the ratio of functional deterrence infrastructure to performative signaling as destabilizing technologies mature; if the functional basis erodes while the constraint persists, reclassify toward piton.',
    'If piton, the constraint persists without solving its founding problem and the extraction is inertial rather than transitional; if scaffold, the sunset expectation is structurally valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_vs_piton_nature, conceptual, 'Ambiguity between transitional scaffold and inertial piton').

omega_variable(
    tech_reversal_trajectory,
    'Will emerging technologies actually reverse the contraction of total war reachability, and on what timeline?',
    'Empirical assessment of destabilizing technology maturation (hypersonics, AI C2, counter-space) and war-game outcomes measuring crisis stability under novel capabilities.',
    'If reversal is probable and near-term, the scaffold''s sunset is approaching and extractiveness may spike as states race to exploit the transition; if reversal is remote, the scaffold may functionally become a rope or mountain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tech_reversal_trajectory, empirical, 'Uncertainty about technology-driven reversal of deterrence contraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twrb_contingent_tr_t0, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(twrb_contingent_tr_t12, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(twrb_contingent_tr_t24, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(twrb_contingent_tr_t36, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 36, 0.32).
narrative_ontology:measurement(twrb_contingent_tr_t48, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 48, 0.36).
narrative_ontology:measurement(twrb_contingent_tr_t60, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(twrb_contingent_tr_t72, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 72, 0.48).
narrative_ontology:measurement(twrb_contingent_tr_t80, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 80, 0.55).

% Extraction over time
narrative_ontology:measurement(twrb_contingent_be_t0, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(twrb_contingent_be_t12, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 12, 0.32).
narrative_ontology:measurement(twrb_contingent_be_t24, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(twrb_contingent_be_t36, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 36, 0.41).
narrative_ontology:measurement(twrb_contingent_be_t48, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 48, 0.46).
narrative_ontology:measurement(twrb_contingent_be_t60, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(twrb_contingent_be_t72, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 72, 0.58).
narrative_ontology:measurement(twrb_contingent_be_t80, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 80, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(twrb_contingent_su_t0, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(twrb_contingent_su_t12, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(twrb_contingent_su_t24, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(twrb_contingent_su_t36, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 36, 0.58).
narrative_ontology:measurement(twrb_contingent_su_t48, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 48, 0.63).
narrative_ontology:measurement(twrb_contingent_su_t60, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 60, 0.69).
narrative_ontology:measurement(twrb_contingent_su_t72, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 72, 0.75).
narrative_ontology:measurement(twrb_contingent_su_t80, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 80, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__dropping_reading).

% DUAL FORMULATION NOTE:
% The total_war_reachability_boundary kernel decomposes into three structurally distinct constraints (readings) that share the referent of total war feasibility but differ on its permanence, contingency, and probability. Each reading carries a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
