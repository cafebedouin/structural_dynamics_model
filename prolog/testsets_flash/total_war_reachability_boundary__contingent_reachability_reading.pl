% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Contingent Total War Reachability Boundary
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint represents the 'contingent reachability' reading of the
 *   total war boundary, arguing that the feasibility of total war is not a
 *   fixed 'mountain' but a 'piton'—an atrophied capability dependent on
 *   current technological and strategic equilibria. While total war currently
 *   appears 'unreachable' due to mutual assured destruction (MAD), this
 *   reading posits that technological advancements (e.g., advanced missile
 *   defense, hypersonic weapons, AI-driven command and control) could reverse
 *   this contraction, making total war 'reachable' again. The current
 *   'contraction' is thus a temporary state, not a permanent one. The
 *   constraint is classified as a piton because the 'unreachability' is
 *   largely performative and inertial, maintained by a specific technological
 *   balance that could shift, rather than an inherent, unchangeable reality.
 *   The beneficiaries are states investing in destabilizing technologies, and
 *   the victims are global populations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.4).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.6).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, piton).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Contingent Total War Reachability Boundary").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, '910e6517-79dd-4b14-8846-cae3f5b06507').
narrative_ontology:cs_kernel_codification('910e6517-79dd-4b14-8846-cae3f5b06507', implicit).
narrative_ontology:cs_authority_grounding('910e6517-79dd-4b14-8846-cae3f5b06507', expertise).
narrative_ontology:cs_reading_relation('910e6517-79dd-4b14-8846-cae3f5b06507', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('910e6517-79dd-4b14-8846-cae3f5b06507', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_axiom('910e6517-79dd-4b14-8846-cae3f5b06507', foundational, strategic_reachability_is_technologically_contingent).
narrative_ontology:cs_axiom_status(strategic_reachability_is_technologically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('910e6517-79dd-4b14-8846-cae3f5b06507', strategic_reachability_is_technologically_contingent, empirically_contingent).
narrative_ontology:cs_axiom('910e6517-79dd-4b14-8846-cae3f5b06507', foundational, current_unreachability_is_an_atrophied_capability).
narrative_ontology:cs_axiom_status(current_unreachability_is_an_atrophied_capability, holdable).
narrative_ontology:cs_axiom_grounding('910e6517-79dd-4b14-8846-cae3f5b06507', current_unreachability_is_an_atrophied_capability, empirically_contingent).
narrative_ontology:cs_reference_frame('910e6517-79dd-4b14-8846-cae3f5b06507', post_cold_war_strategic_equilibrium).
narrative_ontology:cs_drift_state('910e6517-79dd-4b14-8846-cae3f5b06507', contemporary_technological_acceleration, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('910e6517-79dd-4b14-8846-cae3f5b06507', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_destabilizing_technologies).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, global_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states benefit from the perception that total war is becoming more reachable, as it justifies their investments in new offensive and defensive technologies that could shift the strategic balance. They gain leverage and influence by pushing the technological frontier.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_destabilizing_technologies, beneficiary,
    powerful, generational, mobile, global).

% These populations bear the ultimate cost if the reachability boundary shifts and deterrence fails. They also pay for the escalating arms race through taxes and live under the constant threat of catastrophic conflict. Their agency in this dynamic is minimal.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, global_populations, payer,
    powerless, generational, trapped, global).

% These theorists analyze and articulate the conditions of strategic stability and the feasibility of total war. Their interpretations influence policy and public perception, shaping the narrative around reachability. They administer the conceptual framework.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, nuclear_deterrence_theorists, agenda_setter,
    analytical, biographical, analytical, global).

% Organizations like the UN or arms control bodies observe and attempt to manage the strategic environment. They are constrained by the actions of powerful states but work to mitigate risks and promote stability, often relying on theoretical frameworks to guide their efforts.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, international_security_institutions, observer,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint implicitly coordinates state behavior by defining the perceived limits of acceptable conflict escalation, influencing strategic planning and resource allocation towards deterrence or counter-deterrence capabilities.
% TRANSFER_FUNCTION: It transfers a sense of security (or insecurity) and strategic leverage among states, while transferring the ultimate risk and cost of potential conflict to global populations.
% ABSENT_VOICES: Future generations and non-state actors who would bear the consequences of total war are absent from the strategic discourse that defines this boundary. They would advocate for de-escalation and disarmament, but lack direct representation.
% DISAPPEARANCE_RATIONALE: If the perceived reachability boundary for total war vanished, strategic planning would fundamentally alter. States might pursue more aggressive policies, or conversely, a new, more robust form of global security cooperation might emerge. The current deterrence architecture would collapse.
% FOUNDING_PROBLEM: The problem of managing the existential threat posed by nuclear weapons and preventing their use, while maintaining national security interests.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing existence of nuclear arsenals, continuous strategic competition, and the persistent efforts of international security institutions to prevent proliferation and conflict attest to the founding problem's live status. This is corroborated by historical records of near-misses and current geopolitical tensions.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contingent_reachability_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contingent_reachability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contingent_reachability_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).
:- end_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.4) is moderate, reflecting the costs of maintaining deterrence and the arms race, but not direct extraction from a coordinated activity. Suppression (0.6) is higher, as the strategic environment actively suppresses alternatives to the current deterrence paradigm. The high theater ratio (0.7) is key to the piton classification: the 'unreachability' of total war is largely a performance of strategic stability, maintained by a specific technological balance. If this balance shifts, the performance could break down, revealing the underlying reachability. Accessibility collapse (0.3) is low because technological change could open new pathways to total war, and resistance (0.2) is low because the concept of 'unreachability' is widely accepted, even if its contingency is debated.
 *
 * PERSPECTIVAL GAP:
 *   The 'contingent reachability' reading highlights a fundamental perspectival gap between those who view total war's unreachability as a permanent state (e.g., the 'contraction_reading') and those who see it as a fragile, technology-dependent equilibrium. This reading emphasizes the agency of technological development in shaping strategic reality, which is often downplayed by more static deterrence theories. The engine's classification as a piton captures the performative and inertial aspects of the current 'unreachability' from this perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   States investing in destabilizing technologies are beneficiaries (d=0.0-0.2) because the perceived contingency of total war reachability justifies their strategic investments and enhances their leverage. Global populations are payers (d=0.8-1.0) as they bear the costs of the arms race and the ultimate risk of conflict. Nuclear deterrence theorists, while influential, are analytical observers (d=0.5) whose role is to interpret and articulate the boundary, not directly benefit or pay from its operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_determinism_vs_strategic_choice,
    'To what extent is the reachability boundary determined by technological advancements versus strategic choices made by states?',
    'Historical analysis of past technological shifts and their impact on strategic doctrine, combined with counterfactual modeling of alternative state responses to new technologies.',
    'If technology is highly deterministic, the piton classification is robust. If strategic choice dominates, the constraint might be more of a tangled rope, actively maintained by states'' decisions rather than technological inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism_vs_strategic_choice, conceptual, 'Ambiguity between technological determinism and human agency in shaping strategic reachability.').

omega_variable(
    deterrence_stability_measurement,
    'How can the ''stability'' of deterrence be objectively measured, and at what point does technological change constitute a ''destabilizing'' shift sufficient to alter the reachability boundary?',
    'Development of quantitative metrics for strategic stability (e.g., crisis stability, arms race stability) and agreed-upon thresholds for technological impact, potentially through international expert consensus.',
    'Clearer metrics would reduce the ''theater ratio'' by making the boundary''s status more empirically verifiable, potentially shifting the classification towards a more stable rope or even a mountain if unreachability becomes robustly demonstrable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_stability_measurement, empirical, 'Uncertainty in measuring deterrence stability and the impact of technological change.').

omega_variable(
    kernel_reading_contingent_reachability,
    'Is total war reachability a contingent, technology-dependent boundary (this reading), or a permanently contracted space (contraction_reading), or a stable coordination equilibrium (dropping_reading)?',
    'Future technological developments and their impact on strategic stability, combined with ongoing geopolitical events and the evolution of international norms.',
    'If technological shifts make total war more feasible, this reading gains salience. If the nuclear taboo strengthens, the ''contraction_reading'' might be favored. If deterrence remains stable despite technological change, the ''dropping_reading'' might prevail.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contingent_reachability, empirical, 'This constraint is one reading of the ''total_war_reachability_boundary'' kernel. This omega documents the core disagreement with sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 1991, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1991, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1991, 0.8).
narrative_ontology:measurement(tota_tr_t2000, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2000, 0.78).
narrative_ontology:measurement(tota_tr_t2010, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2010, 0.75).
narrative_ontology:measurement(tota_tr_t2020, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2020, 0.72).
narrative_ontology:measurement(tota_tr_t2024, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2024, 0.7).

% Extraction over time
narrative_ontology:measurement(tota_be_t1991, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1991, 0.3).
narrative_ontology:measurement(tota_be_t2000, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement(tota_be_t2010, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(tota_be_t2020, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2020, 0.38).
narrative_ontology:measurement(tota_be_t2024, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1991, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1991, 0.5).
narrative_ontology:measurement(tota_su_t2000, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(tota_su_t2010, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(tota_su_t2020, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement(tota_su_t2024, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contingent_reachability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__dropping_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
