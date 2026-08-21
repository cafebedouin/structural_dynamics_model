% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__behavioral_competence_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: aneyoshi_stone_commitment__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Commitment (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   The Aneyoshi Stone commitment is a physical marker and associated oral
 *   tradition in a Japanese coastal village, instructing residents not to
 *   build below a certain line to avoid tsunami devastation. This constraint
 *   story instantiates the 'behavioral competence' reading, which asserts
 *   that the commitment has retained its operational force as a live land-use
 *   rule for 78 years (from its erection in 1933 to the 2011 tsunami), with
 *   compliance directly linked to survival. The low extractiveness and high
 *   accessibility collapse reflect its status as a deeply internalized,
 *   effective safety measure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, 0.8).
domain_priors:theater_ratio(aneyoshi_stone_commitment__behavioral_competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Stone Commitment (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:requires_active_enforcement(aneyoshi_stone_commitment__behavioral_competence_reading).
domain_priors:emerges_naturally(aneyoshi_stone_commitment__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, '33e4dd8c-b3af-4e53-a685-9ca04502738e').
narrative_ontology:cs_kernel_codification('33e4dd8c-b3af-4e53-a685-9ca04502738e', fixed_text).
narrative_ontology:cs_authority_grounding('33e4dd8c-b3af-4e53-a685-9ca04502738e', lineage).
narrative_ontology:cs_interpretation_layer_present('33e4dd8c-b3af-4e53-a685-9ca04502738e').
narrative_ontology:cs_reading_relation('33e4dd8c-b3af-4e53-a685-9ca04502738e', aneyoshi_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('33e4dd8c-b3af-4e53-a685-9ca04502738e', foundational, tsunami_risk_is_immutable).
narrative_ontology:cs_axiom_status(tsunami_risk_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('33e4dd8c-b3af-4e53-a685-9ca04502738e', tsunami_risk_is_immutable, empirically_contingent).
narrative_ontology:cs_axiom('33e4dd8c-b3af-4e53-a685-9ca04502738e', foundational, ancestral_wisdom_is_binding).
narrative_ontology:cs_axiom_status(ancestral_wisdom_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('33e4dd8c-b3af-4e53-a685-9ca04502738e', ancestral_wisdom_is_binding, conventional).
narrative_ontology:cs_reference_frame('33e4dd8c-b3af-4e53-a685-9ca04502738e', tsunami_survival_mandate).
narrative_ontology:cs_drift_state('33e4dd8c-b3af-4e53-a685-9ca04502738e', post_2011_tsunami_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('33e4dd8c-b3af-4e53-a685-9ca04502738e', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_community_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The community members adhere to the stone's directive, building their homes above the designated tsunami line. They bear the cost of restricted land use (less convenient building sites) but are the direct beneficiaries of the safety and survival it provides, as demonstrated in the 2011 tsunami.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_community_members, beneficiary,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_community_members, payer).

% Local officials administer land-use regulations that align with the Aneyoshi Stone's directive. While they have the formal authority to set rules, their decisions are heavily constrained by the community's deep-seated commitment to the stone and its proven efficacy.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, local_government_officials, agenda_setter,
    institutional, biographical, constrained, local).

% These experts study the Aneyoshi Stone as a case study in effective, long-term disaster preparedness. They analyze its impact on community resilience and land-use patterns, often advocating for similar, locally-grounded solutions.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, disaster_risk_reduction_experts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__behavioral_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates land-use decisions within the Aneyoshi community to ensure collective safety from tsunamis by establishing a clear, immutable boundary for building locations.
% TRANSFER_FUNCTION: Transfers the cost of restricted, less convenient building land to community members, in exchange for the collective benefit of survival and reduced risk from tsunamis.
% ABSENT_VOICES: Potential developers or new residents who might prioritize economic development or convenience over strict adherence to the traditional land-use rule are effectively excluded by the community's strong consensus and local regulations. Their voices would likely advocate for more flexible land use.
% DISAPPEARANCE_RATIONALE: If the commitment embodied by the stone vanished overnight, the community's land-use patterns would likely shift, with new construction potentially occurring in lower, more convenient, but highly vulnerable areas. This would dramatically increase the risk of future tsunami casualties and fundamentally alter the community's relationship with its environment.
% FOUNDING_PROBLEM: Repeated devastating tsunamis in the early 20th century (e.g., 1933 Sanriku earthquake and tsunami) that wiped out coastal settlements, demonstrating that human memory alone was insufficient to prevent future tragedies without a permanent, physical reminder and directive.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of tsunami events, geological evidence of past inundations, and the direct observation of the 2011 Great East Japan Earthquake and Tsunami's impact (where compliant areas survived while non-compliant areas were devastated) all corroborate the founding problem and its ongoing relevance. This corroboration comes from scientific and historical sources outside the immediate community beneficiaries.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(aneyoshi_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_stone_commitment__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.05) because the constraint is perceived as a necessary, life-saving measure rather than an arbitrary imposition; the 'cost' of restricted land use is seen as a fair exchange for survival. Suppression is high (0.8) because the rule is strictly adhered to, enforced by strong community norms and the stark historical lessons it embodies. Theater ratio is low (0.1) as the commitment is genuinely functional and not merely performative. Accessibility collapse is high (0.9) because the alternative (building below the line) is understood to be catastrophic, making it effectively inaccessible for rational actors within the community. Resistance is low (0.1) due to the clear and proven benefit of compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Aneyoshi community, this constraint is a vital, almost natural law of survival, a 'mountain' that ensures their continued existence. From an external, purely economic perspective, it might be seen as a 'rope' or 'snare' due to land-use restrictions, but this reading emphasizes the community's internal experience and the proven efficacy of the rule. The engine's FSM will detect the beneficiaries on a 'mountain' and flag it for review, which is appropriate for a human-made rule treated as natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The Aneyoshi community members are both beneficiaries (of safety) and payers (of restricted land use), but their overall directionality is strongly towards beneficiary due to the life-saving outcome. Local government officials are agenda-setters whose actions are largely shaped by the stone's authority. Disaster risk reduction experts are observers, analyzing its efficacy without direct involvement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_rule,
    'Is the Aneyoshi Stone commitment a genuine natural law of survival (Mountain), or a constructed rule that benefits identifiable agents (the community) and is treated as such?',
    'Analysis of the community''s perception and enforcement mechanisms: if it''s treated as an immutable truth about the environment, it leans natural law; if its persistence depends on active social enforcement, it leans constructed.',
    'If resolved as a constructed rule, the constraint would reclassify from Mountain to a strong Rope, acknowledging its human origin despite its effectiveness and the community''s deep adherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_rule, conceptual, 'Ambiguity between natural law and human construct for the Aneyoshi Stone.').

omega_variable(
    operational_force_vs_symbolic_observance,
    'Does the Aneyoshi Stone commitment retain genuine operational force in land-use decisions, or has it decayed to a purely symbolic observance?',
    'Longitudinal study of building permits and community settlement patterns in Aneyoshi compared to similar coastal communities without such a directive, especially post-2011 tsunami. Direct observation of community adherence and enforcement mechanisms.',
    'If operational force is confirmed, this reading''s low extractiveness and high accessibility collapse are validated. If it''s purely symbolic, the constraint would reclassify as a Piton or Snare, with higher extractiveness (from lost opportunity) and lower suppression (from non-compliance), as its claimed function would be theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operational_force_vs_symbolic_observance, empirical, 'Contest over the stone''s active regulatory function vs. symbolic status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(aney_tr_t10, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(aney_tr_t20, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(aney_tr_t30, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(aney_tr_t40, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(aney_tr_t50, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(aney_tr_t60, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 78, 0.1).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(aney_be_t10, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 10, 0.05).
narrative_ontology:measurement(aney_be_t20, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(aney_be_t30, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 30, 0.05).
narrative_ontology:measurement(aney_be_t40, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 40, 0.05).
narrative_ontology:measurement(aney_be_t50, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(aney_be_t60, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 78, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(aney_su_t10, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(aney_su_t20, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(aney_su_t30, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 30, 0.8).
narrative_ontology:measurement(aney_su_t40, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(aney_su_t50, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 50, 0.8).
narrative_ontology:measurement(aney_su_t60, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 60, 0.8).
narrative_ontology:measurement(aney_su_t78, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 78, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__behavioral_competence_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
