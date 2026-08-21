% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__livelihood_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__livelihood_security_reading, []).

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
 *   constraint_id: performance_legitimacy__livelihood_security_reading
 *   human_readable: Performance Legitimacy: Livelihood Security Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint represents the 'livelihood security' reading of
 *   performance legitimacy, where the state's authority is grounded in its
 *   ability to deliver tangible improvements in daily life (employment,
 *   healthcare, education, elderly care). This reading prioritizes social
 *   welfare and consumption support over traditional capital-intensive
 *   industrial expansion. It is one of several competing interpretations of
 *   how the state maintains its legitimacy, each with different policy
 *   implications and beneficiaries/victims.
 *
 * KEY AGENTS:
 *   - citizens_receiving_services: Primary beneficiary (organized/constrained) — directly experiences improvements.
 *   - service_sectors: Beneficiary (moderate/constrained) — benefits from policy prioritization.
 *   - local_social_bureaus: Agenda-setter (institutional/constrained) — implements and expands welfare programs.
 *   - capital_intensive_industrial_expansion: Payer (powerful/constrained) — faces reduced state investment.
 *   - local_government_infrastructure_spending: Payer (institutional/constrained) — experiences budget cuts.
 *   - state_owned_enterprises_focused_on_heavy_industry: Payer (institutional/constrained) — de-emphasized in policy.
 *   - central_planning_commission: Agenda-setter (institutional/identity_locked) — balances competing demands, prioritizes livelihoods.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, 0.35).
domain_priors:suppression_score(performance_legitimacy__livelihood_security_reading, 0.6).
domain_priors:theater_ratio(performance_legitimacy__livelihood_security_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__livelihood_security_reading, rope).
narrative_ontology:human_readable(performance_legitimacy__livelihood_security_reading, "Performance Legitimacy: Livelihood Security Reading").
narrative_ontology:topic_domain(performance_legitimacy__livelihood_security_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__livelihood_security_reading, 'a7529d3c-edc8-40aa-abdd-dcf1d74ba3f0').
narrative_ontology:cs_kernel_codification('a7529d3c-edc8-40aa-abdd-dcf1d74ba3f0', formalized).
narrative_ontology:cs_authority_grounding('a7529d3c-edc8-40aa-abdd-dcf1d74ba3f0', extraction).
narrative_ontology:cs_interpretation_layer_present('a7529d3c-edc8-40aa-abdd-dcf1d74ba3f0').
narrative_ontology:cs_reading_relation('a7529d3c-edc8-40aa-abdd-dcf1d74ba3f0', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('a7529d3c-edc8-40aa-abdd-dcf1d74ba3f0', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7529d3c-edc8-40aa-abdd-dcf1d74ba3f0', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_axiom('a7529d3c-edc8-40aa-abdd-dcf1d74ba3f0', foundational, social_stability_through_welfare).
narrative_ontology:cs_axiom_status(social_stability_through_welfare, holdable).
narrative_ontology:cs_axiom_grounding('a7529d3c-edc8-40aa-abdd-dcf1d74ba3f0', social_stability_through_welfare, instrumental).
narrative_ontology:cs_axiom('a7529d3c-edc8-40aa-abdd-dcf1d74ba3f0', foundational, state_as_primary_welfare_provider).
narrative_ontology:cs_axiom_status(state_as_primary_welfare_provider, holdable).
narrative_ontology:cs_axiom_grounding('a7529d3c-edc8-40aa-abdd-dcf1d74ba3f0', state_as_primary_welfare_provider, conventional).
narrative_ontology:cs_reference_frame('a7529d3c-edc8-40aa-abdd-dcf1d74ba3f0', post_growth_imbalance_consensus).
narrative_ontology:cs_drift_state('a7529d3c-edc8-40aa-abdd-dcf1d74ba3f0', contemporary_era_of_rising_expectations, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a7529d3c-edc8-40aa-abdd-dcf1d74ba3f0', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, citizens_receiving_services).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, service_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, local_social_bureaus).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, capital_intensive_industrial_expansion).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_spending).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, state_owned_enterprises_focused_on_heavy_industry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly benefit from improved employment, healthcare, education, and elderly care. Their satisfaction is a key metric for this reading's legitimacy. While they benefit, their ability to exit the system or demand different services is constrained by state control.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, citizens_receiving_services, beneficiary,
    organized, immediate, constrained, local).

% Experience increased demand and investment due to the prioritization of social services and consumption support. They are structurally favored by this reading but remain dependent on state policy for their growth.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, service_sectors, beneficiary,
    moderate, biographical, constrained, national).

% Are empowered to implement and expand social welfare programs. They administer the delivery of services and are responsible for meeting citizen needs, aligning their institutional mandate with this reading's core tenets.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, local_social_bureaus, agenda_setter,
    institutional, biographical, constrained, local).

% Faces reduced state investment and prioritization, as resources are diverted towards social welfare and consumption. This sector, traditionally a driver of growth, now bears the cost of a shift in development strategy.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, capital_intensive_industrial_expansion, payer,
    powerful, generational, constrained, national).

% Experiences budget cuts and reduced central government support for large-scale infrastructure projects, as the focus shifts to direct social provision. This represents a significant re-prioritization of public funds.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_spending, payer,
    institutional, generational, constrained, local).

% Historically central to the economy, these entities now face pressure to reorient or downsize as their traditional focus on heavy industry and export-led growth is de-emphasized in favor of domestic consumption and social services.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, state_owned_enterprises_focused_on_heavy_industry, payer,
    institutional, generational, constrained, national).

% Responsible for overall economic strategy, this body must balance competing demands from different readings of performance legitimacy. Under this reading, it prioritizes policies that directly improve livelihoods, even if it means slowing traditional growth metrics. Its identity is tied to the state's overall legitimacy.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, central_planning_commission, agenda_setter,
    institutional, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state resources and policy efforts towards directly improving citizens' daily lives through social services and consumption support, ensuring a baseline of welfare and stability.
% TRANSFER_FUNCTION: Transfers state resources and policy focus from capital-intensive industrial development and large-scale infrastructure projects towards social welfare, public services, and household consumption.
% ABSENT_VOICES: Advocates for unrestrained market liberalization or purely quantitative growth metrics are marginalized, as their proposals would undermine the direct provision of livelihood security. Their voices are suppressed by the state's commitment to this legitimacy framework.
% DISAPPEARANCE_RATIONALE: If this reading of performance legitimacy vanished, the state's policy priorities would immediately shift. Investment would likely flow back into heavy industry and infrastructure, social safety nets might erode, and the political compact with citizens would be fundamentally altered, leading to widespread social unrest and economic reorganization.
% FOUNDING_PROBLEM: The state faced a crisis of legitimacy due to growing inequality, environmental degradation, and a perception that economic growth was not translating into tangible improvements for ordinary citizens.
% FOUNDING_PROBLEM_CORROBORATION: Independent social surveys and citizen satisfaction indices consistently show that livelihood security remains a primary concern for the populace. While the state's official narrative emphasizes its success, the ongoing prioritization of these areas indicates the problem is still actively managed and central to maintaining social stability, corroborated by academic analyses of social policy trends.
narrative_ontology:disappearance_verdict(performance_legitimacy__livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__livelihood_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__livelihood_security_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(performance_legitimacy__livelihood_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__livelihood_security_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__livelihood_security_reading_tests).
:- end_tests(performance_legitimacy__livelihood_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it genuinely coordinates state efforts to deliver public goods, with identifiable beneficiaries (citizens, service sectors). Extractiveness is moderate (0.35) as resources are reallocated from other sectors, imposing costs, but the primary function is coordination. Suppression (0.6) is present as alternative development paths (e.g., purely market-driven growth) are actively suppressed to maintain this policy direction. Theater ratio is low (0.2) because the delivery of services is a direct, functional activity, not primarily performative. The metrics reflect a state actively pursuing a specific development model, which, while beneficial to many, requires active suppression of competing models.
 *
 * PERSPECTIVAL GAP:
 *   Citizens directly receiving services experience this as a clear benefit, while sectors that bear the costs (e.g., heavy industry) experience it as a constraint on their growth. The central planning commission, as an agenda-setter, views it as a necessary and legitimate rebalancing of development priorities to maintain social stability. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizens and service sectors are clear beneficiaries (low d) as the constraint directly subsidizes their welfare and growth. Capital-intensive industries and infrastructure spending are targets (high d) as resources are extracted from them. The central planning commission, while an agenda-setter, is also identity-locked into maintaining the state's overall legitimacy, making its directionality complex but ultimately aligned with the constraint's function.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling genuine coordination (delivery of social services) as pure extraction by acknowledging the direct benefits to citizens. However, it also highlights the active suppression of alternative development models, preventing it from being mislabeled as a pure Mountain. The 'live' status of the founding problem suggests the mandate is still relevant, though the 'contested' corroboration indicates ongoing debate about its necessity versus its extractive aspects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    livelihood_vs_growth_tradeoff,
    'Is the prioritization of livelihood security over capital-intensive growth a zero-sum tradeoff, or can both be achieved simultaneously with different policy instruments?',
    'Empirical analysis of long-term economic data from states pursuing similar strategies, comparing outcomes in livelihood security and overall economic growth, controlling for external factors.',
    'If it''s a zero-sum tradeoff, the extraction from industrial sectors is an unavoidable cost of this legitimacy model. If both can be achieved, the current policy mix might be inefficient or unnecessarily extractive, suggesting alternative policy designs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(livelihood_vs_growth_tradeoff, empirical, 'Whether livelihood security and capital-intensive growth are mutually exclusive policy goals.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the state''s legitimacy truly derived from livelihood security, or is this a convenient narrative to justify centralized control and resource allocation?',
    'Longitudinal studies of public opinion, social unrest, and political stability in response to fluctuations in livelihood security vs. other factors (e.g., national pride, ideological adherence).',
    'If legitimacy is primarily derived from livelihood security, the constraint is a genuine Rope. If it''s a cover for control, the constraint leans towards a Snare, with the ''beneficiary'' aspect being a form of social engineering.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'The true source of state legitimacy: direct welfare vs. control justification.').

omega_variable(
    reading_structural_delta_accuracy,
    'How accurately does the ''livelihood_security_reading'' structural delta (service delivery, consumption support, redistribution) reflect the actual policy implementation compared to other readings?',
    'Detailed policy analysis, budget allocations, and project implementation data, cross-referenced with official statements and academic interpretations of the state''s development strategy.',
    'If the structural delta is less pronounced than claimed, the ''livelihood_security_reading'' might be more performative than functional, pushing the constraint towards a Piton or Tangled Rope. If it''s highly accurate, it reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_structural_delta_accuracy, empirical, 'Accuracy of the claimed policy shift towards livelihood security.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__livelihood_security_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__livelihood_security_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__livelihood_security_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__livelihood_security_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__livelihood_security_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__livelihood_security_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__livelihood_security_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__livelihood_security_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__livelihood_security_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__livelihood_security_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__livelihood_security_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__livelihood_security_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__livelihood_security_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__livelihood_security_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__livelihood_security_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__livelihood_security_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__livelihood_security_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'performance_legitimacy' kernel. Its policy implications and resource allocations directly influence and are influenced by other readings of the same kernel, such as 'quantitative_growth_reading', 'qualitative_development_reading', and 'techno_nationalist_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
