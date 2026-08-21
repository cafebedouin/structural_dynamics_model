% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__quantitative_growth_reading, []).

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
 *   constraint_id: performance_legitimacy__quantitative_growth_reading
 *   human_readable: Performance Legitimacy: Quantitative Growth Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint describes the 'quantitative growth' reading of
 *   performance legitimacy, where state authority is primarily justified by
 *   maintaining high GDP growth rates. This reading prioritizes
 *   investment-driven models, tolerates export dependency and overcapacity,
 *   and measures local government officials on GDP targets. The constraint is
 *   claimed as a Tangled Rope because it genuinely coordinates vast economic
 *   activity towards a national goal, but does so with significant asymmetric
 *   extraction from environmental and labor groups, requiring active
 *   enforcement to maintain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__quantitative_growth_reading, 0.75).
domain_priors:theater_ratio(performance_legitimacy__quantitative_growth_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "Performance Legitimacy: Quantitative Growth Reading").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, '0ba55ba7-a38d-4ade-a2f6-35b9c2e0e471').
narrative_ontology:cs_kernel_codification('0ba55ba7-a38d-4ade-a2f6-35b9c2e0e471', implicit).
narrative_ontology:cs_authority_grounding('0ba55ba7-a38d-4ade-a2f6-35b9c2e0e471', extraction).
narrative_ontology:cs_interpretation_layer_present('0ba55ba7-a38d-4ade-a2f6-35b9c2e0e471').
narrative_ontology:cs_reading_relation('0ba55ba7-a38d-4ade-a2f6-35b9c2e0e471', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('0ba55ba7-a38d-4ade-a2f6-35b9c2e0e471', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_reading_relation('0ba55ba7-a38d-4ade-a2f6-35b9c2e0e471', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('0ba55ba7-a38d-4ade-a2f6-35b9c2e0e471', foundational, gdp_growth_is_primary_legitimacy_metric).
narrative_ontology:cs_axiom_status(gdp_growth_is_primary_legitimacy_metric, holdable).
narrative_ontology:cs_axiom_grounding('0ba55ba7-a38d-4ade-a2f6-35b9c2e0e471', gdp_growth_is_primary_legitimacy_metric, conventional).
narrative_ontology:cs_axiom('0ba55ba7-a38d-4ade-a2f6-35b9c2e0e471', secondary, economic_expansion_ensures_social_stability).
narrative_ontology:cs_axiom_status(economic_expansion_ensures_social_stability, holdable).
narrative_ontology:cs_axiom_grounding('0ba55ba7-a38d-4ade-a2f6-35b9c2e0e471', economic_expansion_ensures_social_stability, empirically_contingent).
narrative_ontology:cs_reference_frame('0ba55ba7-a38d-4ade-a2f6-35b9c2e0e471', uninterrupted_high_growth_era).
narrative_ontology:cs_drift_state('0ba55ba7-a38d-4ade-a2f6-35b9c2e0e471', contemporary_global_slowdown_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0ba55ba7-a38d-4ade-a2f6-35b9c2e0e471', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, local_government_officials).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, environmental_advocates).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, labor_migrants).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, small_and_medium_enterprises).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets national economic policy, prioritizing high GDP growth as the primary metric for state legitimacy. Directs investment, tolerates overcapacity, and manages social stability to ensure growth targets are met. Their legitimacy is tied to this performance.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, state_leadership, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefits from state-directed investment, subsidies, and policies that favor export-oriented heavy industry. Their growth contributes directly to GDP figures, reinforcing the legitimacy narrative. They face pressure to maintain output regardless of market signals.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, industrial_export_complex, beneficiary,
    organized, biographical, constrained, global).

% Their careers and political standing are directly tied to achieving local GDP growth targets. They implement policies that prioritize investment and industrial output, often at the expense of environmental protection or social welfare, to demonstrate 'performance'.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, local_government_officials, beneficiary,
    powerful, immediate, identity_locked, regional).

% Bear the costs of pollution and resource depletion resulting from unchecked industrial expansion. Their concerns are often suppressed or sidelined in favor of growth imperatives. Exit options are limited to local protest or emigration.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, environmental_advocates, payer,
    powerless, generational, trapped, local).

% Provide the workforce for the industrial complex, often under precarious conditions and with limited social protections. Their labor fuels growth, but they receive a disproportionately small share of the benefits and face significant social and economic insecurity. Exit means losing employment.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, labor_migrants, payer,
    powerless, immediate, constrained, national).

% Struggle to compete for resources and attention against large state-backed industries. They often face higher regulatory burdens and less access to credit, despite being significant employers. Their contribution to 'quality' growth is undervalued.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, small_and_medium_enterprises, payer,
    moderate, biographical, constrained, local).

% Argue for a shift from raw GDP growth to 'high-quality development' focused on innovation, sustainability, and efficiency. Their policy proposals are often marginalized or reframed to fit the quantitative growth narrative, as they challenge the core legitimacy claim.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, qualitative_development_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national and local government efforts, industrial policy, and labor allocation towards a singular, measurable goal of GDP growth, providing a clear performance metric for state legitimacy.
% TRANSFER_FUNCTION: Transfers resources (subsidies, land, labor) to the industrial-export complex and local governments, in exchange for their contribution to GDP growth, while externalizing costs (environmental, social) onto the general populace and marginalized groups.
% ABSENT_VOICES: Environmental groups, independent labor unions, and advocates for social welfare or qualitative development are often excluded from policy-making, as their priorities conflict with the singular focus on quantitative growth. Their concerns are either suppressed or reframed as secondary to economic expansion.
% DISAPPEARANCE_RATIONALE: If the legitimacy claim tied to quantitative GDP growth vanished, the entire state apparatus, industrial policy, and local governance incentives would need to fundamentally reorganize. Investment would shift, environmental and social costs would be re-evaluated, and new metrics for state performance would emerge, leading to a profound reordering of the political economy.
% FOUNDING_PROBLEM: The state faced a challenge of establishing legitimacy and demonstrating effective governance, particularly in a context of rapid modernization and development, where tangible economic progress was seen as a key indicator of success.
% FOUNDING_PROBLEM_CORROBORATION: State leadership consistently reiterates the importance of GDP growth for stability and national rejuvenation, citing ongoing development needs. While some internal and external critics argue for a shift to 'quality' growth, the official narrative and policy implementation continue to prioritize quantitative expansion, indicating the problem is still live from the perspective of the agenda-setters.
narrative_ontology:disappearance_verdict(performance_legitimacy__quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__quantitative_growth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(performance_legitimacy__quantitative_growth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__quantitative_growth_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__quantitative_growth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__quantitative_growth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the singular focus on GDP growth leads to externalization of costs (environmental degradation, labor exploitation) and misallocation of resources (overcapacity, inefficient investment) that benefit specific industrial complexes and officials at the expense of broader societal welfare. Suppression (0.75) is high due to the active marginalization of dissenting voices (environmental advocates, labor groups) and the top-down enforcement of growth targets. Theater ratio (0.45) is moderate, reflecting that while some growth is real, a significant portion of economic activity is driven by performative adherence to targets rather than genuine market demand or sustainable development. The slight dip in extractiveness and suppression at the end of the interval reflects recent, nascent pressures for 'quality' growth, but the core quantitative imperative remains strong.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state leadership and the industrial complex, this constraint is a necessary coordination mechanism for national development. From the perspective of environmental advocates and labor migrants, it is a highly extractive system that sacrifices their well-being for abstract growth figures. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State leadership and local government officials are beneficiaries (d near 0.0-0.2) as their legitimacy and careers are directly tied to the growth figures. The industrial-export complex is also a beneficiary (d near 0.1-0.3) due to state support. Environmental advocates, labor migrants, and small-to-medium enterprises are victims (d near 0.7-0.9) as they bear the costs of this growth model without commensurate benefits. Qualitative development advocates are excluded, their concerns suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (delivering economic progress) is still 'live' from the perspective of the agenda-setters, preventing a full Piton classification. However, the rising theater ratio and the 'contested' status of the founding problem suggest a drift towards performative maintenance over genuine problem-solving, indicating a potential future Mandatrophy if the focus on raw GDP growth persists despite its diminishing returns for broad welfare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_quality_vs_quantity,
    'To what extent does the measured GDP growth reflect genuine, sustainable economic development versus inefficient, environmentally damaging, or debt-fueled expansion?',
    'Independent audits of national accounts, disaggregated analysis of growth components (e.g., consumption vs. investment, green vs. brown industries), and long-term environmental impact assessments.',
    'If growth is found to be largely ''low quality,'' the constraint''s effective extractiveness and theater ratio would be higher, and its coordination function would be re-evaluated as less effective, pushing it closer to a Snare or Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_quality_vs_quantity, empirical, 'Distinguishing between ''good'' and ''bad'' growth within the quantitative metric.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the state''s legitimacy genuinely derived from quantitative growth, or is growth merely a convenient proxy for other, unstated sources of legitimacy (e.g., national security, social stability, historical narrative)?',
    'Sociological studies of public opinion, analysis of state propaganda, and examination of policy responses during periods of low growth. If legitimacy holds despite growth slowdowns, the proxy hypothesis gains strength.',
    'If growth is a proxy, the constraint''s true coordination function is obscured, and its persistence may be more tied to other, potentially more extractive, mechanisms of social control, reclassifying it as a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Whether quantitative growth is the true source of legitimacy or a convenient proxy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of environmental and labor advocates structural (external barriers) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-policy-shift suppression trajectory: if suppression persists after the growth-first policy is relaxed, reclassify as partially internalized. Analysis of media control, censorship, and educational narratives.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — advocates carry the suppression with them, making resistance harder even if external barriers are lowered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dissenting voices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__quantitative_growth_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(perf_tr_t8, performance_legitimacy__quantitative_growth_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(perf_tr_t16, performance_legitimacy__quantitative_growth_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(perf_tr_t24, performance_legitimacy__quantitative_growth_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(perf_tr_t32, performance_legitimacy__quantitative_growth_reading, theater_ratio, 32, 0.5).
narrative_ontology:measurement(perf_tr_t40, performance_legitimacy__quantitative_growth_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(perf_be_t8, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(perf_be_t16, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(perf_be_t24, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(perf_be_t32, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(perf_be_t40, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(perf_su_t8, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(perf_su_t16, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(perf_su_t24, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(perf_su_t32, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 32, 0.78).
narrative_ontology:measurement(perf_su_t40, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__quantitative_growth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__quantitative_growth_reading, 0.15).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, state_directed_investment_policy).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, export_oriented_industrialization).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'performance_legitimacy' kernel. Its focus on quantitative growth influences and is influenced by other readings of state legitimacy, as well as specific economic policies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
