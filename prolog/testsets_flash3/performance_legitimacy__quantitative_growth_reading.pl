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
 *   performance legitimacy, where the state's right to rule is primarily
 *   justified by its ability to deliver high GDP growth rates. This reading
 *   prioritizes investment-driven expansion, often tolerating significant
 *   environmental and social costs as necessary for economic progress. The
 *   constraint is claimed as a Tangled Rope because it genuinely coordinates
 *   vast economic activity towards a national goal, but simultaneously
 *   extracts from specific groups (environmental advocates, displaced
 *   communities) and requires active enforcement to maintain this asymmetric
 *   structure. The metrics reflect this: high extractiveness and suppression,
 *   with a rising theater ratio as the narrative of 'growth for all' becomes
 *   harder to sustain.
 *
 * KEY AGENTS:
 *   - state_leadership: Agenda setter (institutional/identity_locked) — defines and enforces the growth imperative.
 *   - industrial_export_complex: Beneficiary (organized/constrained) — profits from state-directed growth policies.
 *   - local_government_officials: Beneficiary/Agenda setter (powerful/constrained) — implement growth policies and benefit from career advancement.
 *   - environmental_advocates: Payer (powerless/trapped) — bear environmental costs, their concerns are suppressed.
 *   - local_communities_displaced: Payer (powerless/trapped) — bear social costs of development, their resistance is suppressed.
 *   - small_and_medium_enterprises: Payer (moderate/constrained) — disadvantaged by focus on large-scale projects.
 *   - qualitative_development_advocates: Excluded (moderate/constrained) — their alternative vision is marginalized.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__quantitative_growth_reading, 0.75).
domain_priors:theater_ratio(performance_legitimacy__quantitative_growth_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "Performance Legitimacy: Quantitative Growth Reading").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, 'acc302a7-fc2c-40a6-a65a-c1ae9da746a1').
narrative_ontology:cs_kernel_codification('acc302a7-fc2c-40a6-a65a-c1ae9da746a1', implicit).
narrative_ontology:cs_authority_grounding('acc302a7-fc2c-40a6-a65a-c1ae9da746a1', extraction).
narrative_ontology:cs_interpretation_layer_present('acc302a7-fc2c-40a6-a65a-c1ae9da746a1').
narrative_ontology:cs_reading_relation('acc302a7-fc2c-40a6-a65a-c1ae9da746a1', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('acc302a7-fc2c-40a6-a65a-c1ae9da746a1', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('acc302a7-fc2c-40a6-a65a-c1ae9da746a1', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('acc302a7-fc2c-40a6-a65a-c1ae9da746a1', foundational, gdp_growth_is_primary_legitimacy_metric).
narrative_ontology:cs_axiom_status(gdp_growth_is_primary_legitimacy_metric, holdable).
narrative_ontology:cs_axiom_grounding('acc302a7-fc2c-40a6-a65a-c1ae9da746a1', gdp_growth_is_primary_legitimacy_metric, conventional).
narrative_ontology:cs_axiom('acc302a7-fc2c-40a6-a65a-c1ae9da746a1', foundational, investment_driven_expansion_is_optimal_path).
narrative_ontology:cs_axiom_status(investment_driven_expansion_is_optimal_path, holdable).
narrative_ontology:cs_axiom_grounding('acc302a7-fc2c-40a6-a65a-c1ae9da746a1', investment_driven_expansion_is_optimal_path, instrumental).
narrative_ontology:cs_reference_frame('acc302a7-fc2c-40a6-a65a-c1ae9da746a1', uninterrupted_high_growth_era).
narrative_ontology:cs_drift_state('acc302a7-fc2c-40a6-a65a-c1ae9da746a1', contemporary_sustainability_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('acc302a7-fc2c-40a6-a65a-c1ae9da746a1', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, local_government_officials).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, environmental_advocates).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, local_communities_displaced).
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

% Receives preferential loans, subsidies, and policy support to drive large-scale industrial production and exports, which directly contribute to GDP growth. Benefits from the state's focus on quantitative expansion, even at the cost of efficiency or environmental impact.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, industrial_export_complex, beneficiary,
    organized, biographical, constrained, global).

% Their career advancement and political standing are directly tied to achieving local GDP growth targets. They actively promote investment, often overlooking environmental or social costs, to meet these metrics. They are both beneficiaries of the system and agents of its enforcement.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, local_government_officials, beneficiary,
    powerful, biographical, constrained, regional).

% Bear the costs of pollution, resource depletion, and ecological damage resulting from unchecked industrial expansion. Their calls for sustainable development are often suppressed or marginalized in favor of growth imperatives.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, environmental_advocates, payer,
    powerless, generational, trapped, national).

% Are often displaced by large-scale infrastructure projects or industrial zones, losing land, livelihoods, and community ties. Their resistance is typically met with state suppression, as their interests are deemed secondary to national growth targets.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, local_communities_displaced, payer,
    powerless, biographical, trapped, local).

% Struggle to compete with large state-backed enterprises for resources, market access, and policy support. They often face higher regulatory burdens and less favorable financing, as the system prioritizes large-scale, GDP-driving projects.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, small_and_medium_enterprises, payer,
    moderate, immediate, constrained, local).

% Argue for a shift from raw GDP growth to 'high-quality development' focused on innovation, sustainability, and efficiency. Their policy proposals are often sidelined or reframed to fit the quantitative growth narrative, as their metrics challenge the core legitimacy claim.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, qualitative_development_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national economic activity towards a singular goal of high GDP growth, mobilizing resources and directing investment to achieve this objective, thereby providing a clear performance metric for state legitimacy.
% TRANSFER_FUNCTION: Transfers resources, policy support, and political capital to large industrial and export-oriented sectors, and from environmental protection and local community interests, to ensure the achievement of quantitative growth targets.
% ABSENT_VOICES: Environmental advocates, displaced communities, and proponents of 'high-quality development' are systematically marginalized. They would argue for a redefinition of legitimacy that prioritizes sustainability, social equity, and innovation over raw GDP figures, but their concerns are suppressed by the growth imperative.
% DISAPPEARANCE_RATIONALE: If the legitimacy claim tied to quantitative GDP growth vanished, the entire state-capitalist model would undergo a profound reorientation. Investment priorities would shift, environmental regulations would strengthen, and local officials would be evaluated on different metrics, leading to a fundamental restructuring of economic and political incentives.
% FOUNDING_PROBLEM: The state faced a challenge of rapid economic development and poverty alleviation, requiring a clear, measurable target to mobilize resources and demonstrate progress to its population and the international community.
% FOUNDING_PROBLEM_CORROBORATION: State leadership and official media consistently attest that rapid growth remains essential for stability and national strength. Independent economists and some international observers acknowledge the historical role of growth in poverty reduction, but increasingly question its sustainability and social costs, suggesting the problem has evolved beyond raw quantitative expansion.
narrative_ontology:disappearance_verdict(performance_legitimacy__quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__quantitative_growth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high because the benefits of growth are concentrated in specific sectors and among officials, while costs (environmental, social, competitive) are diffused or borne by marginalized groups. Suppression is high because dissent against the growth model, particularly from environmental or social justice perspectives, is actively managed and contained to maintain stability and the legitimacy narrative. The theater ratio is rising as the state increasingly emphasizes 'green growth' or 'people-centered development' rhetorically, while actual policy continues to prioritize raw quantitative expansion, creating a gap between stated goals and observed outcomes. The slight dip in extractiveness at the end of the interval reflects growing internal and external pressures for more balanced development, forcing some rhetorical adjustments, but the core growth imperative remains.
 *
 * PERSPECTIVAL GAP:
 *   State leadership and the industrial-export complex perceive this as a necessary and beneficial coordination mechanism for national development. Local government officials, whose careers depend on growth metrics, also see it as a legitimate and effective system. However, environmental advocates and displaced communities experience it as a highly extractive and suppressive force, where their well-being is sacrificed for abstract growth figures. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State leadership and local government officials are beneficiaries (low d) as their power and legitimacy are directly enhanced by the constraint. The industrial-export complex is also a beneficiary, receiving direct support. Environmental advocates, displaced communities, and SMEs are targets (high d) as they bear the costs without commensurate benefits and have limited exit options. Qualitative development advocates are excluded, meaning their directionality is not directly computed but their absence from the conversation is a key feature of the constraint's suppressive function.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (rapid economic development) is still live, but its *form* (raw quantitative growth) is increasingly contested. The classification as Tangled Rope prevents mislabeling it as a pure Snare, acknowledging the genuine coordination function in mobilizing resources for development, while simultaneously highlighting the asymmetric extraction and active enforcement required to maintain the specific 'quantitative growth' reading of legitimacy. The rising theater ratio suggests a potential drift towards Piton if the coordination function further atrophies relative to the performative maintenance of the growth narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantitative_vs_qualitative_legitimacy,
    'Is the state''s legitimacy genuinely tied to raw GDP growth, or is there an unacknowledged shift towards ''high-quality development'' metrics (innovation, sustainability, equity) that this reading suppresses?',
    'Analysis of public discourse, policy documents, and official statements for shifts in rhetorical emphasis and resource allocation away from raw GDP targets, particularly in response to social and environmental pressures.',
    'If a significant shift towards qualitative metrics is observed, this reading''s extractiveness and suppression would be re-evaluated as higher, as it actively resists an emerging, more legitimate, alternative. The constraint might reclassify towards Snare or Piton if the quantitative mandate is largely performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantitative_vs_qualitative_legitimacy, conceptual, 'Ambiguity in the true basis of state legitimacy: raw growth vs. qualitative development.').

omega_variable(
    growth_cost_internalization,
    'To what extent are the environmental and social costs of quantitative growth being internalized by the state, rather than externalized onto victims?',
    'Tracking of environmental protection budgets, enforcement of pollution controls, and compensation for displaced communities over time. A rising trend in internalization would reduce the constraint''s effective extractiveness.',
    'If costs are increasingly internalized, the constraint''s extractiveness would decrease, potentially shifting its classification towards a more benign form (e.g., Rope). If externalization persists, the Snare-like qualities would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(growth_cost_internalization, empirical, 'Whether the costs of growth are borne by the state or externalized onto society.').

omega_variable(
    kernel_reading_distinction,
    'This constraint is one reading of the ''performance_legitimacy'' kernel. How distinct is this ''quantitative_growth_reading'' from its siblings (qualitative_development_reading, techno_nationalist_reading, livelihood_security_reading) in practice, and what specific structural elements would change if a sibling reading became dominant?',
    'Comparative policy analysis across different regions or historical periods where alternative readings have gained prominence, examining shifts in investment priorities, evaluation metrics for officials, and tolerance for social/environmental costs.',
    'If the readings are found to be less distinct in practice, it suggests the ''quantitative_growth_reading'' is more adaptive or has absorbed elements of its siblings, potentially reducing its extractiveness. If highly distinct, it reinforces the current classification and highlights the active suppression of alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinction and practical implications of the ''quantitative_growth_reading'' within the ''performance_legitimacy'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__quantitative_growth_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(perf_tr_t8, performance_legitimacy__quantitative_growth_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(perf_tr_t16, performance_legitimacy__quantitative_growth_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(perf_tr_t24, performance_legitimacy__quantitative_growth_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(perf_tr_t32, performance_legitimacy__quantitative_growth_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(perf_tr_t40, performance_legitimacy__quantitative_growth_reading, theater_ratio, 40, 0.4).

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
narrative_ontology:measurement(perf_su_t32, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(perf_su_t40, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__quantitative_growth_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'performance_legitimacy' kernel. Its focus on quantitative growth influences and is influenced by other readings of state legitimacy, such as those emphasizing qualitative development, techno-nationalism, or livelihood security. Each reading represents a distinct structural claim about the basis of state authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
