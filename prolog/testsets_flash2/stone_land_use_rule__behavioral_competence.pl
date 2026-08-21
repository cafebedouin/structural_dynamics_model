% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__behavioral_competence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__behavioral_competence, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: stone_land_use_rule__behavioral_competence
 *   human_readable: Stone Land-Use Rule (Behavioral Competence Reading)
 *   domain: disaster_anthropology/land_use_governance
 *
 * SUMMARY:
 *   This constraint describes the 'stone rule' as a live land-use
 *   prohibition, where daily spatial practice and institutional memory
 *   enforce compliance. It is one reading of the 'stone_land_use_rule'
 *   kernel, focusing on its active behavioral force and sustained low
 *   extractiveness over 78 years. The rule, often marked by physical 'tsunami
 *   stones', dictates that settlement should not occur below a certain
 *   elevation, a practice maintained through intergenerational transmission
 *   of disaster memory and local governance. This reading emphasizes the
 *   rule's continued efficacy in shaping behavior and land use, accepting the
 *   economic costs of non-development as a necessary trade-off for safety.
 *
 * KEY AGENTS:
 *   - coastal_communities: Primary beneficiary (organized/constrained) — self-enforces compliance, benefits from safety.
 *   - local_government: Agenda setter (institutional/constrained) — formalizes and upholds the rule.
 *   - developers: Payer (powerful/mobile) — bears opportunity costs of non-development.
 *   - future_generations: Beneficiary (powerless/trapped) — ultimate beneficiaries of long-term safety.
 *   - disaster_anthropologists: Observer (analytical/analytical) — studies the rule's efficacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__behavioral_competence, 0.15).
domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, 0.25).
domain_priors:theater_ratio(stone_land_use_rule__behavioral_competence, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, extractiveness, 0.15).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, rope).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Stone Land-Use Rule (Behavioral Competence Reading)").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "disaster_anthropology/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, 'b7d498f4-71f5-41ed-9e14-4c14d3b8a64a').
narrative_ontology:cs_kernel_codification('b7d498f4-71f5-41ed-9e14-4c14d3b8a64a', formalized).
narrative_ontology:cs_authority_grounding('b7d498f4-71f5-41ed-9e14-4c14d3b8a64a', practice).
narrative_ontology:cs_interpretation_layer_present('b7d498f4-71f5-41ed-9e14-4c14d3b8a64a').
narrative_ontology:cs_reading_relation('b7d498f4-71f5-41ed-9e14-4c14d3b8a64a', stone_land_use_rule__commemorative_husk, coexists_with).
narrative_ontology:cs_axiom('b7d498f4-71f5-41ed-9e14-4c14d3b8a64a', foundational, disaster_memory_is_behaviorally_binding).
narrative_ontology:cs_axiom_status(disaster_memory_is_behaviorally_binding, holdable).
narrative_ontology:cs_axiom_grounding('b7d498f4-71f5-41ed-9e14-4c14d3b8a64a', disaster_memory_is_behaviorally_binding, conventional).
narrative_ontology:cs_axiom('b7d498f4-71f5-41ed-9e14-4c14d3b8a64a', secondary, spatial_practice_encodes_prohibition).
narrative_ontology:cs_axiom_status(spatial_practice_encodes_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('b7d498f4-71f5-41ed-9e14-4c14d3b8a64a', spatial_practice_encodes_prohibition, empirically_contingent).
narrative_ontology:cs_reference_frame('b7d498f4-71f5-41ed-9e14-4c14d3b8a64a', active_disaster_memory_governance).
narrative_ontology:cs_drift_state('b7d498f4-71f5-41ed-9e14-4c14d3b8a64a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b7d498f4-71f5-41ed-9e14-4c14d3b8a64a', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, coastal_communities).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(stone_land_use_rule__behavioral_competence, developers).
narrative_ontology:constraint_vindicates(stone_land_use_rule__behavioral_competence, institutional_memory_preservation).
narrative_ontology:constraint_vindicates(stone_land_use_rule__behavioral_competence, disaster_risk_reduction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities live in areas historically impacted by tsunamis. They benefit from the rule by avoiding catastrophic loss of life and property, accepting the economic costs of not developing high-risk coastal zones. Their compliance is largely self-enforced through shared memory and daily practice.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, coastal_communities, beneficiary,
    organized, generational, constrained, local).

% Administers land-use planning and zoning, incorporating the 'stone rule' into local ordinances. While not actively enforcing daily compliance, they uphold the prohibition against development in designated areas, facing political pressure from developers but also from the communities themselves.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, local_government, agenda_setter,
    institutional, biographical, constrained, local).

% Seek to develop coastal properties for economic gain but are prohibited from doing so in areas designated by the 'stone rule'. They bear the opportunity cost of foregone development, but can shift their investments to other regions.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, developers, payer,
    powerful, immediate, mobile, regional).

% Will inherit a landscape protected from historical disaster risks, benefiting from the long-term preservation of safety and ecological integrity. They have no direct voice in the current enforcement but are the ultimate beneficiaries of the rule's persistence.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, future_generations, beneficiary,
    powerless, civilizational, trapped, local).

% Study the long-term efficacy of indigenous and community-based disaster risk reduction strategies, including the 'stone rule' as a case study in institutional memory and behavioral adaptation. They analyze its persistence and impact without direct involvement.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, disaster_anthropologists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates land-use practices across generations to prevent settlement in high-risk coastal zones, ensuring collective safety and preserving institutional memory of past disasters.
% TRANSFER_FUNCTION: Transfers the long-term benefit of safety and disaster avoidance to coastal communities and future generations, at the cost of foregone economic development opportunities for developers and immediate land-use flexibility for current residents.
% ABSENT_VOICES: Historical victims of past tsunamis, whose experiences are encoded in the rule, are absent but their memory is actively invoked. Future generations, while beneficiaries, have no direct voice in maintaining the rule.
% DISAPPEARANCE_RATIONALE: If the 'stone rule' vanished overnight, coastal development would likely accelerate in previously prohibited zones, leading to increased vulnerability to future tsunamis and the loss of critical institutional memory regarding safe land use. The social and physical landscape would fundamentally alter.
% FOUNDING_PROBLEM: Recurrent catastrophic loss of life and property from tsunamis due to settlement in vulnerable coastal areas, and the decay of oral warnings over generations.
% FOUNDING_PROBLEM_CORROBORATION: Coastal communities themselves, through their elders and daily practices, corroborate that the threat of tsunamis is live and the rule remains essential. Disaster anthropologists and historical records from outside the immediate beneficiaries also attest to the ongoing risk and the rule's efficacy.
narrative_ontology:disappearance_verdict(stone_land_use_rule__behavioral_competence, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__behavioral_competence, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__behavioral_competence, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(stone_land_use_rule__behavioral_competence, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__behavioral_competence, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__behavioral_competence_tests).
:- end_tests(stone_land_use_rule__behavioral_competence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the rule primarily serves a collective safety function, with costs (foregone development) accepted as a necessary trade-off. Suppression is low (0.25) because compliance is largely voluntary, driven by shared cultural memory and community norms, rather than active coercion. The rule's persistence relies on behavioral competence and institutional memory, not on suppressing alternatives through force. Theater ratio is very low (0.05) as the rule is genuinely functional and not performative; its physical markers (tsunami stones) are mnemonic devices, not theatrical props. Accessibility collapse is high (0.85) because the rule effectively eliminates the option of safe settlement in prohibited zones, making alternatives (developing elsewhere) the only viable path. Resistance is low (0.08) because the rule is widely accepted within the affected communities, though developers may occasionally challenge it.
 *
 * PERSPECTIVAL GAP:
 *   Coastal communities and future generations experience this as a vital, low-cost coordination mechanism for survival. Developers, however, experience it as a direct economic constraint, limiting their profit opportunities. The local government balances these perspectives, formalizing the rule while managing development pressures. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal communities and future generations are clear beneficiaries (low d) as they gain safety and resilience. Developers are payers (high d) as they bear the economic costs of non-development. The local government acts as an agenda-setter, balancing community safety with development pressures, resulting in a moderate d. The rule's low extractiveness and suppression, combined with high accessibility collapse for unsafe options, means that even for payers, the effective extraction is not extreme, as the safety benefit is widely acknowledged.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a vital, low-extraction coordination mechanism as a snare or piton. The 'stone rule' is not a decaying performance (piton) nor a purely extractive mechanism (snare); its mandate (disaster risk reduction) is demonstrably live, and its function is actively maintained through daily practice and institutional memory. The low theater ratio and high accessibility collapse for unsafe options confirm its active, non-performative role.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_decay_rate,
    'At what rate does the behavioral competence encoded in the ''stone rule'' decay across generations without a direct disaster experience?',
    'Longitudinal ethnographic studies tracking intergenerational transmission of disaster memory and compliance rates in communities with varying intervals since the last major event.',
    'A high decay rate would suggest the rule is more fragile than currently assessed, potentially shifting its classification towards a ''piton'' over longer time horizons if active reinforcement isn''t introduced. A low decay rate would strengthen its ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_decay_rate, empirical, 'Rate of decay of behavioral compliance with the ''stone rule'' over time.').

omega_variable(
    economic_pressure_threshold,
    'What level of economic development pressure (e.g., tourism demand, population growth) would be required to significantly erode compliance with the ''stone rule''?',
    'Comparative case studies of communities facing different levels of development pressure, or agent-based modeling of land-use decisions under varying economic incentives.',
    'A low threshold would indicate the rule''s resilience is weaker than assumed, making it vulnerable to reclassification towards a ''tangled_rope'' or ''snare'' if economic extraction overrides safety coordination. A high threshold would confirm its robustness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_pressure_threshold, empirical, 'Threshold of economic pressure for erosion of ''stone rule'' compliance.').

omega_variable(
    reading_divergence_point,
    'Is the ''stone rule'' primarily a live land-use prohibition (behavioral_competence reading) or a decayed commemorative artifact (commemorative_husk reading)?',
    'Empirical observation of daily land-use decisions, community narratives, and enforcement actions. If development occurs in prohibited zones without significant resistance, the ''commemorative_husk'' reading gains support.',
    'If the ''commemorative_husk'' reading is validated, the constraint''s extractiveness would be near zero (as it no longer imposes costs), and its classification would shift to ''piton'' (inertial artifact) or ''mountain'' (natural feature with no behavioral force).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_divergence_point, conceptual, 'Ambiguity between active behavioral rule and symbolic memorial.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__behavioral_competence, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__behavioral_competence, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ston_tr_t15, stone_land_use_rule__behavioral_competence, theater_ratio, 15, 0.05).
narrative_ontology:measurement(ston_tr_t30, stone_land_use_rule__behavioral_competence, theater_ratio, 30, 0.05).
narrative_ontology:measurement(ston_tr_t45, stone_land_use_rule__behavioral_competence, theater_ratio, 45, 0.05).
narrative_ontology:measurement(ston_tr_t60, stone_land_use_rule__behavioral_competence, theater_ratio, 60, 0.05).
narrative_ontology:measurement(ston_tr_t78, stone_land_use_rule__behavioral_competence, theater_ratio, 78, 0.05).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__behavioral_competence, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ston_be_t15, stone_land_use_rule__behavioral_competence, base_extractiveness, 15, 0.16).
narrative_ontology:measurement(ston_be_t30, stone_land_use_rule__behavioral_competence, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(ston_be_t45, stone_land_use_rule__behavioral_competence, base_extractiveness, 45, 0.14).
narrative_ontology:measurement(ston_be_t60, stone_land_use_rule__behavioral_competence, base_extractiveness, 60, 0.14).
narrative_ontology:measurement(ston_be_t78, stone_land_use_rule__behavioral_competence, base_extractiveness, 78, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__behavioral_competence, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ston_su_t15, stone_land_use_rule__behavioral_competence, suppression_requirement, 15, 0.28).
narrative_ontology:measurement(ston_su_t30, stone_land_use_rule__behavioral_competence, suppression_requirement, 30, 0.26).
narrative_ontology:measurement(ston_su_t45, stone_land_use_rule__behavioral_competence, suppression_requirement, 45, 0.25).
narrative_ontology:measurement(ston_su_t60, stone_land_use_rule__behavioral_competence, suppression_requirement, 60, 0.25).
narrative_ontology:measurement(ston_su_t78, stone_land_use_rule__behavioral_competence, suppression_requirement, 78, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__behavioral_competence, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is the 'behavioral_competence' reading of the 'stone_land_use_rule' kernel. It focuses on the rule's active role in shaping land use and behavior, in contrast to the 'commemorative_husk' reading which views it as a decayed symbolic gesture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
