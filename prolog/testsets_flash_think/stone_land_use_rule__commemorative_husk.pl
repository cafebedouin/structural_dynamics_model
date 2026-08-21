% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__commemorative_husk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__commemorative_husk, []).

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
 *   constraint_id: stone_land_use_rule__commemorative_husk
 *   human_readable: Commemorative Husk of the Stone Land-Use Rule
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   This constraint describes the 'commemorative_husk' reading of the
 *   'stone_land_use_rule' kernel. The stone, originally erected as a
 *   permanent warning and land-use prohibition after a disaster, has over
 *   time lost its behavioral force. It now functions primarily as a memorial
 *   artifact, while land-use decisions proceed independently of its location.
 *   This reading asserts that the constraint imposes zero land-use
 *   restriction, with building decisions driven by convenience (e.g.,
 *   waterfront access), leading to high effective extraction from those
 *   exposed to risk.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, 0.78).
domain_priors:suppression_score(stone_land_use_rule__commemorative_husk, 0.15).
domain_priors:theater_ratio(stone_land_use_rule__commemorative_husk, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, extractiveness, 0.78).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__commemorative_husk, piton).
narrative_ontology:human_readable(stone_land_use_rule__commemorative_husk, "Commemorative Husk of the Stone Land-Use Rule").
narrative_ontology:topic_domain(stone_land_use_rule__commemorative_husk, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__commemorative_husk, '1406f300-e4df-44ae-8cb6-e6908654f64e').
narrative_ontology:cs_kernel_codification('1406f300-e4df-44ae-8cb6-e6908654f64e', fixed_text).
narrative_ontology:cs_authority_grounding('1406f300-e4df-44ae-8cb6-e6908654f64e', distributed).
narrative_ontology:cs_reading_relation('1406f300-e4df-44ae-8cb6-e6908654f64e', stone_land_use_rule__behavioral_competence, forecloses).
narrative_ontology:cs_axiom('1406f300-e4df-44ae-8cb6-e6908654f64e', foundational, memorial_over_prohibition).
narrative_ontology:cs_axiom_status(memorial_over_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('1406f300-e4df-44ae-8cb6-e6908654f64e', memorial_over_prohibition, conventional).
narrative_ontology:cs_reference_frame('1406f300-e4df-44ae-8cb6-e6908654f64e', symbolic_remembrance).
narrative_ontology:cs_drift_state('1406f300-e4df-44ae-8cb6-e6908654f64e', contemporary_land_use_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('1406f300-e4df-44ae-8cb6-e6908654f64e', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__commemorative_husk, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, coastal_developers).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, local_government).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, disaster_risk_population).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the effective absence of land-use restrictions near the waterfront, allowing them to pursue profitable development projects without impediment from the historical warning. They treat the stone as a historical curiosity, not a binding rule.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, coastal_developers, beneficiary,
    powerful, biographical, arbitrage, local).

% Administers land-use planning and permits, but does not enforce the original prohibition associated with the stone. Benefits from increased tax revenue and economic activity from coastal development. Faces political pressure to prioritize economic growth over historical warnings.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, local_government, agenda_setter,
    institutional, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__commemorative_husk, local_government, beneficiary).

% Lives in areas that the stone originally warned against developing, bearing the direct risk of future disasters. They are often unaware of the stone's original meaning or lack the power to influence land-use decisions.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, disaster_risk_population, payer,
    powerless, immediate, trapped, local).

% Historians, local elders, and cultural preservationists who understand the stone's original purpose and the disaster it commemorates. They observe the erosion of its behavioral force but often lack direct power to re-establish it.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, institutional_memory_keepers, observer,
    moderate, civilizational, analytical, local).

% Will inherit the consequences of current land-use decisions, including increased vulnerability to natural disasters due to development in high-risk areas. They are the ultimate victims of the decayed warning.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, future_generations, payer,
    powerless, civilizational, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__commemorative_husk, coastal_developers).
narrative_ontology:fixing_cost_class(stone_land_use_rule__commemorative_husk, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone, in its current state, minimally coordinates collective memory of a past disaster, serving as a symbolic reminder rather than a behavioral guide.
% TRANSFER_FUNCTION: Transfers the risk of future disasters from coastal developers and local government (who benefit from development) to the disaster-risk population and future generations.
% ABSENT_VOICES: The original victims of the disaster, whose experience the stone was meant to enshrine as a warning, are absent. Future generations, who will bear the consequences, also have no voice in current land-use decisions.
% DISAPPEARANCE_RATIONALE: If the stone (as a land-use rule) vanished overnight, the world would be largely unchanged in terms of land-use behavior, as its behavioral force has already atrophied to zero. Development would continue as before, driven by economic incentives rather than historical warnings.
% FOUNDING_PROBLEM: Preventing the recurrence of a specific, devastating natural disaster by establishing a permanent, visible prohibition on land use in high-risk areas.
% FOUNDING_PROBLEM_CORROBORATION: Institutional memory keepers, historical records, and disaster anthropologists corroborate that the original problem was live and the stone was a direct response. However, they also attest that the rule's behavioral force is now dead, with the stone serving primarily as a memorial. Coastal developers and local government implicitly attest to its dead status by their actions, though they may pay lip service to its symbolic value.
narrative_ontology:disappearance_verdict(stone_land_use_rule__commemorative_husk, world_unchanged).
narrative_ontology:founding_problem_status(stone_land_use_rule__commemorative_husk, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__commemorative_husk, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(stone_land_use_rule__commemorative_husk, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__commemorative_husk, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__commemorative_husk_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(stone_land_use_rule__commemorative_husk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the transfer of disaster risk and the benefits of unconstrained development to coastal developers and local government. Suppression is very low (0.15) because the rule is no longer actively enforced; its persistence relies on inertia and its symbolic value, not coercion. The theater ratio is very high (0.85) because the stone's primary function has shifted from a behavioral prohibition to a performative act of remembrance, with little to no functional impact on land use. Accessibility collapse is low (0.10) as alternatives (developing elsewhere, enforcing the rule) are not structurally blocked, but simply ignored. Resistance is negligible (0.05) because the constraint is not actively imposing costs that would provoke it.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of coastal developers and local government, the stone is a benign historical marker that does not impede progress. From the perspective of the disaster-risk population and future generations, its decay represents a dangerous failure of institutional memory, exposing them to preventable harm. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal developers and local government are clear beneficiaries, gaining from the effective removal of land-use restrictions. The disaster-risk population and future generations are the victims, bearing the costs of increased vulnerability. Institutional memory keepers act as observers, understanding the historical context but lacking direct power to re-establish the rule's force.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a clear case of mandatrophy. The original mandate (preventing disaster through land-use prohibition) has atrophied, and the constraint persists as a 'piton' due to institutional inertia and its reinterpretation as a purely symbolic gesture. The founding problem is 'dead' in terms of behavioral impact, but the artifact remains, allowing for extractive drift towards convenient, high-risk development.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reversibility_of_decay,
    'Is the decay of the stone''s behavioral force irreversible, or could a policy intervention or renewed public awareness re-establish its original function?',
    'Analysis of successful historical precedents where similar decayed warnings were reactivated, or a policy experiment attempting to re-enforce the rule.',
    'If reversible, the constraint''s classification could shift towards a ''snare'' (if re-enforced for extractive purposes) or ''rope'' (if genuinely re-established for coordination). If irreversible, its ''piton'' status is more entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_of_decay, empirical, 'Whether the loss of behavioral force is a permanent state or subject to change.').

omega_variable(
    intentional_neglect_vs_passive_decay,
    'Is the ''extractive drift toward waterfront convenience'' a result of intentional, coordinated neglect by beneficiaries, or a passive consequence of the passage of time and changing priorities?',
    'Investigation into lobbying efforts, policy decisions, and public statements by coastal developers and local government regarding the stone''s interpretation and land-use regulations.',
    'If intentional, the extractiveness is more deliberate and the constraint leans more towards a ''snare'' (active suppression of the original rule). If passive, it reinforces the ''piton'' classification (atrophied function due to inertia).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentional_neglect_vs_passive_decay, empirical, 'Distinguishing between active subversion and passive decay of the rule''s intent.').

omega_variable(
    framing_under_determination_stone_rule,
    'Does the ''commemorative_husk'' framing accurately capture the constraint''s structural reality, or would the ''behavioral_competence'' framing (stone as live prohibition) offer a more accurate, albeit aspirational, classification?',
    'Empirical observation of actual land-use decisions and enforcement actions over time. If development patterns consistently ignore the stone, the ''husk'' framing is corroborated. If new development respects the stone''s original intent, the ''competence'' framing gains support.',
    'If the ''behavioral_competence'' framing were adopted, the constraint would likely classify as a ''rope'' or ''mountain'' (if genuinely natural law), with significantly lower extractiveness and higher suppression (enforcement of the prohibition). This would fundamentally alter the analysis of risk and responsibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination_stone_rule, conceptual, 'Ambiguity in whether the stone''s current function is purely symbolic or retains latent behavioral force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__commemorative_husk, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__commemorative_husk, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ston_tr_t10, stone_land_use_rule__commemorative_husk, theater_ratio, 10, 0.3).
narrative_ontology:measurement(ston_tr_t20, stone_land_use_rule__commemorative_husk, theater_ratio, 20, 0.5).
narrative_ontology:measurement(ston_tr_t30, stone_land_use_rule__commemorative_husk, theater_ratio, 30, 0.7).
narrative_ontology:measurement(ston_tr_t40, stone_land_use_rule__commemorative_husk, theater_ratio, 40, 0.8).
narrative_ontology:measurement(ston_tr_t50, stone_land_use_rule__commemorative_husk, theater_ratio, 50, 0.85).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__commemorative_husk, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(ston_be_t10, stone_land_use_rule__commemorative_husk, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(ston_be_t20, stone_land_use_rule__commemorative_husk, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(ston_be_t30, stone_land_use_rule__commemorative_husk, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(ston_be_t40, stone_land_use_rule__commemorative_husk, base_extractiveness, 40, 0.72).
narrative_ontology:measurement(ston_be_t50, stone_land_use_rule__commemorative_husk, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__commemorative_husk, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ston_su_t10, stone_land_use_rule__commemorative_husk, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(ston_su_t20, stone_land_use_rule__commemorative_husk, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(ston_su_t30, stone_land_use_rule__commemorative_husk, suppression_requirement, 30, 0.2).
narrative_ontology:measurement(ston_su_t40, stone_land_use_rule__commemorative_husk, suppression_requirement, 40, 0.18).
narrative_ontology:measurement(ston_su_t50, stone_land_use_rule__commemorative_husk, suppression_requirement, 50, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__commemorative_husk, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
