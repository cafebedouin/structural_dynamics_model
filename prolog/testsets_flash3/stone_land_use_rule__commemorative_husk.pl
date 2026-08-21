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
 *   constraint_id: stone_land_use_rule__commemorative_husk
 *   human_readable: Commemorative Husk Reading of Stone Land-Use Rule
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   This constraint describes the 'commemorative husk' reading of a
 *   historical stone marker, where its original function as a land-use
 *   prohibition has decayed into a mere symbolic gesture. The stone, once a
 *   critical warning against building in hazardous zones, is now treated as a
 *   historical artifact, allowing development to proceed in areas it once
 *   forbade. This reading highlights the high extractiveness of convenience
 *   and profit from ignoring historical warnings, coupled with high
 *   theatricality in maintaining the stone's presence without its behavioral
 *   force.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, 0.85).
domain_priors:suppression_score(stone_land_use_rule__commemorative_husk, 0.1).
domain_priors:theater_ratio(stone_land_use_rule__commemorative_husk, 0.9).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, extractiveness, 0.85).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, theater_ratio, 0.9).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__commemorative_husk, piton).
narrative_ontology:human_readable(stone_land_use_rule__commemorative_husk, "Commemorative Husk Reading of Stone Land-Use Rule").
narrative_ontology:topic_domain(stone_land_use_rule__commemorative_husk, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__commemorative_husk, '64ed908c-530d-4792-a437-16683b6155e1').
narrative_ontology:cs_kernel_codification('64ed908c-530d-4792-a437-16683b6155e1', fixed_text).
narrative_ontology:cs_authority_grounding('64ed908c-530d-4792-a437-16683b6155e1', practice).
narrative_ontology:cs_interpretation_layer_present('64ed908c-530d-4792-a437-16683b6155e1').
narrative_ontology:cs_reading_relation('64ed908c-530d-4792-a437-16683b6155e1', stone_land_use_rule__behavioral_competence, forecloses).
narrative_ontology:cs_axiom('64ed908c-530d-4792-a437-16683b6155e1', foundational, historical_markers_are_symbolic_not_regulatory).
narrative_ontology:cs_axiom_status(historical_markers_are_symbolic_not_regulatory, holdable).
narrative_ontology:cs_axiom_grounding('64ed908c-530d-4792-a437-16683b6155e1', historical_markers_are_symbolic_not_regulatory, conventional).
narrative_ontology:cs_axiom('64ed908c-530d-4792-a437-16683b6155e1', secondary, economic_development_supersedes_ancient_warnings).
narrative_ontology:cs_axiom_status(economic_development_supersedes_ancient_warnings, holdable).
narrative_ontology:cs_axiom_grounding('64ed908c-530d-4792-a437-16683b6155e1', economic_development_supersedes_ancient_warnings, instrumental).
narrative_ontology:cs_reference_frame('64ed908c-530d-4792-a437-16683b6155e1', stone_as_historical_marker).
narrative_ontology:cs_drift_state('64ed908c-530d-4792-a437-16683b6155e1', contemporary_development_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('64ed908c-530d-4792-a437-16683b6155e1', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__commemorative_husk, stone_land_use_rule).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, tourists_and_visitors).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, local_government_officials).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Prioritize maximizing buildable area and waterfront access. They treat the stone as a historical marker with no bearing on contemporary land-use decisions, effectively extracting value from previously restricted zones by building on them.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, coastal_developers, agenda_setter,
    powerful, biographical, mobile, local).

% Are nominally responsible for upholding land-use regulations but face pressure from developers and a public that has forgotten the stone's original purpose. They bear the diffuse cost of maintaining a symbolic gesture without enforcing its original intent, and the potential future cost of disaster if the warning is ignored.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, local_government_officials, payer,
    institutional, generational, constrained, local).

% Hold the historical memory of the stone's original warning and its purpose. They are excluded from contemporary land-use planning processes and their concerns are dismissed as sentimental, but their identity is tied to the land and its history.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, descendants_of_original_settlers, excluded,
    powerless, generational, identity_locked, local).

% Benefit from increased access to waterfront properties and amenities that would otherwise be restricted. They view the stone as an interesting historical curiosity, unaware of its original function as a critical warning.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, tourists_and_visitors, beneficiary,
    moderate, immediate, mobile, regional).

% Observe the increasing development in hazardous zones and predict future disaster impacts, but have no direct authority to enforce the stone's original land-use prohibition. They analyze the gap between historical warning and current practice.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, disaster_risk_analysts, observer,
    analytical, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It nominally coordinates a shared understanding of historical memory and cultural heritage, serving as a focal point for local identity.
% TRANSFER_FUNCTION: It transfers the symbolic value of historical remembrance to the community, while implicitly transferring the right to develop previously restricted land from the collective memory to private developers.
% ABSENT_VOICES: The original intent of the stone, representing the collective wisdom and survival imperative of past generations, is absent from current land-use discourse. The voices of those who experienced the original disaster are no longer heard in decision-making.
% DISAPPEARANCE_RATIONALE: If the stone vanished overnight, current land-use practices would remain unchanged, as its behavioral force has already atrophied. It would only remove a symbolic artifact, not alter any active regulatory or social constraint.
% FOUNDING_PROBLEM: The stone was erected to mark a safe elevation above a historical disaster (e.g., a tsunami or flood), serving as a permanent, unambiguous land-use prohibition to prevent future loss of life.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, oral traditions from descendants of original settlers, and geological evidence corroborate the stone's original purpose as a disaster warning and land-use rule. Contemporary land-use planners and developers, however, treat it as a mere historical artifact with no current regulatory force.
narrative_ontology:disappearance_verdict(stone_land_use_rule__commemorative_husk, world_unchanged).
narrative_ontology:founding_problem_status(stone_land_use_rule__commemorative_husk, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__commemorative_husk, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(stone_land_use_rule__commemorative_husk, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__commemorative_husk, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.85) is high because the constraint's original purpose (preventing hazardous development) has been completely subverted, allowing developers to build on valuable, but dangerous, land. Suppression (0.1) is low because there is almost no active enforcement of the stone's original rule; its behavioral force has atrophied. The theater ratio (0.9) is very high, as the stone is maintained and perhaps even celebrated, but its functional warning is entirely ignored. Accessibility collapse (0.15) is low because alternatives (building elsewhere) are not collapsed; rather, the constraint itself has collapsed, opening up previously inaccessible areas. Resistance (0.05) is low because the original warning is largely forgotten or dismissed, so there is little active opposition to its decay.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of coastal developers, the stone is a non-constraint, allowing them to pursue profitable ventures. From the perspective of descendants of original settlers, it is a critical, ignored warning. The engine's classification as a Piton reflects the atrophied function and performative maintenance, capturing the gap between its original intent and current reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal developers are the primary beneficiaries, gaining access to valuable land. Local government officials bear the diffuse costs of maintaining a defunct rule and the future risk. Descendants of original settlers are excluded, their historical memory ignored. Tourists benefit from new amenities. Disaster risk analysts observe the decay but cannot directly intervene.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint clearly exhibits mandatrophy: its original mandate (preventing disaster through land-use prohibition) is dead, but the artifact persists as a 'commemorative husk.' The classification as a Piton correctly identifies this decay, preventing it from being mislabeled as a Rope (which would imply active, beneficial coordination) or a Snare (which would imply active, coercive extraction). The persistence is due to institutional inertia and the convenience of ignoring the warning, rather than active enforcement or concentrated benefit from its operation as a rule.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_vs_current_interpretation,
    'Is the stone''s original intent as a land-use prohibition still understood and acknowledged by any significant portion of the local population or governing bodies?',
    'Surveys of local residents, interviews with government officials, and analysis of public records and land-use planning documents for explicit references to the stone''s original function.',
    'If a significant understanding persists, the ''commemorative_husk'' reading''s low suppression and high theater ratio might be slightly lower, indicating some residual behavioral force or resistance. If not, the current classification is strongly reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_vs_current_interpretation, empirical, 'Ambiguity regarding the collective memory and understanding of the stone''s original purpose.').

omega_variable(
    future_disaster_risk_acknowledgment,
    'To what extent do current land-use decisions explicitly acknowledge and mitigate the disaster risks that the stone originally warned against, even if not directly referencing the stone?',
    'Review of modern building codes, zoning regulations, and environmental impact assessments for the area, comparing them to the historical disaster profile and the stone''s warning elevation.',
    'If modern regulations effectively mitigate the risk, the ''extractiveness'' of building on hazardous land might be lower, as the ''cost'' of ignoring the warning is being addressed by other means. If not, the current high extractiveness is further validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_disaster_risk_acknowledgment, empirical, 'Whether the underlying disaster risk is addressed by other means, or if the decay of the stone''s rule leaves the community exposed.').

omega_variable(
    framing_of_stone_function,
    'Is the stone primarily a ''commemorative husk'' (this reading) or does it retain some ''behavioral competence'' (sibling reading) in shaping land-use decisions?',
    'Analysis of land-use permits issued for areas near the stone, comparing them to historical patterns and the stone''s implied prohibition. Direct observation of local behavior and development patterns.',
    'If the ''behavioral_competence'' reading were adopted, the constraint would reclassify as a Mountain or Rope, with significantly lower extractiveness and higher suppression, as its original function would be considered active. This reading''s classification as Piton would be foreclosed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_stone_function, conceptual, 'The core conceptual ambiguity between the two readings of the stone''s function.').


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
narrative_ontology:measurement(ston_tr_t40, stone_land_use_rule__commemorative_husk, theater_ratio, 40, 0.85).
narrative_ontology:measurement(ston_tr_t50, stone_land_use_rule__commemorative_husk, theater_ratio, 50, 0.9).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__commemorative_husk, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(ston_be_t10, stone_land_use_rule__commemorative_husk, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(ston_be_t20, stone_land_use_rule__commemorative_husk, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(ston_be_t30, stone_land_use_rule__commemorative_husk, base_extractiveness, 30, 0.75).
narrative_ontology:measurement(ston_be_t40, stone_land_use_rule__commemorative_husk, base_extractiveness, 40, 0.82).
narrative_ontology:measurement(ston_be_t50, stone_land_use_rule__commemorative_husk, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__commemorative_husk, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(ston_su_t10, stone_land_use_rule__commemorative_husk, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(ston_su_t20, stone_land_use_rule__commemorative_husk, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(ston_su_t30, stone_land_use_rule__commemorative_husk, suppression_requirement, 30, 0.2).
narrative_ontology:measurement(ston_su_t40, stone_land_use_rule__commemorative_husk, suppression_requirement, 40, 0.15).
narrative_ontology:measurement(ston_su_t50, stone_land_use_rule__commemorative_husk, suppression_requirement, 50, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__commemorative_husk, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'stone_land_use_rule' kernel. It describes the stone as a commemorative husk, where its original behavioral force has atrophied. The sibling reading, 'behavioral_competence', describes the stone as a live land-use prohibition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
