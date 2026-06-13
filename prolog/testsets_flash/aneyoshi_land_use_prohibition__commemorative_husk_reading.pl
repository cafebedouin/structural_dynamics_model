% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__commemorative_husk_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: aneyoshi_land_use_prohibition__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Stone Land Use Prohibition (Commemorative Husk Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes the Aneyoshi tsunami stone as a 'commemorative
 *   husk' – a historical memorial whose original behavioral force as a
 *   land-use prohibition has decayed, leaving it as a symbol without
 *   practical effect. This reading highlights the high extractiveness for
 *   development interests who benefit from ignoring the prohibition, and the
 *   high theater ratio as its maintenance is purely performative. Future
 *   coastal residents become victims when catastrophe returns.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.85).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.15).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.9).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0.9).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__commemorative_husk_reading, "Aneyoshi Tsunami Stone Land Use Prohibition (Commemorative Husk Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__commemorative_husk_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'a4253803-7c24-45a0-b930-4e8400b820b5').
narrative_ontology:cs_kernel_codification('a4253803-7c24-45a0-b930-4e8400b820b5', fixed_text).
narrative_ontology:cs_authority_grounding('a4253803-7c24-45a0-b930-4e8400b820b5', practice).
narrative_ontology:cs_interpretation_layer_present('a4253803-7c24-45a0-b930-4e8400b820b5').
narrative_ontology:cs_reading_relation('a4253803-7c24-45a0-b930-4e8400b820b5', aneyoshi_land_use_prohibition__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('a4253803-7c24-45a0-b930-4e8400b820b5', foundational, historical_markers_are_not_binding_rules).
narrative_ontology:cs_axiom_status(historical_markers_are_not_binding_rules, holdable).
narrative_ontology:cs_axiom_grounding('a4253803-7c24-45a0-b930-4e8400b820b5', historical_markers_are_not_binding_rules, conventional).
narrative_ontology:cs_axiom('a4253803-7c24-45a0-b930-4e8400b820b5', secondary, economic_development_trumps_historical_precedent).
narrative_ontology:cs_axiom_status(economic_development_trumps_historical_precedent, holdable).
narrative_ontology:cs_axiom_grounding('a4253803-7c24-45a0-b930-4e8400b820b5', economic_development_trumps_historical_precedent, instrumental).
narrative_ontology:cs_reference_frame('a4253803-7c24-45a0-b930-4e8400b820b5', stone_as_historical_memorial).
narrative_ontology:cs_drift_state('a4253803-7c24-45a0-b930-4e8400b820b5', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a4253803-7c24-45a0-b930-4e8400b820b5', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, local_development_interests).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, tourism_industry).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_coastal_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the effective non-enforcement of the prohibition, allowing construction and economic activity in areas historically designated as unsafe. They interpret the stone as a historical artifact, not a binding land-use regulation.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, local_development_interests, beneficiary,
    powerful, biographical, mobile, local).

% Profits from the development of coastal areas, including infrastructure that might be vulnerable to future tsunamis. They promote the stone as a historical attraction, reinforcing its symbolic rather than behavioral function.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, tourism_industry, beneficiary,
    moderate, biographical, mobile, local).

% Will bear the full cost of future tsunami events due to living in areas below the historical safe line, a consequence of the prohibition's decay. They are unaware of the original behavioral force of the stone.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_coastal_residents, payer,
    powerless, generational, trapped, local).

% Administers land-use policies that effectively ignore the stone's original prohibition, prioritizing economic development. While aware of the stone's history, they treat it as a memorial rather than a regulatory instrument, avoiding the political cost of enforcing a strict land-use rule.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, local_government, agenda_setter,
    institutional, generational, constrained, local).

% Observe the decay of the stone's behavioral force and predict the increased vulnerability of coastal communities to future disasters. They analyze the gap between historical wisdom and contemporary practice.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, disaster_risk_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone originally coordinated community settlement patterns to avoid tsunami risk by marking a safe elevation. In this reading, it coordinates nothing behaviorally, only serving as a historical marker.
% TRANSFER_FUNCTION: In its current state, the constraint transfers future disaster risk from present-day development interests to future coastal residents, by allowing unsafe settlement patterns.
% ABSENT_VOICES: The ancestors who erected the stone, and future generations who will suffer from its ignored warning, are absent. They would advocate for strict adherence to the land-use prohibition.
% DISAPPEARANCE_RATIONALE: If the stone vanished overnight, current land-use practices would remain unchanged, as its behavioral force has already atrophied. Its disappearance would remove a historical curiosity but not alter current risk exposure.
% FOUNDING_PROBLEM: The stone was erected to prevent future generations from settling below a safe elevation after devastating tsunamis, solving the problem of intergenerational memory loss regarding disaster risk.
% FOUNDING_PROBLEM_CORROBORATION: Disaster risk analysts and historical records corroborate the founding problem of tsunami risk and the stone's original purpose. Local development interests and the tourism industry, however, contest its current relevance as a binding rule, treating the problem as 'solved' by modern infrastructure, a claim not corroborated by independent experts.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__commemorative_husk_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because the effective non-enforcement of the prohibition allows development in unsafe areas, transferring future disaster risk to residents. Suppression is low (0.15) because there is no active enforcement to maintain the original land-use rule; rather, the constraint persists through inertia and the suppression of historical memory. The theater ratio is very high (0.9) as the stone is maintained as a tourist attraction and historical curiosity, while its primary function has atrophied. The claimed type is Piton because its original function has atrophied, no party benefits enough to maintain its original force, and no party is hurt enough in the present to fix it, though future generations will be.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of local development interests, the stone is a benign historical marker, and the current situation is 'mobile' with ample exit options for economic activity. From the perspective of future coastal residents, the situation is 'trapped' by historical amnesia and present-day development, with no exit from future risk. The local government's perspective is one of 'constrained' choice, balancing economic development against historical warnings.
 *
 * DIRECTIONALITY LOGIC:
 *   Local development interests and the tourism industry are beneficiaries (d near 0.0) as they profit from the effective non-enforcement of the prohibition. Future coastal residents are the primary victims (d near 1.0) as they will bear the costs of future tsunamis due to unsafe settlement. The local government, as the agenda-setter, benefits from avoiding the political cost of enforcement, contributing to the constraint's decay.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits clear mandatrophy: its original mandate (preventing settlement below the safe line) is dead, but the physical stone remains, maintained for symbolic and economic (tourism) reasons. The classification as a Piton correctly identifies this atrophy and the performative nature of its continued 'existence' as a constraint, preventing it from being mislabeled as a Rope (which would imply genuine coordination) or a Snare (which would imply active, concentrated extraction by a party maintaining its original force).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_symbolic_function,
    'Is the Aneyoshi stone primarily a behavioral land-use prohibition or a commemorative symbol?',
    'Analysis of current land-use regulations and building permits in relation to the stone''s marked elevation, and interviews with local residents regarding their understanding of its purpose.',
    'If primarily behavioral, the constraint would be a Rope or Mountain, with low extractiveness. If primarily symbolic, as this reading suggests, it is a Piton with high theater and extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_vs_symbolic_function, empirical, 'Ambiguity in the stone''s functional status.').

omega_variable(
    intergenerational_risk_transfer,
    'To what extent do current development decisions, enabled by the stone''s decay, transfer unacknowledged tsunami risk to future generations?',
    'Geospatial analysis of new construction in historically vulnerable areas, combined with probabilistic tsunami modeling and demographic projections.',
    'Quantifying the risk transfer would strengthen the victim declaration for future residents and further justify the high extractiveness score, potentially reclassifying to a Snare if the transfer is found to be actively engineered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_risk_transfer, empirical, 'Uncertainty regarding the magnitude of intergenerational risk transfer.').

omega_variable(
    kernel_framing_contest,
    'Is the Aneyoshi land-use prohibition a live, operationally enforced rule (behavioral competence reading) or a decayed symbol (commemorative husk reading)?',
    'This is a conceptual omega, resolved by adopting a specific interpretive framework. The ''commemorative husk'' reading is chosen here based on observed land-use patterns and the absence of active enforcement of the original prohibition.',
    'Adopting the ''behavioral competence'' reading would fundamentally alter the constraint''s classification, likely to a Mountain or Rope, with significantly lower extractiveness and theater, and different beneficiaries/victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_contest, conceptual, 'The core contest between the ''behavioral competence'' and ''commemorative husk'' readings of the Aneyoshi land-use prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__commemorative_husk_reading, 1960, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1960, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(aney_tr_t1970, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(aney_tr_t1980, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 1980, 0.5).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 1990, 0.7).
narrative_ontology:measurement(aney_tr_t2000, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 2000, 0.8).
narrative_ontology:measurement(aney_tr_t2010, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 2010, 0.85).
narrative_ontology:measurement(aney_tr_t2020, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 2020, 0.9).

% Extraction over time
narrative_ontology:measurement(aney_be_t1960, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 1960, 0.1).
narrative_ontology:measurement(aney_be_t1970, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(aney_be_t1980, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(aney_be_t2000, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(aney_be_t2010, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(aney_be_t2020, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1960, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement(aney_su_t1970, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(aney_su_t1980, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(aney_su_t1990, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(aney_su_t2000, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement(aney_su_t2010, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 2010, 0.16).
narrative_ontology:measurement(aney_su_t2020, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 2020, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__commemorative_husk_reading, identity_coordination).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Aneyoshi land-use prohibition kernel. The 'commemorative husk' reading describes the stone as a decayed symbol, contrasting with the 'behavioral competence' reading which views it as a live rule. Both are linked as part of the same constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
