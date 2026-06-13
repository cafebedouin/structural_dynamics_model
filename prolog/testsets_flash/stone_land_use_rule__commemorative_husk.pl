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
 *   constraint_id: stone_land_use_rule__commemorative_husk
 *   human_readable: Stone Land Use Rule (Commemorative Husk Reading)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   This constraint describes the 'commemorative husk' reading of the 'stone
 *   land use rule' kernel. The stone markers, originally placed to prohibit
 *   building below a historical inundation line, have lost their behavioral
 *   force. They are now treated as mere memorial artifacts, while land-use
 *   decisions proceed independently, often leading to development in
 *   high-risk zones. The constraint is claimed as a Piton because its
 *   original function has atrophied, and its persistence is largely
 *   theatrical, serving as a historical curiosity rather than an active
 *   warning.
 *
 * KEY AGENTS:
 *   - coastal_developers: Agenda setter (powerful/mobile) — benefits from non-enforcement
 *   - local_residents: Payer (moderate/constrained) — bears increased risk
 *   - disaster_risk_management_agencies: Observer (institutional/analytical) — aware of risk, limited enforcement power
 *   - descendants_of_survivors: Excluded (powerless/identity_locked) — holds memory, excluded from decisions
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
narrative_ontology:human_readable(stone_land_use_rule__commemorative_husk, "Stone Land Use Rule (Commemorative Husk Reading)").
narrative_ontology:topic_domain(stone_land_use_rule__commemorative_husk, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__commemorative_husk, 'b80bcf7f-1c3a-48fe-bfbc-b8942e305bd5').
narrative_ontology:cs_kernel_codification('b80bcf7f-1c3a-48fe-bfbc-b8942e305bd5', fixed_text).
narrative_ontology:cs_authority_grounding('b80bcf7f-1c3a-48fe-bfbc-b8942e305bd5', distributed).
narrative_ontology:cs_reading_relation('b80bcf7f-1c3a-48fe-bfbc-b8942e305bd5', stone_land_use_rule__behavioral_competence, coexists_with).
narrative_ontology:cs_axiom('b80bcf7f-1c3a-48fe-bfbc-b8942e305bd5', foundational, historical_warnings_are_advisory).
narrative_ontology:cs_axiom_status(historical_warnings_are_advisory, holdable).
narrative_ontology:cs_axiom_grounding('b80bcf7f-1c3a-48fe-bfbc-b8942e305bd5', historical_warnings_are_advisory, conventional).
narrative_ontology:cs_axiom('b80bcf7f-1c3a-48fe-bfbc-b8942e305bd5', secondary, economic_development_priority).
narrative_ontology:cs_axiom_status(economic_development_priority, holdable).
narrative_ontology:cs_axiom_grounding('b80bcf7f-1c3a-48fe-bfbc-b8942e305bd5', economic_development_priority, instrumental).
narrative_ontology:cs_reference_frame('b80bcf7f-1c3a-48fe-bfbc-b8942e305bd5', historical_warning_obsolescence).
narrative_ontology:cs_drift_state('b80bcf7f-1c3a-48fe-bfbc-b8942e305bd5', contemporary_development_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('b80bcf7f-1c3a-48fe-bfbc-b8942e305bd5', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__commemorative_husk, stone_land_use_rule).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, local_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops prime waterfront properties, treating the stone markers as historical curiosities rather than active land-use restrictions. Benefits from the lack of enforcement, allowing construction in previously restricted zones.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, coastal_developers, agenda_setter,
    powerful, biographical, mobile, local).

% Live in areas increasingly vulnerable to coastal hazards due to development. They bear the risk of future disasters, but their concerns are often overridden by economic interests and the perceived obsolescence of the stone's warning.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, local_residents, payer,
    moderate, generational, constrained, local).

% Are aware of the historical warning but lack the political or legal mandate to enforce the original land-use restrictions. They issue warnings and prepare for disasters, but cannot prevent development based on the stone's original intent.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, disaster_risk_management_agencies, observer,
    institutional, generational, analytical, regional).

% Hold the memory of the original disaster and the stone's purpose as a sacred trust. They are excluded from land-use decisions and their warnings are dismissed as sentimental, but their identity is tied to preserving the memory.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, descendants_of_survivors, excluded,
    powerless, generational, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone originally coordinated safe land-use practices by marking the historical inundation line, preventing development in high-risk zones. In this reading, it coordinates nothing; it is a historical marker.
% TRANSFER_FUNCTION: In its original function, it transferred safety from the past to the future. In its current state, it transfers prime coastal land from a 'restricted' category to a 'developable' category, benefiting developers at the risk of residents.
% ABSENT_VOICES: The original disaster survivors and their direct descendants, whose lived experience and historical memory would vociferously object to the disregard of the stone's warning. They are excluded from current land-use planning processes.
% DISAPPEARANCE_RATIONALE: If the stone markers vanished overnight, current land-use practices would remain unchanged, as they already disregard the stone's original warning. Development would continue as before, perhaps with slightly less historical context.
% FOUNDING_PROBLEM: To prevent future generations from building in areas vulnerable to catastrophic coastal inundation, based on the memory of a devastating historical tsunami.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preventing building below the inundation line) is widely considered 'dead' by coastal developers and local planning authorities, who prioritize economic growth and modern engineering solutions. Descendants of survivors and disaster anthropologists attest it is still live, but their voices are marginalized.
narrative_ontology:disappearance_verdict(stone_land_use_rule__commemorative_husk, world_unchanged).
narrative_ontology:founding_problem_status(stone_land_use_rule__commemorative_husk, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__commemorative_husk, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(stone_land_use_rule__commemorative_husk, 'none', 1).

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
 *   Extractiveness is high (0.85) because valuable coastal land is 'freed' for development, generating significant profits for developers, while the original safety function is ignored. Suppression is low (0.1) because there's little active enforcement to maintain the original rule; instead, the rule itself is suppressed by neglect. Theater ratio is very high (0.9) as the stones are maintained as historical markers, but their primary warning function is performative, lacking real-world impact on behavior. The temporal measurements show a clear drift from a low-extraction, low-theater state to a high-extraction, high-theater state, reflecting the decay of the warning into a mere symbol.
 *
 * PERSPECTIVAL GAP:
 *   Coastal developers perceive the stones as benign historical markers, allowing them to pursue profitable ventures. Local residents and descendants of survivors, however, experience the decay of the rule as a loss of safety and a betrayal of historical memory, leading to increased vulnerability. Disaster agencies observe the growing risk but are constrained by the lack of political will to re-activate the stone's original function.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal developers are the primary beneficiaries (d=0.0) as they gain access to valuable land. Local residents are the payers (d=1.0) as they bear the increased risk. Descendants of survivors are identity-locked targets (d=1.0) as their identity is tied to the stone's original meaning, which is being eroded. Disaster agencies are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is a clear case of mandatrophy: the original mandate (preventing disaster) has atrophied, but the physical artifacts (the stones) remain, repurposed as commemorative objects. The classification as Piton correctly identifies this as a degraded constraint where the original function is lost, and what remains is mostly theatrical maintenance. It prevents mislabeling it as a Rope (which would imply active coordination) or a Snare (which would imply active, concentrated extraction from its operation, rather than diffuse benefit from its decay).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_commemorative_function,
    'Is the stone primarily a behavioral land-use rule or a commemorative artifact?',
    'Analysis of land-use permits issued for areas below the stone line, and interviews with local planning officials regarding the stone''s legal status.',
    'If found to still exert behavioral force, the constraint would shift towards a Rope or Tangled Rope, with lower extractiveness and theater. If confirmed as purely commemorative, the Piton classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_vs_commemorative_function, empirical, 'Distinguishes between active behavioral constraint and symbolic memorial.').

omega_variable(
    natural_hazard_risk_perception,
    'To what extent do current residents and developers accurately perceive the natural hazard risk that the stone originally warned against?',
    'Surveys of risk perception among residents and developers, compared against scientific assessments of coastal inundation risk.',
    'If risk perception is low despite high objective risk, it reinforces the ''commemorative husk'' reading and the Piton classification, highlighting a failure of institutional memory. If risk perception is high but ignored, it suggests a more active form of extraction or suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_hazard_risk_perception, empirical, 'Assesses the gap between perceived and actual disaster risk.').

omega_variable(
    kernel_reading_divergence,
    'Is this constraint a genuine ''commemorative husk'' or does it retain some ''behavioral competence''?',
    'Cross-referencing land-use decisions with the stone''s location and any formal or informal enforcement actions taken by local authorities or community groups.',
    'If ''behavioral competence'' is found, the extractiveness would be lower, and the constraint might be reclassified as a degraded Rope or Tangled Rope, indicating some residual coordination function. If ''commemorative husk'' is confirmed, the Piton classification is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Ambiguity between the two primary readings of the stone land-use rule kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__commemorative_husk, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t1950, stone_land_use_rule__commemorative_husk, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(ston_tr_t1960, stone_land_use_rule__commemorative_husk, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(ston_tr_t1970, stone_land_use_rule__commemorative_husk, theater_ratio, 1970, 0.45).
narrative_ontology:measurement(ston_tr_t1980, stone_land_use_rule__commemorative_husk, theater_ratio, 1980, 0.6).
narrative_ontology:measurement(ston_tr_t1990, stone_land_use_rule__commemorative_husk, theater_ratio, 1990, 0.75).
narrative_ontology:measurement(ston_tr_t2000, stone_land_use_rule__commemorative_husk, theater_ratio, 2000, 0.83).
narrative_ontology:measurement(ston_tr_t2010, stone_land_use_rule__commemorative_husk, theater_ratio, 2010, 0.87).
narrative_ontology:measurement(ston_tr_t2020, stone_land_use_rule__commemorative_husk, theater_ratio, 2020, 0.9).

% Extraction over time
narrative_ontology:measurement(ston_be_t1950, stone_land_use_rule__commemorative_husk, base_extractiveness, 1950, 0.1).
narrative_ontology:measurement(ston_be_t1960, stone_land_use_rule__commemorative_husk, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(ston_be_t1970, stone_land_use_rule__commemorative_husk, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(ston_be_t1980, stone_land_use_rule__commemorative_husk, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(ston_be_t1990, stone_land_use_rule__commemorative_husk, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(ston_be_t2000, stone_land_use_rule__commemorative_husk, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(ston_be_t2010, stone_land_use_rule__commemorative_husk, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(ston_be_t2020, stone_land_use_rule__commemorative_husk, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t1950, stone_land_use_rule__commemorative_husk, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(ston_su_t1960, stone_land_use_rule__commemorative_husk, suppression_requirement, 1960, 0.25).
narrative_ontology:measurement(ston_su_t1970, stone_land_use_rule__commemorative_husk, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(ston_su_t1980, stone_land_use_rule__commemorative_husk, suppression_requirement, 1980, 0.15).
narrative_ontology:measurement(ston_su_t1990, stone_land_use_rule__commemorative_husk, suppression_requirement, 1990, 0.12).
narrative_ontology:measurement(ston_su_t2000, stone_land_use_rule__commemorative_husk, suppression_requirement, 2000, 0.11).
narrative_ontology:measurement(ston_su_t2010, stone_land_use_rule__commemorative_husk, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(ston_su_t2020, stone_land_use_rule__commemorative_husk, suppression_requirement, 2020, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__commemorative_husk, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('commemorative_husk') of the 'stone_land_use_rule' kernel. The 'behavioral_competence' reading would describe the stone as an active land-use prohibition with lower extractiveness and theater.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
