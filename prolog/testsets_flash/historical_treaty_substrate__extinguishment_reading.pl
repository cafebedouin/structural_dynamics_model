% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__extinguishment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__extinguishment_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: historical_treaty_substrate__extinguishment_reading
 *   human_readable: Historical Treaty Substrate (Extinguishment Reading)
 *   domain: legal_anthropology/indigenous_law/constitutional_theory
 *
 * SUMMARY:
 *   This constraint represents the 'extinguishment reading' of historical
 *   treaties between Indigenous nations and settler states. Under this
 *   reading, treaties are interpreted as completed property transactions
 *   where Indigenous parties irrevocably ceded territorial sovereignty in
 *   exchange for defined reserves and payments. This interpretation
 *   establishes the settler state as the sole legitimate authority over vast
 *   territories, enabling resource extraction and settlement, while
 *   Indigenous nations are relegated to a beneficiary role for limited,
 *   defined rights. The high extractiveness and suppression reflect the
 *   ongoing dispossession and denial of Indigenous self-determination
 *   inherent in this legal framework.
 *
 * KEY AGENTS:
 *   - settler_state_governments: Agenda-setter (institutional/generational) — interprets and enforces treaties as extinguishment, benefits from territorial control.
 *   - settler_landowners: Beneficiary (powerful/biographical) — holds title to lands based on extinguished Indigenous claims.
 *   - resource_extraction_industries: Beneficiary (institutional/generational) — operates on lands deemed 'ceded' by treaties, benefiting from clear title.
 *   - indigenous_nations: Payer (organized/generational) — bears the cost of lost sovereignty and jurisdiction, constrained exit.
 *   - indigenous_citizens: Payer (powerless/biographical) — experiences the direct impact of dispossession and limited rights, identity_locked exit.
 *   - legal_scholars_indigenous_law: Observer (analytical/generational) — critically analyzes the historical and ongoing impacts of this reading, proposes alternative interpretations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, 0.85).
domain_priors:suppression_score(historical_treaty_substrate__extinguishment_reading, 0.9).
domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, snare).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Historical Treaty Substrate (Extinguishment Reading)").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal_anthropology/indigenous_law/constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, 'e2541b6d-5a16-4062-9819-3da6cdb9c667').
narrative_ontology:cs_kernel_codification('e2541b6d-5a16-4062-9819-3da6cdb9c667', fixed_text).
narrative_ontology:cs_authority_grounding('e2541b6d-5a16-4062-9819-3da6cdb9c667', extraction).
narrative_ontology:cs_interpretation_layer_present('e2541b6d-5a16-4062-9819-3da6cdb9c667').
narrative_ontology:cs_reading_relation('e2541b6d-5a16-4062-9819-3da6cdb9c667', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2541b6d-5a16-4062-9819-3da6cdb9c667', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('e2541b6d-5a16-4062-9819-3da6cdb9c667', foundational, indigenous_sovereignty_extinguished_by_treaty).
narrative_ontology:cs_axiom_status(indigenous_sovereignty_extinguished_by_treaty, holdable).
narrative_ontology:cs_axiom_grounding('e2541b6d-5a16-4062-9819-3da6cdb9c667', indigenous_sovereignty_extinguished_by_treaty, conventional).
narrative_ontology:cs_axiom('e2541b6d-5a16-4062-9819-3da6cdb9c667', foundational, treaties_as_real_property_transactions).
narrative_ontology:cs_axiom_status(treaties_as_real_property_transactions, holdable).
narrative_ontology:cs_axiom_grounding('e2541b6d-5a16-4062-9819-3da6cdb9c667', treaties_as_real_property_transactions, conventional).
narrative_ontology:cs_reference_frame('e2541b6d-5a16-4062-9819-3da6cdb9c667', terra_nullius_derived_sovereignty).
narrative_ontology:cs_drift_state('e2541b6d-5a16-4062-9819-3da6cdb9c667', contemporary_indigenous_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e2541b6d-5a16-4062-9819-3da6cdb9c667', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_state_governments).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_landowners).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, resource_extraction_industries).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces historical treaties as instruments of land cession and extinguishment of Indigenous title, thereby asserting and maintaining jurisdiction over vast territories. Benefits from the legal certainty this interpretation provides for governance and resource development.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Holds private property titles to lands that were historically Indigenous territories, with their ownership legally secured by the extinguishment reading of treaties. Benefits from the stability and marketability of these titles.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_landowners, beneficiary,
    powerful, biographical, mobile, local).

% Operates on lands deemed 'ceded' by treaties, benefiting from the legal clarity and perceived lack of Indigenous proprietary claims under the extinguishment reading. This enables large-scale mining, forestry, and energy projects.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, resource_extraction_industries, beneficiary,
    institutional, generational, arbitrage, global).

% Bears the cost of lost territorial sovereignty, jurisdiction, and resource control. Their self-governance is limited to reserves, and their traditional laws are often unrecognized. They are forced to operate within the settler legal framework to assert their rights.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_nations, payer,
    organized, generational, constrained, regional).

% Experiences the direct impacts of dispossession, including limited access to traditional lands and resources, cultural disruption, and socio-economic marginalization. Their identity is deeply tied to their ancestral territories, making 'exit' from the relationship with the land unthinkable.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_citizens, payer,
    powerless, biographical, identity_locked, local).

% Critically analyzes the historical and ongoing impacts of the extinguishment reading, documenting its legal and social consequences. They propose alternative interpretations and legal frameworks that recognize Indigenous sovereignty and treaty rights.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, legal_scholars_indigenous_law, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__extinguishment_reading, settler_state_governments).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__extinguishment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides legal certainty for settler governance and economic development by defining Indigenous rights as limited and territorial claims as extinguished, thereby 'coordinating' the allocation of land and resources to the settler society.
% TRANSFER_FUNCTION: Transfers vast territorial sovereignty and resource control from Indigenous nations to settler state governments and their beneficiaries, in exchange for limited reserve lands and financial payments to Indigenous parties.
% ABSENT_VOICES: Indigenous legal traditions and governance systems, which would assert inherent and unceded sovereignty, are systematically excluded from the dominant legal discourse that upholds the extinguishment reading. Their voices are present in Indigenous communities but largely absent from settler state legislative and judicial processes.
% DISAPPEARANCE_RATIONALE: If the extinguishment reading vanished overnight, the legal basis for settler land titles and resource development would collapse, leading to widespread challenges to property ownership and a fundamental re-evaluation of state sovereignty. The entire legal and economic structure of settler states would need to be renegotiated with Indigenous nations.
% FOUNDING_PROBLEM: The founding problem, from the settler perspective, was to acquire land for settlement and resource exploitation while managing relations with Indigenous peoples, often under the guise of 'civilizing' them. From the Indigenous perspective, it was to establish peaceful coexistence and shared stewardship.
% FOUNDING_PROBLEM_CORROBORATION: Settler state governments maintain that the problem of clear land title and national development is 'live' and that the extinguishment reading provides the necessary framework. Indigenous nations, supported by international legal bodies and critical legal scholars, contend that the original problem of peaceful coexistence and mutual respect was never genuinely solved by the extinguishment reading, and that the current arrangement perpetuates colonial injustice. Historical records, Indigenous oral histories, and international human rights reports corroborate the Indigenous perspective.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__extinguishment_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__extinguishment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__extinguishment_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(historical_treaty_substrate__extinguishment_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__extinguishment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__extinguishment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because this reading enables the settler state and its beneficiaries to control and profit from vast territories and resources without ongoing Indigenous consent. Suppression is also very high (0.90) as the legal and political systems actively enforce this interpretation, suppressing Indigenous claims to sovereignty and self-determination. The theater ratio is low (0.20) because while there is performative adherence to 'treaty obligations,' the core function of the constraint is active extraction and control, not mere inertial maintenance. The historical measurements show a trend of increasing extractiveness and suppression as settler states consolidated power and Indigenous resistance was met with legal and physical force.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of settler state governments and beneficiaries, this reading provides legal certainty and a clear basis for governance and economic activity. From the perspective of Indigenous nations and citizens, it is a mechanism of ongoing dispossession and cultural erosion. The engine's per-seat classification will reflect this divergence, with settler actors computing as beneficiaries of a 'rope' (coordination for land title) and Indigenous actors computing as victims of a 'snare' (coercive extraction of sovereignty).
 *
 * DIRECTIONALITY LOGIC:
 *   Settler state governments, landowners, and resource industries are clear beneficiaries (d near 0.0) as they gain territorial control and economic opportunity. Indigenous nations and citizens are clear targets (d near 1.0) as they lose sovereignty, land, and self-determination. Their exit options are severely constrained, often identity_locked, due to the profound cultural and spiritual connection to land and the lack of viable alternatives for self-governance outside the settler legal framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a snare, not a rope or mountain. Its persistence is not due to natural law or mutual benefit, but to active enforcement and the suppression of alternative interpretations. The 'mandate' of establishing clear title for settlement and resource extraction remains 'live' for the settler state, but the original 'problem' of peaceful coexistence and mutual respect, as understood by Indigenous parties, has been subverted. The classification prevents mislabeling this as a legitimate coordination mechanism by highlighting the coercive and extractive nature of the extinguishment doctrine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extinguishment_vs_stewardship_reading,
    'Is this constraint a completed property transaction (extinguishment reading) or a relational pact for shared stewardship (stewardship reading)?',
    'Judicial re-interpretation of historical intent, or legislative action recognizing ongoing Indigenous sovereignty and co-management responsibilities.',
    'If reclassified as a stewardship reading, Indigenous nations would exit the victim set for territorial jurisdiction and enter the beneficiary set for co-management, significantly reducing the constraint''s extractiveness and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extinguishment_vs_stewardship_reading, conceptual, 'Contested interpretation of treaty intent regarding land and sovereignty.').

omega_variable(
    extinguishment_vs_nation_to_nation_reading,
    'Is this constraint a completed property transaction (extinguishment reading) or an international agreement between sovereign equals (nation-to-nation reading)?',
    'International legal arbitration or domestic constitutional amendment affirming Indigenous nations'' inherent sovereignty and treaty-making capacity.',
    'If reclassified as a nation-to-nation reading, the settler state would lose its claim to sole legitimate authority over ceded territory, and Indigenous nations would gain full recognition of their self-determination, fundamentally altering the power dynamics and reducing extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extinguishment_vs_nation_to_nation_reading, conceptual, 'Contested interpretation of Indigenous sovereignty and treaty status.').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the ''extinguishment'' of Indigenous title a natural consequence of historical events, or a legal construct benefiting identifiable agents?',
    'Critical legal history and decolonial analysis demonstrating the active construction and enforcement of extinguishment doctrines by settler legal systems.',
    'If shown to be a legal construct, the constraint''s claim to naturalness would collapse, exposing its extractive function and strengthening arguments for reparations and restitution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, empirical, 'Ambiguity between natural historical outcome and legal construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__extinguishment_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__extinguishment_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(hist_tr_t100, historical_treaty_substrate__extinguishment_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 50, 0.78).
narrative_ontology:measurement(hist_be_t100, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(hist_su_t50, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 50, 0.82).
narrative_ontology:measurement(hist_su_t100, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__extinguishment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, settler_property_law).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, resource_licensing_regimes).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, indigenous_self_governance_limitations).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'historical_treaty_substrate' kernel. Its ε value is high, reflecting its extractive nature, in contrast to the lower ε values of the 'stewardship_reading' and 'nation_to_nation_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
