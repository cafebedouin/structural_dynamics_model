% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__conceptualization_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_money_emergence_boundary__conceptualization_reading
 *   human_readable: Digital Money Emergence Boundary â Conceptualization Reading
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the conceptualization reading of the
 *   digital_money_emergence_boundary kernel. It treats digital money as
 *   having emerged when it became theoretically thinkableârooted in 1960s
 *   telecommunications advances and formalized in David Chaum's 1985
 *   cryptographic work. This reading is contested by infrastructure readings
 *   (ATMs, ACH, SWIFT) and consumer-holdings readings (e-purses, EMD). The
 *   constraint is a disciplinary boundary that coordinates scholarly
 *   periodization while asymmetrically extracting historical credit for the
 *   academic research community.
 *
 * KEY AGENTS:
 *   - academic_research_community: Primary beneficiary (organized/constrained) â collects priority claims and citation credit
 *   - infrastructure_practitioners: Primary target (moderate/constrained) â bears epistemic exclusion from the origin narrative
 *   - consumer_finance_pioneers: Secondary target (moderate/constrained) â denied foundational status
 *   - monetary_history_gatekeepers: Agenda setter (institutional/mobile) â administers the boundary
 *   - critical_technology_historians: Analytical observer (analytical/analytical) â sees the asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.6).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.65).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Digital Money Emergence Boundary â Conceptualization Reading").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__conceptualization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, 'e68c09f7-1fb9-4396-bf72-6c9eacc0d463').
narrative_ontology:cs_kernel_codification('e68c09f7-1fb9-4396-bf72-6c9eacc0d463', distributed).
narrative_ontology:cs_authority_grounding('e68c09f7-1fb9-4396-bf72-6c9eacc0d463', expertise).
narrative_ontology:cs_interpretation_layer_present('e68c09f7-1fb9-4396-bf72-6c9eacc0d463').
narrative_ontology:cs_reading_relation('e68c09f7-1fb9-4396-bf72-6c9eacc0d463', digital_money_emergence_boundary__infrastructure_reading, coexists_with).
narrative_ontology:cs_reading_relation('e68c09f7-1fb9-4396-bf72-6c9eacc0d463', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('e68c09f7-1fb9-4396-bf72-6c9eacc0d463', foundational, conceptual_precedence_defines_monetary_origin).
narrative_ontology:cs_axiom_status(conceptual_precedence_defines_monetary_origin, holdable).
narrative_ontology:cs_axiom_grounding('e68c09f7-1fb9-4396-bf72-6c9eacc0d463', conceptual_precedence_defines_monetary_origin, conventional).
narrative_ontology:cs_reference_frame('e68c09f7-1fb9-4396-bf72-6c9eacc0d463', conceptual_origins_paradigm).
narrative_ontology:cs_drift_state('e68c09f7-1fb9-4396-bf72-6c9eacc0d463', post_infrastructure_historiography, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e68c09f7-1fb9-4396-bf72-6c9eacc0d463', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, academic_research_community).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, infrastructure_practitioners).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, consumer_finance_pioneers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the disciplinary boundary between digital money and predecessor concepts through journal editorial standards, tenure criteria, and curriculum design. They determine which origin narratives receive scholarly legitimacy and enforce the conceptualization frame against infrastructure or consumer-holdings alternatives.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, monetary_history_gatekeepers, agenda_setter,
    institutional, generational, mobile, global).

% Receives citation priority, research funding, and disciplinary status from the conceptualization-origin narrative. Career advancement and peer recognition depend on operating within the theoretical-precedence framework and citing the 1960sâ1985 origin boundary.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, academic_research_community, beneficiary,
    organized, biographical, constrained, global).

% Built and operated ACH, SWIFT, and ATM networks that moved value electronically before and during the 1980s. Under the conceptualization reading, their work is classified as payment mechanics or pre-digital infrastructure rather than as the emergence of digital money itself, excluding them from foundational historiography.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, infrastructure_practitioners, payer,
    moderate, biographical, constrained, global).

% Developed retail e-purses and electronic money instruments for consumer use from the 1990s onward. The conceptualization reading positions their work as a later diffusion phase rather than the origin point, denying them foundational status in the standard monetary history.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, consumer_finance_pioneers, payer,
    moderate, biographical, constrained, global).

% Study the material and social construction of payment systems. They document the asymmetry between theoretical conceptualization and working infrastructure but are often excluded from core monetary economics curricula and citation networks.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, critical_technology_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__conceptualization_reading, academic_research_community).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__conceptualization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared chronological origin point and periodization framework for monetary history, enabling cumulative scholarship and clear disciplinary boundaries around digital versus pre-digital eras.
% TRANSFER_FUNCTION: Moves historical priority, citation credit, and disciplinary authority from infrastructure implementers and consumer-finance innovators to theoretical researchers and cryptographers.
% ABSENT_VOICES: Infrastructure engineers and retail payment operators who built working systems but do not author disciplinary histories; social historians of technology who emphasize material practice over theoretical conception.
% DISAPPEARANCE_RATIONALE: If the conceptualization boundary vanished, monetary historiography would reorganize around infrastructure or consumer-holdings readings; curricula, citation networks, and research funding priorities would shift toward different origin points and beneficiary communities.
% FOUNDING_PROBLEM: The absence of a coherent origin story and chronological framework for digital money in monetary economics and technology history.
% FOUNDING_PROBLEM_CORROBORATION: Competing scholarly communitiesâinfrastructure historians and consumer-finance researchersâattest to different founding problems; no corroborating source outside the benefiting theoretical parties fully validates the conceptualization framing as the uniquely correct boundary.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__conceptualization_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__conceptualization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__conceptualization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__conceptualization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__conceptualization_reading, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.60) is moderate-to-high because the constraint redistributes epistemic authority and historical credit from builders to theorists. Suppression (0.65) reflects the active disciplinary enforcement required to maintain the conceptualization boundary against well-documented infrastructure alternatives. Theater ratio (0.50) captures the ritualistic citation of Chaum and the 1960s telecom framing that has partially decoupled from actual historical complexity. Accessibility collapse (0.60) indicates that once the conceptualization frame is adopted, infrastructure alternatives become nearly invisible as origin candidates. Resistance (0.55) reflects growing challenge from technology historians and infrastructure scholars. The measurement series share a single time grid (0â40) to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   From the conceptualization seat, the constraint is valid historical method that correctly identifies theoretical breakthrough as the relevant origin. From the infrastructure and consumer-finance seats, the same structure operates as a disciplinary mechanism that extracts credit from practitioners and awards it to theorists. The engine computes this divergence from the structural data: identical historical events produce opposite directionalities depending on whether the agent's contribution is classified as theory or implementation.
 *
 * DIRECTIONALITY LOGIC:
 *   The academic_research_community sits near the beneficiary end: the constraint subsidizes their priority claims and citation networks. Infrastructure_practitioners and consumer_finance_pioneers sit near the target end: the constraint extracts historical credit from their material innovations and transfers it to conceptual work. Monetary_history_gatekeepers sit near the beneficiary end but with mobile exit, giving them low effective extraction despite high power. Critical_technology_historians are analytical with analytical exit, producing neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents mislabeling this constraint as either pure coordination (Rope) or pure extraction (Snare). There is a genuine coordination function: scholarly fields need shared origin stories to accumulate knowledge. But the beneficiary-victim asymmetry and active enforcement requirement show that the same structure also extracts epistemic rents. If the coordination function were absent, the constraint would be a Snare; if there were no beneficiaries, it would be a Piton; the presence of both coordination and asymmetric extraction mandates Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_reading_location,
    'This constraint instantiates the conceptualization reading of the digital_money_emergence_boundary kernel; how would classification change if the infrastructure or consumer-holdings reading were adopted instead?',
    'Compare the three sibling constraints'' epsilon values and beneficiary-victim structures to map the kernel''s reading-dependent variation.',
    'Shifts the beneficiary set from theorists to practitioners or consumers, and moves the extraction target accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_location, conceptual, 'Kernel reading location and sibling displacement').

omega_variable(
    material_vs_conceptual_money_boundary,
    'Does potential money that has not yet circulated qualify as money, or does monetary status require actual transactional use?',
    'Cross-disciplinary consensus or definitional convention in monetary economics and economic sociology.',
    'Resolving toward potential-money expands the conceptualization reading''s validity; resolving toward transactional-use collapses it in favor of infrastructure or consumer readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_vs_conceptual_money_boundary, conceptual, 'Conceptual versus transactional definition of money').

omega_variable(
    epistemic_extraction_materiality,
    'Does the attribution of historical priority to conceptual researchers extract tangible resources from infrastructure practitioners, or is it purely symbolic?',
    'Trace citation and curriculum effects on research funding, career advancement, and institutional prestige across the two communities.',
    'If purely symbolic, extractiveness is lower than measured; if it redirects funding and positions, extractiveness is substantiated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_extraction_materiality, empirical, 'Symbolic versus material epistemic extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__conceptualization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(digi_tr_t8, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(digi_tr_t16, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(digi_tr_t24, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(digi_tr_t32, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 32, 0.46).
narrative_ontology:measurement(digi_tr_t40, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 40, 0.5).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(digi_be_t8, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(digi_be_t16, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(digi_be_t24, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(digi_be_t32, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(digi_be_t40, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 40, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(digi_su_t8, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(digi_su_t16, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(digi_su_t24, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(digi_su_t32, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(digi_su_t40, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the digital_money_emergence_boundary kernel. The conceptualization reading (this file) privileges theoretical work; the infrastructure reading privileges electronic transfer systems; the consumer-holdings reading privileges retail instruments. They form a constraint family decomposed per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
