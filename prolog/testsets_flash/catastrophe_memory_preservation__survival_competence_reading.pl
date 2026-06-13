% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__survival_competence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_memory_preservation__survival_competence_reading
 *   human_readable: Catastrophe Memory Preservation (Survival Competence Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes a ritual practice that, from the 'survival
 *   competence' reading, actively preserves and transmits operational
 *   knowledge for recognizing and responding to existential threats across
 *   generations. It is a demanding, costly practice for present participants,
 *   but its justification is the survival of future generations. The
 *   constraint is claimed as a Tangled Rope because it genuinely coordinates
 *   intergenerational survival, but does so through asymmetric extraction
 *   from the present generation, requiring active enforcement by ritual
 *   elders.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.7).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.6).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Catastrophe Memory Preservation (Survival Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, '8fe49e23-0a7e-4849-aee0-c83916555d2e').
narrative_ontology:cs_kernel_codification('8fe49e23-0a7e-4849-aee0-c83916555d2e', implicit).
narrative_ontology:cs_authority_grounding('8fe49e23-0a7e-4849-aee0-c83916555d2e', lineage).
narrative_ontology:cs_interpretation_layer_present('8fe49e23-0a7e-4849-aee0-c83916555d2e').
narrative_ontology:cs_reading_relation('8fe49e23-0a7e-4849-aee0-c83916555d2e', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('8fe49e23-0a7e-4849-aee0-c83916555d2e', catastrophe_memory_preservation__hybrid_atrophy_reading, influences).
narrative_ontology:cs_axiom('8fe49e23-0a7e-4849-aee0-c83916555d2e', foundational, ritual_transmits_actionable_knowledge).
narrative_ontology:cs_axiom_status(ritual_transmits_actionable_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('8fe49e23-0a7e-4849-aee0-c83916555d2e', ritual_transmits_actionable_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('8fe49e23-0a7e-4849-aee0-c83916555d2e', foundational, collective_survival_requires_embodied_memory).
narrative_ontology:cs_axiom_status(collective_survival_requires_embodied_memory, holdable).
narrative_ontology:cs_axiom_grounding('8fe49e23-0a7e-4849-aee0-c83916555d2e', collective_survival_requires_embodied_memory, deontological).
narrative_ontology:cs_reference_frame('8fe49e23-0a7e-4849-aee0-c83916555d2e', unbroken_survival_chain).
narrative_ontology:cs_drift_state('8fe49e23-0a7e-4849-aee0-c83916555d2e', contemporary_globalized_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('8fe49e23-0a7e-4849-aee0-c83916555d2e', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, future_generations).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in demanding, often painful, ritual practices that re-enact past catastrophes. They bear the direct costs in time, emotional labor, and suppressed individual autonomy, but are identity-locked by communal belonging and the perceived necessity of the ritual for group survival.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants, payer,
    moderate, biographical, identity_locked, local).

% Are the intended recipients of the operational threat-recognition capacity. They benefit from the accumulated knowledge and preparedness, often without direct participation in the most demanding rituals, and are not yet born to consent to the costs.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, future_generations, beneficiary,
    powerless, generational, analytical, local).

% Administer and enforce the ritual practices, ensuring their fidelity and transmission. They believe in the operational efficacy of the rituals and bear the burden of maintaining their integrity against modernizing pressures. Their authority is grounded in their role as custodians of survival knowledge.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_elders_or_keepers, agenda_setter,
    organized, generational, constrained, local).

% Represents the broader cultural forces that challenge the perceived necessity and efficacy of such rituals, offering alternative modes of knowledge transfer and collective memory. Its 'voice' would question the costs and demand empirical validation, but it is structurally excluded from the ritual's internal logic.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, modernity_and_secularism, excluded,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__survival_competence_reading, modernity_and_secularism).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the intergenerational transfer of specific, actionable knowledge and behavioral patterns for recognizing and responding to existential threats, ensuring collective survival by embedding these competencies in ritual form.
% TRANSFER_FUNCTION: Transfers operational threat-recognition capacity and survival competencies from past generations, through present participants, to future generations, at the cost of present-generation autonomy and emotional labor.
% ABSENT_VOICES: The 'voice' of individual autonomy and empirical rationality (often associated with modernity and secularism) is absent; it would question the necessity of the costly ritual demands and seek more efficient, less emotionally taxing methods of knowledge transfer.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the specific, embodied knowledge for threat-recognition would likely be lost or severely degraded, leaving future generations unprepared for recurring catastrophes. The community's long-term survival competence would be fundamentally altered.
% FOUNDING_PROBLEM: The problem of ensuring collective survival in the face of recurring, existential catastrophes by embedding critical threat-recognition and response knowledge in a durable, transmissible form that bypasses purely cognitive or textual transmission.
% FOUNDING_PROBLEM_CORROBORATION: Ritual elders and community historians attest the problem is live, citing historical recurrences of the catastrophe and the demonstrated efficacy of ritual-derived responses. Anthropological studies and oral histories from outside the immediate community corroborate the historical context of the catastrophe and the community's unique survival strategies, even if they dispute the 'operational' claim of the ritual itself.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__survival_competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_preservation__survival_competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) due to the significant demands on present-generation participants (time, emotional labor, autonomy). Suppression is moderate (0.6) as the ritual's persistence relies on social pressure and identity-locking mechanisms, rather than overt coercion. Theater ratio is low (0.2) because, from this reading, the ritual's primary function (operational knowledge transfer) is genuinely active and effective, not merely performative. The metrics reflect a system that is costly but functional for its stated purpose.
 *
 * PERSPECTIVAL GAP:
 *   Present-generation participants experience this as a highly extractive and suppressive constraint on their autonomy, even if they believe in its ultimate purpose. Future generations, the beneficiaries, experience it as a protective, life-saving inheritance. Ritual elders, as agenda-setters, perceive it as a necessary, if difficult, coordination mechanism for collective survival. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations are the full beneficiaries (d=0.0) as they receive the survival competence without bearing the costs. Present-generation participants are the primary targets (d=1.0) due to the high costs and identity-locked exit. Ritual elders are closer to symmetric (d=0.5) as they enforce the constraint and bear the burden of its maintenance, but also benefit from the community's continued existence and their role within it. Modernity and secularism are excluded, their perspective not directly factored into the ritual's internal logic.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly counters a mandatrophy claim by asserting the ritual's continued operational efficacy. The high extractiveness is justified by the 'live' status of the founding problem (existential threat). If the founding problem were 'dead' but the ritual persisted with high extraction, it would signal mandatrophy and a potential reclassification to Piton or Snare. The 'survival competence' reading argues against this by emphasizing the ongoing, active function of the ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_efficacy_validation,
    'Does the ritual genuinely transfer operational threat-recognition capacity, or is it primarily symbolic and identity-forming?',
    'Empirical study of community responses to actual threats, comparing outcomes in ritual-adherent vs. non-adherent groups, or detailed ethnographic analysis of the specific knowledge transferred and its application.',
    'If found to be primarily symbolic, the extractiveness would be reclassified as pure extraction (Snare) rather than coordination cost, and the claimed type would shift from Tangled Rope to Snare. If operational efficacy is confirmed, the Tangled Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_efficacy_validation, empirical, 'Whether the ritual''s claimed operational function is empirically verifiable.').

omega_variable(
    cost_benefit_intergenerational_equity,
    'Is the intergenerational distribution of costs and benefits (present generation pays, future generation benefits) equitable, or does it constitute an unjust burden on the present?',
    'Ethical and philosophical analysis of intergenerational justice, potentially informed by community-led deliberation on alternative, less extractive methods of knowledge transfer.',
    'If deemed inequitable, the constraint''s legitimacy would be challenged, potentially leading to pressure for reform or reclassification as a Snare, even if operationally effective. If deemed equitable, the Tangled Rope classification is reinforced as a necessary, if costly, coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_benefit_intergenerational_equity, preference, 'Ethical evaluation of intergenerational cost-benefit distribution.').

omega_variable(
    kernel_reading_distinction,
    'Is this ''survival competence'' reading of the catastrophe memory preservation kernel distinct from the ''mourning practice'' or ''hybrid atrophy'' readings, or do they represent different facets of the same underlying phenomenon?',
    'Conceptual analysis of the core claims of each reading, identifying irreconcilable differences in their assertions about the ritual''s primary function and empirical status. This omega documents the committer frame.',
    'If the readings are truly distinct, this constraint stands as a valid, separate analysis. If they are found to be conflated, the constraint would need to be decomposed or re-framed to capture the underlying unity or ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'This constraint is one reading of the ''catastrophe_memory_preservation'' kernel, specifically the ''survival_competence_reading''. Sibling readings include ''mourning_practice_reading'' (ritual as symbolic continuity) and ''hybrid_atrophy_reading'' (ritual as atrophied survival competence). This reading emphasizes the active, operational transfer of knowledge, which is a point of contention with the other readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 75, 0.19).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 75, 0.71).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 100, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 75, 0.61).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__survival_competence_reading, 0.08).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_preservation' kernel. Sibling readings include 'mourning_practice_reading' and 'hybrid_atrophy_reading', which offer alternative interpretations of the ritual's function and efficacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
