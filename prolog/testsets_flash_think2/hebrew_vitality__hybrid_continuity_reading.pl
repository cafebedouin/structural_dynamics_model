% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__hybrid_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__hybrid_continuity_reading, []).

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
 *   constraint_id: hebrew_vitality__hybrid_continuity_reading
 *   human_readable: Hebrew Vitality: Hybrid Continuity Reading
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint is the 'hybrid continuity' reading of the
 *   'hebrew_vitality' kernel. It posits that both liturgical preservation (as
 *   a substrate) and active reconstruction (as vernacular revival) were
 *   necessary for Hebrew's modern vitality, attempting to synthesize the
 *   'liturgical' and 'native daily' readings. It functions as an analytical
 *   model that coordinates understanding rather than an actively enforced
 *   rule, hence its low extractiveness and suppression. The claimed type
 *   'rope' reflects its role in coordinating a more comprehensive academic
 *   and practical understanding of language revitalization.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__hybrid_continuity_reading, 0.15).
domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, 0.1).
domain_priors:theater_ratio(hebrew_vitality__hybrid_continuity_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__hybrid_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__hybrid_continuity_reading, "Hebrew Vitality: Hybrid Continuity Reading").
narrative_ontology:topic_domain(hebrew_vitality__hybrid_continuity_reading, "sociolinguistics/language_revitalization/jewish_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__hybrid_continuity_reading, 'bc554a23-4c76-4214-8be6-ffb9848d7f30').
narrative_ontology:cs_kernel_codification('bc554a23-4c76-4214-8be6-ffb9848d7f30', distributed).
narrative_ontology:cs_authority_grounding('bc554a23-4c76-4214-8be6-ffb9848d7f30', expertise).
narrative_ontology:cs_interpretation_layer_present('bc554a23-4c76-4214-8be6-ffb9848d7f30').
narrative_ontology:cs_reading_relation('bc554a23-4c76-4214-8be6-ffb9848d7f30', hebrew_vitality__liturgical_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc554a23-4c76-4214-8be6-ffb9848d7f30', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_axiom('bc554a23-4c76-4214-8be6-ffb9848d7f30', foundational, vitality_requires_both_substrate_and_reconstruction).
narrative_ontology:cs_axiom_status(vitality_requires_both_substrate_and_reconstruction, holdable).
narrative_ontology:cs_axiom_grounding('bc554a23-4c76-4214-8be6-ffb9848d7f30', vitality_requires_both_substrate_and_reconstruction, empirically_contingent).
narrative_ontology:cs_axiom('bc554a23-4c76-4214-8be6-ffb9848d7f30', secondary, liturgical_use_is_necessary_but_insufficient).
narrative_ontology:cs_axiom_status(liturgical_use_is_necessary_but_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('bc554a23-4c76-4214-8be6-ffb9848d7f30', liturgical_use_is_necessary_but_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('bc554a23-4c76-4214-8be6-ffb9848d7f30', complex_multicausal_revitalization).
narrative_ontology:cs_drift_state('bc554a23-4c76-4214-8be6-ffb9848d7f30', contemporary_sociolinguistic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('bc554a23-4c76-4214-8be6-ffb9848d7f30', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, language_revitalization_scholars).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, language_revitalization_activists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, sociolinguists).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, hebrew_language_scholars).
narrative_ontology:constraint_victim(hebrew_vitality__hybrid_continuity_reading, liturgical_preservationists).
narrative_ontology:constraint_victim(hebrew_vitality__hybrid_continuity_reading, native_generation_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective intellectual environment where theories of language revitalization are debated, refined, and validated. It sets the terms for what constitutes a robust explanation of language vitality.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, academic_discourse, agenda_setter,
    institutional, generational, mobile, global).

% Gain a more robust and comprehensive theoretical framework for understanding complex language revitalization processes, moving beyond single-factor explanations. This reading provides a richer model for their research.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, sociolinguists, beneficiary,
    powerful, biographical, mobile, global).

% Benefit from a nuanced historical understanding of Hebrew's revival, integrating liturgical continuity with active vernacular reconstruction. This helps reconcile seemingly contradictory historical narratives.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, hebrew_language_scholars, beneficiary,
    powerful, biographical, mobile, global).

% Acquire a more effective and evidence-based strategy for their efforts, recognizing the multi-faceted requirements for successful language revival. This reading informs their practical approaches.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, language_revitalization_activists, beneficiary,
    organized, generational, constrained, global).

% Their perspective, which emphasizes ritual preservation as the core of vitality, is reframed as 'necessary but insufficient.' This requires them to adjust their understanding and potentially integrate broader revitalization efforts, incurring a conceptual cost.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, liturgical_preservationists, payer,
    moderate, generational, identity_locked, national).

% Their view, which prioritizes native generation as the sole marker of vitality, is reframed to acknowledge the foundational role of prior continuity. This requires them to broaden their historical and theoretical framework, incurring a conceptual cost.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, native_generation_advocates, payer,
    moderate, generational, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__hybrid_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_vitality__hybrid_continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates understanding of complex language revitalization processes by integrating the roles of historical continuity (substrate) and active reconstruction (vernacular revival), offering a more comprehensive explanatory model.
% TRANSFER_FUNCTION: Transfers analytical clarity and a more nuanced historical narrative to the academic and activist discourse, moving away from simpler, often competing, single-factor explanations.
% ABSENT_VOICES: Those who insist on a single, simple cause for language vitality (either purely liturgical or purely native generation) might object, as their models are challenged by this synthesis. They are present in the broader discourse but their singular claims are reframed.
% DISAPPEARANCE_RATIONALE: If this hybrid reading vanished, the nuanced understanding of Hebrew's revival would be lost, leading to a return to simpler, potentially less effective, and more contentious models for language revitalization. The academic and activist discourse would lose a valuable framework for integrating diverse historical factors.
% FOUNDING_PROBLEM: The historical and theoretical debate over what truly constitutes 'language vitality' and how Hebrew achieved it, particularly reconciling the undeniable role of liturgical continuity with the necessity of active vernacular revival.
% FOUNDING_PROBLEM_CORROBORATION: Historical linguists, sociologists of language, and independent historians of the Hebrew revival attest to the ongoing debate and the need for a synthetic understanding, supporting the problem's live status from outside the immediate advocacy groups.
narrative_ontology:disappearance_verdict(hebrew_vitality__hybrid_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__hybrid_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__hybrid_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_vitality__hybrid_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__hybrid_continuity_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__hybrid_continuity_reading_tests).
:- end_tests(hebrew_vitality__hybrid_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) and suppression (0.10) reflect that this is an analytical framework, not a mechanism for resource extraction or coercive enforcement. Its 'rope' classification stems from its function in coordinating a more nuanced and integrated understanding among scholars and activists, resolving tensions between simpler, competing narratives. The low theater ratio (0.05) indicates it's primarily functional as an explanatory model, with minimal performative aspects. Resistance is low (0.20) as it's an academic debate, not a policy mandate, though some scholars may resist its synthesis.
 *
 * PERSPECTIVAL GAP:
 *   There isn't a strong perspectival gap in terms of extraction, as this reading itself is not extractive. The 'gap' is primarily conceptual: those who adhere to the 'liturgical' or 'native daily' readings might perceive this hybrid model as diluting their core claims, while this reading views itself as a more complete and accurate synthesis. The engine's classification will reflect its low-extraction, coordination-focused nature, which aligns with the analytical intent of this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'academic_discourse' acts as the agenda-setter, shaping the acceptance and refinement of such models. Sociolinguists, Hebrew language scholars, and language revitalization activists are beneficiaries, gaining a more robust and effective framework. Liturgical preservationists and native generation advocates are 'payers' in a conceptual sense, as their prior, simpler views are challenged and reframed as partial truths, requiring intellectual adjustment.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    analytical_synthesis_validity,
    'Is this hybrid continuity reading a true historical/sociolinguistic insight, or primarily a conceptual reframing designed to resolve academic tension between existing readings?',
    'Further empirical research into other cases of language revitalization, testing whether similar multi-factor dynamics are consistently observed, or historical re-evaluation of primary sources on Hebrew''s revival.',
    'If primarily a conceptual reframing, its ''rope'' function is more about academic coordination than empirical truth, potentially lowering its perceived ''naturalness'' as an explanatory model. If a true insight, it strengthens its position as a robust explanatory framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(analytical_synthesis_validity, conceptual, 'Ambiguity between empirical truth and conceptual utility in resolving academic debate.').

omega_variable(
    resource_allocation_impact,
    'How does the adoption of this hybrid model impact the allocation of resources and strategic priorities for actual language revitalization efforts?',
    'Longitudinal studies of language revitalization programs that explicitly adopt this hybrid framework, comparing their outcomes and resource distribution to programs based on single-factor models.',
    'If it leads to more balanced and effective resource allocation, it reinforces its ''rope'' classification by demonstrating practical coordination benefits. If it creates new inefficiencies or conflicts, it might suggest unforeseen extractive dynamics or coordination failures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resource_allocation_impact, empirical, 'Practical impact of the analytical model on real-world revitalization strategies and resource distribution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__hybrid_continuity_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t2000, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 2000, 0.04).
narrative_ontology:measurement(hebr_tr_t2005, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 2005, 0.05).
narrative_ontology:measurement(hebr_tr_t2010, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(hebr_tr_t2015, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 2015, 0.05).
narrative_ontology:measurement(hebr_tr_t2020, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 2020, 0.05).
narrative_ontology:measurement(hebr_tr_t2025, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t2000, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement(hebr_be_t2005, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 2005, 0.13).
narrative_ontology:measurement(hebr_be_t2010, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(hebr_be_t2015, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(hebr_be_t2020, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 2020, 0.15).
narrative_ontology:measurement(hebr_be_t2025, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t2000, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 2000, 0.08).
narrative_ontology:measurement(hebr_su_t2005, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 2005, 0.09).
narrative_ontology:measurement(hebr_su_t2010, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(hebr_su_t2015, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 2015, 0.1).
narrative_ontology:measurement(hebr_su_t2020, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 2020, 0.1).
narrative_ontology:measurement(hebr_su_t2025, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 2025, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__hybrid_continuity_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'hebrew_vitality' kernel, offering a synthetic view that integrates elements of the 'liturgical_reading' and 'native_daily_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
