% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__decline_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Dueling as Honor Satisfaction Mechanism (Decline Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This constraint story describes dueling as a mechanism for honor
 *   satisfaction, focusing on its historical decline from a widespread,
 *   albeit dangerous, practice to a fringe activity. Despite increasing state
 *   suppression and evolving social norms, the practice persisted at a
 *   declining frequency, reflecting a gradual erosion of its social
 *   legitimacy and necessity rather than an abrupt categorical shift. This
 *   reading emphasizes the continued conceptual availability of dueling, even
 *   as its practical incidence and social acceptance waned.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.6).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.7).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Dueling as Honor Satisfaction Mechanism (Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, '415c2076-efed-457f-8e34-0023bfcbd497').
narrative_ontology:cs_kernel_codification('415c2076-efed-457f-8e34-0023bfcbd497', implicit).
narrative_ontology:cs_authority_grounding('415c2076-efed-457f-8e34-0023bfcbd497', practice).
narrative_ontology:cs_interpretation_layer_present('415c2076-efed-457f-8e34-0023bfcbd497').
narrative_ontology:cs_reading_relation('415c2076-efed-457f-8e34-0023bfcbd497', honor_satisfaction_mechanism__contraction_reading, influences).
narrative_ontology:cs_reading_relation('415c2076-efed-457f-8e34-0023bfcbd497', honor_satisfaction_mechanism__composite_reading, coexists_with).
narrative_ontology:cs_axiom('415c2076-efed-457f-8e34-0023bfcbd497', foundational, honor_must_be_defended_personally).
narrative_ontology:cs_axiom_status(honor_must_be_defended_personally, holdable).
narrative_ontology:cs_axiom_grounding('415c2076-efed-457f-8e34-0023bfcbd497', honor_must_be_defended_personally, deontological).
narrative_ontology:cs_axiom('415c2076-efed-457f-8e34-0023bfcbd497', secondary, state_cannot_adjudicate_honor).
narrative_ontology:cs_axiom_status(state_cannot_adjudicate_honor, overridden).
narrative_ontology:cs_axiom_grounding('415c2076-efed-457f-8e34-0023bfcbd497', state_cannot_adjudicate_honor, conventional).
narrative_ontology:cs_reference_frame('415c2076-efed-457f-8e34-0023bfcbd497', gentlemanly_honor_code_era).
narrative_ontology:cs_drift_state('415c2076-efed-457f-8e34-0023bfcbd497', late_19th_century, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('415c2076-efed-457f-8e34-0023bfcbd497', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, gentlemen_seeking_honor_redress).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, social_order_maintainers).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, duelists).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, families_of_duelists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who felt their honor had been impugned and sought to restore it through the ritual of dueling. They benefited from the social validation of their honor, even as the practice became more fringe. Their options were constrained by social expectations and legal prohibitions.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, gentlemen_seeking_honor_redress, agenda_setter,
    powerful, biographical, constrained, national).

% The direct participants in duels, facing the immediate risks of injury, death, or legal prosecution. They paid the highest personal cost, often feeling trapped by the honor code and social expectations.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, duelists, payer,
    moderate, immediate, trapped, local).

% Suffered the social stigma, emotional distress, and economic consequences of a family member's participation in a duel, whether victorious or not. They had little agency to prevent duels.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, families_of_duelists, payer,
    powerless, biographical, trapped, local).

% Governments and legal systems that increasingly criminalized dueling and sought to enforce a monopoly on violence. They actively suppressed the practice, viewing it as a challenge to their authority and a breach of public order.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, state_authorities, agenda_setter,
    institutional, generational, analytical, national).

% The changing societal attitudes that gradually delegitimized dueling, shifting towards legal and non-violent means of dispute resolution. This 'agent' represents the diffuse social pressure against the practice.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, evolving_social_norms, observer,
    analytical, generational, analytical, universal).

% The majority of the population who were not part of the aristocratic or gentry class bound by the honor code. They were largely excluded from the practice and its associated social rituals, often viewing it as barbaric.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, common_citizens, excluded,
    powerless, biographical, mobile, local).

% Those who benefited from the perceived, albeit violent, mechanism for resolving disputes among the elite, which could prevent wider feuds or less structured violence. This benefit declined as state authority grew.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, social_order_maintainers, beneficiary,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__decline_reading, gentlemen_seeking_honor_redress).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__decline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a ritualized, albeit violent, mechanism for gentlemen to resolve disputes of honor and maintain social standing, preventing less structured violence or endless feuds among the elite.
% TRANSFER_FUNCTION: Transferred social standing and honor (or disgrace and death) between participants, enforced by a strict social code and the threat of violence.
% ABSENT_VOICES: Common citizens, women, and those who rejected the honor code were structurally excluded. They would have argued for legal resolution and condemned the violence as barbaric and unnecessary.
% DISAPPEARANCE_RATIONALE: If dueling and its underlying honor code had vanished overnight, the social fabric around honor, reputation, and dispute resolution among the elite would have been fundamentally different. Legal systems would have had to fully absorb the function of honor redress much earlier and more completely.
% FOUNDING_PROBLEM: The need for gentlemen to publicly defend their honor and resolve perceived slights in a society where legal recourse for such matters was often inadequate or seen as beneath their status.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars, from outside the dueling class, corroborate that the specific social conditions that made dueling a 'necessary evil' largely disappeared by the late 19th and early 20th centuries, as state legal systems matured and social norms evolved. Legislative records and public discourse from the period also attest to this shift.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__decline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__decline_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_satisfaction_mechanism__decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__decline_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__decline_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.6) reflects the high personal cost for participants, even in decline, but is lower than its peak due to reduced social pressure to duel and increased alternatives. Suppression (0.7) is high due to active state criminalization and growing social opprobrium. The theater ratio (0.4) indicates that while dueling retained ritualistic elements, its core function was still to resolve disputes with real, often fatal, consequences, not merely performance. Accessibility collapse (0.4) is moderate, as legal and social alternatives for dispute resolution became increasingly available. Resistance (0.6) is high, reflecting both state enforcement and the growing societal rejection of the practice. The measurements show a clear decline in extractiveness and a rise in suppression over the period, consistent with the 'decline reading'.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of a gentleman bound by the honor code, dueling was a necessary, if dangerous, means to preserve social standing. From the perspective of state authorities and common citizens, it was a barbaric and illegal act that undermined public order. The engine's classification will reflect this divergence, showing it as a 'tangled_rope' for participants (coordination + extraction) but a 'snare' or 'piton' from the state's view (pure extraction/inertia).
 *
 * DIRECTIONALITY LOGIC:
 *   Gentlemen seeking honor redress and social order maintainers were the primary beneficiaries, as the mechanism, however violent, provided a means of dispute resolution and status maintenance. Duelists and their families were the primary victims, bearing the direct costs of injury, death, and social stigma. State authorities acted as agenda-setters, increasingly suppressing the practice. Evolving social norms represent a diffuse 'observer' force that contributed to the decline.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of inadequate legal recourse for honor disputes became 'dead' as state legal systems matured. However, the constraint persisted due to social inertia, the lingering power of the honor code, and the difficulty of fully eradicating deeply ingrained cultural practices. This persistence, despite the obsolescence of its original justification, is a key aspect of its decline, preventing a clean resolution of mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decline_vs_contraction_mechanism,
    'Was the decline of dueling primarily a gradual reduction in frequency and social acceptance (decline_reading), or did it become cognitively unthinkable, a category-level impossibility (contraction_reading)?',
    'Analysis of primary sources (diaries, legal records, popular literature) for evidence of cognitive framing shifts vs. mere behavioral reduction. Examination of the language used to describe dueling over time.',
    'If contraction_reading is true, the constraint''s accessibility_collapse would be much higher, and its persistence would be driven by a different mechanism (cognitive inertia rather than social pressure). If decline_reading is true, the constraint''s conceptual availability remains, even if rarely enacted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decline_vs_contraction_mechanism, conceptual, 'Distinguishing between a gradual decline and a categorical cognitive shift in the understanding of dueling.').

omega_variable(
    single_vs_composite_causation,
    'Is the decline of dueling best understood as a single, overarching process (decline_reading), or as the result of multiple distinct, interacting mechanisms (e.g., state monopoly on violence, bourgeois norms, insurance, category-shift) as proposed by the composite_reading?',
    'Detailed historical-sociological analysis disentangling the causal pathways and their relative contributions to the overall decline. Counterfactual analysis of specific interventions.',
    'If composite_reading is true, this single constraint story would need to be decomposed into a family of linked constraints, each representing a distinct causal mechanism. If decline_reading is sufficient, the current single-constraint model holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_vs_composite_causation, empirical, 'Whether the decline of dueling is a unitary phenomenon or a composite of distinct causal factors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_mechanism__decline_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(hono_tr_t20, honor_satisfaction_mechanism__decline_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(hono_tr_t40, honor_satisfaction_mechanism__decline_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(hono_tr_t60, honor_satisfaction_mechanism__decline_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(hono_tr_t80, honor_satisfaction_mechanism__decline_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement(hono_tr_t100, honor_satisfaction_mechanism__decline_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(hono_be_t20, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(hono_be_t40, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(hono_be_t60, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(hono_be_t80, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 80, 0.62).
narrative_ontology:measurement(hono_be_t100, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 100, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hono_su_t20, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(hono_su_t40, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(hono_su_t60, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(hono_su_t80, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 80, 0.69).
narrative_ontology:measurement(hono_su_t100, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__decline_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, state_monopoly_on_violence).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, bourgeois_social_norms).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% This is the 'decline_reading' of the 'honor_satisfaction_mechanism' kernel, focusing on its gradual erosion. It is linked to the 'contraction_reading' (cognitive impossibility) and 'composite_reading' (multiple mechanisms) as part of a constraint family exploring the historical transformation of dueling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
