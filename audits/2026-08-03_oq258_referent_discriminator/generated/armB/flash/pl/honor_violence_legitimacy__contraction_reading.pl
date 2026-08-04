% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Redefined to Exclude Violence (Contraction Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the historical process by which the concept of
 *   'honor' itself was redefined in Western societies to exclude dueling and
 *   other forms of interpersonal violence as legitimate responses. Instead of
 *   dueling being suppressed by external forces, the very definition of an
 *   honorable person shifted, making the act of dueling structurally
 *   unthinkable for those who wished to maintain their honor. This is the
 *   'contraction_reading' of the 'honor_violence_legitimacy' kernel, focusing
 *   on internal conceptual shifts.
 *
 * KEY AGENTS:
 *   - civil_society: Primary beneficiary (institutional/generational) — benefits from reduced violence
 *   - state_legal_system: Secondary beneficiary (institutional/generational) — benefits from reduced challenge to its monopoly on violence
 *   - dueling_gentlemen: Primary target (powerful/biographical) — their traditional means of resolving disputes is delegitimized
 *   - honor_code_adherents: Payer (moderate/biographical) — internalize the new definition of honor, losing a prior option
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.15).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.75).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Redefined to Exclude Violence (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:emerges_naturally(honor_violence_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, '51fe86ce-6a65-476b-a4e6-897bf8e8e368').
narrative_ontology:cs_kernel_codification('51fe86ce-6a65-476b-a4e6-897bf8e8e368', implicit).
narrative_ontology:cs_authority_grounding('51fe86ce-6a65-476b-a4e6-897bf8e8e368', practice).
narrative_ontology:cs_interpretation_layer_present('51fe86ce-6a65-476b-a4e6-897bf8e8e368').
narrative_ontology:cs_reading_relation('51fe86ce-6a65-476b-a4e6-897bf8e8e368', honor_violence_legitimacy__drop_reading, influences).
narrative_ontology:cs_reading_relation('51fe86ce-6a65-476b-a4e6-897bf8e8e368', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('51fe86ce-6a65-476b-a4e6-897bf8e8e368', foundational, honor_is_internal_virtue).
narrative_ontology:cs_axiom_status(honor_is_internal_virtue, holdable).
narrative_ontology:cs_axiom_grounding('51fe86ce-6a65-476b-a4e6-897bf8e8e368', honor_is_internal_virtue, deontological).
narrative_ontology:cs_axiom('51fe86ce-6a65-476b-a4e6-897bf8e8e368', secondary, violence_is_barbaric).
narrative_ontology:cs_axiom_status(violence_is_barbaric, holdable).
narrative_ontology:cs_axiom_grounding('51fe86ce-6a65-476b-a4e6-897bf8e8e368', violence_is_barbaric, deontological).
narrative_ontology:cs_reference_frame('51fe86ce-6a65-476b-a4e6-897bf8e8e368', aristocratic_honor_code).
narrative_ontology:cs_drift_state('51fe86ce-6a65-476b-a4e6-897bf8e8e368', enlightenment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('51fe86ce-6a65-476b-a4e6-897bf8e8e368', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, civil_society).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_legal_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, dueling_gentlemen).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, honor_code_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the reduction of violence and the establishment of more peaceful means of dispute resolution. Actively promoted the redefinition of honor through moral discourse and social pressure.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, civil_society, beneficiary,
    institutional, generational, analytical, national).

% Benefits from the erosion of private violence, which strengthens its monopoly on legitimate force. Its legal prohibitions against dueling become more effective as the social legitimacy of dueling declines.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_legal_system, beneficiary,
    institutional, generational, arbitrage, national).

% Historically relied on dueling to defend their honor and social standing. The redefinition of honor makes their traditional practice socially unacceptable, forcing them to choose between their identity and social legitimacy.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, dueling_gentlemen, payer,
    powerful, biographical, identity_locked, local).

% Individuals who internalize the evolving honor code. They bear the cost of losing a prior option for dispute resolution, but gain social acceptance by conforming to the new definition of honor.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, honor_code_adherents, payer,
    moderate, biographical, identity_locked, local).

% Actively campaigned for the redefinition of honor to exclude violence, using religious, philosophical, and social arguments. Their efforts directly shaped the conceptual shift.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, moral_reformers, agenda_setter,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social expectations around what constitutes honorable behavior, reducing ambiguity and conflict over appropriate responses to perceived slights.
% TRANSFER_FUNCTION: Transfers the social legitimacy of violence as an honorable act away from individuals and towards the state, and from traditional aristocratic codes to broader civil society norms.
% ABSENT_VOICES: Those who continued to adhere to older, more violent honor codes were increasingly marginalized and excluded from mainstream discourse, their voices dismissed as anachronistic or barbaric. Their perspective, that honor *required* a violent defense, was systematically delegitimized.
% DISAPPEARANCE_RATIONALE: If the redefinition of honor to exclude violence vanished, the social landscape would fundamentally shift. Dueling might not immediately return, but the conceptual space for private violence as a legitimate response to insult would reopen, challenging the state's monopoly on force and civil society's norms of peaceful dispute resolution. The very fabric of social interaction around honor would need to be renegotiated.
% FOUNDING_PROBLEM: The problem was the perceived social disorder and loss of life caused by dueling and other forms of private violence, which challenged state authority and civil order.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, legal statutes, and philosophical treatises from the period corroborate the widespread concern over dueling's social costs and the active efforts to redefine honor. These sources come from state actors, religious institutions, and intellectual movements, not solely from those who directly benefited from the decline of dueling.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_violence_legitimacy__contraction_reading),
    narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint operates primarily through internal conceptual redefinition rather than direct material extraction. Suppression is high (0.75) because the redefinition of honor effectively 'suppresses' the very idea of dueling as a legitimate option, making it socially unthinkable. Accessibility collapse is high (0.88) as the conceptual space for dueling as an honorable act collapses. Resistance is low (0.1) because the shift is internalized, not externally imposed. Theater ratio is low (0.05) as the constraint is genuinely about a conceptual shift, not performative maintenance of an obsolete practice.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of civil society and the state, this redefinition is a positive evolution towards a more peaceful social order. From the perspective of dueling gentlemen, it represents a loss of a traditional means of defending honor, forcing them to adapt to new social norms. The engine will compute different classifications for these seats based on their structural relationship to the redefined honor code.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil society and the state legal system are beneficiaries, as the redefinition of honor aligns with their interests in social order and monopoly on violence. Dueling gentlemen and honor code adherents are targets, as their options for defending honor are curtailed by the new conceptual framework. The constraint subsidizes the former by making the latter's behavior unthinkable.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its 'mandate' is a conceptual redefinition rather than a functional institution. The question is whether the redefinition is a genuine 'mountain' of moral progress or a 'false summit' (Tangled Rope) that benefits certain social actors by suppressing a prior practice. The omegas address this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the exclusion of violence from honor a natural evolution of moral sentiment, or a socially constructed redefinition driven by state power and changing social norms?',
    'Comparative historical analysis of other honor cultures that retained violence, and examination of the specific mechanisms (legal, social, religious) that enforced the redefinition in this context.',
    'If a social construct, the constraint is a ''false summit'' (Tangled Rope) benefiting identifiable agents (state, civil society) by suppressing a prior practice. If natural, it remains a Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity between natural moral evolution and social construction of honor.').

omega_variable(
    kernel_reading_ambiguity,
    'This constraint is the ''contraction_reading'' of the ''honor_violence_legitimacy'' kernel. Is this reading the most accurate account of dueling''s decline, or do the ''drop_reading'' (external costs) or ''composite_reading'' (both factors) offer a better explanation?',
    'Further historical and sociological research to weigh the relative causal contributions of internal conceptual shifts versus external enforcement and cost pressures.',
    'If the ''drop_reading'' is more accurate, the constraint is a Snare (due to external suppression) or a Piton (due to atrophy). If the ''composite_reading'' is more accurate, the constraint is a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Ambiguity between different readings of dueling''s decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 1700, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__contraction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hono_tr_t50, honor_violence_legitimacy__contraction_reading, theater_ratio, 50, 0.07).
narrative_ontology:measurement(hono_tr_t100, honor_violence_legitimacy__contraction_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__contraction_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(hono_be_t50, honor_violence_legitimacy__contraction_reading, base_extractiveness, 50, 0.2).
narrative_ontology:measurement(hono_be_t100, honor_violence_legitimacy__contraction_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__contraction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hono_su_t50, honor_violence_legitimacy__contraction_reading, suppression_requirement, 50, 0.65).
narrative_ontology:measurement(hono_su_t100, honor_violence_legitimacy__contraction_reading, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, state_monopoly_on_violence).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, social_norms_of_dispute_resolution).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'honor_violence_legitimacy' kernel, focusing on the internal redefinition of honor. It is linked to the 'drop_reading' (external costs) and 'composite_reading' (both factors) which offer alternative explanations for dueling's decline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
