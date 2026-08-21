% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__absolutist_reading, []).

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
 *   constraint_id: speech_protection_kernel__absolutist_reading
 *   human_readable: Speech Protection (Absolutist Reading)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint represents the 'absolutist' reading of speech protection,
 *   where speech is protected near-categorically, and listener harm is
 *   generally not considered a valid ground for restriction, except for very
 *   narrow, historically defined categories (e.g., incitement, true threats).
 *   This reading prioritizes speaker autonomy and a robust, uninhibited
 *   marketplace of ideas, even at the cost of potential harm to individuals
 *   or groups. It is one reading of the broader 'speech_protection_kernel'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, 0.15).
domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, 0.05).
domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__absolutist_reading, mountain).
narrative_ontology:human_readable(speech_protection_kernel__absolutist_reading, "Speech Protection (Absolutist Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__absolutist_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:emerges_naturally(speech_protection_kernel__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__absolutist_reading, '2cf3f959-1b5e-4bc7-b0e2-fdc5cacf3a63').
narrative_ontology:cs_kernel_codification('2cf3f959-1b5e-4bc7-b0e2-fdc5cacf3a63', fixed_text).
narrative_ontology:cs_authority_grounding('2cf3f959-1b5e-4bc7-b0e2-fdc5cacf3a63', lineage).
narrative_ontology:cs_interpretation_layer_present('2cf3f959-1b5e-4bc7-b0e2-fdc5cacf3a63').
narrative_ontology:cs_reading_relation('2cf3f959-1b5e-4bc7-b0e2-fdc5cacf3a63', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('2cf3f959-1b5e-4bc7-b0e2-fdc5cacf3a63', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('2cf3f959-1b5e-4bc7-b0e2-fdc5cacf3a63', speech_protection_kernel__dignity_reading, forecloses).
narrative_ontology:cs_reading_relation('2cf3f959-1b5e-4bc7-b0e2-fdc5cacf3a63', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('2cf3f959-1b5e-4bc7-b0e2-fdc5cacf3a63', foundational, speaker_autonomy_maximization).
narrative_ontology:cs_axiom_status(speaker_autonomy_maximization, holdable).
narrative_ontology:cs_axiom_grounding('2cf3f959-1b5e-4bc7-b0e2-fdc5cacf3a63', speaker_autonomy_maximization, deontological).
narrative_ontology:cs_axiom('2cf3f959-1b5e-4bc7-b0e2-fdc5cacf3a63', foundational, listener_harm_not_grounds_for_restriction).
narrative_ontology:cs_axiom_status(listener_harm_not_grounds_for_restriction, holdable).
narrative_ontology:cs_axiom_grounding('2cf3f959-1b5e-4bc7-b0e2-fdc5cacf3a63', listener_harm_not_grounds_for_restriction, conventional).
narrative_ontology:cs_reference_frame('2cf3f959-1b5e-4bc7-b0e2-fdc5cacf3a63', uninhibited_robust_wide_open_debate).
narrative_ontology:cs_drift_state('2cf3f959-1b5e-4bc7-b0e2-fdc5cacf3a63', contemporary_social_media_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2cf3f959-1b5e-4bc7-b0e2-fdc5cacf3a63', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__absolutist_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, listeners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals or groups expressing ideas, opinions, or information without fear of government censorship or restriction, even if their speech is offensive or causes distress to others. Their autonomy is maximized under this reading.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, speakers, beneficiary,
    moderate, biographical, mobile, national).

% Individuals exposed to speech that they find harmful, offensive, or distressing, with limited recourse for restriction based on the content's impact. They bear the cost of exposure to potentially harmful speech.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, listeners, payer,
    powerless, immediate, constrained, local).

% Agencies and courts tasked with interpreting and enforcing speech protections. Under this reading, their power to restrict speech is severely limited, primarily to narrow, historically recognized categories like incitement or true threats.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, government_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Individuals or groups targeted by speech that causes them psychological, social, or physical harm, but which is not deemed to fall outside the narrow categories of unprotected speech. Their claims for protection are largely rejected under this reading.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, victims_of_hate_speech, excluded,
    powerless, biographical, trapped, local).

% Academics and legal experts who analyze the theoretical underpinnings and practical implications of different speech protection doctrines. They evaluate the consistency and consequences of the absolutist reading.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, predictable boundary for protected speech, minimizing government discretion and fostering a robust, uninhibited exchange of ideas by maximizing speaker autonomy.
% TRANSFER_FUNCTION: Transfers the burden of tolerating offensive or harmful speech from the state to individual listeners and targeted groups, in exchange for maximal speaker freedom.
% ABSENT_VOICES: Victims of speech-related harm (e.g., hate speech, harassment) are structurally marginalized in this framework; their experiences of harm are not considered sufficient grounds for restriction, leading to their exclusion from the policy-setting conversation.
% DISAPPEARANCE_RATIONALE: If this absolutist reading vanished, the legal landscape for speech would fundamentally shift. Governments would likely gain broader powers to regulate speech based on harm, leading to a significant re-evaluation of what is permissible and a chilling effect on certain forms of expression. The balance of power between speakers and listeners would be dramatically altered.
% FOUNDING_PROBLEM: To prevent government tyranny and ensure a free exchange of ideas essential for democratic self-governance, by establishing a robust sphere of individual expression immune from state interference.
% FOUNDING_PROBLEM_CORROBORATION: Many civil liberties organizations and free speech advocates corroborate that the problem of potential government overreach and censorship remains live. However, critics (e.g., civil rights groups, some legal scholars) argue that the absolutist reading has overshot this problem, creating new harms that undermine democratic participation for marginalized groups.
narrative_ontology:disappearance_verdict(speech_protection_kernel__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_kernel__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__absolutist_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__absolutist_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, ExtMetricName, E),
    domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(speech_protection_kernel__absolutist_reading),
    narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(speech_protection_kernel__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.15) is low because the constraint primarily protects speakers, imposing minimal direct costs on them. However, it imposes significant indirect costs on listeners and victims of harmful speech, which is captured by the 'payer' and 'excluded' roles. Suppression (0.05) is very low, reflecting the minimal state intervention in speech. Theater ratio is zero as the function is direct and not performative. Accessibility collapse is high (0.9) because alternatives to this broad protection (e.g., harm-based restrictions) are largely foreclosed. Resistance (0.1) is low from the perspective of the state, which largely adheres to this reading, but high from the perspective of those harmed by speech.
 *
 * PERSPECTIVAL GAP:
 *   Speakers and their advocates perceive this as a pure 'mountain' or 'rope' that secures fundamental liberty. However, listeners and victims of harmful speech experience it as a 'snare' or 'tangled rope' that extracts their safety and dignity for the benefit of speakers, with no effective exit. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers are clear beneficiaries (d=0.0-0.1), as their expressive freedom is maximized. Listeners and victims of hate speech are targets (d=0.9-1.0), bearing the costs of exposure to harmful speech without adequate recourse. Government regulators are constrained agenda-setters (d=0.5), bound by the strictures of this reading, which limits their ability to intervene.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolutist_vs_harm_threshold,
    'Is the categorical rejection of listener harm as a basis for speech restriction a necessary component of robust free speech, or an overextension that creates new forms of social harm?',
    'Empirical studies on the social and psychological impacts of unrestricted harmful speech, combined with legal analysis of alternative frameworks (e.g., ''harm_threshold_reading'') that balance speech with other rights.',
    'If listener harm is recognized as a legitimate basis for restriction, the ''absolutist_reading'' would be reclassified as a ''tangled_rope'' or ''snare'' from the perspective of victims, and the overall extractiveness would increase significantly. If not, its ''mountain'' or ''rope'' classification would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutist_vs_harm_threshold, conceptual, 'The fundamental tension between speaker autonomy and listener protection.').

omega_variable(
    absolutist_vs_dignity_reading,
    'Does the ''absolutist_reading'' inadvertently enable speech that functions as structural subordination, thereby undermining the dignity and equal participation of marginalized groups?',
    'Sociological and legal analysis of how speech operates in contexts of power asymmetry, examining whether the ''absolutist_reading'' is compatible with the goals of equal dignity and democratic participation (as articulated by the ''dignity_reading'' and ''democratic_participation_reading'').',
    'If found to enable structural subordination, the ''absolutist_reading'' would be seen as a ''snare'' for marginalized groups, and its claimed ''naturalness'' would be challenged, potentially leading to reclassification as a ''tangled_rope'' or ''snare'' for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutist_vs_dignity_reading, conceptual, 'Compatibility of absolutist speech with equal dignity and participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__absolutist_reading, 1940, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1940, speech_protection_kernel__absolutist_reading, theater_ratio, 1940, 0.0).
narrative_ontology:measurement(spee_tr_t1960, speech_protection_kernel__absolutist_reading, theater_ratio, 1960, 0.0).
narrative_ontology:measurement(spee_tr_t1980, speech_protection_kernel__absolutist_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_kernel__absolutist_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_kernel__absolutist_reading, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(spee_be_t1940, speech_protection_kernel__absolutist_reading, base_extractiveness, 1940, 0.1).
narrative_ontology:measurement(spee_be_t1960, speech_protection_kernel__absolutist_reading, base_extractiveness, 1960, 0.12).
narrative_ontology:measurement(spee_be_t1980, speech_protection_kernel__absolutist_reading, base_extractiveness, 1980, 0.13).
narrative_ontology:measurement(spee_be_t2000, speech_protection_kernel__absolutist_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(spee_be_t2024, speech_protection_kernel__absolutist_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1940, speech_protection_kernel__absolutist_reading, suppression_requirement, 1940, 0.05).
narrative_ontology:measurement(spee_su_t1960, speech_protection_kernel__absolutist_reading, suppression_requirement, 1960, 0.05).
narrative_ontology:measurement(spee_su_t1980, speech_protection_kernel__absolutist_reading, suppression_requirement, 1980, 0.05).
narrative_ontology:measurement(spee_su_t2000, speech_protection_kernel__absolutist_reading, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement(spee_su_t2024, speech_protection_kernel__absolutist_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'speech_protection_kernel'. Each reading instantiates a distinct constraint with its own structural properties and classification. This 'absolutist_reading' prioritizes speaker autonomy over listener harm, contrasting with other readings that emphasize harm thresholds, truth-discovery, dignity, or democratic participation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
