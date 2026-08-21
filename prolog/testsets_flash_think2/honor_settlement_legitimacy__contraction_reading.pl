% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__contraction_reading, []).

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
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Honor Settlement Legitimacy (Contraction Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint describes the cultural transformation that rendered
 *   dueling cognitively unthinkable as a legitimate means of honor
 *   settlement. It is a reading of the 'honor_settlement_legitimacy' kernel,
 *   focusing on the 'contraction' hypothesis where the entire normative
 *   possibility space for honor culture exited. The constraint is classified
 *   as a Mountain because the cultural shift became an unchangeable, deeply
 *   internalized social fact, making dueling not merely illegal but
 *   incomprehensible as a valid option. Its persistence is due to deep
 *   cultural embedding, not active enforcement against a live alternative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.05).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.95).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Honor Settlement Legitimacy (Contraction Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, '1090c08a-fe4f-4363-9067-4a3ba075a9cb').
narrative_ontology:cs_kernel_codification('1090c08a-fe4f-4363-9067-4a3ba075a9cb', implicit).
narrative_ontology:cs_authority_grounding('1090c08a-fe4f-4363-9067-4a3ba075a9cb', practice).
narrative_ontology:cs_reading_relation('1090c08a-fe4f-4363-9067-4a3ba075a9cb', honor_settlement_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('1090c08a-fe4f-4363-9067-4a3ba075a9cb', honor_settlement_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('1090c08a-fe4f-4363-9067-4a3ba075a9cb', foundational, honor_settlement_by_combat_illegitimate).
narrative_ontology:cs_axiom_status(honor_settlement_by_combat_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('1090c08a-fe4f-4363-9067-4a3ba075a9cb', honor_settlement_by_combat_illegitimate, conventional).
narrative_ontology:cs_reference_frame('1090c08a-fe4f-4363-9067-4a3ba075a9cb', honor_culture_legitimacy).
narrative_ontology:cs_drift_state('1090c08a-fe4f-4363-9067-4a3ba075a9cb', post_enlightenment_social_transformation, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('1090c08a-fe4f-4363-9067-4a3ba075a9cb', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, society_at_large).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, legal_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, honor_culture_adherents_historical).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the elimination of dueling as a legitimate form of dispute resolution, leading to reduced violence and greater social stability. Adapts to new norms of conflict resolution.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, society_at_large, beneficiary,
    moderate, generational, mobile, national).

% No longer needs to actively suppress dueling, as the practice has become culturally unthinkable. Its authority in dispute resolution is strengthened by the cultural shift away from personal combat.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, legal_system, agenda_setter,
    institutional, civilizational, analytical, national).

% Lost the cultural framework that legitimized dueling as a means of honor settlement. Their worldview became obsolete, and their identity tied to the honor code rendered them unable to participate in the new social order without abandoning core tenets.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, honor_culture_adherents_historical, payer,
    powerless, biographical, identity_locked, local).

% Analyze the historical processes through which dueling became culturally unthinkable, documenting the transformation of social norms and legal frameworks.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, cultural_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a non-violent, legally and socially mediated framework for resolving disputes of honor, replacing personal combat with civil processes.
% TRANSFER_FUNCTION: Transferred the right and responsibility to adjudicate honor disputes from individuals to the legal system and broader social norms, thereby reducing physical violence and its associated social costs.
% ABSENT_VOICES: Individuals and groups who clung to the traditional honor code, for whom dueling remained a legitimate, if increasingly dangerous, option. Their perspectives were marginalized and eventually rendered culturally unintelligible by the dominant social transformation.
% DISAPPEARANCE_RATIONALE: If the cultural framework that makes dueling unthinkable were to vanish, and dueling became a cognitively available and legitimate option for honor settlement again, the entire social and legal apparatus for dispute resolution would be fundamentally destabilized, leading to a breakdown of civil order in personal conflicts.
% FOUNDING_PROBLEM: The pervasive violence, loss of life, and social instability caused by the honor code's requirement for dueling as a primary means of dispute resolution, which undermined state authority and civil peace.
% FOUNDING_PROBLEM_CORROBORATION: Extensive historical records, legal statutes, and sociological analyses from the 18th and 19th centuries corroborate the widespread societal problem of dueling and its subsequent decline due to cultural and legal shifts, independent of the perspectives of those who once practiced dueling.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_settlement_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_settlement_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.05) because the constraint primarily functions as a cultural boundary, not a mechanism for resource transfer. Suppression is very high (0.95) because the cultural framework itself deeply suppresses the very idea of dueling as legitimate, making it unthinkable. Theater ratio is very low (0.05) as the transformation was a genuine, profound cultural shift, not a performative maintenance of an atrophied function. Accessibility collapse is near total (0.98) because the alternative (dueling) ceased to be a cognitively available option. Resistance is negligible (0.02) because, once unthinkable, there is no active opposition to the constraint. The temporal measurements reflect a gradual but decisive shift over two centuries, with extractiveness decreasing and suppression increasing as the cultural transformation solidified.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'society_at_large' and the 'legal_system', this constraint is a beneficial, naturalized evolution towards a more civil society. From the perspective of 'honor_culture_adherents_historical', it represents a profound loss of a legitimate cultural practice and a forced redefinition of honor, leading to an identity-locked exit. The engine's per-seat classification would reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Society at large and the legal system are beneficiaries, as they gain from reduced violence and enhanced civil order. Historical adherents of honor culture are payers, as their traditional means of dispute resolution and associated social identity were rendered obsolete and illegitimate. Cultural historians act as observers, analyzing the phenomenon without direct participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_legal_primacy,
    'Was the decline of dueling primarily driven by cultural framework transformation (as this reading asserts) or by legal prohibition and enforcement?',
    'Comparative historical analysis of regions with strong legal prohibitions but persistent dueling, versus regions with weaker legal action but rapid cultural shift.',
    'If legal primacy, the constraint''s suppression would be more attributable to active enforcement, potentially reclassifying it as a Snare or Tangled Rope; if cultural primacy, the Mountain classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_vs_legal_primacy, empirical, 'Determines the primary driver of dueling''s decline: cultural shift or legal force.').

omega_variable(
    cognitive_unthinkability_degree,
    'To what extent did dueling truly become ''cognitively unthinkable'' versus merely socially unacceptable or legally prohibited?',
    'Analysis of primary sources (diaries, literature) for evidence of internal moral revulsion or genuine incomprehension, rather than just fear of legal consequence or social ostracism.',
    'If dueling remained a ''thinkable but forbidden'' option, the accessibility_collapse and suppression metrics would be lower, potentially shifting the classification away from Mountain towards a more actively enforced type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_unthinkability_degree, conceptual, 'Assesses the depth of the cultural transformation from ''forbidden'' to ''unthinkable''.').

omega_variable(
    reading_overlap_with_composite,
    'How much of the ''composite_reading''s'' explanation for dueling''s decline is accounted for by the ''contraction_reading''s'' mechanism?',
    'Detailed causal modeling comparing the explanatory power of cultural contraction against other factors (e.g., rise of bourgeois values, state monopoly on violence) within the composite framework.',
    'If contraction is the dominant mechanism, it strengthens this reading''s claim to explanatory power; if other mechanisms are equally or more significant, it weakens the ''contraction_reading''s'' unique contribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_overlap_with_composite, empirical, 'Clarifies the relative causal weight of cultural contraction within a multi-factor explanation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1700, 0.15).
narrative_ontology:measurement(hono_tr_t1740, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1740, 0.1).
narrative_ontology:measurement(hono_tr_t1780, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1780, 0.08).
narrative_ontology:measurement(hono_tr_t1820, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1820, 0.06).
narrative_ontology:measurement(hono_tr_t1860, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1860, 0.04).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1900, 0.02).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1700, 0.08).
narrative_ontology:measurement(hono_be_t1740, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1740, 0.06).
narrative_ontology:measurement(hono_be_t1780, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1780, 0.05).
narrative_ontology:measurement(hono_be_t1820, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1820, 0.04).
narrative_ontology:measurement(hono_be_t1860, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1860, 0.03).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1900, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1700, 0.7).
narrative_ontology:measurement(hono_su_t1740, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1740, 0.78).
narrative_ontology:measurement(hono_su_t1780, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1780, 0.85).
narrative_ontology:measurement(hono_su_t1820, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1820, 0.9).
narrative_ontology:measurement(hono_su_t1860, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1860, 0.93).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1900, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__contraction_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'honor_settlement_legitimacy' kernel, focusing on the cultural contraction hypothesis. It is linked to sibling readings that offer alternative explanations for dueling's decline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
