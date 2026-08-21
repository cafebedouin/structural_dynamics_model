% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__continuity_reading
 *   human_readable: Vatican II Doctrinal Authority (Continuity Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint instantiates the 'continuity_reading' of the
 *   'vatican_ii_doctrinal_authority' kernel. It frames Vatican II as an
 *   organic development within unchanging tradition, where apparent novelties
 *   are explications of implicit prior teaching. Sibling readings include
 *   'rupture_progressive_reading', 'rupture_traditionalist_reading', and
 *   'composite_overdetermination_reading'. The constraint is claimed as a
 *   'mountain' by its proponents, asserting its natural truth within the
 *   tradition. However, its operation involves active enforcement and
 *   suppression of alternative interpretations, leading to higher measured
 *   extractiveness and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, 0.45).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__continuity_reading, 0.75).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__continuity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__continuity_reading, mountain).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__continuity_reading, "Vatican II Doctrinal Authority (Continuity Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__continuity_reading).
domain_priors:emerges_naturally(vatican_ii_doctrinal_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__continuity_reading, '73fed593-5dc3-48e8-a416-cafeb6f356ad').
narrative_ontology:cs_kernel_codification('73fed593-5dc3-48e8-a416-cafeb6f356ad', fixed_text).
narrative_ontology:cs_authority_grounding('73fed593-5dc3-48e8-a416-cafeb6f356ad', lineage).
narrative_ontology:cs_interpretation_layer_present('73fed593-5dc3-48e8-a416-cafeb6f356ad').
narrative_ontology:cs_reading_relation('73fed593-5dc3-48e8-a416-cafeb6f356ad', vatican_ii_doctrinal_authority__rupture_progressive_reading, forecloses).
narrative_ontology:cs_reading_relation('73fed593-5dc3-48e8-a416-cafeb6f356ad', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('73fed593-5dc3-48e8-a416-cafeb6f356ad', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('73fed593-5dc3-48e8-a416-cafeb6f356ad', foundational, vatican_ii_is_organic_development).
narrative_ontology:cs_axiom_status(vatican_ii_is_organic_development, holdable).
narrative_ontology:cs_axiom_grounding('73fed593-5dc3-48e8-a416-cafeb6f356ad', vatican_ii_is_organic_development, theological).
narrative_ontology:cs_axiom('73fed593-5dc3-48e8-a416-cafeb6f356ad', secondary, magisterial_interpretive_authority).
narrative_ontology:cs_axiom_status(magisterial_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('73fed593-5dc3-48e8-a416-cafeb6f356ad', magisterial_interpretive_authority, theological).
narrative_ontology:cs_reference_frame('73fed593-5dc3-48e8-a416-cafeb6f356ad', pre_conciliar_doctrinal_unity).
narrative_ontology:cs_drift_state('73fed593-5dc3-48e8-a416-cafeb6f356ad', post_vatican_ii_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('73fed593-5dc3-48e8-a416-cafeb6f356ad', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, orthodox_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_critics).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, progressive_critics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, faithful_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Catholic Church, responsible for interpreting and safeguarding doctrine. This reading validates its role in guiding the Church's understanding of Vatican II as consistent with tradition, thereby maintaining its own legitimacy and stability.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, magisterium, agenda_setter,
    institutional, generational, arbitrage, global).

% Theologians whose work aligns with and supports the continuity reading. Their academic and pastoral contributions are validated and promoted within the Church's official structures, securing their professional standing and influence.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, orthodox_theologians, beneficiary,
    powerful, biographical, constrained, global).

% Groups and individuals who perceive Vatican II as a rupture with prior tradition and reject many of its reforms. They face marginalization, censure, and suppression of their views and practices by the Magisterium, bearing the cost of non-conformity to the continuity reading.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_critics, payer,
    organized, biographical, constrained, global).

% Groups and individuals who believe Vatican II initiated a necessary break with pre-conciliar rigidity and authorizes ongoing, radical reform. They also face suppression when their interpretations are deemed to exceed the bounds of the Council's actual texts or the continuity reading, bearing costs for advocating further change.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, progressive_critics, payer,
    organized, biographical, constrained, global).

% The general body of Catholic believers who seek clarity, stability, and unity in doctrine. This reading provides a coherent framework for understanding the Council, reassuring them of the Church's unchanging nature amidst change, though some may feel alienated by its enforcement.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, faithful_laity, beneficiary,
    moderate, biographical, constrained, global).

% Scholars who analyze the historical development of doctrine and the Council's context. They provide critical analysis of the continuity claim, often highlighting complexities or tensions that challenge simplistic narratives, but do not directly participate in the Church's governance.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, historical_theologians, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__continuity_reading, magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, authoritative interpretation of Vatican II, preventing doctrinal chaos and maintaining unity within the Catholic Church by framing post-conciliar developments as consistent with unchanging tradition.
% TRANSFER_FUNCTION: Transfers interpretive authority and legitimacy to the Magisterium and those who uphold this reading, while transferring the burden of conformity (or marginalization) to those who perceive rupture or advocate for interpretations outside its bounds.
% ABSENT_VOICES: Those who have left the Church due to perceived rupture (from either traditionalist or progressive perspectives) are absent. They would argue that the continuity reading is a forced narrative that ignores real changes or real errors, or stifles necessary evolution.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the Catholic Church would face a severe crisis of identity and authority. Competing interpretations of Vatican II would likely lead to widespread doctrinal confusion, potential schism, and a fundamental challenge to the Magisterium's role, reorganizing the entire institutional landscape.
% FOUNDING_PROBLEM: The need to reconcile the reforms and apparent novelties of Vatican II with the Church's claim to unchanging doctrinal tradition, preventing both schism and perceived doctrinal relativism in the post-conciliar era.
% FOUNDING_PROBLEM_CORROBORATION: The Magisterium and a significant body of Catholic theologians and faithful attest to the ongoing need for this hermeneutic to maintain unity and doctrinal integrity. Critics, however, argue that the founding problem is either manufactured, or that the solution itself creates new problems by denying legitimate historical development or suppressing valid dissent.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__continuity_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(vatican_ii_doctrinal_authority__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(vatican_ii_doctrinal_authority__continuity_reading),
    narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is 'mountain' because the continuity reading asserts that Vatican II's development is an intrinsic, organic unfolding of unchanging truth, akin to a natural law of doctrinal development. However, the measured extractiveness (0.45) and suppression (0.75) reflect the active institutional effort required to enforce this interpretation and marginalize dissenting views, particularly from traditionalist and progressive critics. The theater ratio (0.40) indicates that while genuine theological work supports this reading, a significant portion of the effort is performative maintenance of the narrative against challenges. The rising suppression over time reflects the increasing need to actively manage dissent as the post-conciliar era progressed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Magisterium and orthodox theologians, the continuity reading is a necessary and natural defense of doctrinal truth, involving minimal 'extraction' from the tradition itself. From the perspective of traditionalist and progressive critics, the same structure operates as a highly extractive and suppressive force, denying legitimate historical change or suppressing necessary reform. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium and orthodox theologians are beneficiaries, as this reading validates their authority and intellectual work. Traditionalist and progressive critics are targets, as their interpretations are actively suppressed or marginalized. The faithful laity are diffuse beneficiaries, gaining doctrinal stability, but also bear indirect costs if they feel alienated by the enforcement of this reading. Historical theologians act as observers, analyzing the claims without direct participation in the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_vs_forced_continuity,
    'Is the continuity between Vatican II and prior tradition a genuine organic development, or is it a forced interpretive construct designed to maintain institutional authority?',
    'Comprehensive historical-theological analysis, including critical examination of primary sources and the reception history of the Council, from perspectives outside the immediate Magisterial influence.',
    'If continuity is largely a forced construct, the constraint''s ''mountain'' claim is a false summit, and its effective extractiveness and suppression are higher than acknowledged, reclassifying it closer to a Snare or Tangled Rope. If genuine, the ''mountain'' claim is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_forced_continuity, conceptual, 'Ambiguity regarding the structural nature of Vatican II''s development.').

omega_variable(
    post_conciliar_excesses_impact,
    'To what extent do ''post-conciliar excesses'' (liturgical abuses, heterodox theological trends) represent a genuine departure from Vatican II''s intent, and how much do they challenge the claim of doctrinal continuity?',
    'Empirical study of liturgical practice and theological publications, coupled with a rigorous textual analysis of the Council documents to discern their precise intent and limits.',
    'If excesses are widespread and directly contradict conciliar intent, it weakens the continuity reading''s ability to frame them as mere ''implementation errors,'' potentially increasing the perceived rupture and the constraint''s internal inconsistency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_conciliar_excesses_impact, empirical, 'Impact of post-conciliar developments on the continuity claim.').

omega_variable(
    suppression_as_truth_defense_or_power_play,
    'Is the suppression of traditionalist and progressive critiques a necessary defense of doctrinal truth and unity, or an extractive power play to maintain the Magisterium''s interpretive monopoly?',
    'Analysis of the content of suppressed critiques: do they genuinely threaten core doctrine, or do they represent legitimate theological inquiry or pastoral concerns? Examination of the process of suppression: is it transparent and just, or arbitrary and punitive?',
    'If suppression is primarily a power play, the constraint''s effective suppression is higher and its coordination function is weaker, pushing it further towards a Snare. If genuinely a defense of truth, the suppression is a cost of maintaining a Rope-like coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_as_truth_defense_or_power_play, preference, 'Nature of suppression: defense of truth vs. power maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__continuity_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2005, 0.39).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1965, 0.3).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1975, 0.35).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1985, 0.4).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2005, 0.43).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2015, 0.44).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1995, 0.73).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2005, 0.74).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Vatican II Doctrinal Authority kernel, focusing on the claim of continuity. It is part of a family of constraints representing different interpretations of the Council's nature and impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
