% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__unitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__unitarian_reading, []).

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
 *   constraint_id: biblical_divine_nature__unitarian_reading
 *   human_readable: Numerical Singularity of God (Unitarian Reading)
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   This constraint represents the Unitarian reading of divine nature,
 *   asserting the numerical singularity of God, with the Father alone as God,
 *   and the Son/Spirit subordinate or created. From the perspective of its
 *   adherents, this is a fundamental theological truth (claimed as a
 *   Mountain). However, its existence and propagation impose significant
 *   costs on the established Trinitarian institutional hierarchy and credal
 *   orthodoxy (high extractiveness from these 'victim' groups). The Unitarian
 *   reading itself does not actively suppress other views (low suppression),
 *   but rather faces historical suppression from dominant Trinitarianism. The
 *   high accessibility_collapse reflects the conviction of its adherents that
 *   alternatives are theologically untenable once this truth is understood.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, 0.85).
domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, 0.15).
domain_priors:theater_ratio(biblical_divine_nature__unitarian_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, mountain).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Numerical Singularity of God (Unitarian Reading)").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theology/religious_authority/doctrinal_history").

domain_priors:emerges_naturally(biblical_divine_nature__unitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, 'a95c9c69-ca5f-4306-8f36-073405d0efea').
narrative_ontology:cs_kernel_codification('a95c9c69-ca5f-4306-8f36-073405d0efea', fixed_text).
narrative_ontology:cs_authority_grounding('a95c9c69-ca5f-4306-8f36-073405d0efea', lineage).
narrative_ontology:cs_interpretation_layer_present('a95c9c69-ca5f-4306-8f36-073405d0efea').
narrative_ontology:cs_reading_relation('a95c9c69-ca5f-4306-8f36-073405d0efea', biblical_divine_nature__trinitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('a95c9c69-ca5f-4306-8f36-073405d0efea', biblical_divine_nature__modalist_reading, coexists_with).
narrative_ontology:cs_axiom('a95c9c69-ca5f-4306-8f36-073405d0efea', foundational, numerical_singularity_of_god).
narrative_ontology:cs_axiom_status(numerical_singularity_of_god, holdable).
narrative_ontology:cs_axiom_grounding('a95c9c69-ca5f-4306-8f36-073405d0efea', numerical_singularity_of_god, theological).
narrative_ontology:cs_axiom('a95c9c69-ca5f-4306-8f36-073405d0efea', foundational, subordination_of_son_and_spirit).
narrative_ontology:cs_axiom_status(subordination_of_son_and_spirit, holdable).
narrative_ontology:cs_axiom_grounding('a95c9c69-ca5f-4306-8f36-073405d0efea', subordination_of_son_and_spirit, theological).
narrative_ontology:cs_reference_frame('a95c9c69-ca5f-4306-8f36-073405d0efea', early_christian_monotheism).
narrative_ontology:cs_drift_state('a95c9c69-ca5f-4306-8f36-073405d0efea', post_nicene_creedalization, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a95c9c69-ca5f-4306-8f36-073405d0efea', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, unitarian_adherents).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, trinitarian_institutional_hierarchy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, credal_orthodoxy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adherents who find theological clarity and consistency in the belief that God is numerically singular, with the Father alone as God. Their identity is deeply tied to this understanding, making exit from the belief system highly costly.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, unitarian_adherents, beneficiary,
    moderate, generational, identity_locked, global).

% The established religious institutions and leadership that uphold Trinitarian doctrine. The Unitarian reading challenges their foundational theological claims and institutional legitimacy, imposing a significant cost in terms of doctrinal defense and potential loss of authority if the Unitarian view were to gain widespread acceptance.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, trinitarian_institutional_hierarchy, payer,
    institutional, civilizational, constrained, global).

% The collective body of established theological beliefs and confessions that define Trinitarian Christianity. The Unitarian reading directly contradicts these core tenets, requiring constant intellectual and theological defense from those committed to orthodoxy.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, credal_orthodoxy, payer,
    organized, generational, constrained, global).

% Academics and researchers who study the historical and systematic theology of divine nature. They analyze the arguments for and against the Unitarian position, contributing to the ongoing intellectual contestation without necessarily being adherents or institutional actors.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, theologians_and_scholars, observer,
    analytical, biographical, analytical, global).

% Adherents of Modalism, who believe God is one person manifesting in different modes (Father, Son, Holy Spirit) sequentially. While also non-Trinitarian, their specific theological framework is distinct from the Unitarian reading and is excluded from its internal coherence.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, modalist_adherents, excluded,
    moderate, generational, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__unitarian_reading, unitarian_adherents).
narrative_ontology:fixing_cost_class(biblical_divine_nature__unitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unambiguous, and philosophically consistent understanding of God's nature, avoiding perceived complexities or contradictions inherent in Trinitarian or Modalist doctrines for its adherents.
% TRANSFER_FUNCTION: Transfers ultimate theological authority and divine status from a complex, multi-personal doctrine to a singular, unified understanding of God, thereby challenging the institutional power and credal structures built upon Trinitarianism.
% ABSENT_VOICES: Trinitarian and Modalist theologians and adherents are structurally excluded from the internal coherence and foundational premises of the Unitarian reading. They would object to its core claims but are not part of the Unitarian framework's self-justification.
% DISAPPEARANCE_RATIONALE: If the Unitarian reading (as a theological claim and movement) vanished, the theological landscape would significantly rearrange. The dominant Trinitarian narrative would lose a persistent, fundamental challenge, potentially solidifying its perceived naturalness and reducing internal theological pressure for alternative interpretations of divine nature.
% FOUNDING_PROBLEM: Theological inconsistencies, philosophical difficulties, and perceived lack of explicit biblical support for Trinitarian doctrines, particularly regarding the co-equality and co-eternity of the Son and Holy Spirit with the Father.
% FOUNDING_PROBLEM_CORROBORATION: Unitarian scholars and historical critics continue to attest to the live nature of these theological problems, drawing on biblical exegesis and philosophical arguments. However, this corroboration is not generally accepted by Trinitarian or Modalist institutions, which maintain that their doctrines are biblically sound and philosophically coherent.
narrative_ontology:disappearance_verdict(biblical_divine_nature__unitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__unitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__unitarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(biblical_divine_nature__unitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__unitarian_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__unitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, ExtMetricName, E),
    domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(biblical_divine_nature__unitarian_reading),
    narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(biblical_divine_nature__unitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the profound challenge and potential dismantling effect this reading has on the established Trinitarian theological and institutional order, which relies on a different understanding of divine nature. The low suppression (0.15) indicates that the Unitarian reading, as a constraint, does not primarily rely on coercive enforcement to maintain itself, but rather on theological conviction and argument; historically, Unitarianism has been the suppressed, not the suppressor. The low theater_ratio (0.05) underscores its nature as a core theological claim, not a performative or inertial structure. The high accessibility_collapse (0.9) reflects the internal coherence and perceived self-evidence of the claim for its adherents, making alternative theological frameworks seem untenable from their perspective.
 *
 * PERSPECTIVAL GAP:
 *   From the Unitarian adherent's perspective, this is a self-evident truth about God (Mountain). From the Trinitarian institutional hierarchy's perspective, it is a disruptive, heretical challenge that extracts legitimacy and resources (Snare-like impact). The engine's classification will capture this divergence by evaluating the Mountain claim against the high extractiveness and the declared victim set.
 *
 * DIRECTIONALITY LOGIC:
 *   Unitarian adherents are beneficiaries, gaining theological clarity and a framework for their faith (low d). The Trinitarian institutional hierarchy and credal orthodoxy are targets/payers, as the Unitarian reading directly challenges their foundational claims and legitimacy, imposing costs of defense and potential erosion of authority (high d). Theologians and scholars are observers, analyzing the debate. Modalist adherents are excluded, as their distinct non-Trinitarian view is not encompassed by this specific Unitarian framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a fundamental truth about God''s nature, or one specific interpretation among several equally valid readings of biblical texts?',
    'Theological consensus across diverse traditions, or a definitive, universally accepted hermeneutical principle for interpreting divine nature in scripture.',
    'If it is merely one interpretation, its ''emerges_naturally'' claim is weakened, and its classification shifts from Mountain to a more constructed type (e.g., Tangled Rope or Snare, depending on its enforcement). If it is a fundamental truth, its Mountain classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity between a theological truth claim and a specific interpretive reading.').

omega_variable(
    natural_law_vs_constructed_theology,
    'Is the numerical singularity of God a ''natural law'' of theology (a self-evident truth about divine being), or a constructed theological doctrine derived from specific interpretive choices?',
    'Philosophical analysis of the concept of ''natural theology'' and its application to divine attributes, alongside comparative theological studies of how different traditions arrive at their understanding of God''s unity.',
    'If it is a constructed doctrine, the ''emerges_naturally: true'' flag is challenged, potentially reclassifying this Mountain as a False Summit (e.g., Tangled Rope) due to the presence of beneficiaries and victims. If it is genuinely a natural theological law, the Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_theology, conceptual, 'Ambiguity regarding the naturalness of the Unitarian theological claim.').

omega_variable(
    victim_status_of_hierarchy,
    'Is the Trinitarian institutional hierarchy truly a ''victim'' of the Unitarian reading, or merely an opponent in a theological debate?',
    'Analysis of the material and social costs imposed on the hierarchy by the Unitarian challenge (e.g., loss of adherents, need for costly doctrinal defense, erosion of public legitimacy) versus the costs of mere intellectual disagreement.',
    'If the costs are primarily intellectual and not materially or institutionally disruptive, the ''victim'' status is weakened, potentially reducing the measured extractiveness. If the costs are substantial and threaten institutional stability, the victim status and high extractiveness are reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_status_of_hierarchy, empirical, 'Whether the Trinitarian hierarchy experiences genuine extraction from the Unitarian challenge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__unitarian_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bibl_tr_t500, biblical_divine_nature__unitarian_reading, theater_ratio, 500, 0.05).
narrative_ontology:measurement(bibl_tr_t1000, biblical_divine_nature__unitarian_reading, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(bibl_tr_t1500, biblical_divine_nature__unitarian_reading, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(bibl_tr_t2000, biblical_divine_nature__unitarian_reading, theater_ratio, 2000, 0.05).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__unitarian_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(bibl_be_t500, biblical_divine_nature__unitarian_reading, base_extractiveness, 500, 0.8).
narrative_ontology:measurement(bibl_be_t1000, biblical_divine_nature__unitarian_reading, base_extractiveness, 1000, 0.82).
narrative_ontology:measurement(bibl_be_t1500, biblical_divine_nature__unitarian_reading, base_extractiveness, 1500, 0.84).
narrative_ontology:measurement(bibl_be_t2000, biblical_divine_nature__unitarian_reading, base_extractiveness, 2000, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__unitarian_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(bibl_su_t500, biblical_divine_nature__unitarian_reading, suppression_requirement, 500, 0.12).
narrative_ontology:measurement(bibl_su_t1000, biblical_divine_nature__unitarian_reading, suppression_requirement, 1000, 0.13).
narrative_ontology:measurement(bibl_su_t1500, biblical_divine_nature__unitarian_reading, suppression_requirement, 1500, 0.14).
narrative_ontology:measurement(bibl_su_t2000, biblical_divine_nature__unitarian_reading, suppression_requirement, 2000, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__unitarian_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'biblical_divine_nature' kernel, alongside 'trinitarian_reading' and 'modalist_reading'. Each reading represents a distinct constraint with its own structural properties and impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
