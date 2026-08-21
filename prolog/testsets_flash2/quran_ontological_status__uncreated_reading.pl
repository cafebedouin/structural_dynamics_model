% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__uncreated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__uncreated_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: quran_ontological_status__uncreated_reading
 *   human_readable: Qur'an as Uncreated Eternal Divine Speech (Traditional Reading)
 *   domain: islamic_theology/philosophy_of_language/political_authority
 *
 * SUMMARY:
 *   This constraint represents the traditional Sunni theological position
 *   that the Qur'an is the uncreated, eternal speech of God (kalām Allāh
 *   qadīm), coeternal with God's essence. This reading posits revelation as
 *   an ontic constraint, a fixed divine fact rather than a created artifact.
 *   It maximizes prophetic authority and privileges literalist hermeneutics.
 *   This is one reading of the 'quran_ontological_status' kernel, distinct
 *   from 'created_reading' and 'state_enforced_creation_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.15).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.25).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, mountain).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "Qur'an as Uncreated Eternal Divine Speech (Traditional Reading)").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "islamic_theology/philosophy_of_language/political_authority").

domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, 'bd6613c8-70fb-4c04-a3bf-cd246cedd0c5').
narrative_ontology:cs_kernel_codification('bd6613c8-70fb-4c04-a3bf-cd246cedd0c5', fixed_text).
narrative_ontology:cs_authority_grounding('bd6613c8-70fb-4c04-a3bf-cd246cedd0c5', lineage).
narrative_ontology:cs_interpretation_layer_present('bd6613c8-70fb-4c04-a3bf-cd246cedd0c5').
narrative_ontology:cs_reading_relation('bd6613c8-70fb-4c04-a3bf-cd246cedd0c5', quran_ontological_status__created_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd6613c8-70fb-4c04-a3bf-cd246cedd0c5', quran_ontological_status__state_enforced_creation_reading, coexists_with).
narrative_ontology:cs_axiom('bd6613c8-70fb-4c04-a3bf-cd246cedd0c5', foundational, quran_coeternal_with_god).
narrative_ontology:cs_axiom_status(quran_coeternal_with_god, holdable).
narrative_ontology:cs_axiom_grounding('bd6613c8-70fb-4c04-a3bf-cd246cedd0c5', quran_coeternal_with_god, theological).
narrative_ontology:cs_axiom('bd6613c8-70fb-4c04-a3bf-cd246cedd0c5', secondary, textual_meaning_immutable).
narrative_ontology:cs_axiom_status(textual_meaning_immutable, holdable).
narrative_ontology:cs_axiom_grounding('bd6613c8-70fb-4c04-a3bf-cd246cedd0c5', textual_meaning_immutable, deontological).
narrative_ontology:cs_reference_frame('bd6613c8-70fb-4c04-a3bf-cd246cedd0c5', classical_sunni_orthodoxy).
narrative_ontology:cs_drift_state('bd6613c8-70fb-4c04-a3bf-cd246cedd0c5', contemporary_islamic_thought, gap(stable, minor, true)).
narrative_ontology:cs_created_at('bd6613c8-70fb-4c04-a3bf-cd246cedd0c5', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_communities).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_schools).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, muslim_laity).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, prophetic_authority_doctrine).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, textual_inerrancy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their authority and interpretive methodology are grounded in the uncreated nature of the Qur'an, which implies a fixed, eternal meaning accessible through traditional methods. This status elevates their role as guardians of divine truth.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, traditional_jurists, beneficiary,
    institutional, generational, identity_locked, global).

% Find certainty and stability in the belief that the Qur'an is the direct, uncreated word of God, leading to a literal interpretation of its commands and narratives. This provides a clear framework for their faith and practice.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, literalist_communities, beneficiary,
    organized, generational, identity_locked, regional).

% Their theological positions are strengthened by the uncreated doctrine, which limits the scope for rational inquiry to reinterpret or contextualize revelation, thus preserving traditional theological frameworks against philosophical challenges.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, anti_rationalist_schools, beneficiary,
    institutional, generational, identity_locked, global).

% Their attempts to reconcile revelation with reason, often by positing the Qur'an as created in time, are marginalized or condemned by this dominant reading. They seek flexibility in interpretation that the uncreated doctrine restricts.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, rational_theologians, excluded,
    moderate, biographical, constrained, global).

% Their hermeneutical approaches, which emphasize allegorical or symbolic meanings, are often viewed with suspicion or outright rejection by proponents of the uncreated doctrine, who prioritize literal meaning as divinely intended.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, metaphorical_interpreters, excluded,
    moderate, biographical, constrained, regional).

% Seek to re-contextualize Islamic law and ethics for modern challenges, often requiring a more flexible understanding of textual authority. The uncreated doctrine presents a significant barrier to such reforms, as it posits an immutable textual meaning.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, reform_movements, excluded,
    organized, generational, constrained, national).

% Adhere to the doctrine as a core tenet of faith, often without deep theological engagement. They bear the cost of limited interpretive flexibility and potential intellectual stagnation, but gain spiritual certainty and communal belonging.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, muslim_laity, payer,
    powerless, biographical, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a singular, immutable source of divine truth, coordinating theological understanding and legal interpretation across diverse Muslim communities by providing a fixed reference point for all religious discourse.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority from human reason or contextual analysis to the fixed, eternal text, thereby concentrating hermeneutical power in those who claim to best understand its immutable meaning.
% ABSENT_VOICES: Rationalist theologians and reform movements, whose interpretive flexibility is curtailed by this doctrine, are often marginalized or silenced in traditional discourse. They would argue for a more dynamic engagement with revelation.
% DISAPPEARANCE_RATIONALE: If the doctrine of the Qur'an's uncreatedness vanished, it would fundamentally alter Islamic theology, law, and political thought. Interpretive authority would decentralize, opening avenues for diverse hermeneutics and potentially leading to significant shifts in religious practice and social norms.
% FOUNDING_PROBLEM: To safeguard the divine origin and authority of the Qur'an against philosophical challenges and human attempts to diminish its status, ensuring its immutability as God's direct speech.
% FOUNDING_PROBLEM_CORROBORATION: Traditional Islamic scholarship, historical theological consensus (after the Mihna), and the ongoing adherence of the vast majority of Sunni Muslims attest to the problem's continued relevance and the doctrine's foundational role in preserving revelation's sanctity. While rationalist schools contested it, their views did not become mainstream.
narrative_ontology:disappearance_verdict(quran_ontological_status__uncreated_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__uncreated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_ontological_status__uncreated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__uncreated_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__uncreated_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, ExtMetricName, E),
    domain_priors:suppression_score(quran_ontological_status__uncreated_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quran_ontological_status__uncreated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Mountain due to its theological assertion of being coeternal with God, implying an unchangeable, natural-law-like status within its theological framework. Extractiveness is low (0.15) because it primarily structures belief and interpretation rather than direct material extraction, though it does concentrate interpretive authority. Suppression is moderate (0.25) as it historically involved theological debates and occasional persecution (e.g., the Mihna, though that was for the *opposite* doctrine), but its persistence is more due to widespread acceptance and theological inertia than active coercion in most periods. Accessibility collapse is high (0.9) because, within this framework, alternatives to its divine, uncreated status are largely foreclosed. Resistance is low (0.1) because it became the dominant, widely accepted view.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of its beneficiaries, this doctrine is a foundational truth, a Mountain of divine reality. From the perspective of excluded rationalist or reformist groups, it functions as a powerful, identity-locking constraint that limits intellectual freedom and interpretive innovation, though they may still accept its divine origin.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional jurists, literalist communities, and anti-rationalist schools are beneficiaries, as their authority and interpretive methods are validated and strengthened by this doctrine. Rational theologians, metaphorical interpreters, and reform movements are excluded or victims, as their approaches are marginalized or constrained by the doctrine's emphasis on fixed meaning. The Muslim laity are payers, bearing the cost of limited interpretive flexibility but gaining spiritual certainty and communal belonging.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine's mandate to safeguard divine authority remains live within its theological framework. Its persistence is not due to atrophy but to its continued function as a foundational theological principle. The classification as a Mountain reflects its claimed ontological status, while the low extractiveness and suppression indicate it's not primarily an extractive mechanism, but a structuring principle of belief.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_status_ambiguity,
    'Is the Qur''an''s uncreated status an ontological fact (Mountain) or a theological construct that benefits specific interpretive communities (Tangled Rope/Snare)?',
    'Analysis of historical theological debates and the political contexts in which this doctrine gained dominance, particularly the Mihna (inquisition) period, to discern whether its acceptance was purely theological or also driven by power dynamics.',
    'If primarily a theological construct, its classification would shift from Mountain to a more extractive type (e.g., Tangled Rope or Snare), reflecting the power dynamics involved in its establishment and maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_status_ambiguity, conceptual, 'Ambiguity between claimed ontological status and potential socio-political construction.').

omega_variable(
    interpretive_flexibility_cost,
    'What is the true cost of reduced interpretive flexibility imposed by the uncreated doctrine on modern Islamic thought and reform efforts?',
    'Comparative analysis of Islamic legal and ethical developments in contexts where the created doctrine (or a more flexible hermeneutic) has historically held sway, versus those dominated by the uncreated doctrine.',
    'A high cost would increase the measured extractiveness and suppression, particularly for reform movements and rational theologians, potentially shifting the constraint towards a Snare from their perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_flexibility_cost, empirical, 'Quantifying the impact of interpretive rigidity on intellectual and social development.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__uncreated_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qura_tr_t300, quran_ontological_status__uncreated_reading, theater_ratio, 300, 0.05).
narrative_ontology:measurement(qura_tr_t600, quran_ontological_status__uncreated_reading, theater_ratio, 600, 0.05).
narrative_ontology:measurement(qura_tr_t900, quran_ontological_status__uncreated_reading, theater_ratio, 900, 0.05).
narrative_ontology:measurement(qura_tr_t1200, quran_ontological_status__uncreated_reading, theater_ratio, 1200, 0.05).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__uncreated_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(qura_be_t300, quran_ontological_status__uncreated_reading, base_extractiveness, 300, 0.12).
narrative_ontology:measurement(qura_be_t600, quran_ontological_status__uncreated_reading, base_extractiveness, 600, 0.13).
narrative_ontology:measurement(qura_be_t900, quran_ontological_status__uncreated_reading, base_extractiveness, 900, 0.14).
narrative_ontology:measurement(qura_be_t1200, quran_ontological_status__uncreated_reading, base_extractiveness, 1200, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__uncreated_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(qura_su_t300, quran_ontological_status__uncreated_reading, suppression_requirement, 300, 0.22).
narrative_ontology:measurement(qura_su_t600, quran_ontological_status__uncreated_reading, suppression_requirement, 600, 0.23).
narrative_ontology:measurement(qura_su_t900, quran_ontological_status__uncreated_reading, suppression_requirement, 900, 0.24).
narrative_ontology:measurement(qura_su_t1200, quran_ontological_status__uncreated_reading, suppression_requirement, 1200, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, islamic_legal_hermeneutics).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, prophetic_sunna_authority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
