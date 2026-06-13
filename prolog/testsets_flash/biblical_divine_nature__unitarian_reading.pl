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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: biblical_divine_nature__unitarian_reading
 *   human_readable: Biblical Divine Nature (Unitarian Reading)
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   This constraint represents the Unitarian reading of biblical divine
 *   nature, asserting the numerical singularity of God, with the Father alone
 *   as supreme and the Son/Spirit as subordinate or created. It is a
 *   counter-orthodoxy that challenges the dominant Trinitarian view. The
 *   constraint's 'snare' classification reflects its extractive nature on
 *   established trinitarian institutions and credal orthodoxy, which must
 *   expend significant resources to defend against its claims, while offering
 *   a distinct identity and theological clarity to its adherents. The low
 *   institutional authority and flat ecclesiology of unitarianism are a
 *   direct structural delta from trinitarianism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, 0.6).
domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, 0.7).
domain_priors:theater_ratio(biblical_divine_nature__unitarian_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, snare).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Biblical Divine Nature (Unitarian Reading)").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__unitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, '3919e98e-b8d8-403d-b511-7f57b878467b').
narrative_ontology:cs_kernel_codification('3919e98e-b8d8-403d-b511-7f57b878467b', fixed_text).
narrative_ontology:cs_authority_grounding('3919e98e-b8d8-403d-b511-7f57b878467b', distributed).
narrative_ontology:cs_reading_relation('3919e98e-b8d8-403d-b511-7f57b878467b', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('3919e98e-b8d8-403d-b511-7f57b878467b', biblical_divine_nature__modalist_reading, forecloses).
narrative_ontology:cs_axiom('3919e98e-b8d8-403d-b511-7f57b878467b', foundational, god_is_numerically_one_person).
narrative_ontology:cs_axiom_status(god_is_numerically_one_person, holdable).
narrative_ontology:cs_axiom_grounding('3919e98e-b8d8-403d-b511-7f57b878467b', god_is_numerically_one_person, theological).
narrative_ontology:cs_axiom('3919e98e-b8d8-403d-b511-7f57b878467b', foundational, son_and_spirit_are_subordinate_or_created).
narrative_ontology:cs_axiom_status(son_and_spirit_are_subordinate_or_created, holdable).
narrative_ontology:cs_axiom_grounding('3919e98e-b8d8-403d-b511-7f57b878467b', son_and_spirit_are_subordinate_or_created, theological).
narrative_ontology:cs_reference_frame('3919e98e-b8d8-403d-b511-7f57b878467b', early_christian_monotheism).
narrative_ontology:cs_drift_state('3919e98e-b8d8-403d-b511-7f57b878467b', post_nicene_creed_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3919e98e-b8d8-403d-b511-7f57b878467b', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, unitarian_theologians).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, unitarian_congregations).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, trinitarian_orthodoxy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, institutional_hierarchy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, credal_orthodoxy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, individual_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret scripture to assert the numerical singularity of God, with the Father as the sole supreme deity. They actively promote this understanding and challenge trinitarian formulations, often facing academic and ecclesiastical marginalization from mainstream institutions. Their careers and intellectual identity are tied to defending this theological position.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, unitarian_theologians, agenda_setter,
    organized, generational, constrained, global).

% Adhere to the unitarian understanding of God, finding theological clarity and a sense of distinct identity in rejecting trinitarian dogma. They benefit from a simpler theological framework but may face social and institutional isolation from broader Christian communities.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, unitarian_congregations, beneficiary,
    moderate, biographical, mobile, local).

% Represents the dominant theological tradition that asserts God as three persons in one essence. This reading directly challenges their foundational credal statements and institutional authority, forcing them to expend resources on apologetics, doctrinal enforcement, and maintaining boundaries against unitarian interpretations. Their identity is fused with the trinitarian formulation.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, trinitarian_orthodoxy, payer,
    institutional, civilizational, identity_locked, global).

% The established ecclesiastical structures (e.g., councils, synods, denominational leadership) that have historically codified and enforced trinitarian doctrine. The unitarian reading undermines their authority and the theological consensus they maintain, requiring active suppression of dissenting views to preserve institutional stability and legitimacy.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, institutional_hierarchy, payer,
    institutional, generational, constrained, global).

% The body of historical creeds (e.g., Nicene Creed) that define trinitarian doctrine. The unitarian reading directly contradicts these foundational texts, challenging their authority and requiring constant defense by those who uphold them. It is a non-agent entity, but its 'situation' is that it is the object of contestation.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, credal_orthodoxy, payer,
    institutional, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(biblical_divine_nature__unitarian_reading, credal_orthodoxy).

% Those who are part of mainstream trinitarian churches but encounter unitarian arguments. They may experience cognitive dissonance or be pressured to conform to established doctrine, facing social costs if they adopt unitarian views. Their intellectual and spiritual freedom is constrained by the dominant theological framework.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, individual_believers, payer,
    powerless, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unambiguous understanding of God's nature, simplifying theological discourse and worship practices for adherents by asserting a single, supreme divine person.
% TRANSFER_FUNCTION: Transfers theological authority and interpretive power from established trinitarian hierarchies to individual scriptural interpretation and unitarian theological traditions. It also transfers social and institutional costs to those who uphold trinitarian orthodoxy.
% ABSENT_VOICES: Early Church Fathers who codified trinitarian doctrine would object vehemently, arguing that unitarianism undermines the full divinity of Christ and the Holy Spirit, leading to a diminished understanding of salvation. They are absent from contemporary discourse but their historical arguments are central to the debate.
% DISAPPEARANCE_RATIONALE: If the unitarian reading vanished, the theological landscape would shift dramatically. Trinitarian orthodoxy would face less challenge, and the specific communities and intellectual traditions built around unitarianism would lose their foundational identity, leading to a reorganization of theological debates and religious affiliations.
% FOUNDING_PROBLEM: The perceived complexity and philosophical inconsistencies of trinitarian doctrine, and the desire for a simpler, more biblically direct understanding of God's singular nature.
% FOUNDING_PROBLEM_CORROBORATION: Unitarian theologians and congregations attest the problem is live, citing ongoing debates within theology. Mainstream trinitarian institutions acknowledge the historical challenge but assert the problem is resolved by established doctrine; independent historical and theological scholars corroborate the ongoing nature of the debate, noting that the 'problem' persists for those who find trinitarianism problematic.
narrative_ontology:disappearance_verdict(biblical_divine_nature__unitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__unitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__unitarian_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(biblical_divine_nature__unitarian_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__unitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__unitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.6) because the unitarian reading forces established trinitarian institutions to continuously defend their foundational doctrines, diverting resources and intellectual effort. Suppression is high (0.7) because trinitarian institutions actively suppress unitarian views through excommunication, marginalization, and doctrinal enforcement to maintain their authority. Resistance is high (0.75) as unitarianism itself is a form of resistance against trinitarian orthodoxy. Theater ratio is low (0.2) as the theological debate is genuine and consequential, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of unitarian adherents, this reading is a 'rope' or even a 'mountain' of scriptural truth, offering liberation from perceived theological error. From the perspective of trinitarian institutions, it is a 'snare' that undermines their authority and the stability of their doctrine, requiring active suppression. The engine's classification will reflect the latter due to the declared victims and high suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Unitarian theologians and congregations are beneficiaries, gaining theological clarity and a distinct identity. Trinitarian orthodoxy, institutional hierarchies, and credal orthodoxy are victims, as their foundational claims and authority are directly challenged and undermined, requiring active defense. Individual believers may be victims if they face pressure to conform to trinitarian views despite unitarian leanings.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scriptural_interpretation_ambiguity,
    'Is the unitarian reading of scripture the most direct and unambiguous, or does it require selective interpretation that overlooks trinitarian implications?',
    'Comprehensive, non-confessional linguistic and historical analysis of all relevant biblical texts, assessing the interpretive moves required by each reading.',
    'If the unitarian reading requires significant interpretive work to maintain its claims, its ''naturalness'' as a scriptural reading is reduced, potentially increasing its perceived extractiveness on alternative interpretations. If it is demonstrably more direct, it strengthens its claim to being a ''mountain'' of biblical truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_interpretation_ambiguity, empirical, 'Ambiguity in scriptural interpretation regarding divine singularity.').

omega_variable(
    institutional_vs_theological_extraction,
    'To what extent is the ''extraction'' on trinitarian orthodoxy a genuine theological challenge versus a threat to institutional power and control?',
    'Analysis of historical responses to unitarianism: if suppression primarily targeted theological arguments rather than institutional structures or financial assets, it''s more theological. If it targeted institutional control, it''s more about power.',
    'If primarily institutional, the constraint''s extractiveness is more clearly a ''snare'' on power structures. If primarily theological, it''s a ''tangled_rope'' where genuine doctrinal coordination is intertwined with the cost of maintaining a specific theological consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_theological_extraction, conceptual, 'Distinguishing theological challenge from institutional power threat.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (excommunication, marginalization) or internalized (self-censorship, fear of social costs)?',
    'Post-exit suppression trajectory: if individuals continue to self-censor or avoid unitarian views even after leaving trinitarian institutions, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the ''snare'' more pervasive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in theological adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 1500, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1500, biblical_divine_nature__unitarian_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(bibl_tr_t1600, biblical_divine_nature__unitarian_reading, theater_ratio, 1600, 0.12).
narrative_ontology:measurement(bibl_tr_t1700, biblical_divine_nature__unitarian_reading, theater_ratio, 1700, 0.15).
narrative_ontology:measurement(bibl_tr_t1800, biblical_divine_nature__unitarian_reading, theater_ratio, 1800, 0.18).
narrative_ontology:measurement(bibl_tr_t1900, biblical_divine_nature__unitarian_reading, theater_ratio, 1900, 0.19).
narrative_ontology:measurement(bibl_tr_t2024, biblical_divine_nature__unitarian_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1500, biblical_divine_nature__unitarian_reading, base_extractiveness, 1500, 0.4).
narrative_ontology:measurement(bibl_be_t1600, biblical_divine_nature__unitarian_reading, base_extractiveness, 1600, 0.45).
narrative_ontology:measurement(bibl_be_t1700, biblical_divine_nature__unitarian_reading, base_extractiveness, 1700, 0.5).
narrative_ontology:measurement(bibl_be_t1800, biblical_divine_nature__unitarian_reading, base_extractiveness, 1800, 0.55).
narrative_ontology:measurement(bibl_be_t1900, biblical_divine_nature__unitarian_reading, base_extractiveness, 1900, 0.58).
narrative_ontology:measurement(bibl_be_t2024, biblical_divine_nature__unitarian_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t1500, biblical_divine_nature__unitarian_reading, suppression_requirement, 1500, 0.8).
narrative_ontology:measurement(bibl_su_t1600, biblical_divine_nature__unitarian_reading, suppression_requirement, 1600, 0.75).
narrative_ontology:measurement(bibl_su_t1700, biblical_divine_nature__unitarian_reading, suppression_requirement, 1700, 0.7).
narrative_ontology:measurement(bibl_su_t1800, biblical_divine_nature__unitarian_reading, suppression_requirement, 1800, 0.65).
narrative_ontology:measurement(bibl_su_t1900, biblical_divine_nature__unitarian_reading, suppression_requirement, 1900, 0.68).
narrative_ontology:measurement(bibl_su_t2024, biblical_divine_nature__unitarian_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__unitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__unitarian_reading, 0.08).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__modalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'biblical_divine_nature' kernel. Its claims directly challenge and influence the 'trinitarian_reading' and 'modalist_reading' by offering an alternative theological framework for understanding God's nature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
