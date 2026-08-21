% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__diasporist_reading, []).

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
 *   constraint_id: jewish_self_determination__diasporist_reading
 *   human_readable: Diasporist Ideal of Jewish Self-Determination
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'diasporist reading' of Jewish
 *   self-determination. From this perspective, Jewish collective survival and
 *   flourishing are best secured through diaspora pluralism and minority
 *   rights, not territorial sovereignty. Zionism is viewed as a dangerous
 *   deviation that ties Jewish fate to a militarized state, thereby
 *   undermining the diasporist ideal. The constraint itself is the diasporist
 *   ideal/framework, which has become a Piton because its active function in
 *   shaping collective Jewish life has atrophied due to the hegemony of
 *   Zionist narratives and institutions.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities_adhering_to_pluralism: Beneficiary (moderate/constrained)
 *   - zionist_institutions: Agenda_setter (institutional/arbitrage)
 *   - jews_coerced_into_zionism: Payer (powerless/identity_locked)
 *   - jews_endangered_by_israel_actions: Payer (powerless/trapped)
 *   - anti_zionist_jewish_activists: Agenda_setter (moderate/constrained)
 *   - host_nations: Observer (institutional/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.15).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.25).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Diasporist Ideal of Jewish Self-Determination").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, '9142f5fc-0c0a-4280-9ff7-8db211d68c08').
narrative_ontology:cs_kernel_codification('9142f5fc-0c0a-4280-9ff7-8db211d68c08', distributed).
narrative_ontology:cs_authority_grounding('9142f5fc-0c0a-4280-9ff7-8db211d68c08', practice).
narrative_ontology:cs_interpretation_layer_present('9142f5fc-0c0a-4280-9ff7-8db211d68c08').
narrative_ontology:cs_reading_relation('9142f5fc-0c0a-4280-9ff7-8db211d68c08', jewish_self_determination__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('9142f5fc-0c0a-4280-9ff7-8db211d68c08', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('9142f5fc-0c0a-4280-9ff7-8db211d68c08', jewish_self_determination__religious_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('9142f5fc-0c0a-4280-9ff7-8db211d68c08', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('9142f5fc-0c0a-4280-9ff7-8db211d68c08', foundational, diaspora_as_normative_condition).
narrative_ontology:cs_axiom_status(diaspora_as_normative_condition, holdable).
narrative_ontology:cs_axiom_grounding('9142f5fc-0c0a-4280-9ff7-8db211d68c08', diaspora_as_normative_condition, deontological).
narrative_ontology:cs_axiom('9142f5fc-0c0a-4280-9ff7-8db211d68c08', foundational, pluralism_and_minority_rights_as_security).
narrative_ontology:cs_axiom_status(pluralism_and_minority_rights_as_security, holdable).
narrative_ontology:cs_axiom_grounding('9142f5fc-0c0a-4280-9ff7-8db211d68c08', pluralism_and_minority_rights_as_security, instrumental).
narrative_ontology:cs_reference_frame('9142f5fc-0c0a-4280-9ff7-8db211d68c08', historical_diaspora_pluralism).
narrative_ontology:cs_drift_state('9142f5fc-0c0a-4280-9ff7-8db211d68c08', contemporary_zionist_hegemony, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9142f5fc-0c0a-4280-9ff7-8db211d68c08', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_jewish_communities_adhering_to_pluralism).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_coerced_into_zionism).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_endangered_by_israel_actions).
narrative_ontology:constraint_vindicates(jewish_self_determination__diasporist_reading, minority_rights_doctrine).
narrative_ontology:constraint_vindicates(jewish_self_determination__diasporist_reading, anti_colonial_critique).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities and individuals benefit from the diasporist ideal, which posits their flourishing through pluralism and minority rights. They strive to maintain distinct identities and advocate for these principles, but face challenges from dominant narratives that undermine their position.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jewish_communities_adhering_to_pluralism, beneficiary,
    moderate, generational, constrained, global).

% These institutions actively promote and enforce a Zionist framework for Jewish self-determination, often defining Jewish interests in opposition to diasporist pluralism. Their hegemony weakens the diasporist ideal, making it more performative than functional.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, zionist_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Individuals who are pressured to align with Zionist narratives, suppressing internal dissent or alternative visions for Jewish life. They pay the cost of the diasporist ideal's atrophy through diminished pluralism and constrained identity.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_coerced_into_zionism, payer,
    powerless, biographical, identity_locked, global).

% Jews who face increased antisemitism or security threats due to the conflation of Jewish identity with the actions of the Israeli state. The diasporist ideal seeks to prevent this conflation, and its atrophy leaves these individuals vulnerable.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_endangered_by_israel_actions, payer,
    powerless, immediate, trapped, global).

% These activists actively resist Zionist hegemony and advocate for the revival and strengthening of the diasporist ideal. They often face marginalization and exclusion from mainstream Jewish communal life for their stance.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, anti_zionist_jewish_activists, agenda_setter,
    moderate, biographical, constrained, global).

% The policies and legal frameworks of host nations regarding minority rights and definitions of antisemitism can either support the flourishing of diasporist communities or inadvertently reinforce Zionist hegemony, thereby impacting the viability of the diasporist ideal.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, host_nations, observer,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__diasporist_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_self_determination__diasporist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically, the diasporist ideal coordinated Jewish communities globally around shared cultural, religious, and ethical principles, enabling survival and flourishing as minorities in diverse societies, fostering internal pluralism and external engagement.
% TRANSFER_FUNCTION: It aims to transfer agency and security from reliance on a single territorial state to a distributed network of self-governing diaspora communities and international minority rights frameworks, emphasizing ethical conduct and cultural continuity over political sovereignty.
% ABSENT_VOICES: Voices of those who have been marginalized or silenced within Jewish communities for advocating non-Zionist or anti-Zionist perspectives; also, indigenous voices whose land claims are central to the critique of territorial nationalism, which the diasporist ideal implicitly supports.
% DISAPPEARANCE_RATIONALE: If the diasporist ideal vanished, Jewish collective identity would be almost entirely subsumed by territorial nationalism, losing its historical pluralism and potentially increasing vulnerability by tying its fate to a single state, leading to a significant reorganization of Jewish political and cultural life.
% FOUNDING_PROBLEM: The historical problem of Jewish insecurity, persecution, and the need for a framework for collective survival and flourishing in a world without a sovereign Jewish state, emphasizing ethical and cultural continuity.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of Jewish communal life in diaspora, philosophical and theological texts advocating for pluralism, and contemporary anti-Zionist Jewish scholarship corroborate the ongoing relevance of the problem, even as the diasporist solution has atrophied. Zionist institutions, however, contest the status of the problem as 'live' for diasporism, asserting their solution has superseded it.
narrative_ontology:disappearance_verdict(jewish_self_determination__diasporist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__diasporist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__diasporist_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__diasporist_reading_tests).
:- end_tests(jewish_self_determination__diasporist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The diasporist ideal, as a Piton, exhibits low extractiveness (0.15) and suppression (0.25) because the ideal itself is not coercive; rather, its *failure* to be realized imposes costs. The high theater ratio (0.75) reflects that the ideal, while still articulated and aspired to by many, is largely performative or aspirational, not actively shaping collective action as it once might have, due to the dominance of Zionist frameworks. The rising theater ratio and suppression over the interval reflect the increasing marginalization of diasporist alternatives since the early 20th century.
 *
 * PERSPECTIVAL GAP:
 *   Adherents of the diasporist ideal (beneficiaries and anti-Zionist activists) experience it as a vital, if currently undermined, path to Jewish flourishing. Those coerced into Zionism or endangered by its actions experience the atrophy of the diasporist ideal as a cost. Zionist institutions, conversely, view their framework as the legitimate and successful path, often dismissing diasporist critiques as irrelevant or dangerous.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jewish communities adhering to pluralism are beneficiaries of the diasporist ideal, even in its atrophied state, as it provides a framework for their identity and security. Jews coerced into Zionism or endangered by Israeli actions are payers, bearing the costs of the diasporist ideal's decline. Zionist institutions act as agenda-setters, actively undermining the diasporist ideal through their hegemonic influence.
 *
 * MANDATROPHY ANALYSIS:
 *   The diasporist ideal's mandate (securing Jewish survival through pluralism) is still live, but its function has atrophied. The constraint is a Piton because the active, functional aspects of diasporist self-determination have been largely replaced by performative adherence or aspirational advocacy, while Zionist institutions have captured the 'Jewish interest' narrative. This prevents mislabeling it as a Snare (which would imply active extraction *by* the diasporist ideal) or a Rope (which would imply active, functional coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zionist_hegemony_impact,
    'Is the atrophy of the diasporist ideal primarily due to the institutional power and narrative hegemony of Zionist organizations, or to internal communal choices and historical circumstances independent of Zionist influence?',
    'Comparative historical analysis of Jewish communities with varying degrees of exposure to Zionist institutional pressure, examining the persistence and vitality of diasporist practices.',
    'If primarily due to Zionist hegemony, the constraint''s suppression and theater_ratio are more directly attributable to external, extractive forces. If internal factors are dominant, the Piton classification is more robust, reflecting genuine atrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zionist_hegemony_impact, empirical, 'Assessing the primary drivers of the diasporist ideal''s atrophy.').

omega_variable(
    diaspora_resilience_and_revival,
    'To what extent do contemporary non-Zionist and anti-Zionist Jewish movements represent a genuine revival of the diasporist ideal, capable of shifting its status from Piton to a more active form of coordination?',
    'Longitudinal study of the growth, institutionalization, and political impact of these movements, measuring their capacity to shape collective Jewish identity and action.',
    'If these movements achieve significant functional impact, the constraint''s theater_ratio would decrease, and its claimed_type might shift towards a Rope or even a Tangled Rope (if new forms of internal extraction emerge).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diaspora_resilience_and_revival, empirical, 'Potential for revival of the diasporist ideal.').

omega_variable(
    conflation_of_identity_and_security,
    'Is the conflation of Jewish identity with Israeli state actions an inherent risk of territorial nationalism, or a contingent outcome of specific political choices that could be mitigated without abandoning territorial sovereignty?',
    'Comparative political science research on other national liberation movements and their relationship with diaspora communities, examining how identity and state actions are managed.',
    'If inherent, it strengthens the diasporist critique of territorial sovereignty as a dangerous deviation. If contingent, it suggests that a ''safer'' form of Jewish nationalism might be possible, altering the perceived ''danger'' of Zionism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conflation_of_identity_and_security, conceptual, 'Nature of the link between Jewish identity, Israeli state actions, and diaspora security.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1900, jewish_self_determination__diasporist_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(jewi_tr_t1925, jewish_self_determination__diasporist_reading, theater_ratio, 1925, 0.35).
narrative_ontology:measurement(jewi_tr_t1950, jewish_self_determination__diasporist_reading, theater_ratio, 1950, 0.5).
narrative_ontology:measurement(jewi_tr_t1975, jewish_self_determination__diasporist_reading, theater_ratio, 1975, 0.6).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__diasporist_reading, theater_ratio, 2000, 0.7).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__diasporist_reading, theater_ratio, 2024, 0.75).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1900, jewish_self_determination__diasporist_reading, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(jewi_be_t1925, jewish_self_determination__diasporist_reading, base_extractiveness, 1925, 0.08).
narrative_ontology:measurement(jewi_be_t1950, jewish_self_determination__diasporist_reading, base_extractiveness, 1950, 0.1).
narrative_ontology:measurement(jewi_be_t1975, jewish_self_determination__diasporist_reading, base_extractiveness, 1975, 0.12).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__diasporist_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__diasporist_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1900, jewish_self_determination__diasporist_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(jewi_su_t1925, jewish_self_determination__diasporist_reading, suppression_requirement, 1925, 0.15).
narrative_ontology:measurement(jewi_su_t1950, jewish_self_determination__diasporist_reading, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement(jewi_su_t1975, jewish_self_determination__diasporist_reading, suppression_requirement, 1975, 0.22).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__diasporist_reading, suppression_requirement, 2000, 0.24).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__diasporist_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__diasporist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__religious_covenant_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'jewish_self_determination' kernel, each representing a distinct structural claim about Jewish collective flourishing and security.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
