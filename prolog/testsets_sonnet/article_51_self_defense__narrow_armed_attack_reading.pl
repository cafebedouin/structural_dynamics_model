% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__narrow_armed_attack_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__narrow_armed_attack_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: article_51_self_defense__narrow_armed_attack_reading
 *   human_readable: Article 51 Self-Defense: Narrow Armed-Attack Reading
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This story instantiates the narrow armed-attack reading of the Article 51
 *   self-defense kernel: self-defense is lawful only in response to an actual
 *   or imminent armed attack attributable to a state under international law
 *   (the Nicaragua/ICJ 'effective control' standard). This reading is one of
 *   three structurally distinct constraints emitted by the same contested
 *   text — the expansive preventive reading and the unable/unwilling doctrine
 *   reading are separate constraints with different beneficiary/victim
 *   structures and different epsilon values, generated as sibling files and
 *   linked here via network.affects_constraints. This file does not describe
 *   or average over those readings; it presents only the narrow reading as a
 *   clean, internally coherent constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, 0.42).
domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, 0.55).
domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__narrow_armed_attack_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__narrow_armed_attack_reading, "Article 51 Self-Defense: Narrow Armed-Attack Reading").
narrative_ontology:topic_domain(article_51_self_defense__narrow_armed_attack_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__narrow_armed_attack_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, '3af9d3c6-2aec-405b-b104-defa1aeb6705').
narrative_ontology:cs_kernel_codification('3af9d3c6-2aec-405b-b104-defa1aeb6705', fixed_text).
narrative_ontology:cs_authority_grounding('3af9d3c6-2aec-405b-b104-defa1aeb6705', practice).
narrative_ontology:cs_interpretation_layer_present('3af9d3c6-2aec-405b-b104-defa1aeb6705').
narrative_ontology:cs_reading_relation('3af9d3c6-2aec-405b-b104-defa1aeb6705', article_51_self_defense__expansive_preventive_reading, forecloses).
narrative_ontology:cs_reading_relation('3af9d3c6-2aec-405b-b104-defa1aeb6705', article_51_self_defense__unable_unwilling_doctrine_reading, coexists_with).
narrative_ontology:cs_axiom('3af9d3c6-2aec-405b-b104-defa1aeb6705', foundational, armed_attack_requires_state_attribution).
narrative_ontology:cs_axiom_status(armed_attack_requires_state_attribution, holdable).
narrative_ontology:cs_axiom_grounding('3af9d3c6-2aec-405b-b104-defa1aeb6705', armed_attack_requires_state_attribution, conventional).
narrative_ontology:cs_axiom('3af9d3c6-2aec-405b-b104-defa1aeb6705', foundational, prohibition_on_force_is_default_exception_is_narrow).
narrative_ontology:cs_axiom_status(prohibition_on_force_is_default_exception_is_narrow, holdable).
narrative_ontology:cs_axiom_grounding('3af9d3c6-2aec-405b-b104-defa1aeb6705', prohibition_on_force_is_default_exception_is_narrow, deontological).
narrative_ontology:cs_reference_frame('3af9d3c6-2aec-405b-b104-defa1aeb6705', un_charter_founding_settlement).
narrative_ontology:cs_drift_state('3af9d3c6-2aec-405b-b104-defa1aeb6705', post_9_11_counterterrorism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3af9d3c6-2aec-405b-b104-defa1aeb6705', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, weaker_un_member_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, un_security_council).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, icj_and_multilateral_arbitration_bodies).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, great_powers_seeking_unilateral_force_authority).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, states_facing_non_state_armed_groups).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, un_charter_prohibition_on_force_as_default_rule).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, state_sovereignty_as_baseline_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lack the military capacity to deter powerful states through force; rely on the narrow reading of Article 51 to prevent stronger states from invoking self-defense against them absent a genuine armed attack traceable to their own conduct. Their security depends on the rule holding, not on their own capacity to enforce it.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, weaker_un_member_states, beneficiary,
    powerless, generational, trapped, global).

% Holds primary authority under Chapter VII to authorize force; the narrow reading preserves this authority by keeping unilateral self-defense claims tightly bounded so that most force questions must route through Council authorization rather than unilateral state assertion. Its institutional relevance is directly tied to the narrowness of the exception.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, un_security_council, agenda_setter,
    institutional, civilizational, analytical, global).

% Adjudicate attribution and armed-attack thresholds (e.g., Nicaragua v. United States); the narrow reading gives these bodies a clear, litigable standard to apply. A broader doctrine would displace judicial adjudication with unilateral executive determination, reducing the Court's relevance.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, icj_and_multilateral_arbitration_bodies, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__narrow_armed_attack_reading, icj_and_multilateral_arbitration_bodies, observer).

% Face non-state armed groups, cross-border militias, and diffuse threats that the narrow reading does not permit them to strike preemptively or against unattributed non-state actors without violating the constraint (at least on its own terms). They bear the cost of restraint: either accept exposure to the threat, seek Security Council authorization (subject to veto delay), or act outside the constraint and accept the legitimacy cost. Their exit is constrained — they can violate the norm, but only by paying a reputational and coalition-building price.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, great_powers_seeking_unilateral_force_authority, payer,
    powerful, biographical, constrained, global).

% Suffer cross-border attacks from militias or terrorist organizations operating from a neighboring state's territory. Under this narrow reading, they cannot invoke Article 51 unless the attack is attributable to the host state itself, leaving them without a lawful unilateral response option even when the host state is complicit but not the direct attacker. They bear the immediate security cost of the doctrine's narrowness.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, states_facing_non_state_armed_groups, payer,
    moderate, immediate, constrained, regional).

% Territory from which non-state armed groups operate, without necessarily directing or controlling them. The narrow reading protects them from being targeted under self-defense doctrine absent proof of attribution, but they are not parties to the interpretive contest — their sovereignty interest is served by the rule without their having a voice in how attribution standards get set by ICJ jurisprudence and state practice.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, host_states_of_non_state_actors, excluded,
    moderate, biographical, constrained, regional).

% Analyze state practice and opinio juris to determine whether the narrow reading remains customary international law or is eroding under pressure from expansive state practice (drone strikes, cross-border raids). They observe rather than enforce.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_law_scholars_and_icj, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, judicially administrable standard for when unilateral force is lawful, preventing the erosion of the general prohibition on the use of force into a patchwork of self-serving unilateral judgments about threat and necessity.
% TRANSFER_FUNCTION: Moves strategic freedom of action away from powerful states capable of projecting force preemptively, and toward weaker states and multilateral bodies whose security depends on the prohibition on force holding as a general rule rather than a discretionary standard invoked by the strong.
% ABSENT_VOICES: Non-state armed groups themselves have no standing in the framework at all — the doctrine is entirely state-centric, and the states most directly injured by non-state actor violence (states_facing_non_state_armed_groups) find their operational security needs subordinated to an attribution requirement they did not design and cannot easily satisfy evidentially.
% DISAPPEARANCE_RATIONALE: If the narrow reading collapsed as the governing rule, powerful states would face substantially reduced legal friction in justifying unilateral strikes against non-state actors and their hosts; the Security Council's gatekeeping role over the use of force would be marginalized in practice; weaker states would lose a legal shield currently available against speculative or pretextual self-defense claims by stronger states.
% FOUNDING_PROBLEM: The UN Charter framers sought to establish a near-absolute prohibition on interstate force (Article 2(4)) with only a narrow, judicially bounded exception (Article 51) — designed after two world wars to prevent states from using unilateral 'defense' as pretext for aggression, as had occurred repeatedly in the interwar period.
% FOUNDING_PROBLEM_CORROBORATION: The ICJ (Nicaragua, Oil Platforms, Wall Advisory Opinion) and a substantial body of international law scholarship outside any single state's interest attest that the founding problem — preventing pretextual unilateral force — remains live and that erosion of the narrow reading recreates exactly the risk the Charter framers targeted. Powerful states asserting expansive self-defense rights are, definitionally, not neutral corroborators of this reading's continued necessity; their practice is evidence of pressure on the rule, not testimony that the rule's purpose has lapsed.
narrative_ontology:disappearance_verdict(article_51_self_defense__narrow_armed_attack_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__narrow_armed_attack_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__narrow_armed_attack_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__narrow_armed_attack_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__narrow_armed_attack_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__narrow_armed_attack_reading_tests).
:- end_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rising over the interval: the doctrine imposes real constraint-cost on powerful states (foregone unilateral options against diffuse threats) that has intensified as non-state armed conflict has proliferated since the 1990s. Suppression (0.55) reflects the doctrine's dependence on active enforcement — ICJ litigation, UN General Assembly condemnation, coalition diplomatic pressure — to hold the line against erosion by contrary state practice. Theater ratio (0.3) captures genuine but partial performative drift: states increasingly invoke the doctrine's language while acting outside its bounds (justificatory theater), though the doctrine still does real adjudicative work in ICJ and scholarly contexts.
 *
 * PERSPECTIVAL GAP:
 *   From the UN Security Council's and weaker states' seats, this is coordination that preserves a hard-won post-1945 settlement against relapse into pretextual war. From the seat of a great power facing a diffuse non-state threat, the identical rule computes as an externally imposed restriction that denies it strategic options its threat environment demands, enforced by reputational and coalition costs rather than by any tribunal with binding jurisdiction over it.
 *
 * DIRECTIONALITY LOGIC:
 *   Weaker states and multilateral institutions are structural beneficiaries: the narrow reading is the shield that prevents powerful states from re-legitimizing unilateral force against them. Powerful states seeking strategic freedom are the structural targets: the doctrine directly constrains what they can lawfully claim to do. States_facing_non_state_armed_groups occupy an intermediate position — nominally protected by the general prohibition on force but also constrained in responding to real cross-border threats, because the attribution requirement denies them a lawful self-defense claim against non-state actors whose host state is complicit but not controlling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing pretextual unilateral force after two world wars) remains live according to ICJ jurisprudence and much of the scholarly community, even though powerful states increasingly treat the doctrine as obsolete in the face of transnational terrorism. This is precisely the mandatrophy question the R5 interview is designed to surface: is the narrow reading a genuine ongoing coordination function (preventing a return to pretextual aggression) or a doctrine whose founding rationale has been overtaken by a changed threat environment it was never designed to address? The corroboration asymmetry — ICJ and scholarship attest continued necessity; the states bearing the cost of restraint attest obsolescence — is exactly the signal the framework is built to register rather than resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_erosion_status,
    'Has the narrow armed-attack reading eroded as a matter of customary international law given post-9/11 state practice (Afghanistan, cross-border drone strikes, Syrian operations), or does it remain the dominant lex lata despite inconsistent practice?',
    'Systematic review of opinio juris across UN General Assembly debates, ICJ dicta, and state legal justifications for use-of-force actions since 2001, weighted by whether protesting states outnumber acquiescing ones.',
    'If erosion is established, the narrow reading''s claimed status as binding law weakens substantially and the constraint''s classification shifts toward contested/aspirational rather than operative; if the narrow reading persists as lex lata despite violations, the violations are better modeled as breaches than as doctrinal shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_erosion_status, empirical, 'Whether state practice has shifted customary law away from the narrow reading despite its continued formal endorsement by the ICJ.').

omega_variable(
    attribution_evidentiary_asymmetry,
    'Does the state-attribution requirement structurally disadvantage weaker states that lack the intelligence and forensic capacity to prove effective control by a host state, even when the underlying threat is genuine?',
    'Comparative case study of attribution disputes where powerful states (with intelligence resources) succeeded in attribution claims versus weaker states that failed for evidentiary rather than substantive reasons.',
    'If the asymmetry is real, the narrow reading''s benefit to weaker states is partially undercut in the specific case of proving attribution against a hostile neighbor, even though it still protects them as potential targets of others'' self-defense claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_evidentiary_asymmetry, empirical, 'Whether the attribution standard itself reproduces power asymmetry rather than simply constraining the powerful.').

omega_variable(
    kernel_reading_selection_bias,
    'Is the choice among the three readings of Article 51 (narrow, expansive, unable/unwilling) determined by principled legal interpretation, or does it track which reading serves the interpreting state''s strategic position?',
    'Cross-national survey of which states/blocs endorse which reading, correlated with their relative military capacity and exposure to non-state armed threats.',
    'Strong correlation between reading endorsed and strategic interest would suggest the kernel contest is substantially interest-driven rather than doctrinally resolved, which bears on how much independent normative weight any single reading (including this one) should be given.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_bias, conceptual, 'Whether reading-selection across the Article 51 kernel tracks principled interpretation or strategic self-interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__narrow_armed_attack_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1945, 0.12).
narrative_ontology:measurement(arti_tr_t1961, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1961, 0.16).
narrative_ontology:measurement(arti_tr_t1981, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1981, 0.2).
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2001, 0.26).
narrative_ontology:measurement(arti_tr_t2013, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2013, 0.29).
narrative_ontology:measurement(arti_tr_t2025, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1945, 0.22).
narrative_ontology:measurement(arti_be_t1961, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1961, 0.26).
narrative_ontology:measurement(arti_be_t1981, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1981, 0.3).
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2001, 0.36).
narrative_ontology:measurement(arti_be_t2013, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2013, 0.4).
narrative_ontology:measurement(arti_be_t2025, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1945, 0.4).
narrative_ontology:measurement(arti_su_t1961, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1961, 0.44).
narrative_ontology:measurement(arti_su_t1981, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1981, 0.47).
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2001, 0.51).
narrative_ontology:measurement(arti_su_t2013, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2013, 0.53).
narrative_ontology:measurement(arti_su_t2025, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__narrow_armed_attack_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__narrow_armed_attack_reading, 0.12).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_51_expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_51_unable_unwilling_doctrine_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposed from the natural-language concept 'Article 51 self-defense,' per the epsilon-invariance principle: the narrow armed-attack reading, the expansive preventive reading, and the unable/unwilling doctrine reading are structurally distinct claims with different beneficiary/victim sets and different plausible epsilon values, and are therefore authored as separate constraint stories rather than one story with an observable-selection parameter. This file's epsilon (0.42, moderate and constraint-favoring weaker states) should be read against, not averaged with, the sibling files' epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
