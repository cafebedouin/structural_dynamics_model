% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__sovereignty_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: UN Security Council P5 Veto (Sovereignty Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This constraint models the P5 veto power in the UN Security Council as an
 *   instantiation of the Westphalian sovereignty principle, particularly as
 *   it applies to great powers with global-reach enforcement capacity. From
 *   this 'sovereignty reading,' the veto is a structural inevitability, a
 *   'Mountain' that reflects the physical reality of power distribution in
 *   the international system. Any global institution attempting to compel
 *   great-power action without their consent would face the same coordination
 *   failure, making the veto a necessary feature rather than an extractive
 *   mechanism. The low extractiveness and suppression metrics reflect this
 *   view of the veto as a fundamental, unchangeable aspect of international
 *   relations, not a human-imposed burden.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.02).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.05).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "UN Security Council P5 Veto (Sovereignty Reading)").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, '8dbe31d6-c778-41f6-bc7c-ee34cb04d865').
narrative_ontology:cs_kernel_codification('8dbe31d6-c778-41f6-bc7c-ee34cb04d865', fixed_text).
narrative_ontology:cs_authority_grounding('8dbe31d6-c778-41f6-bc7c-ee34cb04d865', lineage).
narrative_ontology:cs_reading_relation('8dbe31d6-c778-41f6-bc7c-ee34cb04d865', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('8dbe31d6-c778-41f6-bc7c-ee34cb04d865', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_axiom('8dbe31d6-c778-41f6-bc7c-ee34cb04d865', foundational, state_sovereignty_is_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('8dbe31d6-c778-41f6-bc7c-ee34cb04d865', state_sovereignty_is_absolute, deontological).
narrative_ontology:cs_axiom('8dbe31d6-c778-41f6-bc7c-ee34cb04d865', secondary, power_determines_enforceability).
narrative_ontology:cs_axiom_status(power_determines_enforceability, holdable).
narrative_ontology:cs_axiom_grounding('8dbe31d6-c778-41f6-bc7c-ee34cb04d865', power_determines_enforceability, empirically_contingent).
narrative_ontology:cs_reference_frame('8dbe31d6-c778-41f6-bc7c-ee34cb04d865', westphalian_sovereignty_principle).
narrative_ontology:cs_drift_state('8dbe31d6-c778-41f6-bc7c-ee34cb04d865', contemporary_international_system, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8dbe31d6-c778-41f6-bc7c-ee34cb04d865', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the operation of international law with the fundamental reality of great power sovereignty, ensuring that no state with global-reach enforcement capacity can be bound by international legal obligations without its explicit consent.
% TRANSFER_FUNCTION: Prevents the transfer of ultimate sovereign decision-making power from great powers to a supranational international body, thereby preserving their autonomy in matters of national security and vital interests.
% ABSENT_VOICES: States advocating for a more egalitarian international system, international legal scholars who prioritize collective security over individual state sovereignty, and smaller states who bear the consequences of great power inaction or unilateral action. These voices are often present in the General Assembly but lack direct power within the Security Council's decision-making structure.
% DISAPPEARANCE_RATIONALE: If the P5 veto, as a reflection of great power sovereignty, vanished overnight, the international system would fundamentally rearrange. Great powers, particularly nuclear states, would likely withdraw from or disregard international institutions that could compel them, leading to a more fragmented and potentially unstable global order. The premise of international law would shift from consent-based to potentially coercive, which is unsustainable given the distribution of military power.
% FOUNDING_PROBLEM: The core problem was how to construct an international security organization that could effectively address global threats while ensuring the participation and buy-in of the most powerful states, without infringing upon their fundamental sovereign prerogatives, especially given their capacity for independent military action and nuclear deterrence.
% FOUNDING_PROBLEM_CORROBORATION: Historians of international relations, international legal scholars, and diplomatic analysts (often outside the P5) corroborate that the tension between state sovereignty and international governance remains a live and unresolved problem, and that the veto was a pragmatic compromise reflecting this enduring reality. This perspective is supported by the historical record of great power behavior and the challenges of enforcing international law against powerful states.
narrative_ontology:disappearance_verdict(article_27_veto_power__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_27_veto_power__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__sovereignty_reading, 0.02, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_27_veto_power__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect the 'Mountain' classification: extractiveness is near zero because the veto is seen as preventing an impossible extraction (compelling a great power against its will). Suppression is low because it's not actively suppressing alternatives to a functional international system, but rather reflecting the limits of what such a system can achieve given state power. Theater ratio is minimal as the veto is viewed as a direct, functional expression of power reality. Accessibility collapse is high because alternatives to great power consent are structurally collapsed by their military and economic capabilities. Resistance is low because, from this perspective, the underlying principle is not effectively resisted by any actor capable of changing it.
 *
 * PERSPECTIVAL GAP:
 *   This reading fundamentally differs from others by viewing the veto as a structural given rather than a policy choice or an extractive tool. Other readings (e.g., 'oligopoly_reading') would see high extraction and suppression, while the 'coordination_reading' would emphasize its function in preventing war. The engine's classification will highlight this divergence from the claimed 'Mountain' type if the metrics were to suggest otherwise, but for this reading, the metrics align with the structural claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Given the 'Mountain' classification and the 'sovereignty reading,' there are no identifiable beneficiaries or victims in the traditional sense. The constraint is a reflection of the international system's fundamental structure, which all states must navigate. The great powers are not 'beneficiaries' in the sense of collecting rents, but rather operate within the bounds of their inherent sovereign capacity, which the veto reflects.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_structural_vs_constructed,
    'Is the P5 veto a structural inevitability given the distribution of global power, or a constructed political arrangement that could be reformed?',
    'Analysis of historical attempts to reform the Security Council, counterfactual scenarios of international law enforcement against great powers, and the observed behavior of nuclear states regarding international obligations.',
    'If primarily structural, the ''Mountain'' classification holds. If primarily constructed, it would be reclassified as a ''Tangled Rope'' or ''Snare'' depending on its coordination and extraction functions, with identifiable beneficiaries (P5 states) and victims (non-P5 states).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_structural_vs_constructed, conceptual, 'Ambiguity regarding the P5 veto''s origin as a natural consequence of power or a deliberate institutional design.').

omega_variable(
    veto_as_war_prevention_mechanism,
    'To what extent does the P5 veto primarily function as a necessary mechanism for preventing great-power war, as argued by the ''coordination_reading''?',
    'Historical analysis of Security Council crises where a veto prevented military intervention, and counterfactual analysis of potential great-power conflicts if the veto did not exist.',
    'If its primary function is war prevention, the ''coordination_reading'' gains strength, potentially shifting the classification towards a ''Rope'' or ''Tangled Rope'' with a strong coordination function. This would imply a higher, but justified, ''base_extractiveness'' as a cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_as_war_prevention_mechanism, empirical, 'Assessing the veto''s role in preventing great-power military confrontation.').

omega_variable(
    veto_as_oligopoly_entrenchment,
    'To what extent does the P5 veto serve to entrench a geopolitical oligopoly and extract authority rents, as argued by the ''oligopoly_reading''?',
    'Economic analysis of the benefits accrued by P5 states from their privileged position, analysis of veto usage patterns to block reforms or interventions against P5 interests, and examination of the suppression of alternative international governance models.',
    'If primarily an oligopoly entrenchment tool, the ''oligopoly_reading'' gains strength, leading to a classification as a ''Snare'' or ''Tangled Rope'' with high ''extractiveness'' and ''suppression'' from the perspective of non-P5 states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_as_oligopoly_entrenchment, empirical, 'Evaluating the veto''s role in maintaining P5 geopolitical dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1970, article_27_veto_power__sovereignty_reading, theater_ratio, 1970, 0.01).
narrative_ontology:measurement(arti_tr_t1980, article_27_veto_power__sovereignty_reading, theater_ratio, 1980, 0.01).
narrative_ontology:measurement(arti_tr_t1990, article_27_veto_power__sovereignty_reading, theater_ratio, 1990, 0.01).
narrative_ontology:measurement(arti_tr_t2000, article_27_veto_power__sovereignty_reading, theater_ratio, 2000, 0.01).
narrative_ontology:measurement(arti_tr_t2010, article_27_veto_power__sovereignty_reading, theater_ratio, 2010, 0.01).
narrative_ontology:measurement(arti_tr_t2020, article_27_veto_power__sovereignty_reading, theater_ratio, 2020, 0.01).

% Extraction over time
narrative_ontology:measurement(arti_be_t1970, article_27_veto_power__sovereignty_reading, base_extractiveness, 1970, 0.02).
narrative_ontology:measurement(arti_be_t1980, article_27_veto_power__sovereignty_reading, base_extractiveness, 1980, 0.02).
narrative_ontology:measurement(arti_be_t1990, article_27_veto_power__sovereignty_reading, base_extractiveness, 1990, 0.02).
narrative_ontology:measurement(arti_be_t2000, article_27_veto_power__sovereignty_reading, base_extractiveness, 2000, 0.02).
narrative_ontology:measurement(arti_be_t2010, article_27_veto_power__sovereignty_reading, base_extractiveness, 2010, 0.02).
narrative_ontology:measurement(arti_be_t2020, article_27_veto_power__sovereignty_reading, base_extractiveness, 2020, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1970, article_27_veto_power__sovereignty_reading, suppression_requirement, 1970, 0.05).
narrative_ontology:measurement(arti_su_t1980, article_27_veto_power__sovereignty_reading, suppression_requirement, 1980, 0.05).
narrative_ontology:measurement(arti_su_t1990, article_27_veto_power__sovereignty_reading, suppression_requirement, 1990, 0.05).
narrative_ontology:measurement(arti_su_t2000, article_27_veto_power__sovereignty_reading, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement(arti_su_t2010, article_27_veto_power__sovereignty_reading, suppression_requirement, 2010, 0.05).
narrative_ontology:measurement(arti_su_t2020, article_27_veto_power__sovereignty_reading, suppression_requirement, 2020, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
