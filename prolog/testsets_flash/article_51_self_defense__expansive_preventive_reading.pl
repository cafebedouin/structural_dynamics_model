% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__expansive_preventive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__expansive_preventive_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Expansive Preventive Self-Defense under UN Charter Article 51
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This constraint represents an expansive interpretation of UN Charter
 *   Article 51, allowing preemptive or preventive self-defense against
 *   non-state actors or emerging threats, with the necessity of force largely
 *   self-judged by the acting state. This reading emerged prominently
 *   post-9/11. It is a reading of the 'article_51_self_defense' kernel,
 *   distinct from 'narrow_armed_attack_reading' and
 *   'unable_unwilling_doctrine_reading'. The structural delta for this
 *   reading is a low constraint on unilateral force, with militarily capable
 *   states and defense sectors as beneficiaries, and target-region
 *   populations and multilateral veto authority as victims.
 *
 * KEY AGENTS:
 *   - militarily_capable_states: Primary agenda_setter (institutional/arbitrage) — benefits from reduced constraints on force.
 *   - target_region_populations: Primary payer (powerless/trapped) — bears the direct costs of intervention.
 *   - un_security_council_veto_authority: Payer (institutional/constrained) — its authority is bypassed.
 *   - defense_industrial_complexes: Beneficiary (organized/mobile) — profits from increased military action.
 *   - international_law_scholars: Observer (analytical/analytical) — critiques and analyzes the legal interpretation.
 *   - non_state_armed_groups: Excluded (moderate/constrained) — targets of action, but without formal voice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.7).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.6).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Expansive Preventive Self-Defense under UN Charter Article 51").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, 'a996bac4-d928-4581-9f88-285fbc885ad1').
narrative_ontology:cs_kernel_codification('a996bac4-d928-4581-9f88-285fbc885ad1', fixed_text).
narrative_ontology:cs_authority_grounding('a996bac4-d928-4581-9f88-285fbc885ad1', lineage).
narrative_ontology:cs_interpretation_layer_present('a996bac4-d928-4581-9f88-285fbc885ad1').
narrative_ontology:cs_reading_relation('a996bac4-d928-4581-9f88-285fbc885ad1', article_51_self_defense__narrow_armed_attack_reading, influences).
narrative_ontology:cs_reading_relation('a996bac4-d928-4581-9f88-285fbc885ad1', article_51_self_defense__unable_unwilling_doctrine_reading, coexists_with).
narrative_ontology:cs_axiom('a996bac4-d928-4581-9f88-285fbc885ad1', foundational, necessity_is_self_judged).
narrative_ontology:cs_axiom_status(necessity_is_self_judged, holdable).
narrative_ontology:cs_axiom_grounding('a996bac4-d928-4581-9f88-285fbc885ad1', necessity_is_self_judged, conventional).
narrative_ontology:cs_axiom('a996bac4-d928-4581-9f88-285fbc885ad1', foundational, preemptive_force_is_legitimate_self_defense).
narrative_ontology:cs_axiom_status(preemptive_force_is_legitimate_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('a996bac4-d928-4581-9f88-285fbc885ad1', preemptive_force_is_legitimate_self_defense, instrumental).
narrative_ontology:cs_reference_frame('a996bac4-d928-4581-9f88-285fbc885ad1', post_9_11_security_paradigm).
narrative_ontology:cs_drift_state('a996bac4-d928-4581-9f88-285fbc885ad1', contemporary_international_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a996bac4-d928-4581-9f88-285fbc885ad1', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, defense_industrial_complexes).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, un_security_council_veto_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with significant military capabilities that interpret Article 51 broadly to justify unilateral preemptive or preventive force against perceived threats, often self-judging the necessity of such actions. They benefit from increased operational flexibility and reduced multilateral constraints.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Populations in regions targeted by preemptive or preventive strikes, who bear the direct costs of conflict, displacement, and instability, often without direct involvement in the originating threat or the decision to use force.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_region_populations, payer,
    powerless, immediate, trapped, local).

% The collective authority of the UN Security Council to authorize or prohibit the use of force, which is bypassed or undermined by unilateral expansive interpretations of self-defense. Its power to constrain unilateral action is diminished.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, un_security_council_veto_authority, payer,
    institutional, generational, constrained, global).

% Industries that profit from military spending, arms sales, and the deployment of advanced weaponry. An expansive interpretation of self-defense, leading to more frequent military interventions, directly increases demand for their products and services.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, defense_industrial_complexes, beneficiary,
    organized, biographical, mobile, global).

% Academics and legal experts who analyze and critique interpretations of international law, including Article 51. They document the evolution of state practice and its consistency with the UN Charter, often highlighting the tension between state sovereignty and collective security.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% Groups that are often the targets of preemptive or preventive strikes, but whose perspectives on the legality or justification of such actions are not formally recognized within the international legal framework governing state-to-state relations.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, non_state_armed_groups, excluded,
    moderate, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to respond to threats to their security, aiming to prevent larger conflicts by allowing early intervention against emerging dangers, particularly from non-state actors.
% TRANSFER_FUNCTION: Transfers the authority to determine the necessity and proportionality of force from multilateral bodies (like the UN Security Council) to individual states, and transfers the costs of conflict to target populations.
% ABSENT_VOICES: The populations in targeted regions, who bear the direct consequences of military action, and non-state actors themselves, whose actions are often framed as threats without their input into the legal justifications for intervention.
% DISAPPEARANCE_RATIONALE: If this expansive reading vanished, militarily capable states would face significantly higher legal and political hurdles for unilateral military action, forcing greater reliance on multilateral authorization or more narrowly defined responses. This would fundamentally alter global security dynamics and state behavior regarding intervention.
% FOUNDING_PROBLEM: The original UN Charter framework for self-defense (Article 51) was primarily designed for state-on-state armed attacks, leaving ambiguity regarding responses to non-state actors and emerging, non-imminent threats in a post-9/11 security environment.
% FOUNDING_PROBLEM_CORROBORATION: Militarily capable states and their defense establishments consistently argue that the threat landscape has evolved beyond the original Charter's scope, necessitating an expansive interpretation. International law scholars and some UN member states, while acknowledging the evolving threats, contest the legality and prudence of unilateral expansive interpretations, advocating for multilateral solutions or stricter adherence to the Charter's original intent.
narrative_ontology:disappearance_verdict(article_51_self_defense__expansive_preventive_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__expansive_preventive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_51_self_defense__expansive_preventive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__expansive_preventive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__expansive_preventive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) is high because this reading allows powerful states to unilaterally impose military costs on others based on their own threat assessments, often without UN Security Council authorization. Suppression (0.6) reflects the active diplomatic and military efforts to legitimize and enforce this interpretation, often overriding objections from other states or international bodies. Theater ratio (0.4) indicates that while genuine security concerns exist, a significant portion of the justification for intervention serves to legitimize unilateral action and maintain strategic flexibility rather than purely addressing an imminent threat. Accessibility collapse (0.3) is low because alternative interpretations and multilateral mechanisms still exist, but resistance (0.75) is high due to ongoing legal and political challenges from states and scholars who advocate for a narrower interpretation of Article 51.
 *
 * PERSPECTIVAL GAP:
 *   Militarily capable states perceive this reading as a necessary adaptation of international law to modern threats, ensuring their security. Target populations and multilateral institutions, however, experience it as an erosion of collective security principles and a justification for unilateral intervention that imposes severe costs on them. The engine's per-seat classification will reflect this divergence, with beneficiaries seeing a 'rope' or 'scaffold' and victims experiencing a 'snare' or 'tangled_rope'.
 *
 * DIRECTIONALITY LOGIC:
 *   Militarily capable states are clear beneficiaries, gaining strategic flexibility and reduced accountability (low d). Target populations are clear victims, bearing the direct costs of conflict (high d). The UN Security Council's veto authority is also a victim, as its role in authorizing force is diminished (high d). Defense industrial complexes benefit from increased military activity (low d).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it purports to solve a coordination problem (responding to evolving threats) but does so through an asymmetric structure that extracts from target populations and multilateral authority while benefiting powerful states. The 'mandatrophy_resolved' flag is not set because the founding problem (ambiguity regarding non-state actors and emerging threats) is still 'live', but the solution (expansive unilateral interpretation) is contested for its extractive nature. The classification prevents mislabeling this as a 'rope' by highlighting the significant extraction and suppression involved, or as a 'snare' by acknowledging the genuine (though contested) coordination function it claims to serve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_self_judgment_legitimacy,
    'Is the self-judgment of ''necessity'' by the acting state a legitimate and sufficient basis for preemptive/preventive force under international law, or does it require independent, objective verification?',
    'A UN General Assembly resolution or ICJ advisory opinion explicitly defining criteria for necessity and proportionality that must be met and independently verified, rather than self-judged.',
    'If independent verification is required, the constraint''s suppression and extractiveness would decrease significantly, as unilateral action would be harder to justify, potentially reclassifying it closer to a ''rope'' or ''scaffold''. If self-judgment is affirmed, the current extractive dynamics would be further entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_self_judgment_legitimacy, conceptual, 'Ambiguity regarding the legitimacy of unilateral necessity determination for preemptive force.').

omega_variable(
    non_state_actor_attribution_threshold,
    'What threshold of state involvement or control is required to attribute the actions of a non-state actor to a state, thereby triggering traditional Article 51 self-defense, versus justifying expansive preventive action against the non-state actor directly?',
    'A clear, internationally agreed-upon legal standard for attribution of non-state actor actions to states, potentially through a new UN convention or ICJ ruling.',
    'A higher attribution threshold would reduce the scope for expansive preventive action against non-state actors, pushing states towards either traditional attribution or multilateral authorization, thereby reducing extractiveness. A lower threshold would further legitimize the expansive reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_state_actor_attribution_threshold, empirical, 'Ambiguity in attributing non-state actor actions to states for self-defense purposes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2001, 0.2).
narrative_ontology:measurement(arti_tr_t2007, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2007, 0.28).
narrative_ontology:measurement(arti_tr_t2013, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2013, 0.33).
narrative_ontology:measurement(arti_tr_t2018, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2018, 0.37).
narrative_ontology:measurement(arti_tr_t2024, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2001, 0.5).
narrative_ontology:measurement(arti_be_t2007, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2007, 0.6).
narrative_ontology:measurement(arti_be_t2013, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2013, 0.65).
narrative_ontology:measurement(arti_be_t2018, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2018, 0.68).
narrative_ontology:measurement(arti_be_t2024, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2001, 0.45).
narrative_ontology:measurement(arti_su_t2007, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2007, 0.5).
narrative_ontology:measurement(arti_su_t2013, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2013, 0.55).
narrative_ontology:measurement(arti_su_t2018, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2018, 0.58).
narrative_ontology:measurement(arti_su_t2024, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, unable_unwilling_doctrine_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, un_security_council_veto_power).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, international_humanitarian_law_compliance).

% DUAL FORMULATION NOTE:
% This constraint is one of three primary readings of the UN Charter Article 51 self-defense kernel. Its expansive interpretation directly influences the operational space and legitimacy of the other, narrower readings, and impacts the authority of the UN Security Council.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
