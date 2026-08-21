% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__state_centric_reading, []).

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
 *   constraint_id: common_article_3_scope__state_centric_reading
 *   human_readable: Common Article 3 Scope (State-Centric Reading)
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   This constraint represents the 'state-centric reading' of Common Article
 *   3 (CA3) of the Geneva Conventions, which holds that CA3 applies only when
 *   an internal armed conflict meets specific thresholds of intensity and
 *   organization, thereby excluding low-level violence and law enforcement
 *   operations from IHL's scope. This interpretation is actively enforced by
 *   states to preserve maximum operational discretion for their security
 *   forces. The high extractiveness reflects the denial of IHL protections to
 *   individuals in conflicts below these thresholds, while high suppression
 *   indicates the active defense of this narrow scope by states. The claimed
 *   type is Tangled Rope, as it purports to coordinate the application of IHL
 *   but does so in a way that benefits states at the expense of vulnerable
 *   populations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.78).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.85).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "Common Article 3 Scope (State-Centric Reading)").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, '5ae51254-5a82-4e14-80a4-31283f7688de').
narrative_ontology:cs_kernel_codification('5ae51254-5a82-4e14-80a4-31283f7688de', fixed_text).
narrative_ontology:cs_authority_grounding('5ae51254-5a82-4e14-80a4-31283f7688de', lineage).
narrative_ontology:cs_interpretation_layer_present('5ae51254-5a82-4e14-80a4-31283f7688de').
narrative_ontology:cs_reading_relation('5ae51254-5a82-4e14-80a4-31283f7688de', common_article_3_scope__expansive_human_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('5ae51254-5a82-4e14-80a4-31283f7688de', common_article_3_scope__icrc_customary_reading, coexists_with).
narrative_ontology:cs_axiom('5ae51254-5a82-4e14-80a4-31283f7688de', foundational, state_sovereignty_primacy).
narrative_ontology:cs_axiom_status(state_sovereignty_primacy, holdable).
narrative_ontology:cs_axiom_grounding('5ae51254-5a82-4e14-80a4-31283f7688de', state_sovereignty_primacy, deontological).
narrative_ontology:cs_axiom('5ae51254-5a82-4e14-80a4-31283f7688de', foundational, ihl_exceptionalism).
narrative_ontology:cs_axiom_status(ihl_exceptionalism, holdable).
narrative_ontology:cs_axiom_grounding('5ae51254-5a82-4e14-80a4-31283f7688de', ihl_exceptionalism, conventional).
narrative_ontology:cs_reference_frame('5ae51254-5a82-4e14-80a4-31283f7688de', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('5ae51254-5a82-4e14-80a4-31283f7688de', contemporary_post_cold_war_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5ae51254-5a82-4e14-80a4-31283f7688de', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, state_governments).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, military_commanders).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, civilians_in_low_intensity_conflicts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As primary interpreters and enforcers of IHL, states define and apply the thresholds for Common Article 3, maximizing their operational discretion in internal security operations and low-intensity conflicts. They benefit from reduced accountability under IHL.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, state_governments, agenda_setter,
    institutional, generational, arbitrage, global).

% Operate under the legal framework set by their states. This narrow interpretation of CA3 provides them with greater flexibility and fewer legal constraints when dealing with non-state armed groups or internal disturbances that do not meet the high thresholds.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, military_commanders, beneficiary,
    powerful, biographical, constrained, national).

% Are denied the minimum humanitarian protections of Common Article 3 if their group's organization or the conflict's intensity falls below the state-defined thresholds. They are treated under domestic criminal law, often with harsher penalties and fewer safeguards.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold, payer,
    powerless, immediate, trapped, local).

% Suffer from the lack of CA3 protections in conflicts deemed below the threshold. This can lead to increased civilian casualties, arbitrary detention, and denial of basic humanitarian treatment, as IHL's protective framework is not applied.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, civilians_in_low_intensity_conflicts, payer,
    powerless, immediate, trapped, local).

% Actively challenge the narrow interpretation, arguing for a broader application of CA3 to ensure minimum humanitarian standards for all victims of armed violence. They document abuses and advocate for legal reform but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, human_rights_advocates, observer,
    organized, generational, analytical, global).

% May adjudicate cases related to IHL application, but their jurisdiction is often limited by state consent or the specific nature of the conflict. They provide legal interpretations that can influence, but not always directly override, state practice.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, international_courts, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a legal framework for distinguishing between situations of 'armed conflict not of an international character' (where CA3 applies) and 'internal disturbances and tensions' (where only domestic law applies), thereby coordinating state obligations.
% TRANSFER_FUNCTION: Transfers the burden of IHL compliance away from state governments and military forces in situations deemed below the threshold, effectively transferring the risk and lack of protection to irregular combatants and civilians in those contexts.
% ABSENT_VOICES: Irregular combatants and civilians in low-intensity conflicts, who are directly affected by the narrow interpretation, are largely excluded from the interpretive processes that define these thresholds. Their perspectives would emphasize the human cost of non-application.
% DISAPPEARANCE_RATIONALE: If this narrow interpretation vanished overnight, states would face immediate pressure to apply CA3 more broadly, significantly altering military operations, legal accountability, and the protection landscape for individuals in various forms of armed violence. The distinction between domestic law enforcement and armed conflict would blur, leading to a major reorganization of state security practices.
% FOUNDING_PROBLEM: To distinguish between genuine armed conflict (where IHL applies) and mere internal disturbances or banditry (where domestic law applies), preventing the over-application of IHL and preserving state sovereignty over internal affairs.
% FOUNDING_PROBLEM_CORROBORATION: State governments and their legal advisors generally maintain that the founding problem of distinguishing conflict types remains live, citing the need to avoid 'humanitarianizing' every internal security challenge. Human rights organizations and some legal scholars, however, attest that the founding problem is substantially solved and the narrow interpretation now primarily serves to avoid accountability, citing numerous conflicts where it has led to protection gaps. This is supported by independent legal analyses and reports from international NGOs.
narrative_ontology:disappearance_verdict(common_article_3_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(common_article_3_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__state_centric_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.78) is high because this reading effectively removes IHL protections from a significant number of individuals caught in armed violence, allowing states to treat them under less protective domestic law. Suppression (0.85) is also high, as states actively resist broader interpretations and enforce their narrow view through legal arguments, military doctrine, and diplomatic pressure. The theater ratio (0.20) is low because the constraint's primary function is to *limit* the application of IHL, which is a direct and functional (though extractive) outcome for states, rather than a performative one. Accessibility collapse is high (0.90) for those below the threshold, as IHL protections are simply unavailable. Resistance is high (0.70) from human rights groups and some legal scholars.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state governments, this interpretation is a necessary coordination mechanism to preserve sovereignty and distinguish between war and crime. From the perspective of victims, it is a snare that denies fundamental protections. The engine's classification will highlight this divergence by computing different effective extraction values for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and military commanders are clear beneficiaries, gaining operational flexibility and reduced accountability (low directionality). Irregular combatants and civilians in low-intensity conflicts are the primary targets, bearing the costs of denied protections (high directionality). Human rights advocates and international courts act as observers, challenging the interpretation but not directly subject to its extraction in the same way as the victims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_ambiguity,
    'Are the ''intensity'' and ''organization'' thresholds for CA3 application sufficiently clear and objectively verifiable, or are they subject to arbitrary state interpretation?',
    'Development of universally accepted, objective criteria and independent monitoring mechanisms for assessing conflict thresholds, or consistent jurisprudence from international courts.',
    'If thresholds are arbitrary, the constraint''s suppression and extractiveness are higher than measured, as states can manipulate them to avoid IHL obligations. If objective, the constraint''s coordination function is more legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_ambiguity, empirical, 'Ambiguity of CA3 application thresholds.').

omega_variable(
    sovereignty_vs_impunity,
    'Does this narrow interpretation genuinely preserve state sovereignty and the distinction between IHL and domestic law, or does it primarily serve to enable impunity for state actors in situations of armed violence?',
    'Empirical analysis of accountability outcomes in conflicts where this interpretation is applied, compared to those with broader CA3 application, and a conceptual re-evaluation of the relationship between sovereignty and human protection.',
    'If it primarily enables impunity, the constraint''s extractiveness is higher and its coordination function is largely theatrical, pushing it closer to a Snare. If it genuinely preserves a necessary distinction, its coordination function is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_impunity, conceptual, 'Whether narrow CA3 scope preserves sovereignty or enables impunity.').

omega_variable(
    ihl_hrl_convergence,
    'To what extent should the state-centric interpretation of IHL''s scope be influenced by the evolving convergence with international human rights law (IHRL)?',
    'Further development of international legal doctrine and jurisprudence on the interplay between IHL and IHRL, and shifts in state practice reflecting this convergence.',
    'If IHRL principles are increasingly seen as applicable, the narrowness of this reading becomes less defensible, increasing its perceived extractiveness and resistance. If the distinction remains strong, the reading''s internal coherence is maintained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ihl_hrl_convergence, preference, 'Tension between state-centric IHL and human rights law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1990, common_article_3_scope__state_centric_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(comm_tr_t1996, common_article_3_scope__state_centric_reading, theater_ratio, 1996, 0.16).
narrative_ontology:measurement(comm_tr_t2002, common_article_3_scope__state_centric_reading, theater_ratio, 2002, 0.17).
narrative_ontology:measurement(comm_tr_t2008, common_article_3_scope__state_centric_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(comm_tr_t2014, common_article_3_scope__state_centric_reading, theater_ratio, 2014, 0.19).
narrative_ontology:measurement(comm_tr_t2020, common_article_3_scope__state_centric_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(comm_be_t1990, common_article_3_scope__state_centric_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(comm_be_t1996, common_article_3_scope__state_centric_reading, base_extractiveness, 1996, 0.69).
narrative_ontology:measurement(comm_be_t2002, common_article_3_scope__state_centric_reading, base_extractiveness, 2002, 0.72).
narrative_ontology:measurement(comm_be_t2008, common_article_3_scope__state_centric_reading, base_extractiveness, 2008, 0.75).
narrative_ontology:measurement(comm_be_t2014, common_article_3_scope__state_centric_reading, base_extractiveness, 2014, 0.77).
narrative_ontology:measurement(comm_be_t2020, common_article_3_scope__state_centric_reading, base_extractiveness, 2020, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1990, common_article_3_scope__state_centric_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(comm_su_t1996, common_article_3_scope__state_centric_reading, suppression_requirement, 1996, 0.75).
narrative_ontology:measurement(comm_su_t2002, common_article_3_scope__state_centric_reading, suppression_requirement, 2002, 0.8).
narrative_ontology:measurement(comm_su_t2008, common_article_3_scope__state_centric_reading, suppression_requirement, 2008, 0.82).
narrative_ontology:measurement(comm_su_t2014, common_article_3_scope__state_centric_reading, suppression_requirement, 2014, 0.84).
narrative_ontology:measurement(comm_su_t2020, common_article_3_scope__state_centric_reading, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'common_article_3_scope' kernel, each representing a distinct interpretation of when Common Article 3 of the Geneva Conventions applies. This 'state-centric reading' emphasizes high thresholds for application, contrasting with more expansive interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
