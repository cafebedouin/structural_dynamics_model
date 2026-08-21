% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__security_maximization_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: geneva_conventions_1949__security_maximization_reading
 *   human_readable: Geneva Conventions (Security Maximization Reading)
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the 1949 Geneva
 *   Conventions, which posits that these international humanitarian laws are
 *   'peacetime aspirations' that must yield to 'operational necessity' in
 *   asymmetric conflicts. This interpretation justifies suspending most
 *   protections for individuals to maximize state security, leading to an
 *   expanded 'unlawful combatant' category, degraded civilian immunity,
 *   indefinite detention, and normalized coercive interrogation. The claimed
 *   type is 'snare' because the coordination story (state security) serves as
 *   a cover for substantial extraction from identifiable victims.
 *
 * KEY AGENTS:
 *   - state_security_apparatus: Primary agenda-setter (institutional/arbitrage) — benefits from expanded operational flexibility.
 *   - political_leadership: Primary beneficiary (institutional/arbitrage) — gains political capital from perceived security.
 *   - detainees_unlawful_combatants: Primary target/victim (powerless/trapped) — denied fundamental protections.
 *   - civilians_in_conflict_zones: Primary target/victim (powerless/trapped) — face degraded immunity.
 *   - human_rights_advocates: Excluded voice (organized/constrained) — challenge the interpretation but are marginalized.
 *   - international_courts: Observer (institutional/analytical) — attempt adjudication but face contested jurisdiction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.85).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.92).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, snare).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Geneva Conventions (Security Maximization Reading)").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, '7fae5451-88bc-4541-967b-6662cc8037d7').
narrative_ontology:cs_kernel_codification('7fae5451-88bc-4541-967b-6662cc8037d7', fixed_text).
narrative_ontology:cs_authority_grounding('7fae5451-88bc-4541-967b-6662cc8037d7', extraction).
narrative_ontology:cs_interpretation_layer_present('7fae5451-88bc-4541-967b-6662cc8037d7').
narrative_ontology:cs_reading_relation('7fae5451-88bc-4541-967b-6662cc8037d7', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('7fae5451-88bc-4541-967b-6662cc8037d7', geneva_conventions_1949__conditional_reciprocity_reading, influences).
narrative_ontology:cs_axiom('7fae5451-88bc-4541-967b-6662cc8037d7', foundational, state_security_paramount).
narrative_ontology:cs_axiom_status(state_security_paramount, holdable).
narrative_ontology:cs_axiom_grounding('7fae5451-88bc-4541-967b-6662cc8037d7', state_security_paramount, deontological).
narrative_ontology:cs_axiom('7fae5451-88bc-4541-967b-6662cc8037d7', foundational, asymmetric_warfare_exceptionalism).
narrative_ontology:cs_axiom_status(asymmetric_warfare_exceptionalism, holdable).
narrative_ontology:cs_axiom_grounding('7fae5451-88bc-4541-967b-6662cc8037d7', asymmetric_warfare_exceptionalism, empirically_contingent).
narrative_ontology:cs_reference_frame('7fae5451-88bc-4541-967b-6662cc8037d7', state_sovereignty_supremacy).
narrative_ontology:cs_drift_state('7fae5451-88bc-4541-967b-6662cc8037d7', post_9_11_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7fae5451-88bc-4541-967b-6662cc8037d7', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, state_security_apparatus).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, political_leadership).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, detainees_unlawful_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilians_in_conflict_zones).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies the conventions in a manner that prioritizes state security, justifying the suspension of protections for detainees and civilians. Benefits from expanded operational flexibility and reduced accountability.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, state_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).

% Authorizes and defends the security-maximization interpretation, gaining political capital from appearing tough on threats and avoiding constraints on national power. Bears minimal direct costs.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, political_leadership, beneficiary,
    institutional, biographical, arbitrage, national).

% Denied prisoner of war status, habeas corpus, and other fundamental protections. Subject to indefinite detention and coercive interrogation. Bear the full cost of this interpretation.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, detainees_unlawful_combatants, payer,
    powerless, immediate, trapped, local).

% Their immunity is degraded through doctrines like 'human shields' and expanded acceptance of collateral damage, increasing their vulnerability in conflict. Bear significant costs.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, civilians_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% Actively challenge this interpretation in courts and public discourse, but their arguments are often dismissed as naive or unpatriotic by proponents of security maximization. Face significant barriers to influencing policy.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, human_rights_advocates, excluded,
    organized, biographical, constrained, global).

% Attempt to adjudicate violations of international law, but their jurisdiction is often contested by states adopting this reading. Their judgments may be ignored or actively resisted.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, international_courts, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Minimally coordinates state action by providing a framework for unilateral decision-making in perceived security crises, ostensibly to protect the state from novel threats.
% TRANSFER_FUNCTION: Transfers legal protections, human rights, and accountability from individuals (detainees, civilians) and international legal bodies to the state, in exchange for enhanced state security and operational flexibility.
% ABSENT_VOICES: Detainees and civilians, whose rights are suspended, are structurally excluded from the interpretive process. International legal scholars and human rights organizations are often marginalized or dismissed.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, states would face significantly increased legal and moral pressure to adhere to traditional international humanitarian law, potentially altering operational doctrines, treatment of detainees, and accountability for civilian harm. The global legal landscape for armed conflict would shift towards greater protection.
% FOUNDING_PROBLEM: The perceived threat of irregular warfare and terrorism, which proponents argued rendered existing international humanitarian law (IHL) inadequate or obsolete for protecting state security.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within national security establishments and some political leaders attest that the problem of asymmetric threats is still live and requires this interpretation. International legal scholars and human rights organizations contest this, arguing that IHL is robust enough and that the 'problem' is a pretext for rights abuses.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__security_maximization_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__security_maximization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__security_maximization_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__security_maximization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__security_maximization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because this reading systematically denies fundamental rights and protections to individuals, shifting the burden of conflict onto them. Suppression is extremely high (0.92) as it relies on active legal reinterpretation, institutional power, and physical coercion to deny alternatives and exits for victims. Theater ratio is high (0.60) because while the Conventions are nominally acknowledged, their application is heavily reinterpreted or suspended, making adherence largely performative. Accessibility collapse is high (0.75) as legal and ethical alternatives are actively suppressed. Resistance is high (0.70) from human rights groups and international legal bodies, but this resistance is largely ineffective against state power.
 *
 * PERSPECTIVAL GAP:
 *   Proponents (state security apparatus, political leadership) view this as a necessary adaptation of law to modern threats, ensuring national survival. Victims (detainees, civilians) experience it as arbitrary detention, torture, and indiscriminate violence. Human rights advocates and international courts see it as a systematic erosion of international law and human rights. The engine's classification as a Snare reflects the structural reality of extraction despite the proponents' 'coordination' framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The state security apparatus and political leadership are clear beneficiaries, gaining operational freedom and political advantage (low directionality). Detainees and civilians are direct targets, bearing the full cost of suspended protections (high directionality). Human rights advocates are excluded from the decision-making process but bear the cost of fighting against the erosion of norms (high directionality).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a highly extractive interpretation as a legitimate coordination mechanism. The 'operational necessity' argument is the cover story; the actual operation involves systematic denial of rights and increased vulnerability for specific populations, which is characteristic of a snare. The high extractiveness and suppression, coupled with the identification of clear victims, confirm this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a valid interpretation of the Geneva Conventions, or a fundamental departure from its core principles?',
    'International legal consensus, rulings by international tribunals with recognized jurisdiction, or a new treaty explicitly superseding or reinterpreting the Conventions.',
    'If deemed a fundamental departure, the legitimacy of state actions taken under this reading would be severely undermined, potentially leading to increased accountability for perpetrators. If deemed valid, it would fundamentally alter the landscape of international humanitarian law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity regarding the interpretive legitimacy of the security maximization reading within the framework of the Geneva Conventions.').

omega_variable(
    operational_necessity_empirical_basis,
    'Are the claims of ''operational necessity'' and the unique challenges of ''asymmetric conflict'' empirically robust enough to justify the suspension of IHL protections?',
    'Independent, peer-reviewed empirical studies on the effectiveness of suspended protections in achieving security objectives, compared to adherence to traditional IHL, and analysis of the actual nature of asymmetric conflicts.',
    'If empirical claims are weak, the justification for extraction collapses, strengthening arguments for adherence to traditional IHL. If strong, it would lend some (though contested) empirical weight to the reading''s claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_necessity_empirical_basis, empirical, 'Empirical validity of claims regarding operational necessity in asymmetric conflict.').

omega_variable(
    unlawful_combatant_boundary,
    'Is the category of ''unlawful combatant'' a legitimate legal distinction within IHL, or an extralegal construct designed to deny protections?',
    'Definitive rulings by international courts on the legal status of individuals not falling neatly into traditional combatant/civilian categories, or a new international legal instrument clarifying these distinctions.',
    'If an extralegal construct, the denial of POW status and habeas corpus would be unequivocally illegal, leading to reclassification of detention practices as pure extraction. If legitimate, it would provide a legal basis for some denials of protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unlawful_combatant_boundary, conceptual, 'Legal legitimacy of the ''unlawful combatant'' category.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 2001, 2021).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2001, 0.4).
narrative_ontology:measurement(gene_tr_t2006, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2006, 0.5).
narrative_ontology:measurement(gene_tr_t2011, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2011, 0.6).
narrative_ontology:measurement(gene_tr_t2016, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2016, 0.65).
narrative_ontology:measurement(gene_tr_t2021, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2021, 0.6).

% Extraction over time
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2001, 0.7).
narrative_ontology:measurement(gene_be_t2006, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2006, 0.78).
narrative_ontology:measurement(gene_be_t2011, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2011, 0.83).
narrative_ontology:measurement(gene_be_t2016, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2016, 0.86).
narrative_ontology:measurement(gene_be_t2021, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2021, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2001, 0.75).
narrative_ontology:measurement(gene_su_t2006, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2006, 0.85).
narrative_ontology:measurement(gene_su_t2011, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2011, 0.9).
narrative_ontology:measurement(gene_su_t2016, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2016, 0.93).
narrative_ontology:measurement(gene_su_t2021, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2021, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__security_maximization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, torture_prohibition).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, habeas_corpus_suspension).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, civilian_immunity_in_conflict).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Geneva Conventions (1949) kernel. Its interpretation of 'operational necessity' directly impacts the application of other IHL principles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
