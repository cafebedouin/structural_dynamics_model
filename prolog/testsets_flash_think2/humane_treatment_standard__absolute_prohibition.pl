% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Absolute Prohibition of Torture and Degrading Treatment (Common Article 3 Reading)
 *   domain: international_humanitarian_law/human_rights
 *
 * SUMMARY:
 *   This constraint represents the 'absolute prohibition' reading of Common
 *   Article 3 of the Geneva Conventions, which establishes non-derogable
 *   minimum standards for humane treatment of persons not taking active part
 *   in hostilities. This reading asserts that no circumstances, including
 *   national security imperatives, permit torture or degrading treatment. It
 *   is a reading of the 'humane_treatment_standard' kernel, which is
 *   contested by 'contextual_necessity' and 'proportionality_balancing'
 *   readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.78).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.85).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.78).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Absolute Prohibition of Torture and Degrading Treatment (Common Article 3 Reading)").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "international_humanitarian_law/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, '74255f29-42b5-42e8-ac25-9cd07597aa58').
narrative_ontology:cs_kernel_codification('74255f29-42b5-42e8-ac25-9cd07597aa58', fixed_text).
narrative_ontology:cs_authority_grounding('74255f29-42b5-42e8-ac25-9cd07597aa58', lineage).
narrative_ontology:cs_interpretation_layer_present('74255f29-42b5-42e8-ac25-9cd07597aa58').
narrative_ontology:cs_reading_relation('74255f29-42b5-42e8-ac25-9cd07597aa58', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('74255f29-42b5-42e8-ac25-9cd07597aa58', humane_treatment_standard__proportionality_balancing, forecloses).
narrative_ontology:cs_axiom('74255f29-42b5-42e8-ac25-9cd07597aa58', foundational, torture_categorically_impermissible).
narrative_ontology:cs_axiom_status(torture_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('74255f29-42b5-42e8-ac25-9cd07597aa58', torture_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('74255f29-42b5-42e8-ac25-9cd07597aa58', foundational, human_dignity_non_derogable).
narrative_ontology:cs_axiom_status(human_dignity_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('74255f29-42b5-42e8-ac25-9cd07597aa58', human_dignity_non_derogable, deontological).
narrative_ontology:cs_reference_frame('74255f29-42b5-42e8-ac25-9cd07597aa58', post_wwii_universal_prohibition).
narrative_ontology:cs_drift_state('74255f29-42b5-42e8-ac25-9cd07597aa58', post_9_11_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('74255f29-42b5-42e8-ac25-9cd07597aa58', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detainees).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, human_rights_advocates).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, state_security_agencies).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, interrogators).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, universal_human_dignity).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, rule_of_law_in_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons deprived of liberty in armed conflict or security operations, who are the direct recipients of the protection offered by the absolute prohibition against torture and degrading treatment. Their ability to exit inhumane conditions is entirely dependent on external enforcement.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detainees, beneficiary,
    powerless, immediate, trapped, universal).

% Organizations and individuals who champion the universal application of human rights standards, including the absolute prohibition of torture. They benefit from the existence of clear, non-derogable standards that they can use to hold states accountable.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Government bodies responsible for intelligence gathering, counter-terrorism, and internal security. They bear the cost of being absolutely constrained in their interrogation methods, often perceiving this as an impediment to national security objectives. Their 'exit' is to violate the prohibition, risking international condemnation.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, state_security_agencies, payer,
    institutional, biographical, constrained, national).

% Individual agents tasked with questioning detainees. They are directly constrained by the prohibition, which limits their toolkit and methods, potentially increasing the difficulty of their work. Their 'exit' is to refuse unlawful orders or to engage in prohibited acts, risking legal or professional consequences.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, interrogators, payer,
    moderate, immediate, constrained, local).

% Judicial bodies (e.g., ICC, ECtHR) that interpret and enforce international humanitarian law. They set the agenda for accountability and define the scope of the prohibition, acting as a critical enforcement mechanism.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, international_courts, agenda_setter,
    institutional, civilizational, analytical, global).

% Sovereign states that have ratified the Geneva Conventions, committing to uphold Common Article 3. They are collectively responsible for maintaining and enforcing the standard, but individual states may also be targets of its enforcement.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, states_parties_to_geneva_conventions, agenda_setter,
    institutional, generational, constrained, global).

% Actors (often within security establishments or certain political factions) who argue that national security imperatives should permit 'enhanced interrogation' in specific contexts. They are excluded from the absolute prohibition framework, as their core premise is incompatible with it.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, contextual_necessity_proponents, excluded,
    powerful, biographical, identity_locked, global).

% Actors who advocate for a balancing test between detainee dignity and security needs, rather than an absolute prohibition. They are excluded from this reading's framework, which permits no such balancing.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, proportionality_balancing_proponents, excluded,
    powerful, biographical, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__absolute_prohibition, diffuse).
narrative_ontology:fixing_cost_class(humane_treatment_standard__absolute_prohibition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a universal, non-derogable minimum standard for humane treatment of persons not taking active part in hostilities, ensuring a baseline of dignity and preventing a race to the bottom in conflict and security operations.
% TRANSFER_FUNCTION: Transfers the perceived 'right' or 'discretion' to use enhanced interrogation techniques from state security agencies to detainees' non-derogable right to humane treatment, thereby imposing a cost on states that wish to employ such methods.
% ABSENT_VOICES: Proponents of 'contextual necessity' or 'proportionality balancing' are structurally excluded from the absolute prohibition framework; they would argue for exceptions based on national security or intelligence imperatives, but their arguments are rejected by this reading.
% DISAPPEARANCE_RATIONALE: If the absolute prohibition vanished overnight, states would immediately revert to or expand 'enhanced interrogation' practices, leading to a severe degradation of human rights standards in conflict and security operations globally, and a significant increase in suffering for detainees.
% FOUNDING_PROBLEM: The widespread atrocities and inhumane treatment of combatants and civilians during armed conflicts, particularly WWII, highlighted the urgent need for universal, non-derogable standards of protection for all persons in the hands of an adversary.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, UN bodies, and numerous historical and contemporary reports from independent observers consistently corroborate the ongoing need for such standards, citing persistent violations and the risk of backsliding, especially in times of perceived security threats.
narrative_ontology:disappearance_verdict(humane_treatment_standard__absolute_prohibition, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__absolute_prohibition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__absolute_prohibition, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(humane_treatment_standard__absolute_prohibition, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__absolute_prohibition, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.78) because the constraint extracts the perceived 'right' of states to use any means necessary for security, imposing a significant cost on those who believe such methods are effective. Suppression is very high (0.85) as the legal and normative framework actively seeks to prevent and punish torture, leaving no legitimate alternatives. Theater ratio is moderate (0.45) because while states publicly adhere to the prohibition, there is a persistent gap between declared policy and actual practice, with violations often occurring covertly or through redefinition of terms. Resistance is high (0.70) due to ongoing pressure from state security interests to circumvent or weaken the prohibition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of detainees and human rights advocates, this constraint is a vital 'rope' ensuring fundamental dignity. From the perspective of state security agencies, it can be perceived as a 'snare' that unduly restricts their ability to protect national interests, forcing them to operate with one hand tied behind their back. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Detainees and human rights advocates are clear beneficiaries, as the constraint directly protects the former and provides a framework for action for the latter. State security agencies and individual interrogators are the primary targets/payers, as the constraint directly limits their operational methods. International courts and states parties act as agenda-setters, responsible for upholding and enforcing the standard.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing inhumane treatment in conflict) remains acutely 'live'. The constraint's mandate is far from atrophied; indeed, it faces constant pressure and resistance, requiring continuous active enforcement. The high extractiveness and suppression are not signs of decay but of the ongoing struggle to uphold a vital, yet contested, standard.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint consistently interpreted as an absolute prohibition, or is its application frequently diluted by ''contextual necessity'' or ''proportionality balancing'' arguments in practice?',
    'Analysis of judicial rulings, state practice, and expert commentary across multiple jurisdictions and international bodies to determine the dominant interpretive trend.',
    'If ''contextual necessity'' or ''proportionality balancing'' readings gain significant traction, the effective extractiveness and suppression of this ''absolute prohibition'' reading would be lower, potentially reclassifying it towards a weaker ''tangled_rope'' or even ''piton'' if the absolute nature becomes purely theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'The actual interpretive dominance of the absolute prohibition reading versus its siblings.').

omega_variable(
    empirical_adherence_gap,
    'What is the true extent of state adherence to the absolute prohibition, beyond official declarations and legal frameworks?',
    'Independent investigations, whistleblower accounts, and declassified documents revealing covert practices, compared against official reports and public statements.',
    'A wider gap between declared adherence and actual practice would increase the ''theater_ratio'' and potentially lower the effective ''suppression'', indicating a more performative rather than functional constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_adherence_gap, empirical, 'The gap between de jure prohibition and de facto state practice regarding torture.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of torture primarily structural (legal frameworks, international pressure) or internalized (normative acceptance by state actors)?',
    'Post-regime-change analysis: if torture persists after legal/structural barriers are removed, reclassify as partially internalized; if it ceases, suppression is primarily structural.',
    'If internalized, the constraint''s effective suppression is more robust and resilient to external pressures; if purely structural, it is more vulnerable to political shifts and enforcement decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for torture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t1949, humane_treatment_standard__absolute_prohibition, theater_ratio, 1949, 0.2).
narrative_ontology:measurement(huma_tr_t1961, humane_treatment_standard__absolute_prohibition, theater_ratio, 1961, 0.25).
narrative_ontology:measurement(huma_tr_t1974, humane_treatment_standard__absolute_prohibition, theater_ratio, 1974, 0.3).
narrative_ontology:measurement(huma_tr_t1986, humane_treatment_standard__absolute_prohibition, theater_ratio, 1986, 0.35).
narrative_ontology:measurement(huma_tr_t1999, humane_treatment_standard__absolute_prohibition, theater_ratio, 1999, 0.4).
narrative_ontology:measurement(huma_tr_t2011, humane_treatment_standard__absolute_prohibition, theater_ratio, 2011, 0.43).
narrative_ontology:measurement(huma_tr_t2024, humane_treatment_standard__absolute_prohibition, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(huma_be_t1949, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1949, 0.6).
narrative_ontology:measurement(huma_be_t1961, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1961, 0.65).
narrative_ontology:measurement(huma_be_t1974, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1974, 0.7).
narrative_ontology:measurement(huma_be_t1986, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1986, 0.73).
narrative_ontology:measurement(huma_be_t1999, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1999, 0.75).
narrative_ontology:measurement(huma_be_t2011, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2011, 0.77).
narrative_ontology:measurement(huma_be_t2024, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t1949, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1949, 0.7).
narrative_ontology:measurement(huma_su_t1961, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1961, 0.75).
narrative_ontology:measurement(huma_su_t1974, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1974, 0.8).
narrative_ontology:measurement(huma_su_t1986, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1986, 0.82).
narrative_ontology:measurement(huma_su_t1999, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1999, 0.83).
narrative_ontology:measurement(huma_su_t2011, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2011, 0.84).
narrative_ontology:measurement(huma_su_t2024, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__absolute_prohibition, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
