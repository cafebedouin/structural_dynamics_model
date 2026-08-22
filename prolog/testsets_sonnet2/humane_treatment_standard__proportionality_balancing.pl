% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__proportionality_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__proportionality_balancing, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: humane_treatment_standard__proportionality_balancing
 *   human_readable: Common Article 3 Proportionality-Balancing Reading
 *   domain: international_humanitarian_law/state_security
 *
 * SUMMARY:
 *   This story instantiates the proportionality-balancing reading of the
 *   Common Article 3 humane-treatment kernel: neither an absolute
 *   non-derogable prohibition nor unlimited executive discretion, but a
 *   case-by-case judicial balancing of detainee dignity against claimed
 *   security necessity. Under this reading, courts become the designated
 *   gatekeepers of permissibility, deciding after the fact whether a given
 *   technique was proportionate. This is a genuinely distinct constraint from
 *   its siblings, not a hedge between them: the absolute-prohibition reading
 *   forecloses the balancing question entirely by treating certain treatments
 *   as always impermissible regardless of security claims, while the
 *   contextual-necessity reading would let the executive's own necessity
 *   claim override the standard with minimal external check. The
 *   proportionality reading occupies neither pole — it creates an active
 *   coordinating role for courts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, 0.52).
domain_priors:suppression_score(humane_treatment_standard__proportionality_balancing, 0.48).
domain_priors:theater_ratio(humane_treatment_standard__proportionality_balancing, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, extractiveness, 0.52).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__proportionality_balancing, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__proportionality_balancing, "Common Article 3 Proportionality-Balancing Reading").
narrative_ontology:topic_domain(humane_treatment_standard__proportionality_balancing, "international_humanitarian_law/state_security").

domain_priors:requires_active_enforcement(humane_treatment_standard__proportionality_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__proportionality_balancing, 'b0980e45-f61f-4597-bf5b-fef5020c31b2').
narrative_ontology:cs_kernel_codification('b0980e45-f61f-4597-bf5b-fef5020c31b2', fixed_text).
narrative_ontology:cs_authority_grounding('b0980e45-f61f-4597-bf5b-fef5020c31b2', lineage).
narrative_ontology:cs_interpretation_layer_present('b0980e45-f61f-4597-bf5b-fef5020c31b2').
narrative_ontology:cs_reading_relation('b0980e45-f61f-4597-bf5b-fef5020c31b2', humane_treatment_standard__absolute_prohibition, coexists_with).
narrative_ontology:cs_reading_relation('b0980e45-f61f-4597-bf5b-fef5020c31b2', humane_treatment_standard__contextual_necessity, influences).
narrative_ontology:cs_axiom('b0980e45-f61f-4597-bf5b-fef5020c31b2', foundational, dignity_and_security_are_commensurable_interests).
narrative_ontology:cs_axiom_status(dignity_and_security_are_commensurable_interests, holdable).
narrative_ontology:cs_axiom_grounding('b0980e45-f61f-4597-bf5b-fef5020c31b2', dignity_and_security_are_commensurable_interests, conventional).
narrative_ontology:cs_axiom('b0980e45-f61f-4597-bf5b-fef5020c31b2', foundational, judicial_case_by_case_review_is_competent_arbiter).
narrative_ontology:cs_axiom_status(judicial_case_by_case_review_is_competent_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('b0980e45-f61f-4597-bf5b-fef5020c31b2', judicial_case_by_case_review_is_competent_arbiter, instrumental).
narrative_ontology:cs_reference_frame('b0980e45-f61f-4597-bf5b-fef5020c31b2', judicial_case_by_case_competence_framework).
narrative_ontology:cs_drift_state('b0980e45-f61f-4597-bf5b-fef5020c31b2', post_war_on_terror_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b0980e45-f61f-4597-bf5b-fef5020c31b2', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__proportionality_balancing, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, detaining_state_security_apparatus).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, reviewing_courts).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, detainees_under_interrogation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, field_interrogators).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, field_interrogators).
narrative_ontology:constraint_vindicates(humane_treatment_standard__proportionality_balancing, judicial_case_by_case_competence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held in custody and subjected to interrogation techniques whose permissibility is decided after the fact by a court weighing security necessity against dignity harm. Cannot invoke a bright-line rule to stop a technique in progress; must wait for post-hoc adjudication, by which point harm has already occurred. Has no seat in the balancing test itself — it is conducted between the state and the reviewing court.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detainees_under_interrogation, payer,
    powerless, immediate, trapped, national).

% Designs and applies interrogation protocols, then justifies specific techniques after the fact by invoking the security side of the balancing test. Because the standard is a balancing test rather than a bright line, it retains substantial discretion to argue any given technique was proportionate to a claimed threat, and captures the benefit of that discretion directly in operational flexibility.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detaining_state_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, detaining_state_security_apparatus, beneficiary).

% Administers the balancing test case-by-case, weighing dignity against security claims after the fact. Gains institutional authority and discretion as the designated arbiter of what counts as proportionate; each ruling both resolves a specific case and expands the body of precedent the court alone interprets, entrenching its own gatekeeping role over the standard's meaning.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, reviewing_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, reviewing_courts, beneficiary).

% Operate under a standard that gives moderate constraint but no bright-line certainty about which techniques are permissible before the fact. Benefit from more latitude than an absolute-prohibition regime would allow, but bear legal and career risk when a court later rules a technique disproportionate — the ambiguity is a genuine cost as well as a genuine license.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, field_interrogators, payer,
    moderate, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, field_interrogators, beneficiary).

% Argue that any balancing test invites erosion of an intended non-derogable minimum; documents specific harms and advocates for the absolute-prohibition reading, but has no formal role inside the court's balancing analysis and can only submit briefs or public pressure from outside the adjudicating institutions.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, human_rights_monitors, excluded,
    organized, generational, constrained, global).

% Study the accumulation of case law under the balancing standard, comparing outcomes across jurisdictions and tracking whether the standard drifts toward permissiveness or toward a de facto bright line through repeated rulings.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__proportionality_balancing, diffuse).
narrative_ontology:fixing_cost_class(humane_treatment_standard__proportionality_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for adjudicating hard cases where security claims and dignity claims genuinely conflict, avoiding both the rigidity of an absolute rule that might block legitimate emergency action and the danger of unlimited executive discretion with no check at all.
% TRANSFER_FUNCTION: Moves the power to define permissible treatment from the detainee (who would hold an inviolable right under an absolute-prohibition reading) to the state (which frames the security interest) and the court (which adjudicates the balance), with the detainee bearing the interim harm while the balance is struck.
% ABSENT_VOICES: Detainees themselves have no procedural voice in the balancing test as it is conducted — they are the subject of the analysis, not a party to it. Human rights monitors document harms from outside the adjudicating structure and are not seated participants in individual proportionality determinations.
% DISAPPEARANCE_RATIONALE: If the proportionality-balancing reading disappeared, the kernel would resolve toward one of its siblings: either the absolute-prohibition reading would govern (foreclosing case-by-case security justification entirely) or the contextual-necessity reading would expand executive discretion unchecked by judicial balancing. Courts would lose their gatekeeping role, security services would either gain or lose latitude sharply, and the entire adjudicative apparatus built around case-by-case proportionality review would have no function to perform.
% FOUNDING_PROBLEM: Common Article 3 was drafted to establish a humane-treatment floor for non-international armed conflict without a full Geneva Convention apparatus, in a context where states resisted binding absolute constraints on their conduct toward captured combatants and irregular fighters.
% FOUNDING_PROBLEM_CORROBORATION: Reviewing courts and state security services attest the balancing framework remains necessary to reconcile competing legitimate interests in live security crises. Human rights monitors and a substantial body of international law scholarship — attesting from outside the state and judicial seats that benefit from continued discretion — argue the balancing reading has in practice licensed treatment the drafters intended to foreclose, making the founding problem's resolution contested rather than settled.
narrative_ontology:disappearance_verdict(humane_treatment_standard__proportionality_balancing, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__proportionality_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__proportionality_balancing, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(humane_treatment_standard__proportionality_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__proportionality_balancing, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__proportionality_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__proportionality_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is measured as moderate (0.52) because the balancing test does perform a genuine coordination function — reconciling two legitimate interests that an absolute rule or unlimited discretion would each fail to reconcile well — but it also allows the state's security framing to shape outcomes in individual detainees' cases with real cost accruing to them in the interim before any judicial correction. Suppression (0.48) reflects that detainees have no standing inside the balancing test itself; they experience the outcome, not the process. Theater ratio (0.42) captures a real and growing share of case law that performs rigorous balancing analysis while substantially deferring to state security claims, a pattern that has hardened over the measured interval as precedent accumulated.
 *
 * DIRECTIONALITY LOGIC:
 *   Detainees are the clearest structural target: they bear the immediate cost of whatever technique the balancing test ultimately validates, with no seat in the process that determines that outcome. The state security apparatus and the reviewing courts are joint beneficiaries of the discretion the balancing framework preserves — the state gains operational latitude, the courts gain (and entrench) an interpretive gatekeeping role. Field interrogators sit in an intermediate position: they get more latitude than an absolute rule would grant but carry personal legal risk from the standard's after-the-fact character.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is deliberately not resolved toward either pure coordination or pure extraction: the balancing function is real (it prevents both an unworkable blanket rule and an unchecked license), but it is sustained by active judicial and state enforcement and produces an identifiable victim class whose treatment is decided without their participation. Reading it as a pure rope would erase the detainee cost; reading it as a pure snare would erase the genuine adjudicative function courts perform relative to the contextual-necessity alternative. The seat divergence — state and court seats experience principled adjudication, detainee seats experience deferred harm under an unpredictable standard — is exactly the asymmetry the tangled_rope type is built to hold open rather than collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_test_capture_risk,
    'Does the proportionality-balancing reading, over successive rulings, drift toward the contextual-necessity reading in practice — with courts systematically deferring to state security claims — even while formally retaining the language of balancing?',
    'Longitudinal case law analysis tracking the ratio of rulings favoring security claims versus dignity claims across jurisdictions applying the balancing standard, compared against jurisdictions applying the absolute-prohibition standard.',
    'If deference dominates, the balancing reading functions as a slow-motion migration toward contextual_necessity, meaning its coordination claim is substantially theatrical; if dignity claims are meaningfully vindicated in a material share of cases, the coordination function is real and distinct from its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_test_capture_risk, empirical, 'Whether the balancing standard drifts toward de facto contextual necessity through accumulated deference.').

omega_variable(
    kernel_framing_underdetermination,
    'Is Common Article 3 itself better read as instantiating a genuine three-way live contest among readings, or does the treaty text and its drafting history more strongly support one reading as the historically intended one, with the others as later interpretive drift?',
    'Review of travaux préparatoires and the drafting history of the 1949 Geneva Conventions common articles, alongside subsequent ICRC commentary and ICTY/ICTR jurisprudence on the non-derogability of Common Article 3 protections.',
    'If drafting history strongly supports the absolute-prohibition reading as originally intended, the proportionality-balancing reading (and contextual-necessity) would need to be understood as a drift/erosion phenomenon rather than a co-equal alternative reading, which would change how this story''s founding_problem_status should be read against its siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three readings are genuinely co-equal or whether drafting history privileges the absolute-prohibition reading as canonical.').

omega_variable(
    detainee_exclusion_from_balancing_process,
    'Is the detainee''s total exclusion from the balancing test itself (as opposed to exclusion merely from the outcome) a fixable procedural gap, or is it structurally inherent to any after-the-fact judicial balancing model?',
    'Comparative study of procedural reforms (e.g., detainee counsel participation in classified proportionality hearings) and whether such reforms have been attempted or found workable in any jurisdiction applying this standard.',
    'If detainee participation is procedurally feasible and simply not implemented, the exclusion looks more like an extractable design choice than an unavoidable feature of balancing; this would raise the effective extraction reading of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(detainee_exclusion_from_balancing_process, conceptual, 'Whether detainee exclusion from the balancing process itself is a remediable design choice or structurally inherent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__proportionality_balancing, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__proportionality_balancing, theater_ratio, 0, 0.25).
narrative_ontology:measurement(huma_tr_t8, humane_treatment_standard__proportionality_balancing, theater_ratio, 8, 0.3).
narrative_ontology:measurement(huma_tr_t16, humane_treatment_standard__proportionality_balancing, theater_ratio, 16, 0.34).
narrative_ontology:measurement(huma_tr_t24, humane_treatment_standard__proportionality_balancing, theater_ratio, 24, 0.37).
narrative_ontology:measurement(huma_tr_t32, humane_treatment_standard__proportionality_balancing, theater_ratio, 32, 0.4).
narrative_ontology:measurement(huma_tr_t40, humane_treatment_standard__proportionality_balancing, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__proportionality_balancing, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(huma_be_t8, humane_treatment_standard__proportionality_balancing, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(huma_be_t16, humane_treatment_standard__proportionality_balancing, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(huma_be_t24, humane_treatment_standard__proportionality_balancing, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(huma_be_t32, humane_treatment_standard__proportionality_balancing, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(huma_be_t40, humane_treatment_standard__proportionality_balancing, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__proportionality_balancing, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(huma_su_t8, humane_treatment_standard__proportionality_balancing, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(huma_su_t16, humane_treatment_standard__proportionality_balancing, suppression_requirement, 16, 0.43).
narrative_ontology:measurement(huma_su_t24, humane_treatment_standard__proportionality_balancing, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(huma_su_t32, humane_treatment_standard__proportionality_balancing, suppression_requirement, 32, 0.47).
narrative_ontology:measurement(huma_su_t40, humane_treatment_standard__proportionality_balancing, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__proportionality_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__contextual_necessity).

% DUAL FORMULATION NOTE:
% Part of a three-member constraint family reading the humane_treatment_standard kernel (Common Article 3). absolute_prohibition treats the text as establishing non-derogable minimums (near-mountain within its own reading, low extraction, high accessibility collapse against security-necessity arguments). contextual_necessity treats the text as a defeasible baseline overridable by security imperative (highest extraction and suppression of the three, closest to snare). proportionality_balancing (this story) occupies the structural middle as tangled_rope: a genuine but actively-enforced coordination function between the two poles, with courts as the enforcing gatekeeper. Each story's ε and stakeholder structure is authored independently per the ε-invariance principle; they are linked here because they share a kernel and a beneficiary/victim network, not because they average to a single truth about Common Article 3.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
