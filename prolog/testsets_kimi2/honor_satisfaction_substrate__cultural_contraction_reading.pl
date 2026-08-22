% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__cultural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__cultural_contraction_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: honor_satisfaction_substrate__cultural_contraction_reading
 *   human_readable: Honor Satisfaction Substrate â Cultural Contraction Reading
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story instantiates the cultural_contraction_reading of
 *   the contested honor_satisfaction_substrate kernel. In this reading, the
 *   honor code is not merely suppressed by exogenous legal enforcement;
 *   rather, the interpretive substrate itself undergoes foundational
 *   transformation. As 'cultures of honor' give way to 'cultures of dignity,'
 *   dueling exits the thinkable action-set because the social and cognitive
 *   substrate that made it intelligible disintegrates. The constraint
 *   operates structurally like a mountainâdefining what is thinkable rather
 *   than enforcing compliance through active coercionâbut it is a mountain
 *   undergoing severe erosion. The authored metrics describe this erosion
 *   trajectory: high extraction at the interval's start (lives and
 *   status-anxiety under the honor regime) declining to residual levels as
 *   dignity culture becomes hegemonic.
 *
 * KEY AGENTS:
 *   - aristocratic_men: Primary beneficiary and secondary payer (powerful/identity_locked) â collect status and deference through the honor code substrate while bearing dueling risk
 *   - honor_compelled_actors: Primary target (moderate/identity_locked) â bear the physical costs and mortality risk of compelled dueling
 *   - women_and_commoners: Excluded population (powerless/trapped) â subject to honor-system externalities without standing or voice
 *   - historical_sociologists: Analytical observer (analytical/analytical) â document the substrate's erosion and transformation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, 0.15).
domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, 0.05).
domain_priors:theater_ratio(honor_satisfaction_substrate__cultural_contraction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__cultural_contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_substrate__cultural_contraction_reading, "Honor Satisfaction Substrate â Cultural Contraction Reading").
narrative_ontology:topic_domain(honor_satisfaction_substrate__cultural_contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__cultural_contraction_reading, '4e8c55d4-0181-4065-81e8-d50ea47d9dc5').
narrative_ontology:cs_kernel_codification('4e8c55d4-0181-4065-81e8-d50ea47d9dc5', distributed).
narrative_ontology:cs_authority_grounding('4e8c55d4-0181-4065-81e8-d50ea47d9dc5', practice).
narrative_ontology:cs_interpretation_layer_present('4e8c55d4-0181-4065-81e8-d50ea47d9dc5').
narrative_ontology:cs_reading_relation('4e8c55d4-0181-4065-81e8-d50ea47d9dc5', honor_satisfaction_substrate__practice_decline_reading, forecloses).
narrative_ontology:cs_reading_relation('4e8c55d4-0181-4065-81e8-d50ea47d9dc5', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('4e8c55d4-0181-4065-81e8-d50ea47d9dc5', foundational, cultural_substrate_constitutes_action).
narrative_ontology:cs_axiom_status(cultural_substrate_constitutes_action, holdable).
narrative_ontology:cs_axiom_grounding('4e8c55d4-0181-4065-81e8-d50ea47d9dc5', cultural_substrate_constitutes_action, empirically_contingent).
narrative_ontology:cs_axiom('4e8c55d4-0181-4065-81e8-d50ea47d9dc5', foundational, endogenous_transformation_primary).
narrative_ontology:cs_axiom_status(endogenous_transformation_primary, holdable).
narrative_ontology:cs_axiom_grounding('4e8c55d4-0181-4065-81e8-d50ea47d9dc5', endogenous_transformation_primary, empirically_contingent).
narrative_ontology:cs_reference_frame('4e8c55d4-0181-4065-81e8-d50ea47d9dc5', honor_culture_hegemony).
narrative_ontology:cs_drift_state('4e8c55d4-0181-4065-81e8-d50ea47d9dc5', dignity_culture_ascendant, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('4e8c55d4-0181-4065-81e8-d50ea47d9dc5', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, aristocratic_men).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, honor_compelled_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, aristocratic_men).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their social identity and public standing are constituted through the honor code; they receive deference and status protection within a bounded aristocratic sphere, and they incur the risk of being challenged to deadly combat when that status is questioned.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, aristocratic_men, beneficiary,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__cultural_contraction_reading, aristocratic_men, payer).

% Men of gentle or lower-aristocratic birth who must accept challenges and fight to maintain their standing within the honor-bound status order, bearing the physical injuries, mortality, and psychological burden of the satisfaction mechanism.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, honor_compelled_actors, payer,
    moderate, biographical, identity_locked, national).

% Situated outside the honor-bearing status; they are subject to the violent fallout of dueling and the hierarchical subordination it enforces, but lack standing to challenge insults or participate in the code's deliberation.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, women_and_commoners, excluded,
    powerless, generational, trapped, local).

% Study the long-term transition from honor cultures to dignity cultures, tracing how the interpretive substrate that made dueling thinkable eroded and was replaced by legal-bureaucratic status mechanisms.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social status hierarchy among armed elite men by providing an unambiguous, extra-legal mechanism for resolving insults and maintaining reputational order without centralized state intervention.
% TRANSFER_FUNCTION: Moves physical risk, injury, and death costs to individual honor-bearing men; moves status security and collective deference to the aristocratic class as a whole.
% ABSENT_VOICES: Women, commoners, and religious pacifists were structurally excluded from the honor-bearing status and its deliberations; they bore secondary costs of the violence but had no standing to challenge the code or articulate alternatives.
% DISAPPEARANCE_RATIONALE: If the honor substrate vanished, aristocratic masculine identity would lose its constitutive grammar, dueling would become unintelligible rather than merely illegal, and status competition would migrate to bureaucratic and economic domains.
% FOUNDING_PROBLEM: How to maintain a coherent, self-enforcing status hierarchy among armed elite men in the absence of strong centralized state courts capable of resolving reputation disputes.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists and legal historians (Elias, Pinker, Cooney) attest from outside the aristocratic beneficiary seat that the problem of aristocratic feuding was resolved by state formation and dignity culture, not by the honor code's own persistence.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__cultural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__cultural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__cultural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 0.15, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is authored high at T=0 (0.72) because the honor code extracts lives, physical safety, and emotional peace from those bound by it; by T=100 it has fallen to 0.15 because the substrate has eroded and few remain bound. Suppression is consistently low (0.05) because this reading frames the constraint as mountain erosion, not as a coercively enforced arrangement; the decline is endogenous cultural transformation, not suppression intensification. Theater ratio remains low throughout (0.05â0.15) because the erosion is genuineâthere is no theatrical maintenance of a function that has actually atrophied. Accessibility collapse starts very high (0.9) and falls to 0.25 as dignity alternatives become cognitively available. Resistance stays near zero because a mountain does not meet active resistance; it simply loses relevance as the social world reorganizes around alternative status logics.
 *
 * PERSPECTIVAL GAP:
 *   The aristocratic beneficiary seat experiences the honor code as constitutive of realityâits rules are as natural as gravityâwhereas the honor-compelled payer seat experiences the same structure as a life-threatening burden. The engine computes this divergence from the structural data: both share identity_locked exit, but the beneficiary role drives directionality toward the subsidy end while the payer role drives it toward extraction. The analytical seat sees the whole trajectory as historical substrate erosion.
 *
 * DIRECTIONALITY LOGIC:
 *   aristocratic_men are declared beneficiaries (low d, damped extraction) because the honor code subsidizes their status position and collective deference. honor_compelled_actors are declared victims (high d, amplified extraction) because they bear the concentrated physical costs of the satisfaction mechanism with no compensatory status gain. The derivation chain needs no override: the beneficiary/victim declarations plus identity_locked exit produce the appropriate asymmetry automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâmaintaining aristocratic order without centralized courtsâwas solved by the rise of dignity culture and state institutions. The honor code's mandate is dead, and by the interval's end the constraint has eroded to near-irrelevance. Declaring mandatrophy_resolved prevents misclassification as a snare (which would require active coercion) or a piton (which would require theatrical maintenance). The residual low extraction at T=100 is cultural lag, not functional operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_culture,
    'Is the honor code a genuine natural-cultural substrate (like language or kinship) or a constructed hierarchy primarily benefiting aristocratic men?',
    'Comparative anthropology across honor cultures and class structures; detection of systematic beneficiary concentration.',
    'If the constraint is constructed rather than natural, the false_summit_mountain signature fires and the engine reclassifies away from mountainâlikely toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_culture, conceptual, 'Whether the honor code is a genuine natural law or a constructed false summit').

omega_variable(
    substrate_or_suppression,
    'Did dueling decline primarily because the cultural substrate eroded (mountain erosion) or because state enforcement actively suppressed the practice?',
    'Historical analysis comparing legal prosecution rates and institutional barriers against cultural attitude evidence (memoirs, correspondence, conduct literature) in the same periods.',
    'If exogenous enforcement was primary, the constraint should be classified as actively enforced (tangled_rope or snare) rather than as eroding mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_or_suppression, empirical, 'Endogenous cultural erosion versus exogenous suppression as the driver of decline').

omega_variable(
    identity_lock_mechanism,
    'Was the honor code''s binding force primarily identity-fusion (aristocratic identity constituted by honor) or rational status calculation?',
    'Analysis of personal correspondence, memoirs, and conduct manuals for emotive/identity-based versus instrumental language around dueling and insult.',
    'If identity-fusion, exit was structurally impossible (identity_locked); if instrumental, exit was merely costly (constrained), altering the effective extraction computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Identity-fusion versus instrumental compliance in honor-culture binding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__cultural_contraction_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hono_tr_t20, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement(hono_tr_t40, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement(hono_tr_t60, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(hono_tr_t80, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 80, 0.12).
narrative_ontology:measurement(hono_tr_t100, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(hono_be_t20, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(hono_be_t40, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(hono_be_t60, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(hono_be_t80, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 80, 0.25).
narrative_ontology:measurement(hono_be_t100, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 100, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(honor_satisfaction_substrate__cultural_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
