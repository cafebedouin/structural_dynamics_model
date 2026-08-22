% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__creditor_discipline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__creditor_discipline_reading, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__creditor_discipline_reading
 *   human_readable: Gold-Fiat Transition: Elimination of Creditor Discipline
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This constraint story analyzes the gold-fiat transition mechanism from
 *   the perspective of 'creditor discipline'. It argues that the shift from a
 *   gold-backed to a fiat monetary system, particularly after the Nixon Shock
 *   in 1971, fundamentally altered the balance of power in international
 *   finance. Specifically, it eliminated the ability of creditor nations to
 *   discipline debtor nations (especially the reserve currency issuer)
 *   through the threat of demanding gold redemption for their currency
 *   holdings. This enabled greater fiscal flexibility for debtor nations but
 *   also concentrated geopolitical power in the hands of the reserve currency
 *   issuer, effectively removing a key external constraint on its own
 *   economic policy. The constraint is claimed as a snare because the
 *   coordination story (flexible monetary policy) is seen as cover for a
 *   significant, asymmetric transfer of power and wealth.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.85).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.9).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, snare).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Gold-Fiat Transition: Elimination of Creditor Discipline").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, '74abc922-628d-4511-adb5-d75e5532568a').
narrative_ontology:cs_kernel_codification('74abc922-628d-4511-adb5-d75e5532568a', formalized).
narrative_ontology:cs_authority_grounding('74abc922-628d-4511-adb5-d75e5532568a', extraction).
narrative_ontology:cs_interpretation_layer_present('74abc922-628d-4511-adb5-d75e5532568a').
narrative_ontology:cs_reading_relation('74abc922-628d-4511-adb5-d75e5532568a', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('74abc922-628d-4511-adb5-d75e5532568a', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('74abc922-628d-4511-adb5-d75e5532568a', foundational, gold_redemption_threat_as_discipline).
narrative_ontology:cs_axiom_status(gold_redemption_threat_as_discipline, holdable).
narrative_ontology:cs_axiom_grounding('74abc922-628d-4511-adb5-d75e5532568a', gold_redemption_threat_as_discipline, conventional).
narrative_ontology:cs_axiom('74abc922-628d-4511-adb5-d75e5532568a', foundational, fiscal_flexibility_as_national_sovereignty).
narrative_ontology:cs_axiom_status(fiscal_flexibility_as_national_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('74abc922-628d-4511-adb5-d75e5532568a', fiscal_flexibility_as_national_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('74abc922-628d-4511-adb5-d75e5532568a', post_bretton_woods_fiat_system).
narrative_ontology:cs_drift_state('74abc922-628d-4511-adb5-d75e5532568a', contemporary_global_finance, gap(stable, minor, true)).
narrative_ontology:cs_created_at('74abc922-628d-4511-adb5-d75e5532568a', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained significant fiscal flexibility and policy autonomy by no longer facing the threat of gold redemption runs, which previously disciplined their spending and balance of payments. They can now run larger deficits without immediate external constraint.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations, beneficiary,
    powerful, generational, arbitrage, global).

% The primary beneficiary, gaining immense geopolitical leverage and 'exorbitant privilege' by issuing the world's reserve currency without gold backing. This allows it to finance deficits more easily and exert influence through monetary policy, effectively eliminating external discipline on its own fiscal policy.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Lost their primary mechanism for disciplining debtor nations: the threat of demanding gold redemption for their dollar holdings. This reduced their leverage in international finance and trade, forcing them to accept fiat currency without the same level of external accountability.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations, payer,
    organized, generational, constrained, global).

% Observe and analyze the new monetary regime, adapting their lending and policy recommendations to a world without gold-backed currency. Their role shifted from enforcing gold-standard discipline to managing fiat currency flows and crises.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, international_financial_institutions, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The transition coordinated the global monetary system around a new, flexible standard no longer tied to a physical commodity, theoretically allowing for more responsive economic policy and preventing deflationary spirals.
% TRANSFER_FUNCTION: Transferred significant economic and political power from creditor nations (who previously held the 'veto' via gold redemption) to debtor nations, particularly the issuer of the reserve currency, by removing a hard external constraint on fiscal and monetary policy.
% ABSENT_VOICES: Advocates for a return to a gold standard or other commodity-backed money, who would argue for external discipline on government spending and against the inflationary potential of fiat currency, are largely marginalized in mainstream policy discourse.
% DISAPPEARANCE_RATIONALE: The gold-fiat transition mechanism itself, as a historical event, cannot 'disappear'. Its effects are embedded in the current global monetary system. If the historical shift had not occurred, the world would be fundamentally different, but the mechanism itself is a past event.
% FOUNDING_PROBLEM: The gold standard imposed rigid constraints on national monetary policy, leading to deflationary pressures and limiting governments' ability to respond to economic crises or finance public goods, particularly for debtor nations.
% FOUNDING_PROBLEM_CORROBORATION: Economists and policymakers widely corroborate the problem of gold standard rigidity. However, the 'solution' (fiat currency) is contested by those who argue it merely shifted the problem from external discipline to internal political discipline, leading to different forms of instability. Independent historical analysis and economic modeling support the initial problem statement.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_unchanged).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__creditor_discipline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85-0.90) because the mechanism fundamentally shifted the cost of fiscal indiscipline from the debtor (via gold outflows) to the creditor (via accepting fiat currency without recourse). Suppression is also high (0.90) because the new system is enforced by the legal tender laws and the institutional power of central banks, with no viable alternative for creditor nations to re-impose gold-based discipline. Theater ratio is low (0.05) as the mechanism's function is direct and not performative; it achieved its goal of removing the gold constraint. Accessibility collapse is high (0.95) as the option of demanding gold redemption was effectively eliminated for creditor nations. Resistance is low (0.10) because, while some advocate for a return to gold, the current system is deeply entrenched and largely uncontested by major institutional actors.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of debtor nations and the reserve currency issuer, the transition was a necessary evolution towards a more flexible and stable global economy (a 'rope' or 'scaffold'). From the perspective of creditor nations, it was a structural expropriation of their disciplinary power, forcing them into a system where their claims could be diluted by inflation (a 'snare'). This story adopts the latter, more critical, reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The reserve currency issuer and debtor nations are clear beneficiaries, experiencing low directionality as the constraint subsidizes their fiscal flexibility. Creditor nations are the primary targets, experiencing high directionality as they lost a critical leverage point. The mechanism fundamentally re-ordered the global financial hierarchy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creditor_discipline_vs_fiscal_flexibility,
    'Was the elimination of creditor veto power a necessary step towards global fiscal flexibility, or an unjustified transfer of power that enabled unchecked spending?',
    'Long-term comparative analysis of economic stability and growth in gold-standard vs. fiat-standard eras, accounting for other confounding factors. Examination of the political economy of fiscal policy in both regimes.',
    'If deemed necessary for flexibility, the extractiveness might be re-evaluated as a ''cost of coordination''. If unjustified, the snare classification is reinforced, highlighting the power transfer as the primary outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_discipline_vs_fiscal_flexibility, preference, 'Whether the trade-off between creditor discipline and fiscal flexibility was beneficial.').

omega_variable(
    reserve_currency_issuer_accountability,
    'To what extent has the reserve currency issuer''s increased fiscal flexibility, enabled by the transition, been offset by new forms of accountability or self-discipline?',
    'Empirical study of the reserve currency issuer''s fiscal and monetary policy decisions post-1971, and the emergence of new international norms or institutions that might impose soft constraints.',
    'If new forms of accountability are significant, the effective extractiveness from creditor nations might be lower than initially assessed. If not, the concentration of power remains a key feature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reserve_currency_issuer_accountability, empirical, 'New forms of accountability for the reserve currency issuer.').

omega_variable(
    kernel_reading_creditor_discipline,
    'Is this constraint a genuine historical mechanism, or is it an interpretation of a broader transition that overemphasizes the ''creditor discipline'' aspect?',
    'Comparative historical analysis with other readings of the gold-fiat transition, assessing the empirical weight of the ''creditor discipline'' narrative against ''automatic constraint'' or ''composite overdetermination'' explanations.',
    'If this reading is found to be a partial or overemphasized account, its classification might be re-contextualized within a broader ''composite overdetermination'' framework, potentially reducing its standalone extractiveness if the power transfer is seen as one of many effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_creditor_discipline, conceptual, 'This constraint is the ''creditor_discipline_reading'' of the ''gold_fiat_transition_mechanism'' kernel. It focuses on the geopolitical power shift and the elimination of external fiscal discipline. Sibling readings include ''automatic_constraint_reading'' (focus on physical limits to money creation) and ''composite_overdetermination_reading'' (focus on multiple, converging causes beyond a single mechanism).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1971, 0.05).
narrative_ontology:measurement(gold_tr_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(gold_tr_t1990, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(gold_tr_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(gold_tr_t2010, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(gold_tr_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1971, 0.85).
narrative_ontology:measurement(gold_be_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1980, 0.86).
narrative_ontology:measurement(gold_be_t1990, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1990, 0.87).
narrative_ontology:measurement(gold_be_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement(gold_be_t2010, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2010, 0.89).
narrative_ontology:measurement(gold_be_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1971, 0.9).
narrative_ontology:measurement(gold_su_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1980, 0.9).
narrative_ontology:measurement(gold_su_t1990, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1990, 0.9).
narrative_ontology:measurement(gold_su_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(gold_su_t2010, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2010, 0.9).
narrative_ontology:measurement(gold_su_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__creditor_discipline_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'gold_fiat_transition_mechanism' kernel, focusing on the elimination of creditor discipline. Other readings exist, such as the 'automatic_constraint_reading' and 'composite_overdetermination_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
