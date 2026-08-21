% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__policy_flexible_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__policy_flexible_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__policy_flexible_reading
 *   human_readable: Dollar Gold Convertibility (Policy Flexible Reading)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This constraint story models the 'policy flexible' reading of dollar-gold
 *   convertibility, prevalent among U.S. policymakers during the Bretton
 *   Woods era. In this reading, the U.S. obligation to convert dollars to
 *   gold was conditional and subordinate to the needs of domestic economic
 *   stability. This allowed the U.S. to pursue expansionary monetary
 *   policies, even at the risk of dollar devaluation, shifting the burden of
 *   adjustment to foreign dollar holders. The constraint is claimed as a Rope
 *   by its proponents (a coordination mechanism), but its operation, as
 *   described by the metrics, reveals it to be a Tangled Rope due to the
 *   asymmetric extraction from foreign central banks and investors.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.65).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.7).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar Gold Convertibility (Policy Flexible Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, 'cddbe2d5-ef39-4462-910b-85a43ed3b2e6').
narrative_ontology:cs_kernel_codification('cddbe2d5-ef39-4462-910b-85a43ed3b2e6', formalized).
narrative_ontology:cs_authority_grounding('cddbe2d5-ef39-4462-910b-85a43ed3b2e6', extraction).
narrative_ontology:cs_interpretation_layer_present('cddbe2d5-ef39-4462-910b-85a43ed3b2e6').
narrative_ontology:cs_reading_relation('cddbe2d5-ef39-4462-910b-85a43ed3b2e6', dollar_gold_convertibility__strict_convertibility_reading, influences).
narrative_ontology:cs_reading_relation('cddbe2d5-ef39-4462-910b-85a43ed3b2e6', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('cddbe2d5-ef39-4462-910b-85a43ed3b2e6', foundational, domestic_stability_priority).
narrative_ontology:cs_axiom_status(domestic_stability_priority, holdable).
narrative_ontology:cs_axiom_grounding('cddbe2d5-ef39-4462-910b-85a43ed3b2e6', domestic_stability_priority, instrumental).
narrative_ontology:cs_axiom('cddbe2d5-ef39-4462-910b-85a43ed3b2e6', foundational, conditional_convertibility_obligation).
narrative_ontology:cs_axiom_status(conditional_convertibility_obligation, holdable).
narrative_ontology:cs_axiom_grounding('cddbe2d5-ef39-4462-910b-85a43ed3b2e6', conditional_convertibility_obligation, conventional).
narrative_ontology:cs_reference_frame('cddbe2d5-ef39-4462-910b-85a43ed3b2e6', us_monetary_autonomy_framework).
narrative_ontology:cs_drift_state('cddbe2d5-ef39-4462-910b-85a43ed3b2e6', post_vietnam_war_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cddbe2d5-ef39-4462-910b-85a43ed3b2e6', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_monetary_authorities).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_domestic_economy).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, international_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains the flexibility to prioritize domestic economic stability over the strict convertibility of the dollar to gold. This allows for monetary policy adjustments (e.g., devaluation) to manage unemployment or inflation, even if it means suspending or altering convertibility. Benefits from increased policy autonomy.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_monetary_authorities, agenda_setter,
    institutional, biographical, mobile, national).

% Benefits from the ability of U.S. monetary authorities to use policy tools to stabilize the domestic economy, avoiding deflationary pressures or recessions that might arise from strict adherence to convertibility. This comes at the potential cost of international confidence in the dollar.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_domestic_economy, beneficiary,
    organized, biographical, constrained, national).

% Hold significant dollar reserves, which are subject to devaluation risk if the U.S. prioritizes domestic policy over convertibility. They are constrained by the dollar's role as the primary reserve currency and medium of international exchange, limiting their ability to exit dollar holdings without systemic disruption.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks, payer,
    institutional, generational, constrained, global).

% Invest in dollar-denominated assets and face potential losses from dollar devaluation if convertibility is suspended or altered. Their exit options are constrained by the size and liquidity of dollar markets, making large-scale divestment difficult without incurring significant costs.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_investors, payer,
    powerful, immediate, constrained, global).

% Monitors the international monetary system and provides policy advice. From this reading, the IMF observes the tension between national policy autonomy and international monetary stability, potentially advocating for reforms that balance these objectives.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for international monetary stability by linking the dollar to gold, while allowing the dominant reserve currency issuer (U.S.) flexibility to manage its domestic economy, thereby preventing global deflationary spirals.
% TRANSFER_FUNCTION: Transfers the risk of monetary instability (devaluation) from the U.S. domestic economy to foreign holders of dollar reserves, in exchange for the U.S. providing global liquidity and a stable (though conditionally convertible) reserve asset.
% ABSENT_VOICES: Smaller nations with limited economic leverage, who are highly dependent on the stability of the dollar for their trade and reserves, would argue for stricter adherence to convertibility to protect their assets from U.S. policy shifts. Their voices are often marginalized in discussions dominated by major economic powers.
% DISAPPEARANCE_RATIONALE: If the conditional convertibility framework vanished overnight, the international monetary system would face immediate and severe instability. Foreign central banks would lose confidence in dollar reserves, leading to a scramble for alternative assets, massive capital flight, and a collapse of the existing global financial architecture. A new, likely more fragmented, system would emerge.
% FOUNDING_PROBLEM: The post-WWII international monetary system needed a stable anchor for exchange rates and a mechanism to provide global liquidity, while avoiding the competitive devaluations and 'beggar-thy-neighbor' policies of the interwar period.
% FOUNDING_PROBLEM_CORROBORATION: U.S. monetary authorities attest that the problem of balancing domestic stability with international obligations remains live, justifying policy flexibility. Foreign central banks and some international economists (outside the U.S. beneficiary set) argue that the 'problem' has shifted to one of U.S. monetary dominance and the inherent instability of a reserve currency issuer having such flexibility, leading to the 'Triffin dilemma'.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__policy_flexible_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__policy_flexible_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dollar_gold_convertibility__policy_flexible_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__policy_flexible_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the U.S.'s ability to export inflation and devaluation risk, effectively taxing foreign dollar holders. Suppression (0.70) is high because foreign central banks had limited alternatives to holding dollars as reserves, making them 'trapped' in the system. The theater ratio (0.40) indicates that while the formal commitment to convertibility was maintained, the actual policy flexibility often undermined its practical effect, leading to a growing gap between rhetoric and reality, especially as U.S. gold reserves dwindled relative to dollar liabilities. The rising extractiveness and suppression over time reflect the increasing strain on the system and the growing reliance on U.S. policy discretion.
 *
 * PERSPECTIVAL GAP:
 *   U.S. monetary authorities experienced this as a necessary flexibility for global stability and domestic welfare (a Rope), while foreign central banks and international investors increasingly experienced it as an extractive mechanism (a Snare or Tangled Rope) due to their exposure to devaluation risk and limited exit options. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. monetary authorities and the U.S. domestic economy are beneficiaries (low d) as they gain policy autonomy and stability. Foreign central banks and international investors are victims (high d) as they bear the costs of devaluation risk and have constrained exit options. The constraint subsidizes the U.S. by allowing it to run current account deficits without immediate gold outflows, effectively transferring resources from external creditors.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the constraint as a pure Rope by highlighting the asymmetric extraction inherent in the 'flexibility.' It also avoids the Piton trap by showing that the constraint was actively enforced and provided substantial benefits to the U.S., even as its coordination function for the rest of the world degraded. The 'contested' status of the founding problem further supports the Tangled Rope classification, indicating that the original coordination rationale was increasingly overshadowed by extractive dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_vs_legal_obligation,
    'Was the U.S. commitment to dollar-gold convertibility primarily a policy choice, or a binding legal obligation under international law?',
    'Analysis of international legal precedents, treaty interpretations, and historical diplomatic communications regarding Article IV of the IMF Articles of Agreement. Examination of U.S. domestic legal frameworks and executive branch pronouncements.',
    'If primarily a policy choice, this ''policy flexible'' reading is strengthened, emphasizing U.S. monetary autonomy. If a binding legal obligation, the ''strict convertibility'' reading gains force, reclassifying the U.S. as an agenda-setter constrained by international law, and increasing the perceived ''suppression'' on U.S. policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_vs_legal_obligation, conceptual, 'Ambiguity regarding the legal vs. policy nature of convertibility.').

omega_variable(
    devaluation_risk_quantification,
    'What was the precise magnitude of devaluation risk borne by foreign central banks and international investors due to U.S. policy flexibility?',
    'Detailed econometric analysis of historical dollar exchange rate movements, gold price fluctuations, and U.S. monetary policy decisions, correlated with foreign reserve holdings and investment portfolios.',
    'A higher quantified risk would increase the ''extractiveness'' metric for foreign dollar holders, potentially pushing their seat classification further towards Snare. A lower risk might suggest a more balanced, albeit still asymmetric, coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(devaluation_risk_quantification, empirical, 'Quantification of devaluation risk for dollar holders.').

omega_variable(
    triffin_dilemma_inevitability,
    'Was the collapse of convertibility (and thus the policy flexibility reading) an inevitable outcome of the Triffin Dilemma, or a consequence of specific U.S. policy choices?',
    'Counterfactual historical analysis exploring alternative U.S. and international policy paths. Economic modeling of the Bretton Woods system under different assumptions about global liquidity needs and U.S. balance of payments.',
    'If inevitable, the ''Triffin structural'' reading is strengthened, suggesting the system was a Snare from its inception due to inherent design flaws. If contingent on U.S. choices, this ''policy flexible'' reading remains a valid description of a chosen (and extractive) policy stance, rather than a system-level inevitability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(triffin_dilemma_inevitability, conceptual, 'Whether the system''s collapse was inevitable or policy-driven.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(doll_tr_t1950, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1958, 0.3).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1965, 0.35).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1971, 0.4).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1944, 0.4).
narrative_ontology:measurement(doll_be_t1950, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1950, 0.48).
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1958, 0.55).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1971, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1944, 0.5).
narrative_ontology:measurement(doll_su_t1950, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1958, 0.65).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1965, 0.68).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1971, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, triffin_structural_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dollar_gold_convertibility' kernel. This 'policy_flexible_reading' emphasizes U.S. monetary autonomy, contrasting with the 'strict_convertibility_reading' (binding legal obligation) and the 'triffin_structural_reading' (inherent unsustainability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
