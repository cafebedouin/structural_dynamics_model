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
 *   human_readable: Dollar-Gold Convertibility (Policy Flexible Reading)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This constraint represents the 'policy flexible' reading of dollar-gold
 *   convertibility, where the U.S. obligation to convert dollars to gold was
 *   understood to be conditional and subordinate to domestic economic
 *   stability. This interpretation allowed the U.S. to pursue expansionary
 *   monetary and fiscal policies, even at the risk of dollar devaluation,
 *   shifting the burden of adjustment to foreign dollar holders. This reading
 *   contrasts with a 'strict convertibility' view that emphasized the binding
 *   nature of the obligation and a 'Triffin dilemma' view that saw the system
 *   as inherently unstable.
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
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar-Gold Convertibility (Policy Flexible Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, '17434ad9-51ab-428e-a6e7-a736f0d4f7df').
narrative_ontology:cs_kernel_codification('17434ad9-51ab-428e-a6e7-a736f0d4f7df', formalized).
narrative_ontology:cs_authority_grounding('17434ad9-51ab-428e-a6e7-a736f0d4f7df', extraction).
narrative_ontology:cs_interpretation_layer_present('17434ad9-51ab-428e-a6e7-a736f0d4f7df').
narrative_ontology:cs_reading_relation('17434ad9-51ab-428e-a6e7-a736f0d4f7df', dollar_gold_convertibility__strict_convertibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('17434ad9-51ab-428e-a6e7-a736f0d4f7df', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('17434ad9-51ab-428e-a6e7-a736f0d4f7df', foundational, domestic_stability_priority).
narrative_ontology:cs_axiom_status(domestic_stability_priority, holdable).
narrative_ontology:cs_axiom_grounding('17434ad9-51ab-428e-a6e7-a736f0d4f7df', domestic_stability_priority, instrumental).
narrative_ontology:cs_axiom('17434ad9-51ab-428e-a6e7-a736f0d4f7df', foundational, convertibility_as_policy_tool).
narrative_ontology:cs_axiom_status(convertibility_as_policy_tool, holdable).
narrative_ontology:cs_axiom_grounding('17434ad9-51ab-428e-a6e7-a736f0d4f7df', convertibility_as_policy_tool, conventional).
narrative_ontology:cs_reference_frame('17434ad9-51ab-428e-a6e7-a736f0d4f7df', bretton_woods_original_intent_flexible).
narrative_ontology:cs_drift_state('17434ad9-51ab-428e-a6e7-a736f0d4f7df', post_vietnam_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('17434ad9-51ab-428e-a6e7-a736f0d4f7df', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, united_states_treasury).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, federal_reserve).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, international_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, domestic_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Prioritizes domestic economic stability and growth, viewing gold convertibility as a policy tool that can be adjusted or suspended if it conflicts with national interests. Benefits from monetary policy flexibility.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, united_states_treasury, agenda_setter,
    institutional, generational, mobile, national).

% Manages domestic monetary policy, including interest rates and money supply, with the understanding that convertibility is secondary to its dual mandate of maximum employment and price stability. Benefits from operational autonomy.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, federal_reserve, agenda_setter,
    institutional, biographical, mobile, national).

% Hold significant dollar reserves, expecting convertibility to gold at a fixed rate. They bear the risk of dollar devaluation if the U.S. prioritizes domestic policy over convertibility, leading to potential losses on their reserves. Their exit is constrained by the lack of viable alternative reserve currencies.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks, payer,
    organized, biographical, constrained, global).

% Invest in dollar-denominated assets, relying on the stability implied by convertibility. They face capital losses and reduced confidence if convertibility is suspended or the dollar devalued for domestic policy reasons. Their exit is constrained by market size and liquidity.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_investors, payer,
    powerful, immediate, constrained, global).

% Benefit from a stable domestic economy and flexible monetary policy that can support growth and employment, even if it means de-prioritizing external convertibility obligations. They are insulated from the direct costs of convertibility suspension.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, domestic_industries, beneficiary,
    organized, biographical, mobile, national).

% Monitors the international monetary system and provides policy recommendations. It observes the tension between U.S. domestic policy and convertibility obligations, but its enforcement power is limited.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for international monetary stability by establishing the dollar as the primary reserve currency, backed by a conditional gold convertibility promise, allowing for U.S. monetary policy flexibility to manage global economic shocks.
% TRANSFER_FUNCTION: Transfers the risk of dollar devaluation from the U.S. domestic economy to foreign central banks and international investors, in exchange for the perceived stability and liquidity of the dollar-based system.
% ABSENT_VOICES: Smaller developing nations, whose economies are highly dependent on dollar stability and who have less leverage to influence U.S. policy, are effectively excluded from the decision-making process regarding convertibility adjustments. They would advocate for stricter adherence to convertibility to protect their reserves and trade stability.
% DISAPPEARANCE_RATIONALE: If the policy-flexible interpretation of convertibility vanished, the U.S. would either be forced into strict adherence (constraining domestic policy) or explicitly abandon convertibility (triggering a global monetary crisis). The international financial system would undergo a significant, rapid reorganization.
% FOUNDING_PROBLEM: The post-WWII international monetary system needed a stable anchor currency to facilitate trade and investment, while allowing major economies some flexibility to manage domestic crises.
% FOUNDING_PROBLEM_CORROBORATION: Economists and historians outside the U.S. Treasury and Federal Reserve corroborate that the tension between international obligations and domestic policy autonomy remains a live problem in international monetary relations, even after the formal end of gold convertibility.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__policy_flexible_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__policy_flexible_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.65) is high because the U.S. could effectively devalue foreign dollar holdings by prioritizing domestic policy, imposing a cost on external creditors. Suppression (0.70) is high because foreign central banks had limited alternatives to holding dollars as reserves, making them captive to U.S. policy choices. Theater ratio (0.40) reflects the growing gap between the stated commitment to convertibility and the actual practice of prioritizing domestic policy, with the 'convertibility' aspect becoming increasingly performative as the system approached its breaking point in 1971. The metrics show a clear trend of increasing extractiveness and suppression as the U.S. leveraged its position.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. perspective, this reading enabled necessary domestic policy flexibility for global stability. From the perspective of foreign dollar holders, it represented an increasingly extractive arrangement where their reserves were at risk due to U.S. unilateral action. The engine's classification will reflect this divergence based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. Treasury and Federal Reserve are the primary beneficiaries, gaining monetary policy autonomy. Foreign central banks and international investors are the victims, bearing the risk of devaluation and having constrained exit options due to the dollar's reserve currency status. Domestic industries benefit from the stable domestic economy enabled by flexible policy. The IMF acts as an observer, monitoring the system without direct enforcement power over U.S. policy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_vs_international_priority,
    'To what extent was the U.S. prioritization of domestic economic stability over convertibility a necessary response to global economic conditions, versus a strategic choice to leverage its monetary power?',
    'Counterfactual historical analysis comparing outcomes under alternative policy regimes, and declassified government archives revealing internal policy debates and motivations.',
    'If primarily a strategic choice, the extractiveness of this reading is higher and more intentional. If primarily a necessary response, the extractiveness is a byproduct of systemic constraints, making it closer to a Tangled Rope with less intentionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_vs_international_priority, conceptual, 'Ambiguity regarding the intentionality and necessity of U.S. policy choices.').

omega_variable(
    foreign_central_bank_exit_options,
    'How constrained were foreign central banks'' exit options from dollar reserves, given the lack of viable alternative reserve currencies during this period?',
    'Historical analysis of alternative reserve asset availability and liquidity, and diplomatic records detailing attempts by foreign nations to diversify reserves.',
    'If exit options were extremely limited, the suppression metric for foreign central banks is higher, reinforcing the Snare-like aspects of the Tangled Rope. If more viable alternatives existed, suppression is lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foreign_central_bank_exit_options, empirical, 'Degree of constraint on foreign central banks'' ability to exit dollar reserves.').

omega_variable(
    convertibility_as_cover_story,
    'At what point did the ''convertibility'' aspect of the system become primarily a cover story for U.S. monetary autonomy, rather than a genuine, albeit conditional, commitment?',
    'Analysis of official communications versus internal policy documents, and the timing of major policy shifts relative to external pressures on convertibility.',
    'If it became primarily a cover story earlier than 1971, the theater_ratio and extractiveness metrics should be higher for earlier time points, pushing the constraint closer to a Snare earlier in its lifecycle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convertibility_as_cover_story, empirical, 'The point at which convertibility became primarily performative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(doll_tr_t1955, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1955, 0.2).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1971, 0.4).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1944, 0.4).
narrative_ontology:measurement(doll_be_t1955, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1955, 0.5).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1971, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1944, 0.45).
narrative_ontology:measurement(doll_su_t1955, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1955, 0.55).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1965, 0.65).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1971, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, global_infrastructure).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, triffin_structural_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dollar_gold_convertibility' kernel. This 'policy flexible' reading emphasizes U.S. monetary autonomy, contrasting with the 'strict convertibility' reading (binding obligation) and the 'Triffin structural' reading (inherent instability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
