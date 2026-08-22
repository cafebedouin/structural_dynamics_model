% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__neoliberal_convertibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__neoliberal_convertibility, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: bretton_woods_treaty_substrate__neoliberal_convertibility
 *   human_readable: Bretton Woods: Neoliberal Convertibility Reading
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This constraint represents the 'neoliberal convertibility' reading of the
 *   Bretton Woods system, particularly after the collapse of fixed exchange
 *   rates in the early 1970s. In this reading, Bretton Woods is interpreted
 *   as a framework that, over time, increasingly prioritized free capital
 *   markets and limited government intervention, leading to constraints on
 *   national policy autonomy. The claimed type is 'tangled_rope' because it
 *   offers a coordination function (global financial integration) but with
 *   significant asymmetric extraction (from national policy space to
 *   international finance).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.7).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.65).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.7).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods: Neoliberal Convertibility Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, '6c54d5bd-7a99-4d4d-9e74-98dc0cf9889a').
narrative_ontology:cs_kernel_codification('6c54d5bd-7a99-4d4d-9e74-98dc0cf9889a', formalized).
narrative_ontology:cs_authority_grounding('6c54d5bd-7a99-4d4d-9e74-98dc0cf9889a', lineage).
narrative_ontology:cs_interpretation_layer_present('6c54d5bd-7a99-4d4d-9e74-98dc0cf9889a').
narrative_ontology:cs_reading_relation('6c54d5bd-7a99-4d4d-9e74-98dc0cf9889a', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, influences).
narrative_ontology:cs_reading_relation('6c54d5bd-7a99-4d4d-9e74-98dc0cf9889a', bretton_woods_treaty_substrate__sovereignty_defense, influences).
narrative_ontology:cs_axiom('6c54d5bd-7a99-4d4d-9e74-98dc0cf9889a', foundational, capital_mobility_is_efficient).
narrative_ontology:cs_axiom_status(capital_mobility_is_efficient, holdable).
narrative_ontology:cs_axiom_grounding('6c54d5bd-7a99-4d4d-9e74-98dc0cf9889a', capital_mobility_is_efficient, empirically_contingent).
narrative_ontology:cs_axiom('6c54d5bd-7a99-4d4d-9e74-98dc0cf9889a', foundational, government_intervention_distorts_markets).
narrative_ontology:cs_axiom_status(government_intervention_distorts_markets, holdable).
narrative_ontology:cs_axiom_grounding('6c54d5bd-7a99-4d4d-9e74-98dc0cf9889a', government_intervention_distorts_markets, empirically_contingent).
narrative_ontology:cs_reference_frame('6c54d5bd-7a99-4d4d-9e74-98dc0cf9889a', liberalized_capital_markets).
narrative_ontology:cs_drift_state('6c54d5bd-7a99-4d4d-9e74-98dc0cf9889a', contemporary_global_financial_system, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6c54d5bd-7a99-4d4d-9e74-98dc0cf9889a', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_institutions).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, global_capital_markets).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, national_policy_autonomy).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the rules of the international monetary system, promoting capital account liberalization and discouraging government intervention in markets. Benefits from the stability and growth of global capital flows.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefits from the free movement of capital across borders, which increases investment opportunities and liquidity. This reading sees Bretton Woods as enabling this freedom by constraining national governments.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, global_capital_markets, beneficiary,
    institutional, generational, arbitrage, global).

% The ability of national governments to use capital controls or other interventions to manage their economies is constrained. This is a conceptual agent representing the policy space lost to international financial discipline.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, national_policy_autonomy, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_non_agent(bretton_woods_treaty_substrate__neoliberal_convertibility, national_policy_autonomy).

% Often face pressure to liberalize capital accounts, which can expose them to volatile capital flows and financial crises. Their policy options for managing these risks are limited by the prevailing interpretation of the Bretton Woods framework.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_economies, payer,
    moderate, generational, constrained, global).

% Would argue that capital controls are legitimate tools for macroeconomic management and that the original Bretton Woods design prioritized domestic stability over capital mobility. Their views are marginalized in this reading.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, keynesian_economists, excluded,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for international monetary cooperation that facilitates cross-border capital flows and reduces exchange rate volatility, thereby coordinating global financial integration.
% TRANSFER_FUNCTION: Transfers policy autonomy from national governments to the international financial system, enabling greater freedom and profit for global capital markets at the cost of domestic economic management tools.
% ABSENT_VOICES: Advocates for capital controls and greater national policy space, particularly from developing nations, are largely absent from the dominant discourse that champions capital account liberalization.
% DISAPPEARANCE_RATIONALE: If the neoliberal convertibility interpretation of Bretton Woods vanished, national governments would likely reassert greater control over capital flows, leading to a more fragmented and less integrated global financial system. Capital markets would face new barriers and volatility, and international financial institutions would lose significant influence.
% FOUNDING_PROBLEM: The post-WWII international monetary system needed stability to prevent competitive devaluations and facilitate trade, while also allowing for reconstruction and development.
% FOUNDING_PROBLEM_CORROBORATION: International financial institutions and proponents of free markets attest that the problem of capital market efficiency and global integration remains live. Critics, including many developing economies and heterodox economists, argue that the original problem of domestic policy space protection has been superseded by an agenda of financial liberalization, making the 'founding problem' a cover for new forms of extraction.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__neoliberal_convertibility, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__neoliberal_convertibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because the emphasis on capital mobility and convertibility imposes significant costs on national governments, particularly developing economies, by limiting their ability to manage domestic economic conditions. Suppression (0.65) reflects the institutional pressure from international financial bodies and the market discipline that discourages capital controls. Theater ratio is low (0.2) as the system is actively functional in shaping global finance, even if its stated coordination benefits are increasingly questioned by those bearing the costs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of international financial institutions, this reading of Bretton Woods is a 'rope' that provides essential global coordination. From the perspective of national policy autonomy and developing economies, it operates as a 'snare' or 'tangled_rope' that extracts sovereignty and limits development options. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   International financial institutions and global capital markets are clear beneficiaries, as the system's rules facilitate their operations and profitability. National policy autonomy (represented as a conceptual agent) and developing economies are victims, as their policy space is curtailed. The 'identity_locked' exit for national_policy_autonomy reflects the deep institutional and ideological embedding of this interpretation, making a departure from it extremely difficult without fundamental systemic change.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate has drifted from its original Keynesian intent of 'embedded liberalism' (protecting domestic policy space) to one that prioritizes capital mobility. This shift has not led to mandatrophy in the sense of a 'piton' because powerful beneficiaries (international finance) actively maintain and enforce this interpretation, ensuring its continued function as an extractive mechanism, not merely an inertial one. The 'contested' status of the founding problem highlights this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_vs_evolved_practice,
    'To what extent does the current ''neoliberal convertibility'' interpretation align with the original intent of the Bretton Woods founders, or is it a significant evolution/deviation?',
    'Historical analysis of primary source documents, diplomatic correspondence, and early policy debates from the Bretton Woods conference and immediate post-war period.',
    'If it''s a significant deviation, it strengthens the argument that the constraint is a ''tangled_rope'' or ''snare'' that has captured the original ''rope'' or ''scaffold'' intent. If it aligns, it suggests a more consistent ''rope'' or ''mountain'' (of economic law) from the outset.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_vs_evolved_practice, empirical, 'Assesses the historical fidelity of the neoliberal convertibility reading.').

omega_variable(
    capital_mobility_vs_stability_tradeoff,
    'Is the emphasis on free capital markets (as per this reading) a net benefit for global economic stability and development, or does it introduce systemic risks and exacerbate inequality?',
    'Empirical studies comparing economic performance, financial crises frequency, and inequality trends in countries with different capital account regimes, as well as counterfactual modeling.',
    'If capital mobility is shown to consistently lead to instability and inequality, the ''extraction'' component of this constraint is amplified, pushing it closer to a ''snare''. If it demonstrably leads to greater stability and development, the ''coordination'' function is strengthened, moving it closer to a ''rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_mobility_vs_stability_tradeoff, empirical, 'Evaluates the real-world consequences of capital account liberalization.').

omega_variable(
    reading_framing_underdetermination,
    'Is the ''neoliberal convertibility'' framing the only defensible interpretation of the Bretton Woods kernel, or do alternative framings (e.g., ''keynesian_embedded_liberalism'') offer equally coherent but structurally different classifications?',
    'Conceptual analysis of the logical coherence and empirical fit of competing interpretations, assessing which framing best accounts for the observed institutional dynamics and outcomes.',
    'If alternative framings are equally coherent and yield different classifications, it highlights the interpretive contestability of the kernel, suggesting the ''neoliberal convertibility'' reading is a ''tangled_rope'' sustained by a particular ideological consensus rather than an objective ''mountain'' of economic necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Documents the interpretive contestability of the Bretton Woods kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1971, 0.1).
narrative_ontology:measurement(bret_tr_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(bret_tr_t2000, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(bret_tr_t2010, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(bret_tr_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1971, 0.4).
narrative_ontology:measurement(bret_be_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(bret_be_t2000, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(bret_be_t2010, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(bret_be_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1971, 0.45).
narrative_ontology:measurement(bret_su_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(bret_su_t2000, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(bret_su_t2010, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement(bret_su_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'bretton_woods_treaty_substrate' kernel. It focuses on the system's role in enabling free capital markets by constraining national intervention, contrasting with readings that emphasize domestic policy space or national sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
