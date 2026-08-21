% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__composite_overdetermination_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: gold_fiat_transition_mechanism__composite_overdetermination_reading
 *   human_readable: Gold-Fiat Transition Mechanism: Composite Overdetermination Reading
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This constraint story analyzes the transition from the gold standard to a
 *   fiat monetary system not as a singular event or policy choice, but as the
 *   composite outcome of multiple, independently evolving structural changes.
 *   These include advancements in telecommunications enabling instant capital
 *   flows, the inherent instability of the Bretton Woods fixed exchange rate
 *   system, shifts in labor's bargaining power, and the maturation of legal
 *   tender enforcement mechanisms. The Nixon Shock of 1971 is viewed as a
 *   symbolic marker, rather than the sole causal node, in this overdetermined
 *   transition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.55).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.3).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, mountain).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Gold-Fiat Transition Mechanism: Composite Overdetermination Reading").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:emerges_naturally(gold_fiat_transition_mechanism__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'cccf0d01-dc4a-42ba-9e00-0c389d5654bd').
narrative_ontology:cs_kernel_codification('cccf0d01-dc4a-42ba-9e00-0c389d5654bd', implicit).
narrative_ontology:cs_authority_grounding('cccf0d01-dc4a-42ba-9e00-0c389d5654bd', distributed).
narrative_ontology:cs_reading_relation('cccf0d01-dc4a-42ba-9e00-0c389d5654bd', gold_fiat_transition_mechanism__automatic_constraint_reading, forecloses).
narrative_ontology:cs_reading_relation('cccf0d01-dc4a-42ba-9e00-0c389d5654bd', gold_fiat_transition_mechanism__creditor_discipline_reading, forecloses).
narrative_ontology:cs_axiom('cccf0d01-dc4a-42ba-9e00-0c389d5654bd', foundational, transition_is_multicausal_convergence).
narrative_ontology:cs_axiom_status(transition_is_multicausal_convergence, holdable).
narrative_ontology:cs_axiom_grounding('cccf0d01-dc4a-42ba-9e00-0c389d5654bd', transition_is_multicausal_convergence, empirically_contingent).
narrative_ontology:cs_axiom('cccf0d01-dc4a-42ba-9e00-0c389d5654bd', secondary, nixon_shock_is_symbolic_not_causal).
narrative_ontology:cs_axiom_status(nixon_shock_is_symbolic_not_causal, holdable).
narrative_ontology:cs_axiom_grounding('cccf0d01-dc4a-42ba-9e00-0c389d5654bd', nixon_shock_is_symbolic_not_causal, empirically_contingent).
narrative_ontology:cs_reference_frame('cccf0d01-dc4a-42ba-9e00-0c389d5654bd', gold_standard_era_instability).
narrative_ontology:cs_drift_state('cccf0d01-dc4a-42ba-9e00-0c389d5654bd', post_bretton_woods_collapse, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('cccf0d01-dc4a-42ba-9e00-0c389d5654bd', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_banks).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, governments).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, financial_sector).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, telecommunications_industry).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_standard_advocates).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, savers_on_fixed_income).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_unions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained significant discretionary power over monetary policy, no longer constrained by gold reserves. Benefited from increased flexibility in managing national economies.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_banks, agenda_setter,
    institutional, generational, arbitrage, global).

% Gained fiscal flexibility, able to finance deficits without immediate balance-of-payments crises or gold outflows. Benefited from reduced external discipline.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, governments, beneficiary,
    institutional, generational, mobile, national).

% Benefited from the expansion of credit and the increased velocity of capital flows enabled by telecommunications technology and the removal of gold constraints.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, financial_sector, beneficiary,
    powerful, biographical, arbitrage, global).

% Its technological advancements (instant capital flows) were a key structural change enabling the transition, indirectly benefiting from the new financial landscape.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, telecommunications_industry, beneficiary,
    organized, biographical, arbitrage, global).

% Lost their preferred monetary system and the perceived stability it offered. Their arguments for a return to gold became increasingly marginalized.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_standard_advocates, payer,
    powerless, generational, trapped, global).

% Experienced erosion of purchasing power due to inflation, which became a more common feature of fiat monetary systems without gold backing.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, savers_on_fixed_income, payer,
    powerless, biographical, constrained, national).

% Experienced a shift in bargaining power as the economic landscape changed, with some analyses linking the decline in labor's share of income to the post-Bretton Woods era.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_unions, payer,
    organized, biographical, constrained, national).

% Analyze the complex interplay of factors that led to the transition, seeking to understand its long-term causes and consequences without direct participation.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, economic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__composite_overdetermination_reading, diffuse).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The transition coordinated multiple independent structural changes (technology, policy shifts, power dynamics) into a new global monetary regime, enabling flexible national monetary policies and global capital mobility.
% TRANSFER_FUNCTION: Shifted monetary authority from a gold-backed system to central bank discretion, transferring power and flexibility to governments and central banks, while imposing costs on those who preferred fixed exchange rates or lost bargaining power.
% ABSENT_VOICES: Those who advocated for a more gradual, internationally negotiated transition, or those who foresaw the long-term inflationary pressures and loss of monetary discipline, were largely absent from the immediate decision-making processes that marked the symbolic end of the gold standard.
% DISAPPEARANCE_RATIONALE: This constraint describes a historical transition mechanism that has already occurred. It cannot 'disappear overnight' as it is an analytical interpretation of past events, not an ongoing, actively enforced structure.
% FOUNDING_PROBLEM: The Bretton Woods system faced increasing strain from global capital flows, balance-of-payments imbalances, and the 'Triffin dilemma,' leading to a need for greater monetary flexibility and a new international financial architecture.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians, international financial institutions (e.g., IMF reports from the era), and contemporary government documents widely corroborate the severe strains on the Bretton Woods system and the need for reform, though the specific causal mechanisms of its collapse remain debated.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__composite_overdetermination_reading, world_unchanged).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__composite_overdetermination_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, ExtMetricName, E),
    domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(gold_fiat_transition_mechanism__composite_overdetermination_reading),
    narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is moderate (0.55) because the transition, while benefiting some actors (governments, financial sector) with increased flexibility and profit opportunities, also imposed costs on others (savers, labor). `Suppression` is low (0.30) because the 'constraint' here is an analytical reading of a historical process, not an actively enforced mechanism in the present; any suppression was part of the constituent structural changes, not the composite transition itself. `Theater_ratio` is low (0.10) as this is an analytical interpretation of history, not a performance. `Accessibility_collapse` is high (0.70) because the convergence of these forces effectively collapsed the viability of a gold-backed system. `Resistance` is moderate (0.40) reflecting the historical opposition to the abandonment of gold, but not to the 'overdetermination' itself.
 *
 * PERSPECTIVAL GAP:
 *   Different actors experienced the transition through the lens of their immediate gains or losses, often attributing causality to single, salient events like the Nixon Shock. This reading, however, emphasizes the underlying, multi-faceted structural shifts, suggesting that the 'event' was overdetermined by these deeper forces. The engine's classification will reflect this analytical distance from any single actor's perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks, governments, and the financial sector are identified as beneficiaries due to the increased flexibility and opportunities afforded by the fiat system. The telecommunications industry is an indirect beneficiary as its technology enabled key aspects of the transition. Gold standard advocates, savers on fixed income, and labor unions are identified as payers, bearing the costs of the new monetary regime. The 'composite overdetermination' itself has no single, concentrated beneficiary, but its *outcome* had clear distributional effects.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_overdetermination,
    'Is the ''composite overdetermination'' of the gold-fiat transition a natural, emergent property of complex systems, or a constructed outcome of human choices and institutional path dependencies?',
    'Comparative historical analysis of other monetary regime shifts, examining the degree to which similar multi-causal convergence patterns emerge across different contexts, or whether specific policy choices consistently override such convergence.',
    'If more natural, it reinforces the ''mountain'' classification as an irreducible historical fact. If more constructed, it suggests a ''tangled_rope'' or ''snare'' classification for the underlying institutional choices that enabled the convergence, implying greater agency and potential for alternative outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_overdetermination, conceptual, 'Ambiguity of natural emergence vs. constructed outcome for a historical ''mountain''.').

omega_variable(
    causal_singularity_challenge,
    'To what extent do the ''automatic constraint'' and ''creditor discipline'' readings misattribute causality to a non-existent unified transition, as this reading claims?',
    'Detailed counterfactual analysis: if the ''singular'' causal nodes (e.g., gold reserves, creditor veto) had been maintained, would the other structural changes (telecoms, labor shifts) still have forced a similar transition?',
    'If this reading''s challenge to singularity is strongly corroborated, it further undermines the explanatory power of the sibling readings, reinforcing the ''forecloses'' relationship. If the singular nodes prove more robust, it might shift the relationship to ''coexists_with'' as partial explanations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_singularity_challenge, empirical, 'Assessing the validity of the challenge to kernel singularity.').

omega_variable(
    disentangling_causal_nodes,
    'Can the individual causal nodes (telecoms, Bretton Woods collapse, labor shifts, legal tender maturation) be sufficiently disentangled to assess their independent contributions to the transition, or are they too interdependent?',
    'Development of more sophisticated historical econometric models or agent-based simulations that can isolate the impact of each structural change while accounting for their interactions.',
    'Greater disentanglement would allow for more precise attribution of distributional effects to specific causal factors, potentially refining the beneficiary/victim declarations. If disentanglement proves impossible, it strengthens the ''composite overdetermination'' claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disentangling_causal_nodes, empirical, 'Difficulty of isolating causal contributions in a composite historical event.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 1960, 1975).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(gold_tr_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(gold_tr_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(gold_tr_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1975, 0.1).

% Extraction over time
narrative_ontology:measurement(gold_be_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement(gold_be_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(gold_be_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(gold_be_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1975, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1960, 0.25).
narrative_ontology:measurement(gold_su_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1965, 0.28).
narrative_ontology:measurement(gold_su_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(gold_su_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1975, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__composite_overdetermination_reading, global_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gold_fiat_transition_mechanism' kernel, focusing on the composite, overdetermined nature of the transition. It challenges the singular causal claims of the 'automatic_constraint_reading' and 'creditor_discipline_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
