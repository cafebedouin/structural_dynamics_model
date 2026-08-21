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
 *   This reading posits that the transition from a gold-backed monetary
 *   system to a fiat system was not a singular event or a simple policy
 *   choice, but rather the overdetermined outcome of multiple, independent
 *   structural changes converging over time. These included advances in
 *   telecommunications enabling rapid capital flows, the inherent instability
 *   of the Bretton Woods fixed exchange rate system, shifts in labor's
 *   bargaining power, and the maturation of legal tender enforcement
 *   mechanisms. The 'Nixon Shock' of 1971 is viewed as a symbolic marker, not
 *   the sole causal node. This challenges readings that attribute the
 *   transition to a single 'automatic constraint' or a specific 'creditor
 *   discipline' mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.45).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.6).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Gold-Fiat Transition Mechanism: Composite Overdetermination Reading").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, '528f6ff6-e5a4-4150-92b4-496ea6687498').
narrative_ontology:cs_kernel_codification('528f6ff6-e5a4-4150-92b4-496ea6687498', distributed).
narrative_ontology:cs_authority_grounding('528f6ff6-e5a4-4150-92b4-496ea6687498', diffuse_epistemic).
narrative_ontology:cs_reading_relation('528f6ff6-e5a4-4150-92b4-496ea6687498', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('528f6ff6-e5a4-4150-92b4-496ea6687498', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_axiom('528f6ff6-e5a4-4150-92b4-496ea6687498', foundational, transition_as_emergent_property).
narrative_ontology:cs_axiom_status(transition_as_emergent_property, holdable).
narrative_ontology:cs_axiom_grounding('528f6ff6-e5a4-4150-92b4-496ea6687498', transition_as_emergent_property, empirically_contingent).
narrative_ontology:cs_axiom('528f6ff6-e5a4-4150-92b4-496ea6687498', secondary, nixon_shock_as_symbolic_marker).
narrative_ontology:cs_axiom_status(nixon_shock_as_symbolic_marker, holdable).
narrative_ontology:cs_axiom_grounding('528f6ff6-e5a4-4150-92b4-496ea6687498', nixon_shock_as_symbolic_marker, conventional).
narrative_ontology:cs_reference_frame('528f6ff6-e5a4-4150-92b4-496ea6687498', multi_causal_historical_process).
narrative_ontology:cs_drift_state('528f6ff6-e5a4-4150-92b4-496ea6687498', contemporary_economic_history, gap(stable, minor, true)).
narrative_ontology:cs_created_at('528f6ff6-e5a4-4150-92b4-496ea6687498', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_banks).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, national_governments).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_standard_advocates).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_exchange_rate_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_unions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained significant discretion over monetary policy, no longer constrained by gold reserves. This allowed for counter-cyclical policy and inflation targeting, but also introduced new political pressures.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_banks, beneficiary,
    institutional, generational, mobile, national).

% Achieved greater fiscal flexibility, freed from the balance-of-payments discipline imposed by gold convertibility. This enabled increased public spending and deficit financing, but also removed a check on fiscal profligacy.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, national_governments, beneficiary,
    institutional, generational, mobile, national).

% Lost the perceived stability and automatic discipline of the gold standard. Their ideological commitment to a gold-backed currency makes adapting to fiat systems difficult, leading to a sense of loss and disempowerment.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_standard_advocates, payer,
    powerless, generational, identity_locked, global).

% The collapse of Bretton Woods pegs forced a transition to floating exchange rates, introducing new volatility and complexity for international trade and finance. Many institutions built around fixed rates had to adapt or dissolve.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_exchange_rate_regimes, payer,
    moderate, biographical, constrained, global).

% Provided the underlying technology (instant capital flows) that made the gold standard increasingly impractical, but did not directly benefit from the transition itself. Their role was enabling, not extractive.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, telecommunications_industry, observer,
    organized, biographical, arbitrage, global).

% Experienced a shift in bargaining power dynamics as inflation became a more prominent feature of fiat systems, often eroding real wages. The transition contributed to a broader decline in labor's influence.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_unions, payer,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a new framework for managing national and international monetary policy, allowing for greater flexibility in response to economic shocks and facilitating global capital flows without the constraints of a physical commodity.
% TRANSFER_FUNCTION: Shifted the locus of monetary control from a commodity-backed system to a discretionary, institutionally managed one, transferring power and flexibility to central banks and national governments, while imposing new forms of risk and adjustment costs on other actors.
% ABSENT_VOICES: The voices of those who benefited from the stability and discipline of the gold standard, particularly smaller nations or those with less influence in international finance, were largely absent from the decision-making process that led to the composite transition. Their concerns about inflation and fiscal discipline were overridden by the imperatives of larger economies.
% DISAPPEARANCE_RATIONALE: The composite nature of the transition means no single 'constraint' could disappear overnight. The underlying structural changes (technology, labor shifts, legal tender) are now deeply embedded. Reverting to a gold standard would require a complete re-engineering of global finance and political economy, not merely the removal of a single rule.
% FOUNDING_PROBLEM: The gold standard faced increasing pressure from growing global trade, capital mobility, and the demands for flexible monetary policy to manage domestic economies, leading to a series of crises and an inability to respond to modern economic challenges.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and mainstream monetary economists widely corroborate the increasing strain on the gold standard due to global economic growth and capital mobility. Central banks and national governments continue to assert the necessity of flexible monetary policy to manage modern economies, a problem the gold standard could not address. While gold standard advocates dispute the 'solution,' they generally acknowledge the historical pressures that led to its abandonment.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__composite_overdetermination_reading, world_unchanged).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).
:- end_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while some actors (central banks, national governments) gained significant flexibility and power, the 'extraction' was diffuse and distributed across various structural shifts, not concentrated by a single agent. Suppression (0.6) reflects the active enforcement of legal tender laws and the institutional inertia that made a return to gold impractical. Theater ratio is low (0.1) as the transition was a genuine, functional adaptation to new realities, not a performative maintenance of an obsolete system. Accessibility collapse is high (0.7) because the confluence of factors made alternatives to fiat increasingly unviable.
 *
 * PERSPECTIVAL GAP:
 *   Other readings (automatic constraint, creditor discipline) would likely compute higher extractiveness and clearer beneficiaries, as they attribute the transition to a more singular, intentional mechanism. This composite reading diffuses both benefit and cost across a broader set of structural changes, leading to a more moderate extractiveness score and a less concentrated beneficiary structure. The engine's classification will reflect this diffusion, likely computing a Rope or Tangled Rope, rather than a Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks and national governments are beneficiaries, gaining monetary and fiscal flexibility. Gold standard advocates and fixed exchange rate regimes are payers, losing their preferred system and facing new volatilities. Labor unions are also payers, experiencing shifts in bargaining power. The telecommunications industry is an observer, providing enabling technology without direct extractive benefit. The diffuse nature of the 'transition' means no single agent captures all the gains or bears all the costs, but the overall shift favored institutional actors with discretionary power.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading inherently resists mandatrophy by denying a singular 'mandate' for the transition. Instead, it views the outcome as an emergent property of multiple, often uncoordinated, forces. The 'function' was not a single problem solved by a single constraint, but a complex adaptation to an overdetermined set of pressures. This prevents mislabeling a complex historical process as a simple extractive snare or a purely coordinative rope, by emphasizing the multi-causal nature of the shift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_attribution_ambiguity,
    'To what extent can specific structural changes (e.g., telecommunications, labor shifts) be causally isolated and quantified in their contribution to the gold-fiat transition, versus being part of an inseparable, overdetermined whole?',
    'Counterfactual historical analysis and econometric modeling attempting to isolate the impact of individual factors, though definitive resolution may be impossible due to path dependence and interaction effects.',
    'If specific factors can be isolated and shown to have dominant causal weight, it might lend more support to readings that emphasize a singular mechanism. If overdetermination is robust, this reading''s emphasis on composite causality is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_attribution_ambiguity, empirical, 'Ambiguity in isolating specific causal contributions to the transition.').

omega_variable(
    kernel_singularity_challenge,
    'Is the ''gold-fiat transition mechanism'' a singular, coherent kernel, or is it a label for a collection of distinct, though related, historical processes?',
    'Conceptual analysis of historical narratives and economic models: if different models describe fundamentally different processes under the same label, the kernel itself is a composite.',
    'If the kernel is truly composite, this reading''s challenge to kernel singularity is validated, and other readings that assume a singular mechanism are conceptually undermined. If a singular kernel can be defended, this reading becomes one perspective on that kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_singularity_challenge, conceptual, 'Whether the ''transition mechanism'' is a singular kernel or a composite label.').

omega_variable(
    distributional_effects_of_structural_changes,
    'How did the distributional effects of each independent structural change (e.g., capital mobility, labor power shifts) interact to produce the overall ''extraction'' observed in the transition?',
    'Detailed historical and economic studies focusing on the specific winners and losers of each individual structural shift and how these aggregated or cancelled out.',
    'A clearer understanding of the aggregated distributional effects would refine the extractiveness score and potentially identify more specific beneficiaries or victims for the overall transition, even if no single agent drove the entire process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_effects_of_structural_changes, empirical, 'Understanding the aggregated distributional effects of multiple structural changes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 1960, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(gold_tr_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1965, 0.07).
narrative_ontology:measurement(gold_tr_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1970, 0.09).
narrative_ontology:measurement(gold_tr_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(gold_tr_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1980, 0.1).

% Extraction over time
narrative_ontology:measurement(gold_be_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1960, 0.3).
narrative_ontology:measurement(gold_be_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(gold_be_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(gold_be_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1975, 0.43).
narrative_ontology:measurement(gold_be_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1980, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement(gold_su_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1965, 0.48).
narrative_ontology:measurement(gold_su_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(gold_su_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement(gold_su_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1980, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__composite_overdetermination_reading, global_infrastructure).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, creditor_discipline_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gold_fiat_transition_mechanism' kernel. This 'composite_overdetermination_reading' emphasizes the multi-causal, emergent nature of the transition, challenging the singular causal claims of the 'automatic_constraint_reading' and 'creditor_discipline_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
