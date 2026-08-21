% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__automatic_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__automatic_constraint_reading, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Gold-Fiat Transition: Automatic Constraint Reading
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This constraint story analyzes the gold-fiat transition from the
 *   perspective that it eliminated an automatic physical constraint on money
 *   creation, replacing it with discretionary central bank authority. The
 *   original gold standard acted as a 'mountain' of sorts, imposing an
 *   external, non-negotiable limit on monetary expansion. Its removal
 *   transformed the constraint into an institutional 'tangled rope' where
 *   central banks coordinate economic policy but also extract value through
 *   inflation from those without direct access to the money creation process.
 *   This reading emphasizes the shift from an external, 'natural' limit to an
 *   internal, 'political' one.
 *
 * KEY AGENTS:
 *   - monetary_authorities: Primary beneficiary (institutional/arbitrage) — gained discretion and power.
 *   - debtor_governments: Secondary beneficiary (institutional/mobile) — gained fiscal flexibility.
 *   - creditor_class: Primary target (powerful/constrained) — lost automatic protection against debasement.
 *   - savers: Secondary target (moderate/constrained) — vulnerable to inflation.
 *   - gold_producers: Excluded (organized/constrained) — lost automatic demand for gold.
 *   - economic_theorists: Analytical observer (analytical/analytical) — analyze long-term consequences.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.85).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.7).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Gold-Fiat Transition: Automatic Constraint Reading").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, '57a83320-fc82-4bc3-b34d-a683dc6ceafc').
narrative_ontology:cs_kernel_codification('57a83320-fc82-4bc3-b34d-a683dc6ceafc', formalized).
narrative_ontology:cs_authority_grounding('57a83320-fc82-4bc3-b34d-a683dc6ceafc', extraction).
narrative_ontology:cs_interpretation_layer_present('57a83320-fc82-4bc3-b34d-a683dc6ceafc').
narrative_ontology:cs_reading_relation('57a83320-fc82-4bc3-b34d-a683dc6ceafc', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('57a83320-fc82-4bc3-b34d-a683dc6ceafc', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('57a83320-fc82-4bc3-b34d-a683dc6ceafc', foundational, money_creation_requires_physical_limit).
narrative_ontology:cs_axiom_status(money_creation_requires_physical_limit, overridden).
narrative_ontology:cs_axiom_grounding('57a83320-fc82-4bc3-b34d-a683dc6ceafc', money_creation_requires_physical_limit, empirically_contingent).
narrative_ontology:cs_axiom('57a83320-fc82-4bc3-b34d-a683dc6ceafc', foundational, discretionary_monetary_policy_is_superior).
narrative_ontology:cs_axiom_status(discretionary_monetary_policy_is_superior, holdable).
narrative_ontology:cs_axiom_grounding('57a83320-fc82-4bc3-b34d-a683dc6ceafc', discretionary_monetary_policy_is_superior, instrumental).
narrative_ontology:cs_reference_frame('57a83320-fc82-4bc3-b34d-a683dc6ceafc', gold_standard_automatic_discipline).
narrative_ontology:cs_drift_state('57a83320-fc82-4bc3-b34d-a683dc6ceafc', post_nixon_shock_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('57a83320-fc82-4bc3-b34d-a683dc6ceafc', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, debtor_governments).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, savers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained significant discretion over money supply and interest rates, no longer bound by physical gold reserves. This allows for counter-cyclical policy and financing of government debt, but also risks inflation and currency debasement.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefited from the ability to finance deficits without the immediate discipline imposed by gold convertibility, gaining fiscal flexibility and reducing the risk of balance-of-payments crises.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, debtor_governments, beneficiary,
    institutional, biographical, mobile, national).

% Lost the automatic protection against inflation and currency debasement that gold convertibility provided. Their wealth is now more vulnerable to discretionary monetary policy, requiring active management to preserve value.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class, payer,
    powerful, generational, constrained, global).

% Experience erosion of purchasing power due to inflation, which is now a policy choice rather than a physical impossibility. Their options for preserving wealth are more complex and require financial literacy.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, savers, payer,
    moderate, biographical, constrained, national).

% Lost the automatic demand for gold as a monetary anchor, shifting their market to industrial and investment demand. They would argue for a return to gold-backed currency to stabilize their industry.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_producers, excluded,
    organized, generational, constrained, global).

% Analyze the long-term consequences of the transition, debating the trade-offs between monetary flexibility and stability, and the implications for economic cycles and wealth distribution.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, economic_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The gold standard coordinated international trade and finance by providing a fixed exchange rate mechanism and a natural limit on money creation, fostering trust in currency value.
% TRANSFER_FUNCTION: The transition transferred the power to create money and manage its value from an automatic, physically constrained system to a discretionary, institutionally managed system. This shifted wealth from creditors/savers (who lost automatic inflation protection) to monetary authorities/debtor governments (who gained flexibility).
% ABSENT_VOICES: Advocates for a return to a gold standard, who believe it provides superior long-term stability and prevents government overspending, are largely excluded from mainstream monetary policy debates. Gold producers would also advocate for its return.
% DISAPPEARANCE_RATIONALE: The gold standard, as an automatic constraint, has already disappeared. Its 'disappearance' was the transition itself, which fundamentally rearranged the global monetary system. The current fiat system would not revert to gold convertibility if central bank discretion vanished; it would likely lead to hyperinflation or a new, perhaps digital, form of discretionary control.
% FOUNDING_PROBLEM: The gold standard was intended to provide a stable, predictable monetary system, limit government spending, and prevent inflation by tying currency value to a physical commodity.
% FOUNDING_PROBLEM_CORROBORATION: While some economists and political groups still advocate for a return to gold, the consensus among mainstream monetary authorities and governments is that the gold standard's limitations (e.g., inability to respond to economic shocks, deflationary bias) rendered it obsolete. Historical accounts and economic analyses from central banks and international financial institutions corroborate its functional demise as a primary monetary system.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__automatic_constraint_reading, world_unchanged).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__automatic_constraint_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__automatic_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because the discretionary power to create money allows for a transfer of wealth, primarily through inflation, from those holding fixed-income assets or savings to those who can create or access new money first (e.g., governments, banks). Suppression (0.70) is significant because individuals and even powerful creditors have limited options to opt out of the fiat system or to automatically protect their wealth from monetary policy decisions. The theater ratio is low (0.10) as the central bank's actions are genuinely functional in managing the economy, even if they are also extractive. Accessibility collapse is high (0.90) because there are virtually no alternatives to participating in the fiat monetary system for most economic actors. Resistance (0.30) is present but diffuse, mainly from academic critics and fringe political movements, not a coordinated force capable of altering the system.
 *
 * PERSPECTIVAL GAP:
 *   Monetary authorities and debtor governments perceive the transition as a necessary evolution, providing flexibility and tools for economic management. Creditors and savers, however, experience it as a loss of automatic protection and a source of wealth erosion. The engine's per-seat classification should reflect this divergence, with beneficiaries seeing a 'rope' or 'scaffold' and victims experiencing a 'snare' or 'tangled rope'.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities and debtor governments are clear beneficiaries (d near 0.0) as they gained significant power and flexibility. The creditor class and savers are targets (d near 1.0) as they bear the costs of inflation and lost automatic protection. Gold producers are excluded, their industry's monetary role eliminated. Economic theorists are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate shifted from providing automatic stability (gold standard) to providing discretionary stability and economic management (fiat system). The 'founding problem' of the gold standard (stable money) is considered 'dead' by mainstream views, but the 'founding problem' of the fiat system (economic management) is 'live'. The classification as a 'tangled rope' acknowledges both the coordination function (economic management) and the asymmetric extraction (inflationary transfers) inherent in the discretionary system, preventing mislabeling it as either pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automatic_vs_discretionary_necessity,
    'Was the shift from an automatic gold constraint to discretionary fiat authority an inevitable evolution driven by economic complexity, or a policy choice driven by political expediency?',
    'Comparative historical analysis of monetary systems in different political-economic contexts, and counterfactual modeling of gold standard resilience under modern economic shocks.',
    'If inevitable, the current system''s structure is more ''mountain-like'' in its necessity; if a choice, its ''tangled rope'' nature (with its associated extraction) is more contingent and amenable to reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automatic_vs_discretionary_necessity, conceptual, 'Ambiguity regarding the necessity of the transition from automatic to discretionary monetary control.').

omega_variable(
    inflation_as_tax_vs_policy_tool,
    'Is inflation, enabled by fiat currency, primarily an implicit tax on savers and creditors, or a necessary policy tool for economic stabilization and growth?',
    'Empirical studies on the distributional effects of inflation across different income and wealth groups, and analysis of the counterfactual economic performance under strict monetary rules.',
    'If primarily a tax, the extractiveness of the ''tangled rope'' is higher and less justifiable; if primarily a policy tool, the coordination function is stronger, and extraction is a necessary cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_as_tax_vs_policy_tool, empirical, 'Ambiguity regarding the primary function and impact of inflation in a fiat system.').

omega_variable(
    kernel_reading_focus,
    'This constraint is the ''automatic_constraint_reading'' of the ''gold_fiat_transition_mechanism'' kernel. How would the classification change if a sibling reading, such as the ''creditor_discipline_reading'' (focusing on loss of creditor veto power) or the ''composite_overdetermination_reading'' (focusing on multiple causal factors), were adopted as the primary frame?',
    'Adopting a different kernel reading would shift the primary beneficiaries/victims and the perceived coordination/extraction functions, leading to a re-evaluation of metrics and potentially a different claimed_type.',
    'The ''creditor_discipline_reading'' would likely emphasize the political power shift and might classify the constraint as more of a ''snare'' for creditors. The ''composite_overdetermination_reading'' might reduce the perceived agency of monetary authorities, potentially lowering extractiveness and suppression if the transition is seen as an emergent property of multiple forces rather than a deliberate policy choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_focus, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1971, 0.05).
narrative_ontology:measurement(gold_tr_t1985, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(gold_tr_t2000, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(gold_tr_t2010, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(gold_tr_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1971, 0.75).
narrative_ontology:measurement(gold_be_t1985, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1985, 0.8).
narrative_ontology:measurement(gold_be_t2000, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2000, 0.83).
narrative_ontology:measurement(gold_be_t2010, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2010, 0.86).
narrative_ontology:measurement(gold_be_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1971, 0.6).
narrative_ontology:measurement(gold_su_t1985, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(gold_su_t2000, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(gold_su_t2010, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(gold_su_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gold_fiat_transition_mechanism' kernel. This reading focuses on the shift from automatic physical constraint to discretionary institutional authority. The 'creditor_discipline_reading' emphasizes the loss of creditor veto power, and the 'composite_overdetermination_reading' views the transition as a multi-causal convergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
