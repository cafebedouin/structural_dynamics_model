% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__triffin_structural_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__triffin_structural_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__triffin_structural_reading
 *   human_readable: Dollar-Gold Convertibility (Triffin Structural Reading)
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   This constraint story analyzes dollar-gold convertibility through the
 *   lens of the Triffin Dilemma, viewing it as an inherently unsustainable
 *   design flaw in the Bretton Woods system. The reading posits that the
 *   system was structurally doomed to collapse due to the conflicting demands
 *   of providing global liquidity (requiring more dollars in circulation) and
 *   maintaining dollar convertibility to gold (requiring fewer dollars
 *   relative to gold reserves). Both the U.S. and creditor nations were
 *   victims of this impossible trilemma, leading to high extraction and
 *   eventual systemic revision. The post-Bretton Woods floating regime is
 *   identified as the conceptual beneficiary, representing the system that
 *   emerged from the collapse.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, 0.85).
domain_priors:suppression_score(dollar_gold_convertibility__triffin_structural_reading, 0.9).
domain_priors:theater_ratio(dollar_gold_convertibility__triffin_structural_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, snare).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Dollar-Gold Convertibility (Triffin Structural Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, '8ce9a859-c420-4df0-9ca1-e669ef039a49').
narrative_ontology:cs_kernel_codification('8ce9a859-c420-4df0-9ca1-e669ef039a49', formalized).
narrative_ontology:cs_authority_grounding('8ce9a859-c420-4df0-9ca1-e669ef039a49', extraction).
narrative_ontology:cs_interpretation_layer_present('8ce9a859-c420-4df0-9ca1-e669ef039a49').
narrative_ontology:cs_reading_relation('8ce9a859-c420-4df0-9ca1-e669ef039a49', dollar_gold_convertibility__strict_convertibility_reading, forecloses).
narrative_ontology:cs_reading_relation('8ce9a859-c420-4df0-9ca1-e669ef039a49', dollar_gold_convertibility__policy_flexible_reading, forecloses).
narrative_ontology:cs_axiom('8ce9a859-c420-4df0-9ca1-e669ef039a49', foundational, impossible_trilemma_structural_inevitability).
narrative_ontology:cs_axiom_status(impossible_trilemma_structural_inevitability, holdable).
narrative_ontology:cs_axiom_grounding('8ce9a859-c420-4df0-9ca1-e669ef039a49', impossible_trilemma_structural_inevitability, empirically_contingent).
narrative_ontology:cs_axiom('8ce9a859-c420-4df0-9ca1-e669ef039a49', foundational, global_liquidity_vs_confidence_tradeoff).
narrative_ontology:cs_axiom_status(global_liquidity_vs_confidence_tradeoff, holdable).
narrative_ontology:cs_axiom_grounding('8ce9a859-c420-4df0-9ca1-e669ef039a49', global_liquidity_vs_confidence_tradeoff, empirically_contingent).
narrative_ontology:cs_reference_frame('8ce9a859-c420-4df0-9ca1-e669ef039a49', bretton_woods_founding_principles).
narrative_ontology:cs_drift_state('8ce9a859-c420-4df0-9ca1-e669ef039a49', pre_nixon_shock_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('8ce9a859-c420-4df0-9ca1-e669ef039a49', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, united_states_monetary_authorities).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, creditor_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for maintaining both domestic economic growth and dollar convertibility, a structural impossibility. They face the dilemma of either sacrificing domestic goals or undermining convertibility, leading to a constant drain on gold reserves and policy flexibility. Their identity is tied to maintaining the global financial order.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, united_states_monetary_authorities, payer,
    institutional, biographical, identity_locked, global).

% Accumulate dollar reserves from trade surpluses but face the risk of dollar devaluation if the U.S. cannot maintain convertibility. They are trapped between needing dollar liquidity for trade and fearing the collapse of the system, leading to demands for gold conversion that accelerate the crisis. Their exit is constrained by the lack of a viable alternative reserve currency.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, creditor_nations, payer,
    organized, biographical, constrained, global).

% The eventual outcome of the convertibility's collapse, characterized by flexible exchange rates and a more diversified international monetary system. This 'beneficiary' is a conceptual entity representing the system that emerged from the structural flaw, not an active agent.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime, beneficiary,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime).

% The institutional body tasked with overseeing the international monetary system. It observes the growing structural tensions and attempts to mediate solutions, but is ultimately unable to resolve the inherent contradiction of the Triffin Dilemma without systemic reform.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aimed to coordinate international trade and finance by providing a stable, gold-backed reserve currency, facilitating global economic integration post-WWII.
% TRANSFER_FUNCTION: Transferred the burden of maintaining global liquidity and convertibility onto the U.S. dollar, while simultaneously transferring the risk of dollar devaluation to creditor nations holding large dollar reserves.
% ABSENT_VOICES: Developing nations, whose economic stability was often sacrificed to maintain the Bretton Woods system, had limited voice in its design or reform. Their interests were subordinated to the stability of the core industrial economies.
% DISAPPEARANCE_RATIONALE: The constraint (dollar-gold convertibility) did disappear, leading to the collapse of the Bretton Woods system and the emergence of a floating exchange rate regime. The entire international monetary system rearranged itself, demonstrating its foundational role.
% FOUNDING_PROBLEM: To establish a stable international monetary system after World War II, avoiding the competitive devaluations and trade wars of the interwar period, and providing sufficient liquidity for expanding global trade.
% FOUNDING_PROBLEM_CORROBORATION: Economists like Robert Triffin (from outside the direct beneficiaries of the system's maintenance) identified the inherent structural flaw long before its collapse. Historical analysis and subsequent economic developments corroborate that the founding problem, as framed by convertibility, was fundamentally unresolvable within that structure.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__triffin_structural_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__triffin_structural_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__triffin_structural_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dollar_gold_convertibility__triffin_structural_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__triffin_structural_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the system imposed an unsustainable burden on the U.S. and created instability for creditor nations. Suppression is high because the system's rules actively prevented alternative reserve currencies or flexible exchange rates, forcing adherence to a flawed structure. Theater ratio increased over time as efforts to maintain convertibility became increasingly performative, masking the underlying structural contradiction. The system's collapse was not a policy failure but an inevitable outcome of its design.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the U.S. and creditor nations, the system was a source of increasing tension and extraction. From an analytical, structural perspective (Triffin's view), the system was a snare from its inception, destined to fail, with the eventual floating regime being the 'beneficiary' of this necessary collapse.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. monetary authorities were targets (high d) as they bore the direct cost of the dilemma, draining gold reserves. Creditor nations were also targets (high d) as they faced the risk of devaluation and were constrained in their ability to exit the dollar system. The 'post_bretton_woods_floating_regime' is a conceptual beneficiary (low d) as it represents the more stable, albeit different, system that emerged from the structural flaw.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (stable convertibility) was structurally impossible to fulfill in the long run. The system did not so much suffer mandatrophy as it was designed with an inherent, unresolvable contradiction that guaranteed its eventual collapse. The classification as a snare reflects this inherent flaw, where the coordination story (global stability) was undermined by the structural extraction from all participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_inevitability_vs_policy_choice,
    'To what extent was the collapse of dollar-gold convertibility an inevitable structural outcome versus a consequence of specific policy choices by the U.S. or other nations?',
    'Counterfactual historical analysis: modeling alternative policy paths (e.g., earlier revaluation of gold, stricter fiscal discipline) to assess if they could have sustained convertibility without fundamental structural change.',
    'If primarily structural, the snare classification is robust. If policy choices played a more decisive role, the constraint might be reclassified as a tangled_rope, implying more agency and less inherent doom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_inevitability_vs_policy_choice, conceptual, 'Distinguishing between inherent structural flaws and contingent policy decisions in the system''s collapse.').

omega_variable(
    triffin_dilemma_applicability_today,
    'Does the Triffin Dilemma, as a structural flaw, apply to contemporary reserve currencies (e.g., the U.S. dollar in a floating regime) or was it specific to the gold-exchange standard?',
    'Empirical analysis of current global financial architecture: assessing if the issuer of a dominant reserve currency still faces a fundamental conflict between domestic monetary policy and providing global liquidity, even without a gold peg.',
    'If applicable, it suggests a persistent structural snare in the global financial system, even post-Bretton Woods. If not, the Triffin reading is historically specific, and the ''post_bretton_woods_floating_regime'' is a more robust beneficiary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(triffin_dilemma_applicability_today, empirical, 'Relevance of the Triffin Dilemma to modern international monetary systems.').

omega_variable(
    beneficiary_as_conceptual_entity,
    'Is ''post_bretton_woods_floating_regime'' a valid beneficiary, given it is a conceptual entity rather than an active agent?',
    'Refinement of beneficiary definition to explicitly include emergent systemic states that resolve prior structural contradictions, or re-framing the ''beneficiary'' as the global economy''s long-term stability.',
    'If not valid, the constraint would lack a clear beneficiary, potentially strengthening its snare classification by removing any coordination-like aspect, even conceptual. If valid, it highlights the framework''s capacity to model systemic evolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_as_conceptual_entity, conceptual, 'Validity of a conceptual entity as a constraint beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1944, 0.2).
narrative_ontology:measurement(doll_tr_t1955, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1955, 0.35).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1965, 0.5).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1971, 0.6).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1944, 0.6).
narrative_ontology:measurement(doll_be_t1955, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1955, 0.7).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1965, 0.8).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1971, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1944, 0.7).
narrative_ontology:measurement(doll_su_t1955, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1955, 0.78).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1965, 0.85).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1971, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__triffin_structural_reading, global_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dollar_gold_convertibility' kernel. This 'triffin_structural_reading' emphasizes the inherent unsustainability of the system, leading to its eventual collapse and the emergence of a new regime. It contrasts with 'strict_convertibility_reading' (binding legal obligation) and 'policy_flexible_reading' (conditional obligation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
