% ============================================================================
% CONSTRAINT STORY: transition_causality__hybrid_trigger_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__hybrid_trigger_reading, []).

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
 *   constraint_id: transition_causality__hybrid_trigger_reading
 *   human_readable: Bretton Woods Collapse: Hybrid Trigger Causality
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story analyzes the collapse of the Bretton Woods
 *   international monetary system through the 'hybrid trigger' reading of
 *   transition causality. This reading posits that while structural
 *   contradictions (primarily the Triffin Dilemma, where the reserve currency
 *   issuer must run deficits to supply global liquidity, eventually
 *   undermining confidence in its convertibility) accumulated over time,
 *   specific contingent trigger events were necessary to actualize the
 *   system's collapse. The Vietnam War's fiscal shock and the subsequent
 *   French gold runs are identified as key triggers that pushed the system
 *   past its breaking point, leading to the Nixon Shock in 1971. The
 *   constraint is claimed as a Tangled Rope, reflecting its dual function of
 *   providing international coordination while enabling asymmetric extraction
 *   by the reserve currency issuer, maintained by active enforcement until
 *   its contradictions became unsustainable.
 *
 * KEY AGENTS:
 *   - us_government: Primary beneficiary/agenda_setter (institutional/arbitrage)
 *   - other_industrialized_nations: Primary payer/constrained beneficiary (institutional/constrained)
 *   - developing_nations: Secondary payer/excluded (powerless/trapped)
 *   - international_monetary_fund: Agenda_setter/observer (institutional/constrained)
 *   - analytical_historians: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, 0.78).
domain_priors:suppression_score(transition_causality__hybrid_trigger_reading, 0.85).
domain_priors:theater_ratio(transition_causality__hybrid_trigger_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__hybrid_trigger_reading, "Bretton Woods Collapse: Hybrid Trigger Causality").
narrative_ontology:topic_domain(transition_causality__hybrid_trigger_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, 'a9aef202-2883-45c4-b2bd-5a0d73135f08').
narrative_ontology:cs_kernel_codification('a9aef202-2883-45c4-b2bd-5a0d73135f08', formalized).
narrative_ontology:cs_authority_grounding('a9aef202-2883-45c4-b2bd-5a0d73135f08', lineage).
narrative_ontology:cs_interpretation_layer_present('a9aef202-2883-45c4-b2bd-5a0d73135f08').
narrative_ontology:cs_reading_relation('a9aef202-2883-45c4-b2bd-5a0d73135f08', transition_causality__contingent_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9aef202-2883-45c4-b2bd-5a0d73135f08', transition_causality__overdetermined_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('a9aef202-2883-45c4-b2bd-5a0d73135f08', foundational, structural_contradictions_accumulate).
narrative_ontology:cs_axiom_status(structural_contradictions_accumulate, holdable).
narrative_ontology:cs_axiom_grounding('a9aef202-2883-45c4-b2bd-5a0d73135f08', structural_contradictions_accumulate, empirically_contingent).
narrative_ontology:cs_axiom('a9aef202-2883-45c4-b2bd-5a0d73135f08', foundational, contingent_triggers_actualize_collapse).
narrative_ontology:cs_axiom_status(contingent_triggers_actualize_collapse, holdable).
narrative_ontology:cs_axiom_grounding('a9aef202-2883-45c4-b2bd-5a0d73135f08', contingent_triggers_actualize_collapse, empirically_contingent).
narrative_ontology:cs_reference_frame('a9aef202-2883-45c4-b2bd-5a0d73135f08', post_bretton_woods_agreement).
narrative_ontology:cs_drift_state('a9aef202-2883-45c4-b2bd-5a0d73135f08', post_vietnam_war_fiscal_shock, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('a9aef202-2883-45c4-b2bd-5a0d73135f08', '').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_government).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_financial_sector).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, other_industrialized_nations).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, developing_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, other_industrialized_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the issuer of the reserve currency, the US government benefited from seigniorage and the ability to run persistent balance of payments deficits without immediate consequence. It actively enforced the fixed exchange rate system while pursuing domestic and foreign policy objectives that strained the system.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_government, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefited from the dollar's central role in international trade and finance, facilitating global transactions and capital flows. Had significant influence on US policy decisions regarding the monetary system.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_financial_sector, beneficiary,
    institutional, biographical, arbitrage, global).

% Benefited from the stability of the Bretton Woods system for trade and investment, but increasingly bore the cost of accumulating dollars as US deficits grew. Nations like France actively resisted by demanding gold convertibility, but faced limited options for systemic change.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, other_industrialized_nations, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, other_industrialized_nations, beneficiary).

% Had little influence over the system's design or operation, yet were vulnerable to its instabilities and the inflationary pressures from dollar accumulation. Their economic development was often tied to the system's stability, but they lacked the power to demand reforms or exit.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, developing_nations, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, developing_nations, excluded).

% The primary institution tasked with overseeing the Bretton Woods system, providing loans and policy advice to maintain exchange rate stability. Its mandate was to manage the system, but it lacked the power to compel the US to address the Triffin Dilemma's core contradiction.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, international_monetary_fund, agenda_setter,
    institutional, generational, constrained, global).

% Study the historical evolution and collapse of the Bretton Woods system, analyzing the interplay of structural factors and contingent events. Their role is to interpret the causality of the transition.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, analytical_historians, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__hybrid_trigger_reading, us_government).
narrative_ontology:fixing_cost_class(transition_causality__hybrid_trigger_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a framework for international monetary stability, fixed exchange rates, and facilitated global trade and capital flows in the post-WWII era.
% TRANSFER_FUNCTION: Transferred seigniorage benefits and the ability to run persistent deficits to the US, while other nations accumulated dollars, effectively financing US spending and absorbing inflationary pressures.
% ABSENT_VOICES: Developing nations, whose economic vulnerabilities were exacerbated by the system's contradictions but who lacked a strong voice in its governance. Also, proponents of alternative monetary systems (e.g., a global reserve currency not tied to a single nation) were largely excluded from the policy discourse.
% DISAPPEARANCE_RATIONALE: The collapse of the Bretton Woods system in 1971 (the 'Nixon Shock') led to the floating of major currencies, a period of significant monetary instability, and a fundamental reorganization of international finance. The world economy did not remain unchanged; it adapted to a new, more volatile monetary regime.
% FOUNDING_PROBLEM: The Bretton Woods system was established to prevent a return to the monetary chaos, competitive devaluations, and protectionism that characterized the interwar period, aiming for stable exchange rates and open trade.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and international relations scholars widely agree that the original problem of interwar monetary instability was largely solved by Bretton Woods. However, the system's success in solving that problem led to new, internal contradictions (the Triffin Dilemma) that ultimately caused its demise, indicating the founding problem was superseded by new challenges.
narrative_ontology:disappearance_verdict(transition_causality__hybrid_trigger_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__hybrid_trigger_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__hybrid_trigger_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(transition_causality__hybrid_trigger_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__hybrid_trigger_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__hybrid_trigger_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(transition_causality__hybrid_trigger_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the Triffin Dilemma allowed the US to externalize costs of its deficits onto other nations, who were forced to accumulate dollars. Suppression (0.85) was very high, as the system relied on active enforcement of fixed exchange rates and the suppression of alternatives to the dollar's reserve role. Theater ratio (0.60) increased over time as the US government maintained the rhetoric of dollar convertibility while its economic policies made it increasingly impossible, leading to a growing gap between stated function and actual operation. Accessibility collapse (0.70) was substantial as nations had few viable alternatives to participating in the dollar-centric system. Resistance (0.75) grew, notably from France, as the contradictions became more apparent. The temporal measurements show a clear accumulation of extractiveness and suppression, alongside rising theatricality, leading up to the system's collapse.
 *
 * PERSPECTIVAL GAP:
 *   The US government and financial sector experienced the Bretton Woods system as a beneficial coordination mechanism that afforded them significant economic and geopolitical advantages. In contrast, other industrialized nations, and especially developing nations, increasingly experienced it as an extractive mechanism that imposed costs and limited their monetary policy autonomy. The IMF, while tasked with maintaining the system, was caught between these diverging interests, unable to resolve the fundamental contradiction.
 *
 * DIRECTIONALITY LOGIC:
 *   The US government and financial sector were clear beneficiaries, gaining seigniorage and financial dominance (low directionality). Other industrialized nations were payers, accumulating dollars and facing inflationary pressures, despite some benefits from stability (higher directionality). Developing nations were primarily victims, with minimal benefits and high vulnerability (highest directionality). The IMF, while an agenda-setter, was structurally constrained by the system's design and the power of its dominant member, placing it closer to symmetric or slightly targeted.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling the Bretton Woods system as a pure Rope (ignoring its extractive aspects) or a pure Snare (ignoring its genuine coordination function for a period). The rising theater ratio and extractiveness over time, coupled with the 'contested' status of the founding problem, indicate a system that drifted from its initial coordination mandate towards increasing extraction, eventually collapsing under the weight of its own contradictions and contingent triggers. The 'hybrid trigger' reading emphasizes that while the structural contradictions were necessary, they were not sufficient for collapse without specific events.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contingent_choice_vs_hybrid,
    'Could different US fiscal or monetary policy choices (e.g., avoiding Vietnam War spending, earlier dollar devaluation) have averted the Bretton Woods collapse, or merely delayed it?',
    'Counterfactual historical analysis, econometric modeling of alternative policy paths, and comparative studies of similar monetary regimes under different policy choices.',
    'If different choices could have averted collapse, it would lend more support to the ''contingent choice'' reading, suggesting the system was more robust than the ''hybrid trigger'' reading implies. If only delayed, it reinforces the ''hybrid trigger'' view that structural issues were paramount.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contingent_choice_vs_hybrid, empirical, 'The role of specific policy decisions versus structural inevitability in the system''s collapse.').

omega_variable(
    overdetermined_vs_hybrid,
    'Were the specific trigger events (Vietnam War fiscal shock, French gold runs) truly necessary for the collapse, or was the system so inherently unstable due to the Triffin Dilemma that any sufficiently large shock would have caused it to fail?',
    'Theoretical modeling of system resilience under various hypothetical shocks, and comparative historical analysis of other periods of stress within the Bretton Woods system.',
    'If any large shock would have sufficed, it would support the ''overdetermined collapse'' reading, diminishing the unique causal role of the specific triggers. If the specific triggers had unique properties that made them decisive, it strengthens the ''hybrid trigger'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermined_vs_hybrid, conceptual, 'The necessity of specific triggers versus the inevitability of collapse from structural contradictions.').

omega_variable(
    counterfactual_viability_of_timing,
    'Given the structural contradictions, how much could the timing and nature of the Bretton Woods collapse have varied with different contingent events?',
    'Detailed counterfactual historical simulations exploring alternative timelines and outcomes based on different trigger event scenarios.',
    'High counterfactual viability (i.e., the collapse could have happened much later or differently) would strongly support the ''hybrid trigger'' reading''s emphasis on the contingent nature of the triggers. Low viability (i.e., collapse was imminent regardless of specific triggers) would lean towards the ''overdetermined collapse'' reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_viability_of_timing, empirical, 'The degree to which the collapse''s timing was sensitive to specific contingent events.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__hybrid_trigger_reading, 1958, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1958, transition_causality__hybrid_trigger_reading, theater_ratio, 1958, 0.2).
narrative_ontology:measurement(tran_tr_t1961, transition_causality__hybrid_trigger_reading, theater_ratio, 1961, 0.3).
narrative_ontology:measurement(tran_tr_t1964, transition_causality__hybrid_trigger_reading, theater_ratio, 1964, 0.4).
narrative_ontology:measurement(tran_tr_t1967, transition_causality__hybrid_trigger_reading, theater_ratio, 1967, 0.5).
narrative_ontology:measurement(tran_tr_t1970, transition_causality__hybrid_trigger_reading, theater_ratio, 1970, 0.58).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__hybrid_trigger_reading, theater_ratio, 1971, 0.6).

% Extraction over time
narrative_ontology:measurement(tran_be_t1958, transition_causality__hybrid_trigger_reading, base_extractiveness, 1958, 0.5).
narrative_ontology:measurement(tran_be_t1961, transition_causality__hybrid_trigger_reading, base_extractiveness, 1961, 0.58).
narrative_ontology:measurement(tran_be_t1964, transition_causality__hybrid_trigger_reading, base_extractiveness, 1964, 0.65).
narrative_ontology:measurement(tran_be_t1967, transition_causality__hybrid_trigger_reading, base_extractiveness, 1967, 0.72).
narrative_ontology:measurement(tran_be_t1970, transition_causality__hybrid_trigger_reading, base_extractiveness, 1970, 0.76).
narrative_ontology:measurement(tran_be_t1971, transition_causality__hybrid_trigger_reading, base_extractiveness, 1971, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1958, transition_causality__hybrid_trigger_reading, suppression_requirement, 1958, 0.6).
narrative_ontology:measurement(tran_su_t1961, transition_causality__hybrid_trigger_reading, suppression_requirement, 1961, 0.68).
narrative_ontology:measurement(tran_su_t1964, transition_causality__hybrid_trigger_reading, suppression_requirement, 1964, 0.75).
narrative_ontology:measurement(tran_su_t1967, transition_causality__hybrid_trigger_reading, suppression_requirement, 1967, 0.8).
narrative_ontology:measurement(tran_su_t1970, transition_causality__hybrid_trigger_reading, suppression_requirement, 1970, 0.83).
narrative_ontology:measurement(tran_su_t1971, transition_causality__hybrid_trigger_reading, suppression_requirement, 1971, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__hybrid_trigger_reading, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
