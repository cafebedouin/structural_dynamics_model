% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__sovereignty_maximalist_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: Absolute State Sovereignty (Sovereignty Maximalist Reading)
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty maximalist' reading of the
 *   RBIO (Rules-Based International Order) practice-norm complex. It asserts
 *   that state sovereignty is absolute, and that RBIO norms are legitimate
 *   only insofar as they protect states from external interference.
 *   Humanitarian exceptions are explicitly framed as pretexts for regime
 *   change. This reading prioritizes state autonomy and non-interference
 *   above all else, even at the cost of human rights protection for
 *   populations trapped under repressive governments. The classification as a
 *   Tangled Rope reflects a genuine coordination function (preventing
 *   unwanted interference) coupled with severe asymmetric extraction (from
 *   vulnerable populations).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.85).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.9).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "Absolute State Sovereignty (Sovereignty Maximalist Reading)").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, '520d999f-dad5-4a19-8836-9cf3750f180a').
narrative_ontology:cs_kernel_codification('520d999f-dad5-4a19-8836-9cf3750f180a', formalized).
narrative_ontology:cs_authority_grounding('520d999f-dad5-4a19-8836-9cf3750f180a', extraction).
narrative_ontology:cs_interpretation_layer_present('520d999f-dad5-4a19-8836-9cf3750f180a').
narrative_ontology:cs_reading_relation('520d999f-dad5-4a19-8836-9cf3750f180a', rbio_practice_norm_complex__liberal_institutional_reading, forecloses).
narrative_ontology:cs_reading_relation('520d999f-dad5-4a19-8836-9cf3750f180a', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('520d999f-dad5-4a19-8836-9cf3750f180a', foundational, state_sovereignty_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_absolute, holdable).
narrative_ontology:cs_axiom_grounding('520d999f-dad5-4a19-8836-9cf3750f180a', state_sovereignty_absolute, deontological).
narrative_ontology:cs_axiom('520d999f-dad5-4a19-8836-9cf3750f180a', secondary, humanitarian_intervention_regime_change_pretext).
narrative_ontology:cs_axiom_status(humanitarian_intervention_regime_change_pretext, holdable).
narrative_ontology:cs_axiom_grounding('520d999f-dad5-4a19-8836-9cf3750f180a', humanitarian_intervention_regime_change_pretext, empirically_contingent).
narrative_ontology:cs_reference_frame('520d999f-dad5-4a19-8836-9cf3750f180a', westphalian_sovereignty_principle).
narrative_ontology:cs_drift_state('520d999f-dad5-4a19-8836-9cf3750f180a', contemporary_humanitarian_crises_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('520d999f-dad5-4a19-8836-9cf3750f180a', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, states_prioritizing_non_interference).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_regimes).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, humanitarian_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and benefit from the maximalist interpretation of sovereignty, using it to shield internal actions from external scrutiny and intervention. They leverage this norm to maintain power and suppress dissent without fear of international reprisal.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the norm by ensuring their own internal affairs are protected from external interference, even if they do not engage in widespread repression. They see it as a fundamental principle of international order, preventing destabilizing interventions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, states_prioritizing_non_interference, beneficiary,
    institutional, generational, mobile, global).

% Bear the primary cost of this norm, as it denies them external recourse or protection when their own governments commit atrocities or suppress fundamental rights. They are trapped by the very sovereignty that is meant to protect them.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_regimes, payer,
    powerless, immediate, trapped, national).

% Work to highlight human rights abuses and advocate for international protection, but their efforts are consistently hampered by the maximalist sovereignty norm. They face significant resistance and are often accused of undermining state legitimacy.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, humanitarian_advocates, payer,
    organized, biographical, constrained, global).

% Advocate for a conditional view of sovereignty, where state rights are balanced with human rights and international responsibilities. Their arguments for intervention or accountability are systematically rejected or framed as illegitimate interference by proponents of the maximalist view.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_institutionalists, excluded,
    analytical, generational, analytical, global).

% As permanent members of the UN Security Council, they hold the power to veto resolutions authorizing intervention, often aligning with the sovereignty maximalist view to protect their own interests or those of their allies, or to prevent precedents that could be used against them.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, p5_veto_powers, agenda_setter,
    institutional, civilizational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, non-negotiable boundary for state action, preventing external military or political interference in the internal affairs of sovereign states, thereby ensuring state autonomy and stability in the international system.
% TRANSFER_FUNCTION: Transfers absolute impunity to states regarding their internal conduct, particularly human rights, by denying any legitimate external authority for intervention beyond self-defense. This effectively transfers the burden of suffering from unaccountable regimes to their trapped populations.
% ABSENT_VOICES: Populations under repressive regimes are structurally absent from the international legal and political discourse that entrenches this maximalist view of sovereignty. Human rights organizations and advocates for the Responsibility to Protect (R2P) are present but systematically marginalized or dismissed as agents of external interference.
% DISAPPEARANCE_RATIONALE: If this maximalist interpretation of sovereignty vanished overnight, the international system would undergo a profound reorganization. The threshold for intervention would lower, international accountability mechanisms would gain teeth, and the balance of power between state rights and human rights would fundamentally shift, leading to widespread re-evaluation of state legitimacy and international obligations.
% FOUNDING_PROBLEM: To prevent powerful states from interfering in the internal affairs of weaker states, particularly in the post-colonial era, and to uphold the principle of self-determination and non-aggression among nations.
% FOUNDING_PROBLEM_CORROBORATION: States that benefit from non-interference (especially authoritarian ones) assert the problem of external interference is still live and paramount. Human rights organizations and some liberal states argue the founding problem has largely shifted from preventing colonial interference to enabling internal repression, and that the original problem is substantially solved in its original form, making the current application of the norm a pretext for impunity. This is corroborated by UN reports and independent human rights investigations.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__sovereignty_maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__sovereignty_maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because this reading grants regimes broad impunity, allowing them to extract from their populations without external accountability. Suppression (0.90) is very high, as it actively blocks any legitimate external intervention or recourse for victims. The theater ratio (0.40) is moderate, reflecting that while the rhetoric of sovereignty is consistently invoked, its application is often selective, serving the interests of powerful states or regimes rather than a universal principle. The rising trend in extractiveness and suppression over the interval reflects the hardening of this maximalist stance in response to increasing calls for humanitarian intervention and accountability.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of authoritarian regimes, this norm is a legitimate defense of national self-determination and a bulwark against neo-colonialism. From the perspective of trapped populations and humanitarian advocates, it is a cruel mechanism that enables severe human rights abuses by denying external protection. The engine's classification as Tangled Rope captures this dual nature: a coordination function for states, but an extractive snare for vulnerable populations.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes and states prioritizing non-interference are clear beneficiaries (low directionality), as the norm shields them from external pressure. Populations under repressive regimes and humanitarian advocates are the primary targets (high directionality), bearing the costs of impunity and denied recourse. Liberal institutionalists and P5 veto powers, while having different motivations, play roles in either challenging or upholding this norm, with P5 powers often acting as agenda-setters by blocking interventions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_absolute_vs_conditional,
    'Is state sovereignty an absolute, unconditional right, or is it conditional upon a state''s adherence to certain international human rights obligations?',
    'Conceptual resolution through international legal consensus or a shift in state practice and treaty interpretation that explicitly redefines the scope of sovereignty.',
    'If resolved as conditional, the maximalist reading''s foundational axiom would be overridden, leading to a reclassification towards a more Rope-like or Scaffold-like constraint with greater accountability. If resolved as absolute, the current classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_absolute_vs_conditional, conceptual, 'The fundamental conceptual disagreement over the nature of state sovereignty.').

omega_variable(
    humanitarian_intervention_motive_ambiguity,
    'Are ''humanitarian exceptions'' genuinely motivated by human protection, or are they consistently pretexts for geopolitical interests and regime change?',
    'Empirical analysis of intervention outcomes, including post-intervention stability, human rights improvements, and the absence of clear geopolitical gains for intervening powers. This requires rigorous, independent, and long-term studies.',
    'If interventions are consistently found to be pretexts, the maximalist reading''s claim is strengthened, reinforcing its extractive classification. If genuine humanitarian motives and positive outcomes are demonstrated, the ''pretext'' axiom would be challenged, potentially weakening the constraint''s suppressive and extractive force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_intervention_motive_ambiguity, empirical, 'The empirical question of the true motives and outcomes of humanitarian interventions.').

omega_variable(
    cost_of_non_interference_for_populations,
    'What is the quantifiable human cost (lives lost, suffering endured, development stunted) for populations trapped under repressive regimes due to the strict adherence to non-interference?',
    'Comprehensive, independent epidemiological and socio-economic studies conducted by international bodies or NGOs, focusing on regions where intervention was debated but not undertaken due to sovereignty concerns.',
    'Quantifying this cost would provide strong empirical evidence of the extraction from victims, potentially shifting the perceived legitimacy of the constraint and increasing pressure for alternative norms or mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_of_non_interference_for_populations, empirical, 'The unmeasured human cost borne by populations due to the non-interference principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(rbio_tr_t1995, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1995, 0.32).
narrative_ontology:measurement(rbio_tr_t2000, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(rbio_tr_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2005, 0.37).
narrative_ontology:measurement(rbio_tr_t2010, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(rbio_tr_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2015, 0.39).
narrative_ontology:measurement(rbio_tr_t2020, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(rbio_be_t1995, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1995, 0.78).
narrative_ontology:measurement(rbio_be_t2000, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(rbio_be_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2005, 0.82).
narrative_ontology:measurement(rbio_be_t2010, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(rbio_be_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2015, 0.84).
narrative_ontology:measurement(rbio_be_t2020, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(rbio_su_t1995, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1995, 0.83).
narrative_ontology:measurement(rbio_su_t2000, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(rbio_su_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2005, 0.87).
narrative_ontology:measurement(rbio_su_t2010, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(rbio_su_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2015, 0.89).
narrative_ontology:measurement(rbio_su_t2020, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the RBIO practice-norm complex kernel. This 'sovereignty maximalist' reading emphasizes absolute state sovereignty, contrasting with the 'liberal institutional' view of conditional sovereignty and the 'hegemonic extraction' critique of RBIO as a frozen hegemonic project.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
