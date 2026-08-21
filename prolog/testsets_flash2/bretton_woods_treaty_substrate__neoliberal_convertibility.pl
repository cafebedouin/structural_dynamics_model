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
 *   constraint_id: bretton_woods_treaty_substrate__neoliberal_convertibility
 *   human_readable: Bretton Woods: Neoliberal Convertibility Reading
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This constraint represents the 'neoliberal convertibility' reading of the
 *   Bretton Woods treaty substrate, which interprets the original agreement
 *   as primarily enabling free capital markets by constraining government
 *   intervention. This reading emerged and gained dominance after the
 *   collapse of the fixed exchange rate system in the early 1970s, shifting
 *   the focus from embedded liberalism to capital mobility. The constraint is
 *   claimed as a Rope by its proponents, but its operation, as described by
 *   the metrics, is substantially extractive and actively enforced, making it
 *   a Tangled Rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.78).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.85).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.78).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods: Neoliberal Convertibility Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, 'd73bacf4-6f1f-49dc-9a8c-4e127aae8de6').
narrative_ontology:cs_kernel_codification('d73bacf4-6f1f-49dc-9a8c-4e127aae8de6', formalized).
narrative_ontology:cs_authority_grounding('d73bacf4-6f1f-49dc-9a8c-4e127aae8de6', extraction).
narrative_ontology:cs_interpretation_layer_present('d73bacf4-6f1f-49dc-9a8c-4e127aae8de6').
narrative_ontology:cs_reading_relation('d73bacf4-6f1f-49dc-9a8c-4e127aae8de6', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, influences).
narrative_ontology:cs_reading_relation('d73bacf4-6f1f-49dc-9a8c-4e127aae8de6', bretton_woods_treaty_substrate__sovereignty_defense, influences).
narrative_ontology:cs_axiom('d73bacf4-6f1f-49dc-9a8c-4e127aae8de6', foundational, capital_mobility_is_efficient).
narrative_ontology:cs_axiom_status(capital_mobility_is_efficient, holdable).
narrative_ontology:cs_axiom_grounding('d73bacf4-6f1f-49dc-9a8c-4e127aae8de6', capital_mobility_is_efficient, empirically_contingent).
narrative_ontology:cs_axiom('d73bacf4-6f1f-49dc-9a8c-4e127aae8de6', foundational, government_intervention_distorts_markets).
narrative_ontology:cs_axiom_status(government_intervention_distorts_markets, holdable).
narrative_ontology:cs_axiom_grounding('d73bacf4-6f1f-49dc-9a8c-4e127aae8de6', government_intervention_distorts_markets, empirically_contingent).
narrative_ontology:cs_reference_frame('d73bacf4-6f1f-49dc-9a8c-4e127aae8de6', post_bretton_woods_liberal_order).
narrative_ontology:cs_drift_state('d73bacf4-6f1f-49dc-9a8c-4e127aae8de6', contemporary_global_finance, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d73bacf4-6f1f-49dc-9a8c-4e127aae8de6', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_institutions).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, global_capital_markets).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, national_governments_policy_autonomy).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the rules of the international monetary system, promoting capital mobility and convertibility. Benefits from the stability and growth of global financial markets, which it helps to regulate and expand. Its mandate is interpreted to prioritize open capital flows.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Thrives on the free movement of capital across borders, enabled by currency convertibility and reduced government intervention. Benefits from increased investment opportunities and reduced transaction costs, leading to higher profits and influence.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, global_capital_markets, beneficiary,
    institutional, generational, arbitrage, global).

% Faces constraints on its ability to use capital controls or independent monetary policy to manage domestic economic conditions. The need to maintain currency convertibility and attract foreign investment limits policy choices, leading to a loss of economic sovereignty.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, national_governments_policy_autonomy, payer,
    powerful, biographical, constrained, national).

% Are particularly vulnerable to capital flight and external financial shocks due to open capital accounts. They bear the costs of volatile exchange rates and limited policy tools to protect nascent industries or manage debt, often leading to austerity measures imposed by IFIs.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_economies, payer,
    moderate, generational, trapped, regional).

% Would argue for the necessity of capital controls to preserve domestic policy space and prevent financial instability. Their views are marginalized in the dominant interpretation of Bretton Woods, which prioritizes capital mobility.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, keynesian_economists, excluded,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for stable exchange rates and currency convertibility, facilitating international trade and investment by reducing currency risk and transaction costs.
% TRANSFER_FUNCTION: Transfers policy autonomy from national governments to the international financial system, enabling global capital markets to operate with fewer restrictions and extract rents from cross-border transactions.
% ABSENT_VOICES: Advocates for capital controls and greater national policy autonomy, particularly from developing nations, are excluded from the dominant discourse that frames such measures as violations of the Bretton Woods spirit.
% DISAPPEARANCE_RATIONALE: If the neoliberal convertibility interpretation of Bretton Woods vanished, national governments would regain significant policy space, potentially reintroducing capital controls. Global capital markets would face increased friction and fragmentation, leading to a reorganization of international finance and trade.
% FOUNDING_PROBLEM: The interwar period was characterized by competitive devaluations, trade wars, and financial instability, hindering global economic recovery and leading to a breakdown of international cooperation.
% FOUNDING_PROBLEM_CORROBORATION: International financial institutions and global capital markets attest that the problem of financial instability and the need for convertibility remains live, justifying the current framework. Critics, including some national governments and economists, argue that the current interpretation exacerbates instability for many nations, suggesting the original problem has mutated or been reinterpreted to serve new interests.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__neoliberal_convertibility, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__neoliberal_convertibility, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) because national governments, particularly developing economies, bear significant costs in terms of lost policy autonomy and vulnerability to financial shocks, while international finance benefits. Suppression is very high (0.85) as the international financial architecture, enforced by institutions like the IMF, actively discourages and penalizes capital controls. Theater ratio is low (0.15) because the enforcement of capital mobility is a core, active function, not merely performative. The metrics reflect the post-1971 evolution of the system, where the emphasis on capital mobility intensified.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of international financial institutions, this reading of Bretton Woods provides essential coordination for global economic stability. From the perspective of national governments and developing economies, it is an extractive mechanism that limits their sovereign policy choices. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   International financial institutions and global capital markets are the primary beneficiaries (d near 0.0), as the constraint enables their operation and profit. National governments' policy autonomy and developing economies are the primary victims (d near 1.0), as they bear the costs of limited policy tools and financial vulnerability. Keynesian economists are excluded, representing a voice that would challenge the foundational premises of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_vs_reinterpretation,
    'To what extent does the ''neoliberal convertibility'' reading align with the original intent of the Bretton Woods architects, versus representing a reinterpretation driven by evolving economic thought and power dynamics?',
    'Historical analysis of primary source documents, diplomatic correspondence, and economic debates from the 1940s, compared with post-1970s policy shifts and academic discourse.',
    'If it''s primarily a reinterpretation, the constraint''s legitimacy as a ''Rope'' (coordination) is weakened, supporting a ''Tangled Rope'' or ''Snare'' classification due to its constructed nature and beneficiaries. If it aligns closely with original intent, its coordination function is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_vs_reinterpretation, conceptual, 'Ambiguity between original intent and subsequent reinterpretation of the Bretton Woods agreement.').

omega_variable(
    capital_mobility_vs_stability_tradeoff,
    'Is the high degree of capital mobility enabled by this reading genuinely optimal for global economic stability, or does it introduce systemic risks and instability, particularly for vulnerable economies?',
    'Empirical studies comparing economic stability, growth, and inequality outcomes in countries with varying degrees of capital account openness, controlling for other factors. Analysis of financial crisis propagation mechanisms.',
    'If capital mobility is found to consistently destabilize, the ''coordination'' aspect of the constraint is undermined, strengthening its ''extraction'' classification. If it consistently promotes stability, the ''Rope'' claim gains empirical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_mobility_vs_stability_tradeoff, empirical, 'Trade-off between capital mobility and economic stability.').

omega_variable(
    reading_structural_delta,
    'Does the ''neoliberal convertibility'' reading structurally shift national policy autonomy into the victim set and international finance into the beneficiary set, as hypothesized?',
    'Comparative analysis of policy space and financial sector growth in the pre- and post-1971 eras, across countries adopting this reading versus those resisting it. Examination of IFI conditionalities.',
    'Confirmation of the structural delta reinforces the high extractiveness and suppression metrics, solidifying the ''Tangled Rope'' classification. Disconfirmation would require re-evaluating beneficiary/victim declarations and extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_structural_delta, empirical, 'Verification of the hypothesized structural delta for this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1971, 0.25).
narrative_ontology:measurement(bret_tr_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(bret_tr_t1995, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(bret_tr_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2008, 0.16).
narrative_ontology:measurement(bret_tr_t2015, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(bret_tr_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1971, 0.5).
narrative_ontology:measurement(bret_be_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1985, 0.65).
narrative_ontology:measurement(bret_be_t1995, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1995, 0.72).
narrative_ontology:measurement(bret_be_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2008, 0.75).
narrative_ontology:measurement(bret_be_t2015, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2015, 0.77).
narrative_ontology:measurement(bret_be_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1971, 0.6).
narrative_ontology:measurement(bret_su_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(bret_su_t1995, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1995, 0.8).
narrative_ontology:measurement(bret_su_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2008, 0.83).
narrative_ontology:measurement(bret_su_t2015, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2015, 0.84).
narrative_ontology:measurement(bret_su_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, enforcement_mechanism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate__sovereignty_defense).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, washington_consensus_policies).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, global_financial_regulation).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Bretton Woods treaty substrate. This 'neoliberal convertibility' reading emphasizes free capital markets and constrains government intervention, contrasting with the 'keynesian_embedded_liberalism' reading (which prioritizes domestic policy space) and the 'sovereignty_defense' reading (which prioritizes national monetary sovereignty). Each reading instantiates a structurally different constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
