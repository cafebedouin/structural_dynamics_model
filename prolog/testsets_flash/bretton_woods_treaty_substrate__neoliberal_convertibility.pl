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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bretton_woods_treaty_substrate__neoliberal_convertibility
 *   human_readable: Bretton Woods Neoliberal Convertibility Reading: Constraints on Government Intervention for Free Capital Markets
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This constraint represents the 'neoliberal convertibility' reading of the
 *   Bretton Woods treaty substrate, which emerged prominently after the
 *   collapse of the fixed exchange rate system in the early 1970s. In this
 *   reading, Bretton Woods is interpreted as fundamentally aiming to enable
 *   free capital markets by constraining government intervention,
 *   particularly through capital controls. This interpretation views national
 *   policy autonomy as a victim of necessary market discipline, and
 *   international finance as the primary beneficiary, with capital controls
 *   seen as violations rather than legitimate policy tools. The constraint is
 *   classified as a Snare due to its high extractiveness from national
 *   governments and its active suppression of alternative policy choices.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.85).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.75).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.85).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, snare).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods Neoliberal Convertibility Reading: Constraints on Government Intervention for Free Capital Markets").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, 'd85f4ef5-8ea8-42de-85cf-6972cd517287').
narrative_ontology:cs_kernel_codification('d85f4ef5-8ea8-42de-85cf-6972cd517287', formalized).
narrative_ontology:cs_authority_grounding('d85f4ef5-8ea8-42de-85cf-6972cd517287', extraction).
narrative_ontology:cs_interpretation_layer_present('d85f4ef5-8ea8-42de-85cf-6972cd517287').
narrative_ontology:cs_reading_relation('d85f4ef5-8ea8-42de-85cf-6972cd517287', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, forecloses).
narrative_ontology:cs_reading_relation('d85f4ef5-8ea8-42de-85cf-6972cd517287', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('d85f4ef5-8ea8-42de-85cf-6972cd517287', foundational, capital_mobility_is_efficient).
narrative_ontology:cs_axiom_status(capital_mobility_is_efficient, holdable).
narrative_ontology:cs_axiom_grounding('d85f4ef5-8ea8-42de-85cf-6972cd517287', capital_mobility_is_efficient, empirically_contingent).
narrative_ontology:cs_axiom('d85f4ef5-8ea8-42de-85cf-6972cd517287', foundational, government_intervention_distorts_markets).
narrative_ontology:cs_axiom_status(government_intervention_distorts_markets, holdable).
narrative_ontology:cs_axiom_grounding('d85f4ef5-8ea8-42de-85cf-6972cd517287', government_intervention_distorts_markets, empirically_contingent).
narrative_ontology:cs_reference_frame('d85f4ef5-8ea8-42de-85cf-6972cd517287', unfettered_capital_flows).
narrative_ontology:cs_drift_state('d85f4ef5-8ea8-42de-85cf-6972cd517287', post_asian_financial_crisis, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('d85f4ef5-8ea8-42de-85cf-6972cd517287', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_institutions).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, global_capital_holders).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, national_governments).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_policy_autonomy).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, labor_movements).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).

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
 *   The extractiveness (0.85) is high because national governments are forced to prioritize capital mobility over domestic policy goals, leading to austerity measures and reduced social spending to maintain investor confidence. Suppression (0.75) is significant, as international financial institutions (IFIs) and global market pressures actively discourage and penalize deviations from capital account liberalization. The theater ratio (0.1) is low, indicating that the constraint's function is largely direct extraction and enforcement, with little performative maintenance. Accessibility collapse (0.7) is high as alternative development models (e.g., state-led industrialization with capital controls) are increasingly foreclosed. Resistance (0.6) is substantial, coming from national governments, labor movements, and civil society organizations pushing back against austerity and loss of sovereignty.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of international financial institutions and global capital holders, this constraint is a necessary 'Rope' for global economic efficiency and stability. From the perspective of national governments and domestic populations, it operates as a 'Snare' that extracts wealth and policy autonomy, forcing them into a race to the bottom to attract capital. The engine's classification as a Snare reflects the latter, more extractive, structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   International financial institutions (IMF, World Bank) and global capital holders are the primary beneficiaries (d=0.0-0.2), as they profit from increased capital mobility and the 'discipline' imposed on national economies. National governments, particularly those in developing countries, are the primary targets (d=0.8-1.0), bearing the costs of constrained policy choices and capital flight. Domestic policy autonomy and labor movements are also victims, experiencing reduced leverage and social protections. The constraint subsidizes global finance by externalizing the costs of market volatility onto national populations.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of Bretton Woods has not resolved mandatrophy; rather, it has reinterpreted the original mandate (post-WWII stability and reconstruction) to serve a new, more extractive function (global capital accumulation). The original problem of preventing competitive devaluations and fostering reconstruction has been largely superseded, but the institutional framework persists, now enforcing a different set of constraints. The classification as a Snare highlights this shift from a coordination-focused mandate to an extraction-focused one, preventing it from being mislabeled as a benign Rope or a natural Mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine commitment to free capital markets, or a reinterpretation of Bretton Woods to serve neoliberal ends?',
    'Historical analysis of policy shifts and institutional mandates post-1970s, comparing stated goals with actual outcomes and beneficiary structures.',
    'If a reinterpretation, the constraint''s claimed type shifts from a ''rope'' (coordination for growth) to a ''snare'' (extraction via imposed convertibility), with higher effective extraction for national governments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between original Bretton Woods intent and neoliberal reinterpretation.').

omega_variable(
    policy_autonomy_vs_market_discipline,
    'To what extent does the ''discipline'' imposed by free capital markets genuinely foster economic stability and growth, versus merely transferring wealth to global capital holders?',
    'Empirical studies comparing economic performance and inequality in countries with varying degrees of capital account openness and policy autonomy over long time horizons.',
    'If market discipline primarily transfers wealth, the constraint''s extractiveness is higher and its coordination function is weaker, pushing it further towards a Snare. If it genuinely fosters broad-based growth, it might retain some Rope-like characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_autonomy_vs_market_discipline, empirical, 'The true impact of capital market discipline on national economies.').

omega_variable(
    neoliberal_convertibility_reading_of_bretton_woods,
    'This constraint is the ''neoliberal_convertibility'' reading of the ''bretton_woods_treaty_substrate'' kernel. How would the classification change under the ''keynesian_embedded_liberalism'' or ''sovereignty_defense'' readings?',
    'Analyze the structural deltas: ''keynesian_embedded_liberalism'' would shift national policy autonomy from victim to beneficiary, and capital controls from violation to tool, likely yielding a Rope or Tangled Rope. ''sovereignty_defense'' would emphasize national monetary control as a core beneficiary, potentially yielding a Rope or even a Mountain (if framed as an inherent right).',
    'The classification would likely shift from Snare to Rope or Tangled Rope, with significantly lower extractiveness and suppression, and different beneficiary/victim sets, reflecting the alternative interpretations of the treaty''s core purpose.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neoliberal_convertibility_reading_of_bretton_woods, conceptual, 'Impact of alternative readings of the Bretton Woods kernel on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 1971, 2001).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t0, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bret_tr_t10, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 10, 0.15).
narrative_ontology:measurement(bret_tr_t20, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 20, 0.12).
narrative_ontology:measurement(bret_tr_t30, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(bret_be_t0, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(bret_be_t10, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(bret_be_t20, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(bret_be_t30, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t0, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(bret_su_t10, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(bret_su_t20, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(bret_su_t30, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, enforcement_mechanism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, washington_consensus_policy_prescriptions).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, structural_adjustment_programs).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'bretton_woods_treaty_substrate' kernel. This 'neoliberal_convertibility' reading emphasizes free capital markets and government non-intervention, contrasting with the 'keynesian_embedded_liberalism' reading (which prioritizes domestic policy space) and the 'sovereignty_defense' reading (which prioritizes national monetary autonomy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
