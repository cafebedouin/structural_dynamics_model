% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__keynesian_embedded_liberalism, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
 *   human_readable: Bretton Woods Capital Controls â Keynesian Embedded Liberalism Reading
 *   domain: international political economy / monetary history
 *
 * SUMMARY:
 *   This is the keynesian_embedded_liberalism reading of the
 *   bretton_woods_treaty_substrate kernel. Under this reading, the Bretton
 *   Woods agreements create structural constraints on international capital
 *   mobilityâchiefly through national capital controls and IMF-endorsed
 *   exchange-rate disciplinesâin order to protect domestic macroeconomic
 *   policy space. The constraint is not a natural law but a constructed
 *   institutional arrangement that redistributes policy autonomy from global
 *   financial markets to nation-states. It coexists with competing readings
 *   that frame the same treaty substrate as constraining government
 *   intervention (neoliberal_convertibility) or as constraining external
 *   monetary discipline (sovereignty_defense).
 *
 * KEY AGENTS:
 *   - national_governments: Primary agenda-setter (institutional/arbitrage) â administers capital controls and captures policy autonomy
 *   - cross_border_investors: Primary target (powerful/constrained) â bears mobility restrictions and reduced liquidity
 *   - domestic_constituencies: Diffuse beneficiary (organized/constrained) â receives macro-policy stability without direct control over the arrangement
 *   - imf_bureaucracy: Analytical observer (institutional/analytical) â provides surveillance architecture without direct enforcement of controls
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.62).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.58).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.62).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods Capital Controls â Keynesian Embedded Liberalism Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "international political economy / monetary history").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'f2417997-12b3-4a0b-81d6-1937373d05b5').
narrative_ontology:cs_kernel_codification('f2417997-12b3-4a0b-81d6-1937373d05b5', formalized).
narrative_ontology:cs_authority_grounding('f2417997-12b3-4a0b-81d6-1937373d05b5', lineage).
narrative_ontology:cs_interpretation_layer_present('f2417997-12b3-4a0b-81d6-1937373d05b5').
narrative_ontology:cs_reading_relation('f2417997-12b3-4a0b-81d6-1937373d05b5', bretton_woods_treaty_substrate__neoliberal_convertibility, coexists_with).
narrative_ontology:cs_reading_relation('f2417997-12b3-4a0b-81d6-1937373d05b5', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('f2417997-12b3-4a0b-81d6-1937373d05b5', foundational, capital_controls_legitimate_macroprudential_tool).
narrative_ontology:cs_axiom_status(capital_controls_legitimate_macroprudential_tool, holdable).
narrative_ontology:cs_axiom_grounding('f2417997-12b3-4a0b-81d6-1937373d05b5', capital_controls_legitimate_macroprudential_tool, conventional).
narrative_ontology:cs_axiom('f2417997-12b3-4a0b-81d6-1937373d05b5', foundational, domestic_stability_priority_over_mobility).
narrative_ontology:cs_axiom_status(domestic_stability_priority_over_mobility, holdable).
narrative_ontology:cs_axiom_grounding('f2417997-12b3-4a0b-81d6-1937373d05b5', domestic_stability_priority_over_mobility, instrumental).
narrative_ontology:cs_reference_frame('f2417997-12b3-4a0b-81d6-1937373d05b5', embedded_liberalism_compromise).
narrative_ontology:cs_drift_state('f2417997-12b3-4a0b-81d6-1937373d05b5', neoliberal_hegemony_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('f2417997-12b3-4a0b-81d6-1937373d05b5', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_constituencies).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, cross_border_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated and administered the Bretton Woods capital control frameworks, enforcing fixed exchange rates and restricting cross-border capital movements to preserve domestic fiscal and monetary policy autonomy. They set the rules of the IMF Articles and retained the right to adjust controls unilaterally.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, agenda_setter,
    institutional, generational, arbitrage, global).

% Face legal restrictions on repatriating capital, arbitraging interest rate differentials, and exiting positions during macroeconomic adjustments. Their liquidity is reduced and investment horizons lengthened by control regimes that require official approval for large transfers.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, cross_border_investors, payer,
    powerful, biographical, constrained, global).

% Receive the downstream benefit of governments' ability to maintain full-employment targets and countercyclical fiscal policies without immediate capital flight forcing premature austerity or exchange-rate devaluation.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_constituencies, beneficiary,
    organized, generational, constrained, national).

% Provides the surveillance and balance-of-payments financing architecture within which national capital controls operate. It monitors exchange rate compliance but does not directly enforce controls; its analytical frame treats capital restrictions as permissible under Article VI.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, imf_bureaucracy, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the macroeconomic policy trilemma by enabling fixed exchange rates and domestic policy autonomy simultaneously, at the cost of constraining international capital mobility. Prevents competitive devaluations and destabilizing speculative flows that characterized the interwar period.
% TRANSFER_FUNCTION: Moves policy discretion and macroeconomic stability from international capital markets to national governments and their domestic constituencies; transfers liquidity and freedom of movement from cross-border investors to the domestic policy space.
% ABSENT_VOICES: Offshore financial centers and future Eurodollar market participants were not present at Bretton Woods; they would later argue that technological and market innovation made capital controls unworkable. Neoliberal challengers who read the treaty as a constraint on government rather than capital were politically marginal at founding but grew in later decades.
% DISAPPEARANCE_RATIONALE: If the capital control constraints vanished overnight while fixed exchange rates remained, immediate capital flight and interest-rate arbitrage would force monetary convergence, eliminating domestic policy autonomy and destabilizing the cooperative fixed-rate structure. Governments would lose the macro-policy space that justified the embedded-liberalism compromise.
% FOUNDING_PROBLEM: The interwar gold-standard collapse demonstrated that unregulated international capital flows caused competitive devaluations, austerity spirals, and domestic political destabilization when combined with fixed exchange rates and no adjustment mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians Barry Eichengreen and Harold James attest the interwar monetary instability from outside the benefiting national governments. Keynes, as intellectual architect, attested from the design seat but not as a direct beneficiary of extraction. Neoliberal critics later contested the diagnosis, attributing interwar failure to policy errors rather than capital mobility itself.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-to-high because capital controls significantly restrict the freedom of cross-border investors and capture policy space for governments. Suppression (0.58) reflects the active legal and administrative machinery required to monitor and block prohibited capital flows. Theater_ratio (0.30 at t0) rises over the interval as Eurodollar markets and regulatory arbitrage increasingly hollow out the functional bindingness of controls, converting enforcement into performative compliance by the late 1960s. Accessibility_collapse (0.75) is high for investors because, once inside the BW framework, legally unrestricted capital movement is simply unavailable. Resistance (0.55) is moderate: international banking centers and offshore markets mounted steady pressure, but the constraint persisted for decades before yielding.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (national governments) experiences the constraint as necessary coordination for macroeconomic stability and democratic legitimacy; the payer seat (cross-border investors) experiences the identical structure as extraction of their liquidity and strategic flexibility. The engine computes this divergence from the structural dataâbeneficiaries with arbitrage exit versus victims with constrained exitârather than from any authored classification override.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments are structural beneficiaries with arbitrage-grade exit (they can unilaterally modify or suspend controls), placing their directionality near the beneficiary end. Cross-border investors are structural targets with constrained exit, placing their directionality near the full-target end. Domestic constituencies sit closer to symmetric: they benefit from policy stability but cannot exit the national economy. The IMF bureaucracy sits at analytical distance with no direct extraction or payment.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents mislabeling the arrangement as pure extraction (Snare) because a genuine coordination function is present: without capital controls, fixed exchange rates and domestic policy autonomy are structurally incompatible under the impossible trinity. Conversely, it prevents mislabeling as pure coordination (Rope) because identifiable victims bear asymmetric costsâcross-border investors lose liquidity and mobility. The mandatrophy question is whether the founding problem (interwar instability) remains live; by the late 1960s the problem had mutated, yet the arrangement persisted, generating rising theater and drift toward Piton-like inertia before final collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trilemma_structural_vs_contingent,
    'Is the impossible trinity a natural structural feature of monetary economics or a historically contingent policy construction?',
    'Comparative historical analysis of monetary regimes that sustained capital mobility with fixed rates and policy autonomy; if any exist, the trilemma is falsified as natural law.',
    'If the trilemma is structural, the constraint''s extraction is the necessary price of coordination; if contingent, the extraction is a politically chosen redistribution and the coordination story weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trilemma_structural_vs_contingent, conceptual, 'Whether the trilemma is a mountain or a constructed justification').

omega_variable(
    eurodollar_erosion_mechanism,
    'Did the Eurodollar market and regulatory arbitrage technologically undermine Bretton Woods capital controls, or did political abandonment precede functional erosion?',
    'Timeline analysis comparing control effectiveness metrics against legislative and administrative loosening events in major economies.',
    'If technological, the constraint''s decay was exogenous and natural; if political, the constraint was viable but deliberately dismantled, implying its classification as Tangled Rope remained stable until the abandonment decision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eurodollar_erosion_mechanism, empirical, 'Technological vs political source of constraint erosion').

omega_variable(
    capital_control_cost_allocation,
    'Were the efficiency costs of capital controls absorbed by cross-border investors or transmitted to domestic economies through reduced investment and misallocation?',
    'Empirical growth and investment studies comparing control and non-control regimes, tracing incidence of deadweight loss.',
    'If domestic constituencies ultimately paid, the beneficiary/victim structure is misaligned and national governments extract from both sides, shifting the computed directionality for domestic constituencies toward the target end.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capital_control_cost_allocation, empirical, 'Who ultimately bore the economic cost of the controls').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t0, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bret_tr_t5, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 5, 0.25).
narrative_ontology:measurement(bret_tr_t10, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 10, 0.3).
narrative_ontology:measurement(bret_tr_t15, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 15, 0.42).
narrative_ontology:measurement(bret_tr_t20, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 20, 0.55).
narrative_ontology:measurement(bret_tr_t27, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 27, 0.68).

% Extraction over time
narrative_ontology:measurement(bret_be_t0, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(bret_be_t5, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(bret_be_t10, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(bret_be_t15, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(bret_be_t20, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(bret_be_t27, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 27, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t0, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(bret_su_t5, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(bret_su_t10, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(bret_su_t15, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(bret_su_t20, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(bret_su_t27, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 27, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the bretton_woods_treaty_substrate kernel, which decomposes into at least three structurally distinct claims: keynesian_embedded_liberalism (constraints on capital), neoliberal_convertibility (constraints on government), and sovereignty_defense (constraints on external discipline). Each reading carries its own epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family through the kernel committer frame, not through causal network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
