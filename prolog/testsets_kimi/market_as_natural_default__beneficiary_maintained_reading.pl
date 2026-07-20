% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__beneficiary_maintained_reading, []).

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
 *   constraint_id: market_as_natural_default__beneficiary_maintained_reading
 *   human_readable: Market Naturalization as Beneficiary-Maintained Default
 *   domain: political_economy/ideology/economic_history
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel
 *   'market_as_natural_default'. The beneficiary_maintained_reading holds
 *   that the treatment of market allocation as the natural and inevitable
 *   default is not an organic consensus but an engineered ideological closure
 *   actively defended post-hoc by incumbent financial and corporate
 *   interests. It differs from the lapsed_alternative_reading (which
 *   attributes dominance to historical forgetting) and the
 *   hybrid_amnesia_reading (which synthesizes forgetting and capture) by
 *   locating causal primacy in identifiable beneficiary action: PR campaigns,
 *   think-tank funding, and institutional capture. The kernel is decomposed
 *   per the epsilon-invariance principle because the mechanism of persistence
 *   (active defense versus passive amnesia) implies different epsilon values
 *   and stakeholder structures.
 *
 * KEY AGENTS:
 *   - Finance capital (agenda_setter): Funds and orchestrates the ideological defense of market naturalization; captures regulatory discourse.
 *   - Corporate incumbents (beneficiary): Collect distributional advantages from the default framing without administering the ideological apparatus.
 *   - Labor movements (payer): Bear costs of suppressed wage bargaining and blocked public alternatives.
 *   - Public sector constituencies (payer): Lose legitimacy and funding as public provision is reframed as unnatural.
 *   - Heterodox academics (excluded): Empirically document alternatives but are marginalized from policy discourse.
 *   - Social movements (payer): Advocate decommodification but are filtered out by the naturalization frame.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, 0.48).
domain_priors:suppression_score(market_as_natural_default__beneficiary_maintained_reading, 0.62).
domain_priors:theater_ratio(market_as_natural_default__beneficiary_maintained_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__beneficiary_maintained_reading, "Market Naturalization as Beneficiary-Maintained Default").
narrative_ontology:topic_domain(market_as_natural_default__beneficiary_maintained_reading, "political_economy/ideology/economic_history").

domain_priors:requires_active_enforcement(market_as_natural_default__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__beneficiary_maintained_reading, '6a13c016-419e-48fa-9faf-a26ece98624a').
narrative_ontology:cs_kernel_codification('6a13c016-419e-48fa-9faf-a26ece98624a', distributed).
narrative_ontology:cs_authority_grounding('6a13c016-419e-48fa-9faf-a26ece98624a', extraction).
narrative_ontology:cs_interpretation_layer_present('6a13c016-419e-48fa-9faf-a26ece98624a').
narrative_ontology:cs_reading_relation('6a13c016-419e-48fa-9faf-a26ece98624a', market_as_natural_default__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('6a13c016-419e-48fa-9faf-a26ece98624a', market_as_natural_default__hybrid_amnesia_reading, coexists_with).
narrative_ontology:cs_axiom('6a13c016-419e-48fa-9faf-a26ece98624a', foundational, market_naturalism_is_beneficiary_maintained).
narrative_ontology:cs_axiom_status(market_naturalism_is_beneficiary_maintained, holdable).
narrative_ontology:cs_axiom_grounding('6a13c016-419e-48fa-9faf-a26ece98624a', market_naturalism_is_beneficiary_maintained, empirically_contingent).
narrative_ontology:cs_axiom('6a13c016-419e-48fa-9faf-a26ece98624a', foundational, active_closure_suppresses_alternatives).
narrative_ontology:cs_axiom_status(active_closure_suppresses_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('6a13c016-419e-48fa-9faf-a26ece98624a', active_closure_suppresses_alternatives, empirically_contingent).
narrative_ontology:cs_reference_frame('6a13c016-419e-48fa-9faf-a26ece98624a', market_allocation_as_default_state).
narrative_ontology:cs_drift_state('6a13c016-419e-48fa-9faf-a26ece98624a', post_2008_financial_crisis, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6a13c016-419e-48fa-9faf-a26ece98624a', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, finance_capital).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, corporate_incumbents).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, labor_movements).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, public_sector_constituencies).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, social_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Funds neoliberal think tanks, captures regulatory bodies, and orchestrates PR campaigns to maintain the framing that market allocation is the natural and inevitable default for organizing economic life. Benefits from deregulatory presets and the delegitimization of public alternatives.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, finance_capital, agenda_setter,
    powerful, generational, arbitrage, global).

% Benefit from policy environments where market outcomes are treated as natural benchmarks, enabling regulatory capture and the crowding out of public or cooperative competitors. Do not directly administer the ideological apparatus but reap its distributional rewards.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, corporate_incumbents, beneficiary,
    powerful, generational, arbitrage, global).

% Bear the costs of wage suppression and precarity as non-market labor protections are reframed as market distortions. Their policy alternatives are dismissed as economically illiterate or politically infeasible within the naturalized frame.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, labor_movements, payer,
    organized, biographical, constrained, national).

% Lose funding and legitimacy as public goods and services are systematically reframed as market failures or inefficiencies. Their preferred decommodified arrangements are actively crowded out by the naturalization narrative.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, public_sector_constituencies, payer,
    moderate, generational, constrained, national).

% Research documenting non-market coordination successes and market failures is marginalized in funding streams, publishing gatekeeping, and policy formation. Their empirical work is treated as ideological rather than scientific.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, heterodox_academics, excluded,
    moderate, biographical, constrained, national).

% Advocate for decommodified goods such as housing and healthcare but face the framing that such demands violate natural economic order. Their proposals are filtered out of mainstream policy discourse before reaching the agenda.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, social_movements, payer,
    powerless, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__beneficiary_maintained_reading, finance_capital).
narrative_ontology:fixing_cost_class(market_as_natural_default__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared default mechanism for resource allocation and price discovery across complex economies, reducing the negotiation and information costs of decentralized coordination.
% TRANSFER_FUNCTION: Moves policy legitimacy, regulatory forbearance, and public acceptance from non-market coordination advocates to incumbent market actors by framing market outcomes as natural and inevitable rather than politically contingent.
% ABSENT_VOICES: Heterodox economists, public-sector planners, and social movements advocating for decommodified provision are structurally excluded from policy discourse; their empirical cases are dismissed as politically motivated or economically illiterate.
% DISAPPEARANCE_RATIONALE: If the naturalization constraint vanished overnight, policy space would open for public options, planning mechanisms, and mixed-economy arrangements; incumbent beneficiaries would lose the 'inevitability' shield that prevents regulatory challenge, and distributional outcomes would shift.
% FOUNDING_PROBLEM: Historical coordination failures of early industrial economiesâinformation scarcity, calculation problems, and the need for decentralized allocationâcreated a demand for mechanisms to coordinate complex production and exchange.
% FOUNDING_PROBLEM_CORROBORATION: Labor historians and institutional economists outside the beneficiary set attest that decentralized coordination problems have been addressed by multiple mechanisms historically and that the original calculation debate has been superseded by modern information technology; beneficiary-funded think tanks assert the problem remains live and markets are the only viable solution.
narrative_ontology:disappearance_verdict(market_as_natural_default__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__beneficiary_maintained_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__beneficiary_maintained_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_as_natural_default__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__beneficiary_maintained_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.48) because the naturalization narrative extracts policy space and distributional surplus from non-market actors, but some genuine coordination function remains in the underlying market mechanism. Suppression (0.62) reflects active institutional and discursive barriers to alternatives. Theater ratio (0.45) captures the performative dimension: much 'economic science' defense of market defaultness functions as beneficiary-serving theater rather than open inquiry. Accessibility collapse (0.58) is incomplete because alternatives are suppressed but not forgotten; historical memory of public coordination persists. Resistance (0.55) is significant because labor and social movements actively contest the frame. The measurement series show rising extraction and theater from 1980â2020 as neoliberal consolidation deepened and then entered a defended, post-hoc phase.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as legitimate coordination they maintain for social efficiency; payer and excluded seats experience it as an enforced ideological closure that extracts policy possibility. The engine computes this divergence from the structural asymmetry in power and exit: finance capital has arbitrage-grade exit and generational time horizons, while labor and social movements are constrained or trapped.
 *
 * DIRECTIONALITY LOGIC:
 *   Finance capital and corporate incumbents are structural beneficiaries (low d): the constraint subsidizes their regulatory environment and delegitimizes challengers. Labor movements, public sector constituencies, and social movements are structural targets (high d): they pay the costs of constrained policy imagination and distributional shift. Heterodox academics are excluded (high d, but through exclusion rather than direct extraction). The directionality is amplified by scope: the constraint operates at national-to-global scale, making verification of alternatives harder.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) because market mechanisms do solve genuine coordination problems; it also prevents mislabeling it as pure coordination (rope) because the naturalization layer is actively defended by identifiable beneficiaries who capture asymmetric rents. The mandatrophy questionâwhether the mandate has outlived its functionâis partially live: the original coordination problem remains real, but the beneficiary-maintained naturalization is a zombie layer atop it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_structural_vs_ideological,
    'Is the suppression of non-market alternatives achieved through structural barriers (funding capture, institutional exclusion) or through ideological internalization (policymakers and public genuinely believe markets are natural)?',
    'Comparative policy analysis across jurisdictions with different media and think-tank capture levels; if alternatives resurface when structural funding shifts, suppression is structural.',
    'If ideological, effective extraction is higher than the structural measure suggestsâthe constraint travels with agents across institutional contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_ideological, conceptual, 'Structural versus internalized suppression mechanism').

omega_variable(
    naturalization_vs_mechanism_separation,
    'Does this reading isolate the ideological naturalization as a separable constraint from the market mechanism itself, or do they constitute a single constraint?',
    'Comparative analysis of market mechanisms operating without naturalization narratives (e.g., social democratic corporatism) to see if extraction persists without the beneficiary-maintained framing.',
    'If inseparable, the epsilon belongs to a larger constraint; if separable, this reading is validly epsilon-invariant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_vs_mechanism_separation, conceptual, 'Separability of naturalization layer from market mechanism').

omega_variable(
    beneficiary_capture_measurement,
    'To what extent is the defense of market naturalization directly funded and orchestrated by identifiable incumbent beneficiaries versus diffusely reproduced by cultural inertia?',
    'Trace funding and personnel flows from corporate and finance actors to PR firms, think tanks, and academic centers promoting naturalization narratives.',
    'High capture supports the tangled_rope or snare reading; low capture with high diffuse reproduction would shift toward a piton or rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_measurement, empirical, 'Direct beneficiary capture versus diffuse cultural reproduction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__beneficiary_maintained_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(market_nat_ben_tr_t0, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(market_nat_ben_tr_t8, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(market_nat_ben_tr_t16, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(market_nat_ben_tr_t24, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(market_nat_ben_tr_t32, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 32, 0.43).
narrative_ontology:measurement(market_nat_ben_tr_t40, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(market_nat_ben_be_t0, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(market_nat_ben_be_t8, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(market_nat_ben_be_t16, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(market_nat_ben_be_t24, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 24, 0.42).
narrative_ontology:measurement(market_nat_ben_be_t32, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 32, 0.45).
narrative_ontology:measurement(market_nat_ben_be_t40, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(market_nat_ben_su_t0, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(market_nat_ben_su_t8, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(market_nat_ben_su_t16, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(market_nat_ben_su_t24, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(market_nat_ben_su_t32, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 32, 0.61).
narrative_ontology:measurement(market_nat_ben_su_t40, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__beneficiary_maintained_reading, resource_allocation).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default__hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel market_as_natural_default. The epsilon values and stakeholder structures differ across readings: this reading emphasizes active beneficiary defense with moderate-to-high extractiveness, while sibling readings emphasize historical amnesia or hybrid mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
