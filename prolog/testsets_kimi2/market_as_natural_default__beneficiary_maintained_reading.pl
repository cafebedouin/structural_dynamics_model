% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: market_as_natural_default__beneficiary_maintained_reading
 *   human_readable: Market Naturalization as Actively Defended by Incumbent Beneficiaries
 *   domain: political_economy/ideology
 *
 * SUMMARY:
 *   This is the beneficiary_maintained_reading of the
 *   market_as_natural_default kernel. The constraint is the standing
 *   arrangement in which market allocation is treated as the natural,
 *   unmarked default state of economic organization. In this reading, the
 *   persistence of that naturalization is not explained by historical inertia
 *   or genuine consensus but by the active, interested defense of
 *   identifiable incumbent beneficiariesâfinancial and corporate
 *   actorsâthrough institutional capture, public relations, and the
 *   colonization of policy discourse. The arrangement coordinates
 *   expectations and stabilizes the political economy (a genuine coordination
 *   function) while asymmetrically extracting by suppressing non-market
 *   alternatives.
 *
 * KEY AGENTS:
 *   - financial_incumbents: primary beneficiary (institutional/arbitrage) â collects rents and funds ideological defense
 *   - corporate_incumbents: primary beneficiary (institutional/arbitrage) â lobbies for market-default policies
 *   - mainstream_policy_institutions: agenda_setter (institutional/constrained) â enforces the naturalization administratively
 *   - general_public: primary target (powerless/identity_locked) â bears diffuse costs of foregone public alternatives
 *   - public_sector_agencies: secondary target (organized/constrained) â suffers budget erosion and delegitimation
 *   - heterodox_reformers: excluded voice (moderate/constrained) â argues for alternatives but is marginalized
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
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__beneficiary_maintained_reading, "Market Naturalization as Actively Defended by Incumbent Beneficiaries").
narrative_ontology:topic_domain(market_as_natural_default__beneficiary_maintained_reading, "political_economy/ideology").

domain_priors:requires_active_enforcement(market_as_natural_default__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__beneficiary_maintained_reading, '83e1e80d-02e6-4383-a40e-1e641353cacf').
narrative_ontology:cs_kernel_codification('83e1e80d-02e6-4383-a40e-1e641353cacf', distributed).
narrative_ontology:cs_authority_grounding('83e1e80d-02e6-4383-a40e-1e641353cacf', extraction).
narrative_ontology:cs_interpretation_layer_present('83e1e80d-02e6-4383-a40e-1e641353cacf').
narrative_ontology:cs_reading_relation('83e1e80d-02e6-4383-a40e-1e641353cacf', market_as_natural_default__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('83e1e80d-02e6-4383-a40e-1e641353cacf', market_as_natural_default__hybrid_amnesia_reading, coexists_with).
narrative_ontology:cs_axiom('83e1e80d-02e6-4383-a40e-1e641353cacf', foundational, active_beneficiary_defense_maintains_market_legitimacy).
narrative_ontology:cs_axiom_status(active_beneficiary_defense_maintains_market_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('83e1e80d-02e6-4383-a40e-1e641353cacf', active_beneficiary_defense_maintains_market_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('83e1e80d-02e6-4383-a40e-1e641353cacf', market_default_under_beneficiary_maintenance).
narrative_ontology:cs_drift_state('83e1e80d-02e6-4383-a40e-1e641353cacf', contemporary_post_crisis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('83e1e80d-02e6-4383-a40e-1e641353cacf', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, financial_incumbents).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, corporate_incumbents).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, general_public).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, public_sector_agencies).
narrative_ontology:constraint_vindicates(market_as_natural_default__beneficiary_maintained_reading, market_fundamentalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Capture rents from deregulated financial markets and benefit from the closure of public and collective alternatives; fund think tanks, academic chairs, and policy advocacy to defend the framing of market allocation as the natural and inevitable default.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, financial_incumbents, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from reduced regulatory threat, tax competition, and the privatization of public goods; actively promote market-default framing through lobbying, revolving doors, and institutional funding.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, corporate_incumbents, beneficiary,
    institutional, generational, arbitrage, global).

% Administer and enforce market-default policies in legislation, regulation, and central banking; career advancement and funding depend on adherence to market-fundamentalist orthodoxy, making deviation costly.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, mainstream_policy_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Bears the cost of foregone public goods, degraded social insurance, and constrained democratic choice; exposed to pervasive ideological framing that treats market outcomes as inevitable and individually deserved.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, general_public, payer,
    powerless, biographical, identity_locked, national).

% Suffer budget erosion, privatization pressure, and delegitimation as market mechanisms are installed as the default solution to coordination problems that were previously addressed through public provision.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, public_sector_agencies, payer,
    organized, biographical, constrained, national).

% Argue for non-market coordination, public ownership, and democratic planning but are structurally marginalized in academic hiring, policy discourse, and media representation.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, heterodox_reformers, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes macroeconomic expectations and reduces deliberation costs by treating market allocation as the unmarked default, which coordinates policymakers, investors, and publics around a shared script for resource allocation.
% TRANSFER_FUNCTION: Moves policy space, legitimacy, and public resources from collective and public-sector coordination mechanisms to market-mediated allocation, with the surplus captured by incumbent financial and corporate actors through preserved rent streams and deflected regulatory threat.
% ABSENT_VOICES: Heterodox economists, public-sector planners, and labor organizers who would argue for non-market coordination are structurally excluded from mainstream policy discourse, funding streams, and respectable opinion.
% DISAPPEARANCE_RATIONALE: If the active defense of market naturalization vanished, the policy default would shift toward explicit democratic negotiation over allocation mechanisms, public alternatives would regain legitimacy, and incumbent rent streams would face higher regulatory and redistributive threat.
% FOUNDING_PROBLEM: How to coordinate complex economic activity and resource allocation in industrial societies without comprehensive centralized planning, and how to legitimize market allocation against organized collective alternatives.
% FOUNDING_PROBLEM_CORROBORATION: Heterodox economists and economic historians outside the beneficiary set attest that the founding coordination problem has been substantially addressed by diverse mixed and democratic planning mechanisms; incumbent-funded think tanks and policy institutions assert the problem remains acute. Independent comparative political economy research supports the mixed-economy reading.
narrative_ontology:disappearance_verdict(market_as_natural_default__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__beneficiary_maintained_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__beneficiary_maintained_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.48) is moderate-to-high because the constraint systematically forecloses public and collective coordination mechanisms, channeling policy space toward market allocation. Suppression (0.62) reflects active mechanismsâfunding capture, revolving doors, media exclusionâthat keep alternatives off the agenda, not merely passive forgetting. Theater ratio (0.45) captures the post-hoc performative defense: claims about market efficiency and naturalness that exceed empirical support and intensify as the arrangement faces challenge. Accessibility collapse (0.58) indicates that while seeing through the naturalization restores cognitive access to alternatives, structural barriers (career risk, funding dependence, institutional design) keep those alternatives closed. Resistance (0.40) records persistent but marginalized contestation from social movements and heterodox scholarship.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats, the constraint appears as necessary order: without the market default, coordination collapses into planning failure. From the payer seats, the same structure appears as a captured political economy that channels collective problems into private solutions. The agenda-setter seat experiences the constraint as professional necessity rather than active extraction. The engine computes these divergences from the structural data; the authored claim does not resolve them.
 *
 * DIRECTIONALITY LOGIC:
 *   Financial and corporate incumbents are declared beneficiaries with arbitrage-grade exit, placing them at the full-beneficiary end of the directionality spectrum. The general public is declared victim with identity-locked exit, placing them at the full-target end. Mainstream policy institutions are not declared in either base array and revert to canonical fallback for institutional power, yielding a moderate directionality that reflects their captured but not directly rent-collecting position. Heterodox reformers are excluded but analytically mobile, sitting near the observer end.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was originally built to solve a genuine coordination problemâhow to allocate complex resources without comprehensive centralized planningâbut its mandate has atrophied. The naturalization of markets as the only legitimate default now persists less because it optimally coordinates and more because identifiable beneficiaries actively defend it. The founding problem status is contested: beneficiaries claim it remains live, while critics argue mixed and democratic mechanisms have substantially superseded it. This contested status prevents automatic classification as either pure coordination (rope) or pure extraction (snare), supporting the tangled_rope reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalization_mechanism_ambiguity,
    'Is market naturalization maintained primarily through active beneficiary conspiracy and institutional capture, or through organic convergence of elite beliefs and structural incentives?',
    'Comparative historical analysis of policy network funding and ideological diffusion; FOIA and funding disclosures for think tanks; network analysis of personnel rotation between finance, corporate boards, and policy institutions.',
    'If organic convergence, the constraint is better classified as rope or piton (coordination without directed extraction); if active capture, the tangled_rope or snare classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_mechanism_ambiguity, empirical, 'Whether maintenance is directed capture or organic convergence.').

omega_variable(
    suppression_vs_consensus,
    'Do policymakers and publics genuinely believe markets are the natural default, or do they suppress knowledge of alternatives due to coercion, career risk, or funding dependence?',
    'Survey of policy professionals'' private versus public beliefs; analysis of suppressed policy alternatives in legislative history; exit-interviews from public-sector agencies.',
    'If genuine consensus, suppression is lower and the constraint is less extractive; if suppressed dissent, effective suppression and extractiveness are higher than surface metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_vs_consensus, empirical, 'Whether observed alignment reflects belief or suppressed dissent.').

omega_variable(
    kernel_reading_boundary,
    'How does adopting the lapsed_alternative_reading change the beneficiary structure and extractiveness of this constraint?',
    'Comparative analysis of the same empirical evidence through the lapsed reading lens: if market naturalization is explained by historical forgetting rather than active defense, the incumbent beneficiaries become historical accidents rather than structural capturers.',
    'If the lapsed reading is adopted, the constraint''s extractiveness collapses toward piton (inertial forgetting) or rope (genuine coordination), and the moral assessment of incumbent agency shifts from culpable extraction to path dependence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural difference between active defense and historical forgetting readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__beneficiary_maintained_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mark_tr_t5, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(mark_tr_t10, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(mark_tr_t15, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(mark_tr_t20, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(mark_tr_t25, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(mark_tr_t30, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(mark_be_t5, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(mark_be_t10, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(mark_be_t15, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement(mark_be_t20, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(mark_be_t25, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 25, 0.46).
narrative_ontology:measurement(mark_be_t30, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 30, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(mark_su_t5, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 5, 0.53).
narrative_ontology:measurement(mark_su_t10, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(mark_su_t15, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(mark_su_t20, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(mark_su_t25, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 25, 0.61).
narrative_ontology:measurement(mark_su_t30, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
