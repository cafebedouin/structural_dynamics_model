% ============================================================================
% CONSTRAINT STORY: regulatory_capture_dynamic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capture_dynamic, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: regulatory_capture_dynamic
 *   human_readable: Regulatory Capture Dynamic
 *   domain: political_economy/governance
 *
 * SUMMARY:
 *   Regulatory capture represents a structural constraint that arises when
 *   regulatory agencies designed to protect the public interest become
 *   systematically oriented toward benefiting the regulated industry. The
 *   constraint exhibits a tangled coordination-extraction hybrid: genuine
 *   regulatory function coexists with systematic extraction of consumer
 *   surplus and competitive opportunity. The classic mechanism involves
 *   asymmetric information (regulated industry knows more about operations
 *   than regulators), asymmetric stakes (industry has concentrated
 *   incentives; public has dispersed interests), asymmetric access (industry
 *   has resources for lobbying; public has limited capacity), and asymmetric
 *   careers (regulators have post-regulatory employment opportunities in
 *   industry). Over time, the constraint intensifies: initial regulatory
 *   function declines as agencies accumulate captured personnel, rules become
 *   less enforced, and theater increases (agencies perform legitimacy while
 *   effectiveness erodes). The dynamics create a structural tension between
 *   the coordination function (necessary baseline
 *   environmental/safety/consumer protection) and the extraction function
 *   (above-market rents, exclusion of competition, consumer welfare loss).
 *   The constraint cannot be classified as pure extraction (snare) because
 *   some public protection occurs; cannot be classified as pure coordination
 *   (rope) because asymmetric extraction is systematic and substantial.
 *
 * KEY AGENTS:
 *   - Regulated Industry: Primary beneficiary (institutional/arbitrage) — captures extracted consumer surplus, receives market protection through regulatory barriers, shapes regulatory rules in favorable direction
 *   - Public Interest / Consumers: Primary victim (powerless/trapped) — cannot exit regulated markets, bear extraction via higher prices and reduced choice, diffuse interests prevent collective action
 *   - Competitive Entrants: Secondary victim (powerless/trapped) — face regulatory barriers to market entry constructed to exclude competition, cannot overcome permitting obstacles or compliance costs
 *   - Captured Regulator: Institutional actor (institutional/constrained) — experiences mixed coordination and extraction; constrained by career incentives, institutional dependence on industry expertise, post-regulatory employment prospects
 *   - Regulatory Framework: Institutional apparatus (institutional/constrained) — maintains performative legitimacy (published rules, formal procedures) while substantive enforcement declines
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees capture as structural delegation problem inherent to regulatory design, not contingent failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capture_dynamic, 0.58).
domain_priors:suppression_score(regulatory_capture_dynamic, 0.65).
domain_priors:theater_ratio(regulatory_capture_dynamic, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capture_dynamic, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_capture_dynamic, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(regulatory_capture_dynamic, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capture_dynamic, tangled_rope).
narrative_ontology:human_readable(regulatory_capture_dynamic, "Regulatory Capture Dynamic").
narrative_ontology:topic_domain(regulatory_capture_dynamic, "political_economy/governance").

domain_priors:requires_active_enforcement(regulatory_capture_dynamic).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capture_dynamic, regulated_industry).
narrative_ontology:constraint_victim(regulatory_capture_dynamic, public_interest).
narrative_ontology:constraint_victim(regulatory_capture_dynamic, competitive_entrants).
narrative_ontology:constraint_victim(regulatory_capture_dynamic, consumer_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC INTEREST (SNARE) — Citizens cannot exit the regulated market; bear extraction via higher prices, reduced innovation, and restricted choice. No exit mechanism for trapped agents. Maximum experienced extraction. The public interest is abstract and unorganized — cannot coordinate collective action or regulatory pressure. Pure extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(regulatory_capture_dynamic, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETITIVE ENTRANT (SNARE) — Potential competitors face regulatory barriers constructed by incumbent industry; trapped by permitting delays, compliance costs, and discretionary enforcement. No realistic exit short of abandoning market entry. Regulatory apparatus used explicitly for exclusion. High suppression, minimal coordination benefit. Pure extraction mechanism.
constraint_indexing:constraint_classification(regulatory_capture_dynamic, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATED INDUSTRY (ROPE) — Industry benefits from regulatory barriers that exclude competition; possesses arbitrage exit options (can influence regulatory design, relocate to favorable jurisdictions, or shift to unregulated markets). Experiences the constraint as coordination mechanism: stabilized market conditions, predictable pricing, protected margins. Net beneficiary with genuine exit capacity — low or negative effective extraction from this perspective.
constraint_indexing:constraint_classification(regulatory_capture_dynamic, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CAPTURED REGULATOR (TANGLED ROPE) — Regulatory agency experiences mixed coordination and extraction. Genuine coordination function: industry stability supports economy, market predictability aids planning. But agency also extracted from: dependence on industry expertise, career incentives (post-regulatory employment), epistemic capture (internalized industry worldview). Constrained exit — regulator cannot easily break captured role without institutional upheaval. High suppression (internal and institutional), active enforcement required. Both coordination and asymmetric extraction present.
constraint_indexing:constraint_classification(regulatory_capture_dynamic, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — Formal regulatory apparatus persists despite degraded function. The published rules (antitrust law, consumer protection standards, transparency requirements) exist on paper but are not enforced with vigor against captured regulated industry. Theater ratio (0.68) reflects performative compliance: agencies conduct reviews, hold hearings, publish reports — activities that create appearance of regulation while substantive protection erodes. Framework maintained through institutional inertia; exit mechanism blocked by political economy.
constraint_indexing:constraint_classification(regulatory_capture_dynamic, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, regulatory capture is a structural feature of delegation: whenever government delegates authority to regulate an industry, the regulated entity has incentives and capacity to shape regulation in its favor. Genuine coordination function (industry compliance with baseline safety/environmental/labor standards) coexists with systematic extraction (above-market rents, exclusion of competition, consumer surplus transfer). The constraint is simultaneously necessary and extractive. Theater ratio rises as agencies perform legitimacy while effectiveness declines.
constraint_indexing:constraint_classification(regulatory_capture_dynamic, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capture_dynamic_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capture_dynamic, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture_dynamic, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capture_dynamic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capture_dynamic, TR),
    TR >= 0.70.

:- end_tests(regulatory_capture_dynamic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint exhibits moderate-to-high extraction. The regulated industry captures substantial economic rents via higher prices, protected market share, and exclusion of competition. However, extraction is not maximal (not snare-level 0.66+) because some genuine regulatory function persists: environmental compliance is higher than without regulation, safety standards are enforced at baseline levels, and consumer protection mechanisms exist (though weakened). The measurement trajectory shows extraction increasing from 0.32 to 0.58 as captured personnel accumulate and enforcement atrophies. Suppression (0.65): High. Multiple barriers prevent exit or challenge: citizens cannot exit regulated markets (trapped), competitors face regulatory barriers to entry (trapped), regulators are constrained by institutional incentives and career paths, and political economy blocks reform (organized industry lobbying vs diffuse public interest). Suppression reflects structural barriers, not just coercion — the problem is partially internalized (regulators have adopted industry worldview). Theater ratio (0.68): Moderate-high. The regulatory apparatus maintains performative legitimacy — agencies conduct reviews, hold hearings, publish reports, enforce some rules — creating appearance of protection while substantive capacity erodes. The increase from 0.40 to 0.68 over 20 time units reflects accumulating theater as agencies become more captured and rely increasingly on legitimacy performance rather than functional protection.
 *
 * PERSPECTIVAL GAP:
 *   The widest perspectival gap exists between the regulated industry (rope — sees coordination benefit, net beneficiary) and the public interest (snare — experiences pure extraction, trapped). The captured regulator occupies the middle ground (tangled rope) — neither pure beneficiary nor pure victim, but extracted from and extracting. This gap reveals that 'regulation' is not a unified phenomenon: for the industry it solves coordination problems (market stabilization, predictable rules); for the public it solves coordination problems (baseline protection) but also creates extraction (consumer welfare loss, innovation suppression); for the regulator it provides career coordination but also identity capture. The piton perspective (degraded regulatory framework) observes that the published rules no longer function effectively — they persist through institutional inertia. The analytical observer's tangled rope classification differs from the public's snare perspective because the analytical view recognizes genuine coordination function coexisting with extraction, whereas the trapped agent only experiences the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position in the extraction flow. The regulated industry holds institutional power and arbitrage exit options (can influence regulation, relocate, access alternative markets) — they are beneficiaries with escape capacity, producing low or negative effective extraction from their perspective (rope classification). The public interest holds powerless status and trapped exit options (cannot exit regulated markets, cannot organize effectively) — they are victims with no escape, producing high effective extraction from their perspective (snare classification). Competitive entrants are trapped like the public but with moderate potential power if organized (powerless/trapped produces snare). The captured regulator is institutional but constrained (cannot easily break dependency without institutional crisis) and is both victim and perpetrator (extracted from by career incentives, extracting from public through weak enforcement) — producing tangled rope classification. The regulatory framework is institutional but constrained by political economy, maintained through inertia despite degraded function (piton classification). The analytical observer at civilizational scope sees the capture as structural to delegation itself (tangled rope — necessary coordination coexists with inevitable extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The regulatory capture dynamic resolves the mandatrophy by showing that tangled rope is the correct canonical classification: the constraint simultaneously coordinates industry stability (genuine function) and extracts consumer surplus (systematic asymmetry). The industry would not persist in complex coordination if there were no genuine baseline function — but the function is substantially smaller than the extracted rent suggests. This is the diagnostic signature of tangled rope: both axes are active (coordination and extraction), neither is negligible, and organized interests benefit while diffuse interests bear costs. The snare perspective (from the powerless public) represents legitimate experiential truth — the public does experience pure extraction — but captures only one side of the structural reality. The rope perspective (from the beneficiary) is aspirational: the industry frames the relationship as pure coordination, but asymmetric extraction is measurable. The analytical observer's tangled rope classification prevents mislabeling this as either pure coordination (false) or pure extraction (incomplete): the constraint genuinely requires both coordinates to explain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_mechanism_identity_lock,
    'Is the captured regulator experiencing material constraints (constrained exit) or cognitive/identity capture (identity_locked exit)?',
    'Counterfactual analysis: if institutional incentives were inverted (post-regulatory employment prospects tied to aggressive enforcement), would personnel behavior change? If yes, capture is material-constrained. If no, capture is partly identity-fused (regulators have internalized industry worldview).',
    'If material-constrained: capture is a standard institutional design problem solvable by structural incentive realignment. If identity-locked: personnel would need to reconstruct professional identity to break capture; remediation is slower and requires cultural change, not just rule changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_mechanism_identity_lock, empirical, 'Whether regulatory capture operates through material constraints or identity fusion').

omega_variable(
    coordination_function_necessity,
    'How much of the regulatory apparatus''s baseline function (e.g., environmental monitoring, safety inspection, consumer complaint investigation) is genuinely necessary coordination vs performative theater?',
    'Comparative analysis: jurisdictions with different capture levels but similar industries; measurement of actual enforcement action rates vs published rules; correlation between regulatory stringency and industry outcomes.',
    'If high necessity (> 60% core function required): tangled_rope classification justified — coordination + extraction. If low necessity (< 30% core function required): constraint slides toward snare — extraction masquerading as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'What fraction of regulation is necessary coordination vs theater').

omega_variable(
    public_interest_organization_potential,
    'Could the public interest (currently powerless/trapped) achieve organized status through collective action?',
    'Historical cases of successful public interest mobilization (environmental movements, consumer advocacy coalitions); measurement of coordination capacity and exit options when organization exists.',
    'If organizability > 0.6: powerless perspective could shift to organized, moving from snare to snare-with-coalition-threat. This would create dynamic tension and potentially force reclassification toward tangled_rope or scaffold (if sunset conditions emerge).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_interest_organization_potential, empirical, 'Whether public interest can organize to challenge capture').

omega_variable(
    alternative_regulatory_models,
    'Do alternative governance structures (independent agencies, elected regulators, user-run systems, decentralized protocols) produce lower capture rates?',
    'Comparative institutional analysis across regulatory domains and governance models; measurement of capture indicators (price-cost margins, innovation rates, consumer satisfaction) as proxy for effectiveness.',
    'If alternatives show meaningfully lower capture: current structure is contingent choice, not natural law. Scaffold or rope perspectives become viable — sunset to alternative model is structurally possible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_regulatory_models, empirical, 'Whether alternative regulatory models reduce capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capture_dynamic, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regcap_tr_t0, regulatory_capture_dynamic, theater_ratio, 0, 0.4).
narrative_ontology:measurement(regcap_tr_t10, regulatory_capture_dynamic, theater_ratio, 10, 0.55).
narrative_ontology:measurement(regcap_tr_t20, regulatory_capture_dynamic, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(regcap_be_t0, regulatory_capture_dynamic, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(regcap_be_t10, regulatory_capture_dynamic, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(regcap_be_t20, regulatory_capture_dynamic, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capture_dynamic, enforcement_mechanism).
narrative_ontology:affects_constraint(regulatory_capture_dynamic, asymmetric_information_barrier).
narrative_ontology:affects_constraint(regulatory_capture_dynamic, public_choice_problem).
narrative_ontology:affects_constraint(regulatory_capture_dynamic, incumbent_advantage_lock).

% DUAL FORMULATION NOTE:
% Regulatory capture is downstream of structural features (asymmetric information, asymmetric stakes, asymmetric access) but constitutes its own constraint with distinct extractiveness (0.58) reflecting the degree of industry influence over regulatory rules. The upstream constraints have their own ε values reflecting information asymmetry; the capture dynamic has its own ε reflecting extraction measured through price-cost margins and competitive entry barriers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capture_dynamic, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
