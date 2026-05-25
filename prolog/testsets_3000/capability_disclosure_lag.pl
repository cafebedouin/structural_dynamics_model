% ============================================================================
% CONSTRAINT STORY: capability_disclosure_lag
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_capability_disclosure_lag, []).

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
 *   constraint_id: capability_disclosure_lag
 *   human_readable: Capability Disclosure Lag in Competitive Environments
 *   domain: strategic/competitive/institutional
 *
 * SUMMARY:
 *   Capability disclosure lag — the period between when a capability is
 *   developed and when it is publicly revealed — creates structural
 *   extraction: those with information advantages can position competitively
 *   while others remain blindsided. This constraint operates across
 *   competitive domains (technology, military, markets, scientific research)
 *   and generates perspectival classification from mountain (natural feature
 *   of competitive advantage) to snare (pure extraction trap). The tension
 *   emerges from a genuine coordination function (first-movers need time to
 *   prepare supply chains, markets, and strategic positioning) layered over
 *   asymmetric extraction (information advantage compounds over the lag
 *   period). Theater increases as disclosure regimes mature: proprietary
 *   rights law maintains institutional commitment to secrecy even as
 *   technical diffusion erodes actual secrecy, creating a piton effect. The
 *   constraint's suppression (0.62) reflects both structural barriers
 *   (information asymmetry, legal enforcement, proprietary costs) and
 *   internalized norms (secrecy as sign of valuable capability).
 *   Extractiveness (0.58) reflects moderate but durable extraction — not
 *   total like a snare, but well above pure coordination.
 *
 * KEY AGENTS:
 *   - Capability Holders: Primary beneficiary (institutional/arbitrage) — control disclosure timing and capture information advantage. Net beneficiary from the lag.
 *   - First Movers: Primary beneficiary (powerful/arbitrage) — establish market position during lag period. Experience pure coordination benefit.
 *   - Disadvantaged Competitors: Primary victim (powerless/trapped) — face material barriers to discovering capabilities; cannot exit the information disadvantage.
 *   - Market Efficiency (Collective): Secondary victim (powerless/trapped) — abstract good bearing cost of information asymmetry; cannot organize or exit.
 *   - Regulatory Framework: Organized actor (organized/constrained) — enforces disclosure mandates with sunset logic as transparency technologies mature.
 *   - Proprietary Rights Regime: Institutional actor (institutional/arbitrage) — maintains legal enforcement of disclosure lag despite technical erosion of actual secrecy (piton mechanism).
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as laws of competitive nature.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capability_disclosure_lag, 0.58).
domain_priors:suppression_score(capability_disclosure_lag, 0.62).
domain_priors:theater_ratio(capability_disclosure_lag, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capability_disclosure_lag, extractiveness, 0.58).
narrative_ontology:constraint_metric(capability_disclosure_lag, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(capability_disclosure_lag, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capability_disclosure_lag, tangled_rope).
narrative_ontology:human_readable(capability_disclosure_lag, "Capability Disclosure Lag in Competitive Environments").
narrative_ontology:topic_domain(capability_disclosure_lag, "strategic/competitive/institutional").

domain_priors:requires_active_enforcement(capability_disclosure_lag).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(capability_disclosure_lag, capability_holders).
narrative_ontology:constraint_beneficiary(capability_disclosure_lag, first_movers).
narrative_ontology:constraint_victim(capability_disclosure_lag, disadvantaged_competitors).
narrative_ontology:constraint_victim(capability_disclosure_lag, market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFORMATION-STARVED COMPETITOR (SNARE) — Faces material barriers to discovering competitor capabilities: no access to private data, limited resources for intelligence gathering, market asymmetry prevents exit. Bears full cost of capability surprise. Maximum extraction experienced — no coordination benefit.
constraint_indexing:constraint_classification(capability_disclosure_lag, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: POTENTIAL CHALLENGER (TANGLED ROPE) — Experiences both coordination (delayed disclosure prevents premature competitive response) and extraction (information asymmetry creates sustained disadvantage). High cost to challenge but not impossible — constrained exit. Benefits from some aspects of delayed disclosure (time to prepare) but bears extraction costs.
constraint_indexing:constraint_classification(capability_disclosure_lag, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPABILITY HOLDER (ROPE) — Experiences the lag as pure coordination mechanism: controlling disclosure timing enables market positioning, supply chain preparation, and competitive surprise. Net beneficiary with full exit optionality — can disclose at strategic moment.
constraint_indexing:constraint_classification(capability_disclosure_lag, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY FRAMEWORK (SCAFFOLD) — Disclosure mandates (securities regulations, environmental impact statements, product liability frameworks) create structured transparency gates. Organized enforcement actors see the lag as a temporary problem being solved by sunsetted requirements. Theater ratio low — genuine enforcement function. Sunset mechanism: as real-time disclosure technologies mature and become mandatory, the lag phase becomes obsolete.
constraint_indexing:constraint_classification(capability_disclosure_lag, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PROPRIETARY RIGHTS REGIME (PITON) — Patent and trade secret law maintains institutional commitment to disclosure lag despite erosion of functional value. As information diffusion accelerates (reverse engineering, employee mobility, data breaches), the protective barrier degrades while institutional forms persist. Theater maintained through legal ritual rather than functional secrecy.
constraint_indexing:constraint_classification(capability_disclosure_lag, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, information asymmetry is inherent to capability development: novel capabilities cannot be fully disclosed during creation without destroying their value. The disclosure lag is seen as an immutable feature of how competitive advantage works. However, this risks naturalizing what is a contingent institutional arrangement — the lag duration and enforceability depend entirely on disclosure regime maturity.
constraint_indexing:constraint_classification(capability_disclosure_lag, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capability_disclosure_lag_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(capability_disclosure_lag, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capability_disclosure_lag, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(capability_disclosure_lag, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(capability_disclosure_lag, TR),
    TR >= 0.70.

:- end_tests(capability_disclosure_lag_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated from initial estimate (0.42) due to evidence that information diffusion does not reliably undermine the lag — reverse engineering takes years, employee mobility creates only partial diffusion, supply chain signals leak slowly. The lag persists at 2-5 years in most competitive domains, suggesting genuine extraction rather than temporary asymmetry. However, not at snare levels (0.66+) because some legitimate coordination benefit accrues to first-movers. Suppression (0.62): Moderate-high. Legal enforcement of proprietary rights, information economics (revealing destroys value), and asymmetric access to discovery channels all create real barriers. But not total — some information always leaks, and aggressive competitors can sometimes close the gap through heavy investment. Theater ratio (0.65): Increasing over the interval. Patent systems, trade secret law, and confidentiality agreements maintain institutional commitment to disclosure lag even as practical secrecy erodes. Information diffusion technologies (data breaches, employee mobility, reverse engineering) make true secrecy increasingly rare, yet legal theater persists. The theater increase from 0.35 to 0.65 reflects institutional lag between functional decline and ritual maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The gap between capability holder (Rope) and disadvantaged competitor (Snare) is the core diagnostic. Capability holders experience the lag as legitimate coordination — they need time to prepare. Disadvantaged competitors experience it as pure trap — they cannot obtain information through any action available to them. The gap is not perceptual (both see the same information asymmetry) but structural (they occupy fundamentally different positions relative to disclosure). The piton perspective (institutional/arbitrage) reveals that proprietary rights law has outlived its functional purpose — actual secrecy is increasingly technical fantasy, yet legal theater maintains the institutional form. The mountain perspective (analytical/civilizational) risks a false summit: it naturalizes the lag as inherent to competition, obscuring that lag duration and enforceability are entirely contingent on institutional arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Capability holders hold low d values (high beneficiary status, arbitrage exit) → experience negative effective extraction (the constraint benefits them). Disadvantaged competitors hold high d values (victim status, trapped exit) → experience high effective extraction (the constraint extracts from them). Regulatory framework holds moderate d values (organized but constrained, enforcing rather than benefiting from the lag) → experiences moderate extraction. The constraint's classification depends critically on power and exit options: institutional actors with arbitrage experience Rope; powerless trapped actors experience Snare; organized constrained actors experience Scaffold.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing the legitimate coordination function (first-movers need preparation time) from the extractive asymmetry (information barriers prevent catch-up). A pure Rope analysis would miss the extraction component; a pure Snare analysis would miss the coordination component. The Tangled Rope classification holds both: genuine coordination benefit for first-movers (satisfies Rope gate with beneficiaries) combined with asymmetric extraction from disadvantaged competitors (satisfies Snare gate with victims). The constraint requires active enforcement (legal regimes) to maintain, marking it as hybrid rather than pure. The theater increase suggests institutional drift toward Piton — as practical secrecy erodes, the legal structure becomes increasingly performative, maintained through ritual rather than function. The potential sunset is technological (real-time disclosure systems making lag impossible to enforce) and regulatory (transparency mandates that outrun proprietary protections).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disclosure_threshold_ambiguity,
    'At what point of capability maturity does withholding disclosure shift from legitimate competitive strategy to extractive information hoarding?',
    'Comparative analysis of disclosure timing across regulatory regimes; correlation between lag duration and downstream market inefficiency; measurement of competitor catch-up speeds as function of lag period',
    'If threshold is short (weeks): most capability holders appear extractive. If threshold is long (years): disclosure lag appears normal and necessary. Classification shift from Snare/Tangled Rope toward Rope across victim perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_threshold_ambiguity, preference, 'Ambiguity in defining extractive vs legitimate disclosure lag duration').

omega_variable(
    information_diffusion_mechanism,
    'Does capability information diffuse through reverse engineering, employee mobility, and supply chain signals with sufficient speed to undermine the lag''s extraction mechanism?',
    'Empirical tracking of information diffusion timelines for specific capabilities; measurement of actual lag duration vs intended lag duration; comparison of formal disclosure vs informal information leakage',
    'If diffusion undermines the lag: suppression metric drops, extractiveness drops, constraint shifts toward Piton (theater-driven). If diffusion fails: suppression and extractiveness remain high, snare classification persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_diffusion_mechanism, empirical, 'Whether informal information diffusion undermines disclosure lag mechanism').

omega_variable(
    first_mover_coordination_necessity,
    'Is the disclosure lag primarily a coordination function (enabling first-mover to prepare markets and supply chains) or primarily an extraction mechanism (preventing competitor response)?',
    'Analysis of cases where disclosure was accelerated: did earlier competitor response damage first-mover advantage, or did coordinated preparation benefit both parties? Measurement of market outcomes under different lag regimes.',
    'If primarily coordination: constraint should be Rope from most perspectives, not Tangled Rope. If primarily extraction: Snare/Tangled Rope classifications hold. This determines whether the beneficiary relationship is genuinely mutual or asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(first_mover_coordination_necessity, empirical, 'Whether disclosure lag functions as coordination or pure extraction').

omega_variable(
    regulatory_enforcement_sustainability,
    'Can regulatory disclosure mandates be enforced as information diffusion technologies make true secrecy technically impossible?',
    'Tracking of regulatory compliance as diffusion technologies mature; measurement of enforcement costs vs benefit; observation of institutional adaptation when secrecy becomes technically unenforceable',
    'If enforcement degrades: constraint transitions from Tangled Rope toward Piton (theatrical compliance without real secrecy). Scaffold sunset clause becomes operative. If enforcement strengthens through legal escalation: Snare persists despite technology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_sustainability, empirical, 'Sustainability of regulatory disclosure enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capability_disclosure_lag, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(capdis_tr_t0, capability_disclosure_lag, theater_ratio, 0, 0.35).
narrative_ontology:measurement(capdis_tr_t3, capability_disclosure_lag, theater_ratio, 3, 0.52).
narrative_ontology:measurement(capdis_tr_t6, capability_disclosure_lag, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(capdis_be_t0, capability_disclosure_lag, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(capdis_be_t3, capability_disclosure_lag, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(capdis_be_t6, capability_disclosure_lag, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capability_disclosure_lag, information_standard).
narrative_ontology:affects_constraint(capability_disclosure_lag, first_mover_advantage).
narrative_ontology:affects_constraint(capability_disclosure_lag, information_asymmetry_extraction).
narrative_ontology:affects_constraint(capability_disclosure_lag, proprietary_rights_enforcement).

% DUAL FORMULATION NOTE:
% Capability disclosure lag decomposes into two structurally distinct constraints: (1) capability_coordination_necessity (ε≈0.15, Rope) — the legitimate need for first-movers to prepare markets and supply chains, and (2) capability_disclosure_lag (ε≈0.58, Tangled Rope) — the extractive information asymmetry that persists beyond what coordination requires. This story focuses on the hybrid form. See network.affects_constraints for downstream constraints that depend on the lag duration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(capability_disclosure_lag, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
