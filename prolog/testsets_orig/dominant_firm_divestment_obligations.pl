% ============================================================================
% CONSTRAINT STORY: dominant_firm_divestment_obligations
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dominant_firm_divestment_obligations, []).

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
 *   constraint_id: dominant_firm_divestment_obligations
 *   human_readable: Dominant Firm Divestment Obligations in Digital Markets
 *   domain: antitrust/regulatory
 *
 * SUMMARY:
 *   Divestment obligations imposed on dominant digital firms represent a
 *   structural remedy aimed at reducing market concentration by forcing
 *   separation of formerly integrated operations. The constraint exhibits
 *   classic Tangled Rope properties: genuine coordination function (enabling
 *   market entry and reducing network effects concentration) coexists with
 *   asymmetric extraction (dominant firm bears full cost of forced asset
 *   separation, operational fragmentation, and shareholder dilution). The
 *   constraint's extractiveness declines over time (0.68 → 0.52) as separated
 *   entities stabilize and competitive equilibrium emerges, but its theater
 *   ratio increases (0.35 → 0.58) as firms master compliance techniques that
 *   preserve de facto integration despite formal separation. This pattern
 *   creates a critical diagnostic ambiguity: whether divestment is a genuine
 *   structural remedy (Rope/Scaffold perspective) or a performative
 *   regulatory ritual that concentrates costs on one firm while preserving
 *   underlying market concentration (Piton perspective).
 *
 * KEY AGENTS:
 *   - Dominant Firm (Compelled Divesting Entity): Primary victim (powerless/trapped) — forced to divest; bears shareholder dilution, operational fragmentation, and competitive disadvantage. Cannot renegotiate or appeal mandatory separation.
 *   - Market Entrants and Competing Platforms: Secondary beneficiary (organized/constrained) — gain competitive access through reduced network effects lock-in but face high barriers to leveraging the opening. Organized agents with real constraints.
 *   - Antitrust Enforcement Authority: Institutional beneficiary (institutional/arbitrage) — solves collective action problem of market power concentration; gains enforcement legitimacy and market contestability.
 *   - Divested Subsidiary Operations: Nominal independent entities (institutional/constrained) — formally separated but face pressure to maintain operational ties with parent. Caught between regulatory separation requirement and economic integration incentives.
 *   - Consumers and Small Competitors: Distributed beneficiary (powerless/constrained) — benefit from reduced concentration but face persistent barriers to actual market entry. Power asymmetry means benefits are diffuse.
 *   - Analytical Observer: Neutral position (analytical/analytical) — tracks whether divestment achieves genuine market competition or becomes compliance theater masking continued concentration.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dominant_firm_divestment_obligations, 0.52).
domain_priors:suppression_score(dominant_firm_divestment_obligations, 0.65).
domain_priors:theater_ratio(dominant_firm_divestment_obligations, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dominant_firm_divestment_obligations, extractiveness, 0.52).
narrative_ontology:constraint_metric(dominant_firm_divestment_obligations, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(dominant_firm_divestment_obligations, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dominant_firm_divestment_obligations, tangled_rope).
narrative_ontology:human_readable(dominant_firm_divestment_obligations, "Dominant Firm Divestment Obligations in Digital Markets").
narrative_ontology:topic_domain(dominant_firm_divestment_obligations, "antitrust/regulatory").

domain_priors:requires_active_enforcement(dominant_firm_divestment_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dominant_firm_divestment_obligations, market_entrants).
narrative_ontology:constraint_beneficiary(dominant_firm_divestment_obligations, competing_platforms).
narrative_ontology:constraint_victim(dominant_firm_divestment_obligations, dominant_firm_shareholders).
narrative_ontology:constraint_victim(dominant_firm_divestment_obligations, ecosystem_integration).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPELLED DIVESTING FIRM (SNARE) — Subject to mandatory divestment; cannot appeal or renegotiate essential structural separation. Bears full extraction of forced asset liquidation, shareholder dilution, and operational fragmentation. No exit from the regulatory mandate itself.
constraint_indexing:constraint_classification(dominant_firm_divestment_obligations, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARKET ENTRANTS (TANGLED ROPE) — Benefit from divestment-enabled competition but also depend on interoperability with divested assets and must coordinate with fragmented infrastructure. Face significant entry costs despite reduced dominant firm control. Organized agents with real but constrained exit options.
constraint_indexing:constraint_classification(dominant_firm_divestment_obligations, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENFORCEMENT AUTHORITY (ROPE) — Solves collective action problem of market power concentration through coordinated structural remedy. Pure coordination function with minimal extractive overhead from the authority's perspective. Benefits from enforcement legitimacy.
constraint_indexing:constraint_classification(dominant_firm_divestment_obligations, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOMINANT FIRM (SCAFFOLD) — Divestment operates as temporary structural remedy with implicit sunset: if the separated entities compete effectively and markets remain contestable, the separation constraint becomes obsolete. Powerful agent retains mobility through transaction costs and strategic repositioning. High suppression during separation period but declining as new equilibrium stabilizes.
constraint_indexing:constraint_classification(dominant_firm_divestment_obligations, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: DIVESTMENT COMPLIANCE RITUAL (PITON) — Over time, mandatory divestitures become performative compliance rituals: firms create nominally independent subsidiaries that maintain operational ties, information-sharing, or indirect control mechanisms. The structural separation persists through regulatory theater despite erosion of its functional purpose. Theater ratio increases as firms master compliance while neutralizing competitive effect.
constraint_indexing:constraint_classification(dominant_firm_divestment_obligations, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Divestment generates both genuine coordination (market reentry becomes possible) and asymmetric extraction (dominant firm bears entire structural cost). Neither pure coordination nor pure extraction. The classification depends critically on whether separated entities can remain independent and whether entrants have sufficient resources to leverage the competitive opening. Observable-dependent but not a false summit.
constraint_indexing:constraint_classification(dominant_firm_divestment_obligations, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dominant_firm_divestment_obligations_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dominant_firm_divestment_obligations, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dominant_firm_divestment_obligations, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dominant_firm_divestment_obligations, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dominant_firm_divestment_obligations, TR),
    TR >= 0.70.

:- end_tests(dominant_firm_divestment_obligations_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Divestment extracts significant costs from dominant firms (forced asset sales, operational restructuring, lost synergies) but simultaneously creates genuine competitive opportunity. Not as severe as pure extraction (Snare ε ≥ 0.66) because the remedy addresses a real coordination failure (market concentration). The declining extractiveness trajectory (0.68 → 0.52) reflects that the major shareholder costs are front-loaded; ongoing extraction declines as separated entities reach new equilibrium. Suppression (0.65): High. Divestment mandates impose regulatory coercion with no negotiation or exit option for the targeted firm. Barriers to competitive entry remain high even post-divestment (capital requirements, network effects, switching costs). Suppression remains elevated throughout because the regulatory coercion is sustained (not time-limited) and entrant barriers persist. Theater ratio (0.58): Moderate-high. Initial compliance burden is substantial (restructuring operations, creating independent governance). Theater increases over time as firms develop mastery of compliance techniques: maintaining de facto integration through technology licensing, shared services, indirect information flows, and subsidiary board interlocks. The compliance theater masks the degree to which formal separation achieves actual competition.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap separates the dominant firm's Snare experience from the organized entrants' Tangled Rope experience. From the dominant firm's perspective, divestment is pure extraction: mandatory asset sales, shareholder dilution, operational fragmentation, no exit negotiation. From market entrants' perspective, divestment is mixed — it removes the lock-in barrier that prevented entry (coordination function) but doesn't eliminate capital barriers or network effects (remaining extraction). The Scaffold perspective (dominant firm retains mobility through strategic repositioning) depends on whether separated entities can truly compete, which depends on interoperability enforcement — a critical omega variable. The Piton perspective (compliance theater masking continued concentration) dominates if indirect control mechanisms successfully neutralize separation while formal compliance persists. The analytical observer's Tangled Rope classification is stable only if empirical data confirms that entrants actually enter and competition actually emerges post-divestment.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant firm directionality (d ≈ 0.95): Primary target of mandatory divestment with no arbitrage exit options. Bears maximum extraction through forced asset sales and shareholder loss. Derives high d through victim status + trapped exit framework. Market entrants directionality (d ≈ 0.35): Beneficiaries of reduced lock-in but face high barriers to actual market entry. Organized power allows some exit through alternative product development, but entry costs remain significant. Lower d than beneficiaries with arbitrage because they don't fully capture the competitive opportunity. Enforcement authority directionality (d ≈ 0.08): Net beneficiary with arbitrage options. Benefits from market contestability and enforcement legitimacy. Very low d reflects institutional position and policy flexibility. Separated subsidiary directionality (d ≈ 0.65): Constrained beneficiary-victim. Nominally independent but pressured to maintain operational ties. Not fully trapped (has some operational autonomy) but not mobile (regulatory constraints and economic pressure to recombine). Mid-range d reflects mixed structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   STRUCTURAL TENSION: The classification depends critically on whether divestment achieves genuine market competition (Rope/Scaffold perspective strengthened) or becomes compliance theater with minimal competitive effect (Piton perspective dominates). The Tangled Rope classification resolves this ambiguity by acknowledging both functions simultaneously: genuine coordination problem (market concentration) AND asymmetric extraction (dominant firm bears all structural cost). The mandatrophy is resolved by recognizing that divestment is neither pure remedy nor pure punishment, but a hybrid that generates both benefits (market contestability) and harms (forcibly fragmented operations). The perspectival gap itself is the diagnostic signal: if all agents classified the constraint identically, mandatrophy would be unresolved. The fact that dominant firms see Snare, entrants see Tangled Rope, and enforcers see Rope indicates that the classification is capturing real structural heterogeneity rather than papering over contradiction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidiary_independence_durability,
    'Do divested subsidiaries remain functionally independent or do indirect control mechanisms (shared services, information flows, technology licensing) gradually neutralize competitive separation?',
    'Post-divestment operational audits; measurement of cross-firm transaction volumes, shared infrastructure dependencies, and information asymmetries; tracking of market outcomes for separated entities vs true competitors',
    'If subsidiaries remain independent: divestment is effective coordination remedy (Rope classification gains force). If indirect control persists: divestment becomes performative (Piton classification dominates), extractiveness remains high despite formal compliance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidiary_independence_durability, empirical, 'Whether divested subsidiaries maintain genuine independence').

omega_variable(
    entrant_viability_threshold,
    'What capital and competency thresholds are required for market entrants to actually leverage the competitive opening created by divestment?',
    'Historical analysis of post-divestment market entry patterns; correlation between capital availability and successful entry; assessment of remaining barriers unrelated to dominant firm control',
    'If threshold is low: divestment broadly enables competition (Rope/Scaffold). If threshold is prohibitive: divestment benefits only large organized competitors (Tangled Rope with organized agents only). If threshold is extremely high: divestment fails to remedy concentration (Snare persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrant_viability_threshold, empirical, 'Whether market entrant barriers are surmountable post-divestment').

omega_variable(
    regulatory_arbitrage_across_jurisdictions,
    'Can dominant firms legally maintain consolidated operations in jurisdictions without divestment mandates while complying with separation in jurisdictions that impose it?',
    'International regulatory analysis; tracking of firm structure across multiple regulatory regimes; assessment of whether cross-border integration recreates concentration despite local divestment',
    'If arbitrage possible: divestment is regionalized constraint with limited global effect (Tangled Rope scope collapses to national/regional). If constrained: global enforcement scope holds (global scope maintained). Creates potential for regulatory capture across borders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_across_jurisdictions, empirical, 'Scope of regulatory arbitrage across jurisdictional boundaries').

omega_variable(
    integration_value_asymmetry,
    'Does the integrated dominant firm generate significantly greater economic value than the sum of separated entities, creating perpetual pressure to recombine?',
    'Comparative valuation of integrated vs separated operating periods; measurement of efficiency losses, eliminated synergies, and transaction cost increases post-divestment; historical analysis of re-consolidation attempts',
    'If high integration value: divestment generates ongoing extraction (suppression remains high indefinitely, Snare classification persists). If low: separation stabilizes and becomes less extractive (Scaffold sunset becomes real, Rope gains force). Determines whether suppression_trajectory shows sustainable decline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_value_asymmetry, empirical, 'Economic value differential between integrated and separated structures').

omega_variable(
    interoperability_mandate_effectiveness,
    'Can regulatory authorities enforce meaningful interoperability between divested entities, or do dominant firms retain de facto control through technical/commercial lock-in?',
    'Interoperability audits post-divestment; measurement of cross-platform compatibility, API openness, and switching costs; tracking of de facto lock-in mechanisms that survive formal separation',
    'If interoperability enforceable: competitive entrants gain genuine market access (Tangled Rope classification sustained, suppression declines). If lock-in persists: divestment is cosmetic (Snare classification dominates, suppression remains high). Critical for Tangled Rope vs Snare discrimination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interoperability_mandate_effectiveness, empirical, 'Whether interoperability requirements prevent de facto recombination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dominant_firm_divestment_obligations, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(div_tr_t0, dominant_firm_divestment_obligations, theater_ratio, 0, 0.35).
narrative_ontology:measurement(div_tr_t3, dominant_firm_divestment_obligations, theater_ratio, 3, 0.48).
narrative_ontology:measurement(div_tr_t6, dominant_firm_divestment_obligations, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(div_be_t0, dominant_firm_divestment_obligations, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(div_be_t3, dominant_firm_divestment_obligations, base_extractiveness, 3, 0.6).
narrative_ontology:measurement(div_be_t6, dominant_firm_divestment_obligations, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dominant_firm_divestment_obligations, enforcement_mechanism).
narrative_ontology:affects_constraint(dominant_firm_divestment_obligations, network_effects_lock_in).
narrative_ontology:affects_constraint(dominant_firm_divestment_obligations, platform_ecosystem_integration).
narrative_ontology:affects_constraint(dominant_firm_divestment_obligations, merger_approval_gate).

% DUAL FORMULATION NOTE:
% Divestment obligations are downstream structural remedies for market concentration claims. Related constraints (network effects lock-in, ecosystem integration) have their own extractiveness values reflecting empirical concentration levels; divestment has its own extractiveness reflecting the remedy's structural costs and effectiveness. Decomposition enables measurement of whether the remedy achieves its coordination purpose or becomes extractive theater.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dominant_firm_divestment_obligations, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
