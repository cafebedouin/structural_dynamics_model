% ============================================================================
% CONSTRAINT STORY: sotu_1970_nixon_revenue_sharing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1970_nixon_revenue_sharing, []).

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
 *   constraint_id: sotu_1970_nixon_revenue_sharing
 *   human_readable: Federal Revenue Sharing and Fiscal Federalism Restructuring (1970s)
 *   domain: governance/fiscal_federalism
 *
 * SUMMARY:
 *   Nixon's 1970 revenue-sharing proposal restructures fiscal federalism by
 *   returning federal tax revenue to state and local governments with minimal
 *   categorical restrictions. The constraint operates as a tangled mechanism:
 *   genuine coordination function (decentralization aligns incentives for
 *   efficient resource allocation) coexists with asymmetric extraction
 *   (federal executive authority reduces direct accountability for service
 *   equity; wealthy jurisdictions capture disproportionate benefit;
 *   low-capacity jurisdictions face service collapse without federal program
 *   floors). The constraint demonstrates perspectival variance across six
 *   distinct institutional positions, from the mountain view naturalizing
 *   fiscal federalism tensions as immutable, to the snare view experienced by
 *   resource-poor jurisdictions trapped between service obligations and
 *   revenue shortfalls. Theater ratio (0.38 rising to 0.38) reflects that
 *   revenue-sharing formal mechanism is genuinely functional (lower theater
 *   than categorical program bureaucracy) but coexists with persistent
 *   categorical grant programs that create performative parallel structures.
 *   Extractiveness (0.52) reflects moderate extraction: beneficiaries
 *   (wealthy states, federal executive) gain autonomy and reduced direct
 *   accountability; victims (low-capacity jurisdictions, equity-seeking
 *   populations) face service risk without exit mechanism.
 *
 * KEY AGENTS:
 *   - Wealthy/High-Capacity State Governments: Primary beneficiary (institutional/arbitrage) — gain federal funding without restrictions; can leverage independent tax bases; experience pure coordination benefit
 *   - Federal Executive Authority: Secondary beneficiary (institutional/arbitrage) — reduces direct service delivery burden; retains leverage through revenue-sharing structure and categorical program control
 *   - Low-Capacity Rural and Urban Jurisdictions: Primary victim (powerless/trapped) — cannot generate sufficient local revenue; dependent on federal transfers; face service collapse if federal equity floors removed
 *   - Federal Program Beneficiaries (welfare, education, healthcare): Structural victim (powerless/trapped) — dispersed populations vulnerable to service variation as programs devolve without equity protections
 *   - Redistributive Function (National Equity): Abstract victim (powerless/trapped) — federal government's equalizing power over geography/demography risks degradation as revenue-sharing weakens categorical program controls
 *   - Moderate-Capacity States: Mixed position (powerful/constrained) — gain autonomy but constrained by demographic/economic pressures; extract from federal system while being extracted from by pressure for service expansion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1970_nixon_revenue_sharing, 0.52).
domain_priors:suppression_score(sotu_1970_nixon_revenue_sharing, 0.35).
domain_priors:theater_ratio(sotu_1970_nixon_revenue_sharing, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1970_nixon_revenue_sharing, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1970_nixon_revenue_sharing, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sotu_1970_nixon_revenue_sharing, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1970_nixon_revenue_sharing, tangled_rope).
narrative_ontology:human_readable(sotu_1970_nixon_revenue_sharing, "Federal Revenue Sharing and Fiscal Federalism Restructuring (1970s)").
narrative_ontology:topic_domain(sotu_1970_nixon_revenue_sharing, "governance/fiscal_federalism").

domain_priors:requires_active_enforcement(sotu_1970_nixon_revenue_sharing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1970_nixon_revenue_sharing, state_governments).
narrative_ontology:constraint_beneficiary(sotu_1970_nixon_revenue_sharing, local_governments).
narrative_ontology:constraint_beneficiary(sotu_1970_nixon_revenue_sharing, federal_executive_authority).
narrative_ontology:constraint_victim(sotu_1970_nixon_revenue_sharing, federal_program_beneficiaries).
narrative_ontology:constraint_victim(sotu_1970_nixon_revenue_sharing, low_capacity_jurisdictions).
narrative_ontology:constraint_victim(sotu_1970_nixon_revenue_sharing, redistributive_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-CAPACITY JURISDICTIONS (SNARE) — Trapped in dependency on revenue sharing without capacity to generate local revenue or manage complex redistributive transfers. Benefits from other jurisdictions' superior resource bases evaporate as funds flow to wealthy jurisdictions with tax bases. No exit mechanism — must accept revenue-sharing terms or collapse essential services.
constraint_indexing:constraint_classification(sotu_1970_nixon_revenue_sharing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WEALTHY STATE GOVERNMENTS (ROPE) — Experience revenue sharing as coordination with net benefit: receive federal funds without categorical restrictions, enabling strategic allocation to state priorities while maintaining independent tax authority. Can arbitrage between federal funding and state revenue sources. Genuine coordination benefit — decentralization aligns incentives.
constraint_indexing:constraint_classification(sotu_1970_nixon_revenue_sharing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: MODERATE-CAPACITY STATE GOVERNMENTS (TANGLED ROPE) — Constrained by variable federal funding and demographic/economic pressures. Both coordinate (benefit from autonomy over resource allocation) and face extraction (federal funds lag inflation, forcing local tax increases or service cuts). Mixed mechanism — genuine coordination function coupled with asymmetric extraction.
constraint_indexing:constraint_classification(sotu_1970_nixon_revenue_sharing, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PROGRESSIVE REFORM COALITION (SCAFFOLD) — Federal revenue sharing as temporary transition toward rationalized federalism. Organized actors (reformers, intergovernmental affairs councils) see sunset logic: decentralization combined with federal equity-protection floors. Low theater — genuine institutional innovation. Constraint has defined exit path if federal oversight and equity standards are maintained alongside decentralization.
constraint_indexing:constraint_classification(sotu_1970_nixon_revenue_sharing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL CATEGORICAL PROGRAM STRUCTURE (PITON) — Revenue sharing introduces performative federalism: federal program apparatus persists through grants-in-aid alongside general revenue sharing, creating parallel funding streams. Theater ratio reflects redundant program certification, reporting, and compliance mechanisms operating alongside block grants. Degraded institution maintained through inertia — neither pure categorical control nor pure decentralization, producing administrative theater.
constraint_indexing:constraint_classification(sotu_1970_nixon_revenue_sharing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From civilizational scale, resource redistribution necessarily involves tension between centralized equity and decentralized autonomy. This appears as an immutable constraint on federalist systems: any decentralization sacrifices equity; any centralization sacrifices efficiency. However, structural beneficiaries exist — federal executive power and wealthy jurisdictions benefit from this framing. Engine false summit detection flags the naturalness claim as contingent institutional arrangement.
constraint_indexing:constraint_classification(sotu_1970_nixon_revenue_sharing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1970_nixon_revenue_sharing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1970_nixon_revenue_sharing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1970_nixon_revenue_sharing, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1970_nixon_revenue_sharing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1970_nixon_revenue_sharing, TR),
    TR >= 0.70.

:- end_tests(sotu_1970_nixon_revenue_sharing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Revenue sharing transfers federal fiscal responsibility to states/localities without equivalent transfer of revenue capacity. Extractiveness reflects that federal executive and wealthy jurisdictions benefit from the decentralization (gain autonomy, reduce direct accountability) while costs are borne by low-capacity jurisdictions and service beneficiaries. The value increased from 0.35 to 0.52 over the interval as federal equity protections eroded and categorical program controls weakened, concentrating extraction on powerless agents. Suppression (0.35): Moderate. Low-capacity jurisdictions face suppression through lack of alternative revenue sources and federal withdrawal of direct program management. Suppression is not total because federal revenue continues to flow and some jurisdictions retain service capacity. Theater ratio (0.38): Low-moderate. Revenue-sharing mechanism itself is functionally genuine (direct transfers, minimal bureaucratic overhead) relative to categorical programs. Theater reflects persistent categorical programs operating alongside general revenue sharing, creating redundant compliance/reporting structures. Theater has risen slightly as distinction between general revenue and categorical grants blurred in practice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces maximum perspectival divergence. Wealthy states (Rope) experience pure coordination — decentralization solves their efficiency problem with genuine benefit and no extraction cost. Federal executive (institutional perspectives vary from Rope to Piton depending on time horizon) sees institutional redesign as neutral or beneficial — shifts service delivery burden to states. Low-capacity jurisdictions (Snare) experience maximum extraction — federal withdrawal exposes their structural vulnerability. Progressive reformers (Scaffold) see temporary phase toward better federalism — sunset logic makes extraction temporally bounded. Federal categorical program apparatus (Piton) sees degraded institutional function — categorical controls persist alongside revenue sharing, creating redundant bureaucracy. Civilizational analytical view (Mountain) risks naturalizing fiscal federalism tensions as immutable constraints on federalist systems — but structural beneficiaries (wealthy states, federal executive) incentivize this naturalness claim. The perspectival gap reveals that 'decentralization' is legitimate coordination benefit for wealthy jurisdictions but extraction mechanism for poor jurisdictions. Same structural policy produces opposite classifications depending on structural power.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from structural position within the extraction mechanism. Wealthy state governments (institutional/arbitrage) have low d (~0.15) — they are beneficiaries with exit options (can supplement federal revenue with state taxation). Federal executive authority (institutional/arbitrage) has moderate-low d (~0.25) — benefits from reduced direct accountability while retaining revenue-sharing control leverage. Moderately-resourced states (powerful/constrained) have moderate d (~0.50) — both benefit (autonomy) and extract/are extracted from (demographic/economic pressures). Low-capacity jurisdictions (powerless/trapped) have high d (~0.85) — maximum extraction experience because they depend absolutely on federal transfers without revenue capacity or exit option. Beneficiary/victim declarations drive the sigmoid f(d) computation: beneficiaries push effective extraction χ downward; victims push it upward. Organized reform coalition (organized/constrained) has moderate d (~0.55) — they perceive the constraint as solvable (sunset logic) rather than immutable, producing moderate extraction experience and Scaffold classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through recognizing that revenue sharing IS genuine coordination for wealthy jurisdictions AND genuine extraction for poor jurisdictions simultaneously. This is not ambiguity about classification — both are correct from their respective structural positions. The tangled rope classification (0.52 extractiveness + 0.35 suppression + active enforcement) captures this hybrid: decentralization solves coordination problems (genuine rope function) while asymmetrically extracting from low-capacity jurisdictions (genuine snare function operating on subset of agents). The classification prevents three mislabelings: (1) Pure Rope framing (ignores extraction from poor jurisdictions); (2) Pure Snare framing (ignores coordination benefits for wealthy states); (3) Natural Law / Mountain framing (naturalizes what is contingent institutional arrangement favoring existing wealth). The analytical perspective's mountain classification is a false summit — fiscal federalism tensions are real but not immutable. They are contingent on whether federal equity floors and program controls are maintained. The constraint's classification stabilizes at Tangled Rope because active federal enforcement of equity standards must coexist with state decentralization for both functions to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equity_floor_enforceability,
    'Can federal government maintain minimum service standards across states without categorical program control?',
    'Historical measurement of service equity post-revenue sharing; comparison of outcomes in high-capacity vs low-capacity jurisdictions; effectiveness of outcome-based federal oversight vs input-based program requirements',
    'If enforceable: constraint reclassifies as Scaffold with genuine sunset (temporary transition with equity safeguards). If unenforceable: constraint is pure Snare disguised as decentralization — low-capacity jurisdictions bear extraction without exit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equity_floor_enforceability, empirical, 'Whether federal equity floors can be maintained without categorical control').

omega_variable(
    tax_base_divergence_mechanism,
    'Does revenue sharing accelerate divergence between wealthy and poor jurisdictions by rewarding existing wealth disparities?',
    'Longitudinal analysis of Gini coefficient for per-capita service provision across states; comparison of pre/post revenue-sharing inequality trends; measurement of whether federal equalization function is preserved or degraded',
    'If divergence accelerates: suppression of low-capacity jurisdictions increases (they fall further behind). If maintained or reduces: constraint is genuine tangled rope with balanced trade-offs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tax_base_divergence_mechanism, empirical, 'Whether revenue sharing accelerates fiscal federalism inequality').

omega_variable(
    federal_power_retention_motive,
    'Does federal government pursue revenue sharing primarily for fiscal relief or to restructure intergovernmental power dynamics?',
    'Analysis of administration rhetoric and policy documentation; measurement of whether federal control mechanisms are retained over revenue-sharing grants; comparison of de facto autonomy granted vs nominal decentralization',
    'If fiscal relief motive: constraint is genuine coordination (Rope). If power restructuring motive: constraint is extraction mechanism disguised as decentralization (Snare/Piton hybrid).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_power_retention_motive, conceptual, 'Federal motive structure in revenue-sharing proposal').

omega_variable(
    categorical_program_persistence,
    'Why do categorical grant programs persist and even expand alongside general revenue sharing?',
    'Historical analysis of federal budget composition before/after revenue sharing; measurement of categorical vs block grant funding ratios; identification of which constituencies maintain categorical program support',
    'If categorical programs persist due to genuine coordination failure at state level: constraint has irreducible hybrid character (Tangled Rope). If persistence is bureaucratic inertia: theater_ratio rises, reclassification toward Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_program_persistence, empirical, 'Mechanism of categorical program persistence alongside revenue sharing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1970_nixon_revenue_sharing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_1970_nixon_revenue_sharing, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sotu_tr_t5, sotu_1970_nixon_revenue_sharing, theater_ratio, 5, 0.32).
narrative_ontology:measurement(sotu_tr_t10, sotu_1970_nixon_revenue_sharing, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_1970_nixon_revenue_sharing, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu_be_t5, sotu_1970_nixon_revenue_sharing, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(sotu_be_t10, sotu_1970_nixon_revenue_sharing, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1970_nixon_revenue_sharing, resource_allocation).
narrative_ontology:affects_constraint(sotu_1970_nixon_revenue_sharing, federal_categorical_program_proliferation).
narrative_ontology:affects_constraint(sotu_1970_nixon_revenue_sharing, interstate_fiscal_competition).
narrative_ontology:affects_constraint(sotu_1970_nixon_revenue_sharing, poverty_alleviation_decentralization).

% DUAL FORMULATION NOTE:
% Revenue sharing as decentralized resource allocation (this story) is structurally upstream of outcomes for specific service domains (education, welfare, healthcare). Those domain-specific constraints inherit their extractiveness profile partly from whether revenue-sharing equity floors remain intact. This story's extractiveness trajectory (rising from 0.35 to 0.52) predicts that downstream service constraints will show extraction accumulation as equity protections erode.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1970_nixon_revenue_sharing, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
