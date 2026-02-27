% ============================================================================
% CONSTRAINT STORY: bwb_adeg_rewesale_conditions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bwb_adeg_rewesale_conditions, []).

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
 *   constraint_id: bwb_adeg_rewesale_conditions
 *   human_readable: BWB Conditions on Rewe's Adeg Store Divestment
 *   domain: economic/competition_law
 *
 * SUMMARY:
 *   The Austrian Federal Competition Authority (BWB) imposed conditions on
 *   Rewe Group's transfer of approximately 75 Adeg grocery stores to
 *   independent merchants as part of a market consolidation mitigation. The
 *   conditions constrain how divested stores can operate post-transfer,
 *   including supply chain restrictions, pricing guidance, and operational
 *   autonomy limits. This constraint exhibits both genuine coordination
 *   (preventing predatory post-sale integration) and extractive elements
 *   (operational burden on small merchants). The perspectival gap reveals the
 *   core tension: for independent merchants, the conditions are a trap —
 *   compliance is mandatory to access the stores but severely limits
 *   operational freedom. For the retail competition ecosystem, the conditions
 *   serve a coordination function: they maintain store operator autonomy and
 *   prevent covert reconsolidation. For Rewe, the conditions are a
 *   coordination cost that enables clean divestment. For the BWB itself, the
 *   conditions represent temporary scaffolding for market transition, though
 *   enforcement theater (monitoring, reporting, audits) may exceed functional
 *   necessity.
 *
 * KEY AGENTS:
 *   - Independent Merchants: Primary target (powerless/trapped) — acquire stores only by accepting all conditions; cannot renegotiate or exit
 *   - Rewe Group: Primary beneficiary (institutional/arbitrage) — complies with conditions to achieve clean divestment and avoid further regulatory action
 *   - Austrian Retail Competition: Structural beneficiary (moderate/constrained) — gains store distribution autonomy and prevents consolidation, but bears enforcement burden
 *   - Austrian Federal Competition Authority (BWB): Regulatory architect (organized/constrained) — designs and monitors conditions to prevent abuse; sees temporary intervention with eventual sunset
 *   - Legacy Regulatory Framework: Institutional inertia (institutional/arbitrage) — 1980s-era consolidation rules applied to 2020s divestment; high theater relative to functional necessity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent regulatory design as inherent market law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bwb_adeg_rewesale_conditions, 0.52).
domain_priors:suppression_score(bwb_adeg_rewesale_conditions, 0.68).
domain_priors:theater_ratio(bwb_adeg_rewesale_conditions, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bwb_adeg_rewesale_conditions, extractiveness, 0.52).
narrative_ontology:constraint_metric(bwb_adeg_rewesale_conditions, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(bwb_adeg_rewesale_conditions, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bwb_adeg_rewesale_conditions, tangled_rope).
narrative_ontology:human_readable(bwb_adeg_rewesale_conditions, "BWB Conditions on Rewe's Adeg Store Divestment").
narrative_ontology:topic_domain(bwb_adeg_rewesale_conditions, "economic/competition_law").

domain_priors:requires_active_enforcement(bwb_adeg_rewesale_conditions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bwb_adeg_rewesale_conditions, independent_merchants).
narrative_ontology:constraint_beneficiary(bwb_adeg_rewesale_conditions, austrian_retail_competition).
narrative_ontology:constraint_victim(bwb_adeg_rewesale_conditions, rewe_group_operational_autonomy).
narrative_ontology:constraint_victim(bwb_adeg_rewesale_conditions, market_consolidation_logic).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT MERCHANT (SNARE) — Trapped in the conditions framework. To acquire stores, must comply with all BWB mandates (pricing controls, supply chain restrictions, operational autonomy limits). Cannot renegotiate without losing the acquisition. No exit from the constraint once stores are transferred.
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: AUSTRIAN RETAIL COMPETITION (TANGLED ROPE) — Benefits from the constraint (increased merchant autonomy, store distribution to independents), but faces enforcement costs and market fragmentation. Exit constrained by regulatory structure. Mixed coordination (preventing consolidation) and extraction (enforcement burden).
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REWE GROUP DIVESTMENT PATHWAY (ROPE) — Experiences conditions as coordination mechanism for orderly market exit. Can divest under the framework; conditions reduce uncertainty. Arbitrage option: comply fully and exit cleanly, or challenge selectively. Net beneficiary of certainty despite operational limits.
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: AUSTRIAN FEDERAL COMPETITION AUTHORITY (SCAFFOLD) — Temporary intervention designed to prevent market consolidation during transition. Theater ratio moderate — enforcement requires monitoring but sunset is implicit (conditions decaying as merchants establish independent operations). Sees constraint as transitional support with eventual removal.
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY REGULATORY FRAMEWORK (PITON) — Structural rules designed for 1980s retail consolidation now applied to divestment in 2020s e-commerce era. Theater high (compliance reporting, monitoring, audit trails that add cost without clear market effect). Functional core (preventing abuse of dominance) may be atrophied relative to enforcement burden. Maintained through institutional inertia.
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN VIEW) — Some constraint on dominant firm divestment appears inherent to market function: if a dominant firm divests subsidiary operations without oversight, the acquiring entities inherit structural market disadvantage (supplier leverage, customer switching costs, network effects). This perspective sees the constraint as an emergent market equilibrium condition, not as regulatory imposition. However, this naturalizes what is contingent institutional design — the engine flags this as a false summit.
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bwb_adeg_rewesale_conditions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bwb_adeg_rewesale_conditions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bwb_adeg_rewesale_conditions, TR),
    TR >= 0.70.

:- end_tests(bwb_adeg_rewesale_conditions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The conditions impose measurable operational costs on merchants — supply restrictions limit sourcing flexibility, pricing guidance constrains margin optimization, compliance reporting consumes management time. However, extraction is not maximal because merchants do gain access to established store locations and customer bases; the transfer itself is not predatory. The extractiveness reflects regulatory overhead plus operational constraint, not pure rent extraction. Suppression (0.68): High. Merchants have limited exit — refusing conditions means no store acquisition. Suppliers of restricted merchandise have limited options (forced Rewe relationship). The suppression derives from the mandatory compliance regime, not from overt coercion, but it is substantial. Theater ratio (0.58): Moderate-high. Enforcement requires extensive monitoring (compliance audits, reporting, pricing verification) that may exceed the actual prevention of market consolidation. Some theater derives from regulatory theater (the visible process of oversight) rather than functional necessity. Merchants experience paperwork and compliance burden that may be primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   The merchant's snare and the retail competition's tangled rope represent the core perspectival gap. From the merchant's position, the constraint is extractive and inescapable — they must comply to acquire stores. From the retail competition's position, the constraint solves a genuine coordination problem (preventing post-sale reconsolidation). The scaffold perspective (BWB's own view) assumes sunset and temporary necessity. The piton perspective reveals that some conditions may be regulatory theater — applying 1980s consolidation logic to 2020s divestment where online retail has already fragmented the market. The mountain perspective risks naturalizing what is contingent institutional design as an inherent requirement for preventing market abuse.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality flows asymmetrically: independent merchants bear the extraction (trapped exit, compliance burden, operational limits), while the beneficiaries (Rewe, Austrian retail competition) experience the constraint as coordination or arbitrage opportunity. The BWB occupies an institutional position with constrained exit but also enforcement authority — they experience the constraint as both architecture and burden. The derivation chain flows: merchants (powerless/trapped) → maximum d → high experienced extraction; retail competition (moderate/constrained) → moderate d → mixed extraction/coordination; Rewe (institutional/arbitrage) → low d → arbitrage benefit; BWB (organized/constrained) → moderate-low d → enforcement cost perceived as temporary.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that what appears as 'regulatory extraction' at the merchant level is actually 'market coordination' at the competition level. The merchants experience snare-like constraints, but these constraints serve a genuine coordination function (preventing predatory post-sale behavior). The mandatrophy is resolved by acknowledging both truths: (1) from the merchant's perspective, the constraint is extractive and inescapable, and (2) from the retail market's perspective, the constraint solves a real coordination problem. The analytical observer's mountain view (natural law of market stabilization) is flagged as a false summit — the constraint is a contingent institutional design, not an emergent necessity. The piton perspective identifies that regulatory theater may be substituting for functional necessity as enforcement technology costs increase relative to actual market consolidation risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    condition_enforceability_threshold,
    'What level of merchant compliance with BWB conditions actually prevents market reconsolidation, versus what level is performative oversight?',
    'Longitudinal tracking of divested store operational metrics post-transfer; correlation between condition strictness and merchant survival/independence over 5-10 years',
    'If conditions prevent reconsolidation: tangled_rope classification confirmed. If conditions are theater while consolidation proceeds covertly: classification shifts toward piton or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(condition_enforceability_threshold, empirical, 'Whether conditions effectively prevent market reconsolidation').

omega_variable(
    supply_chain_restriction_necessity,
    'Are supply chain restrictions (e.g., exclusive supplier relationships, minimum purchase volumes) necessary to merchant independence or do they constitute extractive overreach?',
    'Comparative analysis: merchant operational outcomes under restrictive vs permissive supply conditions; merchant exit rates correlated with supply restriction severity',
    'If necessary: extraction component justified as coordination cost (tangled_rope confirmed). If overreach: extractiveness shifts higher, classification risks moving toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_chain_restriction_necessity, empirical, 'Whether supply restrictions are necessary for merchant independence').

omega_variable(
    temporal_sunset_mechanism,
    'Do the conditions have a de facto or de jure sunset date, or do they persist indefinitely as standing restrictions on divested stores?',
    'Review of BWB enforcement decisions and merchant petitions for condition relief; tracking of condition expiration dates or removal petitions',
    'If sunset is real and approaching: scaffold classification confirmed. If indefinite: reclassifies toward piton (inertial constraint) or snare (permanent extraction on merchants).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_sunset_mechanism, empirical, 'Whether conditions have built-in sunset or persist indefinitely').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bwb_adeg_rewesale_conditions, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bwb_adeg_tr_t0, bwb_adeg_rewesale_conditions, theater_ratio, 0, 0.45).
narrative_ontology:measurement(bwb_adeg_tr_t3, bwb_adeg_rewesale_conditions, theater_ratio, 3, 0.52).
narrative_ontology:measurement(bwb_adeg_tr_t6, bwb_adeg_rewesale_conditions, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(bwb_adeg_be_t0, bwb_adeg_rewesale_conditions, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bwb_adeg_be_t3, bwb_adeg_rewesale_conditions, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(bwb_adeg_be_t6, bwb_adeg_rewesale_conditions, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bwb_adeg_rewesale_conditions, enforcement_mechanism).
narrative_ontology:affects_constraint(bwb_adeg_rewesale_conditions, austrian_retail_consolidation_baseline).
narrative_ontology:affects_constraint(bwb_adeg_rewesale_conditions, dominant_firm_divestment_obligations).

% DUAL FORMULATION NOTE:
% The BWB conditions decompose into two structurally distinct constraints: (1) the baseline prevention of retail consolidation (Austrian market structure constraint, epsilon ~0.15, mountain-rope family), and (2) the specific enforcement mechanism on Rewe divestment (this constraint, epsilon 0.52, tangled rope). The divestment conditions are downstream of the consolidation baseline but represent a distinct structural claim about how dominant firms must manage exit. Both constraints share the Austrian regulatory domain but have different ε values and different agent structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bwb_adeg_rewesale_conditions, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
