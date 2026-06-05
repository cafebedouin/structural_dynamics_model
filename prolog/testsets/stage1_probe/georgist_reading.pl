% ============================================================================
% CONSTRAINT STORY: georgist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_georgist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: georgist_reading
 *   human_readable: Georgist Reading: Land Rent Extraction Separated from Improvement Value
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   The Georgist reading of price formation claims that land rent (the
 *   premium arising from location scarcity and monopoly position) is
 *   fundamentally separable from improvement value (the product of productive
 *   labor and capital investment). This reading undergirds a distinct
 *   political economy: rent extraction is unearned; improvement production is
 *   earned. The institutional constraint emerges when property markets,
 *   financial systems, and tax regimes fail to maintain this
 *   separation—speculative landholders capture location premium while workers
 *   and productive capital bear the cost. From the Georgist perspective, this
 *   is a snare disguised as coordination: the 'coordination' of price
 *   formation naturalizes what is actually asymmetric extraction. The
 *   constraint's empirical trajectory (extractiveness rising from 0.35 to
 *   0.71 over the interval) reflects financialization intensifying rent
 *   capture: real estate becomes an asset class for speculative extraction
 *   rather than a commons to be developed. Theater ratio (0.35-0.38) reflects
 *   that economic language of 'market price' obscures the redistribution of
 *   unearned location premium. This is ONE reading of a contested kernel (the
 *   price_formation_kernel). Sibling readings—naturalist, institutional, and
 *   financialization—instantiate different causal structures and beneficiary
 *   arrangements. The Georgist reading specifically claims that rent can be
 *   separated from improvement value analytically and that this separation
 *   reveals an extractive mechanism hidden in aggregate price data.
 *
 * KEY AGENTS:
 *   - Landowners and land speculators: Primary beneficiaries (institutional/arbitrage) — capture unearned location premium without producing location value; exit flexibility through portfolio management
 *   - Extractive financial institutions: Secondary beneficiaries (institutional/arbitrage) — leverage land as collateral, financing speculation and rent extraction; benefit from rising land values
 *   - Labor force and housing seekers: Primary victims (powerless/trapped) — pay location premium as housing cost; trapped by geography and wage dependency; cannot exit without mobility costs
 *   - Productive capital and builders: Secondary victims (moderate/constrained) — create improvement value but location premium capture erodes returns on productive investment; constrained exit through sunk improvements
 *   - Georgist analytical tradition: Observer position (analytical/analytical) — identifies the separation logic and extraction mechanism; risks naturalizing the constraint as immutable law of scarcity
 *   - Land-value taxation systems: Institutional remedies (institutional/constrained) — attempt to separate and tax location value; degraded implementation (piton perspective) leaves coordination function undermined
 *   - Housing reform movements and organized labor: Organized resistance (organized/mobile) — recognize both coordination and extraction functions; mobilizing political exit through collective action
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(georgist_reading, 0.62).
domain_priors:suppression_score(georgist_reading, 0.48).
domain_priors:theater_ratio(georgist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(georgist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(georgist_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(georgist_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(georgist_reading, tangled_rope).
narrative_ontology:human_readable(georgist_reading, "Georgist Reading: Land Rent Extraction Separated from Improvement Value").
narrative_ontology:topic_domain(georgist_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(georgist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(georgist_reading, 'f6fbb62c-6fab-42c1-9e61-d509676dd13c').
narrative_ontology:cs_kernel_codification('f6fbb62c-6fab-42c1-9e61-d509676dd13c', distributed).
narrative_ontology:cs_authority_grounding('f6fbb62c-6fab-42c1-9e61-d509676dd13c', extraction).
narrative_ontology:cs_interpretation_layer_present('f6fbb62c-6fab-42c1-9e61-d509676dd13c').
narrative_ontology:cs_reading_relation('f6fbb62c-6fab-42c1-9e61-d509676dd13c', georgist_reading__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6fbb62c-6fab-42c1-9e61-d509676dd13c', georgist_reading__institutional_reading, influences).
narrative_ontology:cs_reading_relation('f6fbb62c-6fab-42c1-9e61-d509676dd13c', georgist_reading__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('f6fbb62c-6fab-42c1-9e61-d509676dd13c', foundational, land_rent_separable_analytically).
narrative_ontology:cs_axiom_status(land_rent_separable_analytically, holdable).
narrative_ontology:cs_axiom_grounding('f6fbb62c-6fab-42c1-9e61-d509676dd13c', land_rent_separable_analytically, instrumental).
narrative_ontology:cs_axiom('f6fbb62c-6fab-42c1-9e61-d509676dd13c', foundational, unearned_rent_extract_unjust).
narrative_ontology:cs_axiom_status(unearned_rent_extract_unjust, holdable).
narrative_ontology:cs_axiom_grounding('f6fbb62c-6fab-42c1-9e61-d509676dd13c', unearned_rent_extract_unjust, deontological).
narrative_ontology:cs_reference_frame('f6fbb62c-6fab-42c1-9e61-d509676dd13c', separable_rent_and_improvement_distinction).
narrative_ontology:cs_drift_state('f6fbb62c-6fab-42c1-9e61-d509676dd13c', financialization_era_contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f6fbb62c-6fab-42c1-9e61-d509676dd13c', '').
narrative_ontology:cs_kernel_id(georgist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(georgist_reading, landowners).
narrative_ontology:constraint_beneficiary(georgist_reading, land_speculators).
narrative_ontology:constraint_beneficiary(georgist_reading, extractive_financial_institutions).
narrative_ontology:constraint_victim(georgist_reading, labor_force).
narrative_ontology:constraint_victim(georgist_reading, productive_capital).
narrative_ontology:constraint_victim(georgist_reading, housing_access_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LABOR FORCE (SNARE) — Trapped within geography and locked into wage dependency. The constraint extracts unearned location premium as housing cost. Labor bears the rent burden without producing the location value; exit requires geographical mobility with high structural costs (job mobility, family separation, social networks). Maximum experienced extraction.
constraint_indexing:constraint_classification(georgist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRODUCTIVE CAPITAL (TANGLED ROPE) — Small builders and productive enterprises coordinate to create improvement value (labor, materials, innovation), but the rent extraction mechanism captures much of their surplus as land-value inflation. Mixed coordination (producing real improvements) and extraction (location premium captured by speculative landholders). Constrained exit: can relocate production but face switching costs and sunk improvements.
constraint_indexing:constraint_classification(georgist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXTRACTIVE FINANCE (ROPE) — Perceives land as pure arbitrage: location premium can be captured, borrowed against, and traded without producing anything. Experiences the constraint as coordination mechanism for capturing unearned value. High exit flexibility through portfolio diversification and financial engineering. Benefits directly from rent extraction logic.
constraint_indexing:constraint_classification(georgist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GEORGIST ANALYTICAL (MOUNTAIN) — From the Georgist reading's own framework, the fixed supply and location scarcity of land constitute a natural law: no additional location value can be produced; location premium emerges necessarily from scarcity and competition. However, this perspective risks naturalizing a contingent institutional arrangement (property rights, speculation rules, financial leverage) as immutable law. The constraint's extractiveness (0.62) suggests the 'mountain' classification may be a false summit: the separation of rent from improvement value is enforced, not inherent.
constraint_indexing:constraint_classification(georgist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: LAND-VALUE TAXATION SYSTEMS (PITON) — Where implemented (Denmark, Estonia, Taiwan), land-value taxes (LVT) represent a degraded version of the original Georgist program. The mechanism persists (taxing location value separately from improvements) but the institutional commitment has atrophied — many LVT systems coexist with other forms of rent extraction and do not fully implement the Georgist separation logic. The ritual of separation remains; the functional authority has eroded. Theater_ratio reflects performative compliance without full extraction prevention.
constraint_indexing:constraint_classification(georgist_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ORGANIZED RESISTANCE (TANGLED ROPE) — Housing reform movements and labor organizations recognize the coordination function (productive improvements) and the extraction mechanism (location rent). Organized position allows exit options (political organizing, cooperative housing models, ballot initiatives). Mixed classification: coordination function visible but extraction remains dominant. Exit options partially mobilized through collective action.
constraint_indexing:constraint_classification(georgist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(georgist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(georgist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(georgist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(georgist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(georgist_reading, TR),
    TR >= 0.70.

:- end_tests(georgist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): The Georgist reading quantifies this as the proportion of total property value attributable to location premium vs. improvement cost. Empirically, in high-density markets, location premium often exceeds 50-70% of total price, while improvement costs (construction, materials, labor) represent 30-50%. The victim groups (labor, productive capital) pay this premium in housing costs and diminished investment returns. The trajectory from 0.35 to 0.71 reflects financialization: as real estate becomes tradable asset rather than productive infrastructure, speculative extraction amplifies. Suppression (0.48): Operates through multiple channels: (1) property law structures enable speculative ownership without productive use; (2) housing costs trap labor geographically (mobility costs exceed benefits); (3) financial leverage rules allow landholders to borrow against rising location premium, amplifying extraction; (4) tax policy often exempts unearned gains on land while taxing productive investment. Theater ratio (0.35): Lower than other extractive constraints because the Georgist reading explicitly reveals the separation mechanism. Economic theory of 'land value' makes the extraction visible (though contested). However, aggregate 'market price' rhetoric obscures the redistribution—theater involves linguistic naturalization ('supply and demand,' 'market clearing') of what Georgists identify as institutional extraction.
 *
 * PERSPECTIVAL GAP:
 *   The Georgist reading generates maximum perspectival divergence. The landowner sees pure arbitrage (Rope) with flexible exit. The labor force sees entrapment (Snare) with geographic immobility. Productive capital sees mixed coordination-extraction (Tangled Rope). Land-value taxation systems see a degraded institutional remedy (Piton). Organized resistance sees both functions clearly but mobilizes through collective action (Tangled Rope). The analytical observer risks adopting the Georgist mountain perspective—naturalizing location scarcity as immutable—but the extractiveness metrics and enforcement structures suggest false summit: the separation of rent from improvement value depends on active institutional support (property law, speculative financial markets, tax policy), not on laws of physics or mathematics. Sibling readings would collapse these gaps differently: the naturalist reading sees all as market coordination (Rope); the institutional reading emphasizes regime-specific enforcement (Tangled Rope from all views); the financialization reading focuses on speculative mechanics (Snare from labor/capital views, Rope from financial institutions). The gap persists because the kernel (price formation) admits multiple readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from structural position: Landowners benefit directly from rent extraction (d ≈ 0.1, low target burden); labor bears location premium costs with trapped exit (d ≈ 0.9, high target burden); productive capital gets mixed treatment (d ≈ 0.6, moderate). The engine's sigmoid f(d) amplifies extraction experienced by trapped agents and dampens it for arbitrage agents. Beneficiary declarations (landowners, extractive finance) produce low d; victim declarations (labor, productive capital) produce high d. No directionality overrides needed—the derivation from beneficiary/victim + exit options yields accurate structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The Georgist reading resolves mandatrophy by asserting that price formation has a specific analytical structure: rent (unearned, location-based) is separable from improvement value (earned, productive). The mandate—ensuring that rent extraction does not dominate productive activity—survives as long as this separation is visible and enforceable. Mandatrophy would occur if the separation becomes impossible to track (indexes collapse, asset classes merge, financial engineering obscures land value within bundled securities). Current trajectory suggests approaching mandatrophy: REITs, securitized mortgages, and derivative markets obscure location premium in complex financial instruments, making the separation difficult to measure and enforce. Land-value taxation persists (Piton) but cannot fully implement the Georgist program when the financial system actively obscures land value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    georgist_vs_naturalist_rent_origin,
    'Does land rent emerge from the natural scarcity of location (mountain) or from the institutional structure of property rights and speculation (snare)?',
    'Historical analysis of land price dynamics pre- and post-property-rights formalization; comparison of jurisdictions with vs. without speculation restrictions; examination of whether location premium persists under different institutional regimes (common ownership, usufruct, leasehold systems)',
    'If institutional: the constraint is a snare from all perspectives; the separation of rent from improvement value is not a natural law but an artifact of property regime. If natural: the constraint is mountain; extraction of location premium is inevitable, and policy can only redirect it, not eliminate it. This is the core dispute between Georgist reading and naturalist reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(georgist_vs_naturalist_rent_origin, empirical, 'Origin of land rent: natural scarcity vs. institutional property structure').

omega_variable(
    improvement_coordination_function,
    'Is the improvement-value component genuinely a coordination mechanism, or does it serve primarily as cover for extracting unearned rent?',
    'Measurement of improvement-value ratio (improvement cost / total property price) across jurisdictions and time periods; correlation with actual construction activity and capital investment; analysis of whether properties appreciate in value without improvements (speculative capture) or primarily through development',
    'If primarily coordination: the tangled_rope classification holds; real production happens alongside extraction. If primarily cover: the constraint reclassifies as snare across all perspectives; the rhetoric of ''productive improvement'' naturalizes pure location extraction. This determines whether the constraint has a legitimate coordination function or is purely extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(improvement_coordination_function, empirical, 'Whether improvement value functions as genuine coordination or as rhetorical cover').

omega_variable(
    georgist_kernel_reading_contestation,
    'Is the Georgist separation of rent from improvement value a discovery of natural economic law or a normative reading of price formation that competes with alternative framings?',
    'Historiographical analysis of Georgist tradition vs. competing schools (neoclassical, Marxist, institutional); examination of which reading dominates in policy (zoning law, tax codes, valuation systems); assessment of whether ''land value'' is a measurable economic fact or a constructed analytical category that depends on the framework chosen',
    'If natural law: the Georgist reading is the true structure; other readings are error or ideology. If contested kernel: the Georgist reading is one coherent framing among multiple equally coherent readings; the constraint has a different ε, beneficiary/victim structure, and classification under each reading. This omega documents that this constraint story is ONE reading of a kernel, not THE structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(georgist_kernel_reading_contestation, conceptual, 'Whether Georgist separation is natural law or one reading of a contested kernel').

omega_variable(
    rent_extraction_mechanism_enforcement,
    'What specific institutional mechanisms enforce the separation of rent from improvement value, and how are they maintained?',
    'Institutional analysis of property law, tax policy, financial leverage rules, and speculation protections; mapping of which actors benefit from each enforcement mechanism; identification of where the mechanism depends on active enforcement vs. self-reinforcing incentives',
    'If actively enforced: the requires_active_enforcement flag is correct; removing enforcement would collapse the separation. If self-reinforcing: speculative financial incentives maintain the separation even without explicit enforcement. If weakly enforced: the piton classification may underestimate functional degradation. Determines whether the constraint requires active institutional effort or has become entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_extraction_mechanism_enforcement, empirical, 'Which institutional mechanisms enforce rent/improvement separation').

omega_variable(
    suppression_mechanism_specificity,
    'Does the measured suppression (0.48) reflect coercive barriers to exit, internalized ideology naturalizing rent extraction, or both?',
    'Qualitative analysis of labor migration patterns and barriers; measurement of housing-cost burden as percentage of income vs. voluntary vs. constrained residential choices; examination of whether workers perceive rent extraction as unjust (surmountable with political will) or natural (immutable limit)',
    'If primarily external barriers: suppression is structural; raising exit costs (mobility support, housing support) would reduce experienced extraction. If primarily internalized: suppression persists even when barriers are lowered; the constraint has identity_locked components. If both: the measured suppression underestimates functional effect because internalization multiplies external constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_specificity, empirical, 'Suppression mechanism: external barriers vs. internalized ideology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(georgist_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(georgist_theater_1890s, georgist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(georgist_theater_1950s, georgist_reading, theater_ratio, 2, 0.3).
narrative_ontology:measurement(georgist_theater_2000s, georgist_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(georgist_theater_2020s, georgist_reading, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(georgist_extractiveness_1890s, georgist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(georgist_extractiveness_1950s, georgist_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(georgist_extractiveness_2000s, georgist_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(georgist_extractiveness_2020s, georgist_reading, base_extractiveness, 6, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(georgist_suppression_1890s, georgist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(georgist_suppression_1950s, georgist_reading, suppression_requirement, 2, 0.45).
narrative_ontology:measurement(georgist_suppression_2000s, georgist_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(georgist_suppression_2020s, georgist_reading, suppression_requirement, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(georgist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(georgist_reading, 0.12).
narrative_ontology:affects_constraint(georgist_reading, naturalist_reading).
narrative_ontology:affects_constraint(georgist_reading, institutional_reading).
narrative_ontology:affects_constraint(georgist_reading, financialization_reading).

% DUAL FORMULATION NOTE:
% The price_formation_kernel admits multiple readings with distinct structural properties. The Georgist reading (this file) extracts the separation logic and focuses on rent-as-extraction. The naturalist reading would emphasize scarcity-driven market clearing. The institutional reading would highlight regime-specificity. The financialization reading would focus on speculation mechanisms. Each reading has a different claimed_type distribution, beneficiary/victim structure, and temporal trajectory. They are linked via network.affects_constraints to enable comparative analysis across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
