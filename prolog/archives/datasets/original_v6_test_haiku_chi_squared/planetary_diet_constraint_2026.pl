% ============================================================================
% CONSTRAINT STORY: planetary_diet_constraint_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_planetary_diet_constraint_2026, []).

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
 *   constraint_id: planetary_diet_constraint_2026
 *   human_readable: Planetary Boundary Dietary Alignment
 *   domain: ecological/economic/social
 *
 * SUMMARY:
 *   The planetary boundary dietary alignment constraint creates a structural
 *   tension between the legitimate ecological need to reorient global food
 *   systems and the asymmetric distribution of costs and benefits across the
 *   Global North/South divide. The constraint is framed as a universal health
 *   imperative ('the Mediterranean diet is good for the planet and for you'),
 *   but operates as a mechanism transferring the carbon accounting burden to
 *   regions that contributed least to climate change while concentrating
 *   dietary choice and premium food access in wealthy markets. The
 *   Mediterranean diet — featuring olive oil, legumes, fish, seasonal
 *   vegetables — does represent a lower-impact dietary pattern. However, its
 *   promotion as the planetary boundary standard naturalizes the assumption
 *   that Global North consumers should maintain dietary abundance (albeit
 *   'optimized'), while Global South agricultural producers should specialize
 *   in producing 'sustainable' exports under certification regimes. The
 *   constraint manifests across six perspectives ranging from pure extraction
 *   (indigenous food systems displaced by standard adoption) through mixed
 *   coordination-extraction hybrids (Global North consumers benefiting while
 *   bearing moderate costs) to institutional theater (agricultural subsidy
 *   structures persisting despite misalignment with planetary diet
 *   standards). The theater ratio has risen from 0.35 (2020) to 0.64 (2026)
 *   as marketing and certification proliferate while supply chain practices
 *   remain largely unchanged.
 *
 * KEY AGENTS:
 *   - Smallholder Farmers (Global South): Primary victims (powerless/trapped) — forced to adopt certification-compliant monocultures or abandon farming; no alternative livelihood pathways
 *   - Indigenous Food Systems: Primary victims (powerless/trapped) — traditional diets delegitimized by planetary diet standards; land-use pressure increases; cultural erasure via dietary assimilation
 *   - Global North Consumers: Secondary beneficiaries (moderate/constrained) — gain access to marketed 'planetary health' food at premium prices; face modest consumption constraint but within abundance framework
 *   - Food System Certification Bodies: Institutional beneficiaries (institutional/arbitrage) — expand market segments through ESG/carbon credit mechanisms; capture intermediary rents
 *   - Industrial Agriculture Complex: Institutional actor (institutional/arbitrage) — maintains commodity monocultures despite misalignment; persists through subsidy lock-in (piton)
 *   - Food Sovereignty Movements: Organized agents (organized/constrained) — building alternative localized systems with genuine sustainability; represent sunset pathway
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination need but detects systematic extraction disguised as universal standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(planetary_diet_constraint_2026, 0.52).
domain_priors:suppression_score(planetary_diet_constraint_2026, 0.58).
domain_priors:theater_ratio(planetary_diet_constraint_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(planetary_diet_constraint_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(planetary_diet_constraint_2026, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(planetary_diet_constraint_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(planetary_diet_constraint_2026, tangled_rope).
narrative_ontology:human_readable(planetary_diet_constraint_2026, "Planetary Boundary Dietary Alignment").
narrative_ontology:topic_domain(planetary_diet_constraint_2026, "ecological/economic/social").

domain_priors:requires_active_enforcement(planetary_diet_constraint_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(planetary_diet_constraint_2026, global_north_consumers).
narrative_ontology:constraint_beneficiary(planetary_diet_constraint_2026, industrial_agriculture).
narrative_ontology:constraint_beneficiary(planetary_diet_constraint_2026, carbon_credit_markets).
narrative_ontology:constraint_beneficiary(planetary_diet_constraint_2026, mediterranean_diet_marketers).
narrative_ontology:constraint_victim(planetary_diet_constraint_2026, global_south_agricultural_producers).
narrative_ontology:constraint_victim(planetary_diet_constraint_2026, smallholder_farmers).
narrative_ontology:constraint_victim(planetary_diet_constraint_2026, indigenous_food_systems).
narrative_ontology:constraint_victim(planetary_diet_constraint_2026, planetary_boundary_buffer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLHOLDER FARMER (SNARE) — Trapped between subsistence requirements and planetary diet standards. Cannot exit the land-use pressure regime; forced to adopt monoculture or abandon farming. No alternative livelihood. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.68.
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INDIGENOUS FOOD SYSTEMS (SNARE) — Trapped between preservation of traditional diets and pressure to align with global planetary health standards. No exit from land dispossession; high suppression through policy and market mechanisms. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.75.
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: GLOBAL NORTH CONSUMERS (TANGLED ROPE) — Constrained by food availability and price volatility, but also positioned to benefit from Mediterranean diet rebranding as premium/healthy/planetary. Coordination function: align consumption with planetary boundaries. Extraction function: cost externalized to producers. d≈0.58, f(d)≈0.72, σ=1.0 → χ≈0.37.
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CERTIFICATION BODIES (ROPE) — Benefit from expanding planetary diet compliance mechanisms. Experience as coordination: certifying 'planetary health' food systems. High arbitrage opportunity in carbon credits and ESG market segments. d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.06. Net beneficiary via institutional premium.
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FOOD SOVEREIGNTY MOVEMENTS (SCAFFOLD) — Organized alternative to top-down planetary diet framing. See temporary coercive regime (2025-2040) being superseded by localized food systems with genuine sustainability. d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.31. Suppression declining as alternative systems prove viable.
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INDUSTRIAL AGRICULTURE COMPLEX (PITON) — Maintains commodity crop monocultures (corn, soy) despite planetary diet standards favoring vegetables, legumes, nuts. The constraint is largely performative: subsidy structures and supply chain inertia persist despite misalignment with climate goals. theater_ratio=0.67. The system sees its own mechanisms as degraded but persists through structural lock-in.
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees genuine coordination need (align human diet to planetary boundaries) but detects asymmetric extraction: costs concentrated in Global South, benefits concentrated in Global North + food markets. The constraint IS necessary but currently configured as transfer mechanism disguised as universal health standard. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.68.
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(planetary_diet_constraint_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(planetary_diet_constraint_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(planetary_diet_constraint_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(planetary_diet_constraint_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(planetary_diet_constraint_2026, TR),
    TR >= 0.70.

:- end_tests(planetary_diet_constraint_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts meaningful costs from Global South producers (land-use change, monoculture pressure, compliance burden) while distributing benefits toward Global North consumers (food access + premium market positioning) and certification intermediaries. The extraction is not total (some producers benefit from premium pricing; some consumers face real dietary restrictions) but the asymmetry is structural. The trajectory shows increasing extractiveness over the 2020-2026 period as certification infrastructure expands (0.30→0.52) and scale-up begins. Suppression (0.58): Moderate-high. Multiple mechanisms limit alternatives: subsidy structures lock in commodity crops; land consolidation reduces smallholder viability; certification costs create entry barriers; marketing frames 'planetary diet' as scientifically inevitable (not negotiable). But suppression is not total — food sovereignty movements are visible and growing, localized alternatives are emerging, and some producers can exit to higher-value crops. Theater ratio (0.64): Moderate-high and rising. Significant performative content: carbon accounting treats land-use change as externality rather than addressing subsidy-driven monoculture; certification rituals create appearance of compliance without supply chain transformation; 'planetary health' branding creates moral authority for what is fundamentally a consumption pattern favoring wealthy markets. Theater has increased 0.35→0.64 as marketing infrastructure expands faster than supply-chain restructuring.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a perspectival chasm between Global North and Global South, and between institutional actors and affected communities. Smallholder farmers see pure extraction (Snare) — no exit, no coordination benefit, only pressure to conform to externally-defined standards. Indigenous food systems see erasure disguised as universalism (Snare at highest extraction). Global North consumers see beneficial coordination with modest personal constraint (Tangled Rope) — they benefit from optimized nutrition + environmental credibility without lifestyle collapse. Certification bodies see pure coordination (Rope) — they solve legitimate problem (aligning diet to boundaries) while capturing institutional premium. Industrial agriculture sees its own degradation (Piton) — supply chains persist despite misalignment, theater rising as marketing compensates for unchanged practices. Food sovereignty movements see temporary coercive regime with a real exit path (Scaffold) — open-source agriculture and localized food systems represent genuine sunset mechanism. The analytical observer sees the constraint as a necessary coordination mechanism currently configured as an extraction mechanism (Tangled Rope) — it CAN be restructured to distribute costs fairly, but the current institutional path locks in asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Smallholder farmers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Indigenous systems: Victim + trapped + cultural + generational → d≈0.95, f(d)≈1.42. Maximum extraction. Global North consumers: Both beneficiary (dietary premium) + victim (constraint on consumption) + mobile (exit available through non-compliance) → d≈0.58, f(d)≈0.72. Moderate extraction. Certification bodies: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary. Food sovereignty movements: Organized + constrained (facing institutional pressure) but with visible exit path (localized systems proving viable) → d≈0.45, f(d)≈0.50. Moderate extraction with declining trajectory. Industrial agriculture: Beneficiary (maintains subsidy structure) but sees own mechanism as degraded (theater) → d≈0.10, f(d)≈-0.07. Piton classification. Analytical observer: Neutral position revealing extraction structure → d≈0.72, f(d)≈1.15. Observer-level extraction reveals the constraint's true asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing the COORDINATION NEED (genuine) from the EXTRACTION MECHANISM (current instantiation). The planetary boundary dietary alignment IS necessary coordination — human consumption must reorient toward lower-impact patterns to preserve ecological stability. But the current institutional form (Mediterranean diet as universal standard, enforced through supply chain pressure and certification regimes) is a transfer mechanism, not a coordination solution. The mandatrophy resolves by asking: 'Is there an alternative coordination pathway that distributes costs fairly?' Answer: YES — food sovereignty movements demonstrating localized food systems with equivalent or superior planetary boundary performance, without requiring Global South specialization in Global North dietary exports. The Tangled Rope classification is therefore structurally correct and stable: genuine coordination function (planetary alignment) + asymmetric extraction (cost transfer to Global South). The constraint COULD transition to Rope (pure coordination) if supply chains were restructured to benefit producers equally, or to Scaffold (temporary coordination with sunset) if localized systems succeed in replacing the global standard. Currently, it is Tangled Rope with high extraction loading. The false natural law move would be treating 'the need for planetary diet coordination' as justifying 'the Mediterranean standard must dominate.' These are distinct: the first is necessary; the second is contingent institutional choice that concentrates benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mediterranean_diet_scalability,
    'Can the Mediterranean diet model (olive oil, legumes, fish, seasonal vegetables) be globally scaled without reproducing the land-use asymmetries it claims to resolve?',
    'Life-cycle assessment comparing Mediterranean diet production footprint at regional vs global scale; land-use modeling for global adoption; comparison with localized plant-forward diets in each bioregion',
    'If not scalable: planetary diet is regional-capture mechanism (Snare). If scalable: provides legitimate coordinated pathway (Rope → Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mediterranean_diet_scalability, empirical, 'Scalability of Mediterranean diet model globally').

omega_variable(
    producer_income_redistribution,
    'Do carbon credit and premium pricing mechanisms from planetary diet adoption actually flow to smallholder farmers, or do they concentrate in certification/marketing intermediaries?',
    'Supply chain audit of carbon credit distribution; farmer income data before/after planetary diet certification; intermediary markup analysis',
    'If redistribution fails: constraint is pure extraction (Snare). If redistribution succeeds: constraint becomes genuine Rope with coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(producer_income_redistribution, empirical, 'Whether planetary diet premiums reach agricultural producers').

omega_variable(
    indigenous_diet_equivalence,
    'Do indigenous food systems in their original territories provide equivalent or superior planetary boundary alignment compared to Mediterranean diet adoption in the same regions?',
    'Comparative land-use analysis; biodiversity and soil health metrics; carbon sequestration potential; nutrition adequacy analysis',
    'If equivalent/superior: planetary diet is cultural erasure mechanism enforcing Global North standards (Snare). If inferior: indigenous systems genuinely require external coordination (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_diet_equivalence, empirical, 'Planetary boundary performance of indigenous vs Mediterranean diets by region').

omega_variable(
    constraint_reversibility,
    'Once planetary diet adoption infrastructure is built (supply chains, certification systems, market segments), can it be reversed if food sovereignty movements succeed, or does it create path dependency toward Global North dietary hegemony?',
    'Historical analysis of comparable dietary transitions; modeling of lock-in risk; organizational commitment analysis of institutional actors',
    'If reversible: scaffold sunset logic is credible. If locked-in: constraint becomes permanent Snare or Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constraint_reversibility, conceptual, 'Reversibility of planetary diet institutional infrastructure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(planetary_diet_constraint_2026, 2020, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pdieta_tr_t0, planetary_diet_constraint_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pdieta_tr_t3, planetary_diet_constraint_2026, theater_ratio, 3, 0.5).
narrative_ontology:measurement(pdieta_tr_t6, planetary_diet_constraint_2026, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(pdieta_be_t0, planetary_diet_constraint_2026, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(pdieta_be_t3, planetary_diet_constraint_2026, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(pdieta_be_t6, planetary_diet_constraint_2026, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(planetary_diet_constraint_2026, resource_allocation).
narrative_ontology:affects_constraint(planetary_diet_constraint_2026, land_use_change_pressure).
narrative_ontology:affects_constraint(planetary_diet_constraint_2026, agricultural_subsidy_lock_in).
narrative_ontology:affects_constraint(planetary_diet_constraint_2026, food_sovereignty_movements).
narrative_ontology:affects_constraint(planetary_diet_constraint_2026, carbon_credit_monetization).

% DUAL FORMULATION NOTE:
% The planetary diet constraint decomposes into two structurally distinct claims: (1) ECOLOGICAL_ALIGNMENT: individual/collective consumption must reorient toward lower-impact patterns (ε≈0.08, Mountain or Rope depending on whether behavioral flexibility exists); (2) INSTITUTIONAL_EXTRACTION: the current mechanism (Mediterranean diet standard + certification + Global North markets) transfers ecological accounting burden to Global South producers (ε≈0.52, Tangled Rope). This story analyzes the institutional extraction layer. The pure coordination claim should be decomposed into its own story with lower ε if the focus is on verifying the legitimate ecological boundary, independent of institutional distribution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(planetary_diet_constraint_2026, powerless, 0.92).
constraint_indexing:directionality_override(planetary_diet_constraint_2026, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
