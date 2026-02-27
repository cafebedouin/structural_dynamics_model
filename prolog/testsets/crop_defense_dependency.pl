% ============================================================================
% CONSTRAINT STORY: crop_defense_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_crop_defense_dependency, []).

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
 *   constraint_id: crop_defense_dependency
 *   human_readable: Dependency on External Pesticides due to Crop Biological Limitations
 *   domain: technological/agricultural_biotechnology
 *
 * SUMMARY:
 *   Modern agriculture exhibits a structural dependency on synthetic
 *   pesticides that emerges not from inherent biological limits but from
 *   breeding choices that prioritized yield and transportability over natural
 *   pest resistance. Over the past 50-70 years, staple crops (corn, wheat,
 *   rice) have been progressively bred to depend on external chemical inputs
 *   to manage pests. This constraint illustrates a Tangled Rope structure:
 *   the industrial seed and agrochemical companies solve a genuine
 *   coordination problem (feeding billions with consistent, affordable
 *   yields) while simultaneously extracting rents through annual seed
 *   purchases, pesticide licensing, and technological lock-in. Smallholder
 *   farmers and agricultural ecosystems bear the costs: chemical exposure,
 *   soil degradation, pollinator collapse, and dependency. The constraint is
 *   neither pure coordination (because victims are locked in and bear
 *   asymmetric costs) nor pure extraction (because the system does deliver
 *   productivity gains). The theater ratio (0.48) reflects that much of the
 *   pesticide system is functionally necessary but some is performative —
 *   certification programs, regulatory compliance, and marketing of
 *   'integrated pest management' solutions that maintain rather than resolve
 *   the fundamental dependency. Innovation communities see the constraint as
 *   temporary (Scaffold), with explicit sunset paths through trait recovery
 *   and agroecological transitions over 15-25 years.
 *
 * KEY AGENTS:
 *   - Smallholder Farmers: Primary victim (powerless/trapped) — lack capital for transition, face yield penalties without modern varieties, cannot coordinate globally
 *   - Agricultural Ecosystems: Primary victim (organized/constrained) — soil health, pollinator populations, water quality degraded; some local organizing capacity through environmental groups
 *   - Agrochemical Manufacturers and Seed Companies: Primary beneficiary (institutional/arbitrage) — extract rents through recurring pesticide sales and seed licensing; invest in R&D and supply coordination
 *   - Regulatory and Certification Systems: Secondary institutional actor (institutional/constrained) — maintain performative compliance theater; under-resourced and capture-prone
 *   - Agricultural Innovation Communities: Organized agent (organized/constrained) — building alternative pathways through trait recovery, agroecology, participatory breeding; have exit vision
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional lock-in as inherent agricultural biology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(crop_defense_dependency, 0.58).
domain_priors:suppression_score(crop_defense_dependency, 0.72).
domain_priors:theater_ratio(crop_defense_dependency, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(crop_defense_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(crop_defense_dependency, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(crop_defense_dependency, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(crop_defense_dependency, tangled_rope).
narrative_ontology:human_readable(crop_defense_dependency, "Dependency on External Pesticides due to Crop Biological Limitations").
narrative_ontology:topic_domain(crop_defense_dependency, "technological/agricultural_biotechnology").

domain_priors:requires_active_enforcement(crop_defense_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(crop_defense_dependency, agrochemical_manufacturers).
narrative_ontology:constraint_beneficiary(crop_defense_dependency, industrial_seed_companies).
narrative_ontology:constraint_victim(crop_defense_dependency, smallholder_farmers).
narrative_ontology:constraint_victim(crop_defense_dependency, agricultural_ecosystems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLHOLDER FARMER (SNARE) — Trapped by biological vulnerability of modern crop varieties and lack of capital for alternative farming systems. No meaningful exit: yields collapse without pesticides, traditional varieties produce lower yields, organic certification requires multi-year transition with no income support. Maximum extraction: farmers pay rental fees to seed companies annually, purchase pesticides at monopolistic prices, and bear health/environmental costs. Cannot organize effectively due to geographic dispersal and capital constraints.
constraint_indexing:constraint_classification(crop_defense_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AGRICULTURAL ECOSYSTEMS & RURAL COMMUNITIES (TANGLED ROPE) — Constrained exit options: local agricultural production systems benefit from the high-yield varieties (coordination function) but suffer pesticide damage to soil health, pollinator populations, and groundwater quality (extraction function). Communities have some organizing capacity (environmental groups, farmer associations) but face regulatory barriers and cultural lock-in. Genuine coordination (feeding large populations with modern varieties) paired with asymmetric extraction (environmental costs borne locally, benefits distributed globally).
constraint_indexing:constraint_classification(crop_defense_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AGROCHEMICAL MANUFACTURERS & SEED COMPANIES (ROPE) — Primary beneficiaries with arbitrage exit options. The constraint solves a genuine coordination problem: modern agriculture requires inputs to bridge the yield gap created by breeding for yield over defense. Companies extract steady revenue through recurring pesticide purchases and seed licensing fees. However, they also invest in R&D, distribution infrastructure, and supply chain coordination that enable global food security. Net beneficiary position: extraction flows toward this agent, but the coordination function is real and valued.
constraint_indexing:constraint_classification(crop_defense_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY & CERTIFICATION SYSTEMS (PITON) — Agricultural extension services, pesticide regulation, and organic certification programs are maintained through institutional inertia despite degraded function. Many regulatory agencies are under-resourced and capture-prone (industry-friendly regulations). Certification programs are performative theater that allow companies to greenwash while maintaining the high-suppression system. The regulatory apparatus persists because alternatives haven't been fully deployed, not because the current system works well. Theater ratio is elevated by compliance documentation divorced from functional safety outcomes.
constraint_indexing:constraint_classification(crop_defense_dependency, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: AGRICULTURAL INNOVATION COMMUNITIES (SCAFFOLD) — Organized coalitions (agroecology networks, participatory plant breeding programs, regenerative agriculture movements) are building alternative pathways with explicit sunset logic. Push traits back into crop varieties through marker-assisted selection or traditional breeding; restore soil biology through diverse rotations; shift to integrated pest management. These movements have agency and see the dependency as temporary — solvable through technology and policy change over 15-25 year horizon. Extraction is low because the innovation communities have exit vision and organizing capacity.
constraint_indexing:constraint_classification(crop_defense_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — From a civilizational/universal perspective, some pest pressure is inherent to agriculture: crops attract pests, and all farming systems must solve this problem. The analytical observer risks naturalizing the pesticide dependency as inherent to agriculture itself. However, this conflates two distinct constraints: (a) the inherent need for pest management (which is genuinely mountain-adjacent), and (b) the specific institutional lock-in to synthetic pesticides because crop varieties were bred to depend on external inputs (which is contingent and institutional). The false mountain reading obscures the second constraint.
constraint_indexing:constraint_classification(crop_defense_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(crop_defense_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(crop_defense_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(crop_defense_dependency, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(crop_defense_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(crop_defense_dependency, TR),
    TR >= 0.70.

:- end_tests(crop_defense_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Moderate-high. The system extracts steady revenue from farmers through annual pesticide purchases and seed licensing, and extracts from ecosystems through chemical externalities. However, it is not maximum extraction because the coordination benefit (reliable yields, global food security) is real and substantial — farmers prefer modern varieties despite their vulnerability. The measure reflects that extraction is significant but justified partly by genuine utility. Suppression (0.72): High. Multiple barriers prevent exit: capital requirements for alternative systems, multi-year transition periods, yield risk, cultural dependence on familiar crop varieties, and technological lock-in (seeds and agrochemicals are bundled). Regulatory barriers (pesticide residue standards that allow high-toxicity inputs, certification costs) amplify suppression. Theater ratio (0.48): Moderate. Functional necessity (pest management genuinely needed for modern agriculture) coexists with performative elements (greenwashing by agrochemical companies, compliance documentation divorced from safety outcomes, organic certification as market segmentation rather than fundamental change). The trajectory shows increasing theater as regulatory and marketing theater displaces actual innovation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Seed companies and agrochemical manufacturers see Rope: they are solving the coordination problem of matching crop varieties to modern pest pressures. Smallholder farmers see Snare: they are trapped by variety vulnerability and capital constraints. Agricultural ecosystems see Tangled Rope: genuine coordination benefit (high-yield crops feed populations) paired with severe extraction (pesticide damage, soil degradation, pollinator loss). Innovation communities see Scaffold: the dependency is real but temporary, with explicit sunset paths through trait recovery and agroecological transitions. Regulatory systems see Piton: the pest management apparatus persists through institutional inertia, with much theater displaced from functional necessity. The analytical observer risks Mountain: pest pressure is inherent to agriculture. This false mountain reading obscures the contingent institutional lock-in that creates the specific dependency structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural positions. Smallholder farmers as trapped agents with no arbitrage exit experience high d (near 0.95) → high f(d) ≈ 1.42 → high experienced extraction. Agrochemical beneficiaries with arbitrage exit options experience low d (near 0.05) → low f(d) ≈ -0.12 → negative effective extraction (they are subsidized). Agricultural ecosystems as constrained organized agents experience moderate-high d (near 0.65) → moderate f(d) ≈ 1.00 → moderate experienced extraction. Innovation communities with constrained exit but agency and exit vision experience lower d (near 0.40) → lower f(d) ≈ 0.40 → moderate extraction, but scaffold classification reflects the sunset mechanism. The piton classification derives from theater ratio rather than high experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that the constraint is genuinely Tangled Rope — not a misclassified Rope (pure coordination) nor a misclassified Snare (pure extraction). The coordination function (matching crop genetics to pest pressures to sustain global yields) is real and valued. The extraction function (rents to agrochemical companies, ecosystem damage, smallholder dependency) is also real and asymmetric. The constraint requires BOTH active enforcement (regulatory approval of pesticides, patent protection for seeds, subsidies that favor industrial agriculture) AND genuine coordination (pest management R&D, supply chains, knowledge transfer). The mandatrophy is fully resolved by the structured analysis of beneficiaries (agrochemical companies, industrial seed firms) and victims (smallholder farmers, ecosystems) with different structural relationships to the same constraint. The theater ratio indicates this is not a degraded coordination mechanism (piton) but an active extraction mechanism with some performative overlay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trait_recovery_feasibility,
    'Can defense traits be recovered in modern crop varieties without unacceptable yield penalties through marker-assisted selection, CRISPR, or participatory breeding?',
    'Field trials measuring yield vs. pest damage tradeoff curves across diverse agroecologies; cost-benefit analysis of trait recovery timelines; comparison with breeding gains from other objectives',
    'If feasible with <10% yield penalty: scaffold timeline shortens (sunset achievable in 10-15 years). If penalty is high (>20%): dependency becomes partially mountain-like (inherent tradeoff between yield and defense).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trait_recovery_feasibility, empirical, 'Feasibility of recovering defense traits in modern varieties without severe yield loss').

omega_variable(
    agroecological_scalability,
    'Do integrated pest management and agroecological approaches (crop diversity, biological control, soil health) actually achieve yield parity with pesticide-dependent monocultures at farm and landscape scales?',
    'Longitudinal studies comparing yields and net margins (including pesticide costs) in diverse agroecological systems vs. conventional systems across climate zones and crop types; farmer income data from transition cohorts',
    'If achievable: snare classification for smallholders may be too severe (exit options improve). If impractical: smallholder trapping is deeper (agroecology is aspirational, not viable). If viable but slower: scaffold timeline extends.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(agroecological_scalability, empirical, 'Whether agroecological systems achieve yield and economic parity with conventional pesticide-dependent agriculture').

omega_variable(
    ecosystem_recovery_lag,
    'How long does agricultural ecosystem recovery take after transitioning away from synthetic pesticides? Are there irreversible degradation pathways?',
    'Soil microbiome and pollinator population recovery timelines from organic transition studies; identification of tipping points in soil health or biodiversity',
    'If recovery is fast (<5 years): ecosystem victims'' perspective improves. If recovery is slow (>15 years) or partial: victim classification becomes more severe (long-term lock-in to ecosystem damage). If irreversible thresholds exist: snare designation for ecosystems is permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_recovery_lag, empirical, 'Ecosystem recovery timeline and reversibility of pesticide-induced degradation').

omega_variable(
    regulatory_capture_degree,
    'To what extent are national and international agricultural regulators captured by agrochemical industries? How much do industry-favorable regulations (e.g., high pesticide residue tolerances, lax approval timelines) exceed what science would justify?',
    'Comparative analysis of pesticide approval thresholds across regulatory jurisdictions; revolving-door analysis of regulator employment at industry; voting records and testimony patterns of regulatory advisory boards; meta-analysis of industry-funded vs. independent safety studies',
    'If capture is severe (>70%): piton classification confirmed (regulatory theater). If minimal (<20%): regulatory systems are functioning and piton downgrade applies. Affects mandatrophy resolution path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_degree, empirical, 'Degree of agrochemical industry capture of agricultural regulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(crop_defense_dependency, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cropdef_tr_t0, crop_defense_dependency, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cropdef_tr_t25, crop_defense_dependency, theater_ratio, 25, 0.42).
narrative_ontology:measurement(cropdef_tr_t50, crop_defense_dependency, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(cropdef_be_t0, crop_defense_dependency, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cropdef_be_t25, crop_defense_dependency, base_extractiveness, 25, 0.5).
narrative_ontology:measurement(cropdef_be_t50, crop_defense_dependency, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(crop_defense_dependency, resource_allocation).
narrative_ontology:affects_constraint(crop_defense_dependency, monoculture_crop_vulnerability).
narrative_ontology:affects_constraint(crop_defense_dependency, agrochemical_industry_market_structure).
narrative_ontology:affects_constraint(crop_defense_dependency, agricultural_water_pollution).

% DUAL FORMULATION NOTE:
% The crop defense dependency constraint decomposes into two related but distinct claims: (a) biological constraint — modern crops have reduced defense mechanisms (inherent breeding tradeoff), (b) institutional constraint — the agrochemical system maintains dependency through regulatory and economic structures. The biological constraint (a) is upstream and near-mountain (inherent yield-defense tradeoff); the institutional constraint (b) is downstream and Tangled Rope (governance choices that lock in dependency). These are linked: biological vulnerability creates demand for external inputs, and institutional structures prevent trait recovery. This story focuses on (b) — the institutional lock-in. Story (a) would focus on the breeding biology and would classify as closer to Mountain or constrained Rope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(crop_defense_dependency, powerless, 0.93).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
