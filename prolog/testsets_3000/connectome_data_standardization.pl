% ============================================================================
% CONSTRAINT STORY: connectome_data_standardization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_connectome_data_standardization, []).

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
 *   constraint_id: connectome_data_standardization
 *   human_readable: Connectome Data Standardization Constraint
 *   domain: neuroscience/computational_biology/data_infrastructure
 *
 * SUMMARY:
 *   Connectome data standardization represents a structural coordination
 *   problem with embedded extractive mechanisms. The fragmentation of
 *   connectome data formats across labs creates genuine scientific barriers —
 *   integrative analysis, meta-analysis, and large-scale circuit
 *   reconstruction all require interoperable data representations. However,
 *   the standardization process itself generates asymmetric costs and
 *   benefits. Well-resourced institutions and data platform operators benefit
 *   from standardization through ecosystem control and aggregation
 *   advantages; resource-constrained labs bear heavy adaptation costs without
 *   proportional return. The constraint exhibits the core tension of tangled
 *   rope: real coordination function (standards do solve fragmentation)
 *   alongside systematic extraction (the standardization process concentrates
 *   power and imposes costs on those least able to bear them). The theater
 *   ratio (0.64) reflects that much standards-setting activity is
 *   performative — lengthy working group meetings, format specification
 *   documents, compliance certification — while actual adoption lags and many
 *   labs maintain hybrid or legacy systems in parallel.
 *
 * KEY AGENTS:
 *   - Resource-Constrained Lab: Primary victim (powerless/trapped) — bears full costs of format migration, tool adoption, staff retraining without equivalent benefit from sharing infrastructure
 *   - Data Platform Operator: Primary beneficiary (institutional/arbitrage) — captures aggregation value, ecosystem lock-in, and data monopoly benefits from coordination
 *   - Mid-Tier Research Group: Secondary actor (moderate/constrained) — constrained by adaptation costs but genuine beneficiary from shared tools and format compatibility
 *   - Standards Organization: Organized coalition (organized/constrained) — manages standardization process; claims sunset horizon as adoption matures
 *   - Legacy Format Community: Institutional actor (institutional/arbitrage) — maintains non-standard systems through inertia; sees own practice as degraded
 *   - Analytical Observer: Universal view (analytical/analytical) — sees full structure: coordination function real, but extraction mechanism systematic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(connectome_data_standardization, 0.52).
domain_priors:suppression_score(connectome_data_standardization, 0.58).
domain_priors:theater_ratio(connectome_data_standardization, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(connectome_data_standardization, extractiveness, 0.52).
narrative_ontology:constraint_metric(connectome_data_standardization, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(connectome_data_standardization, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(connectome_data_standardization, tangled_rope).
narrative_ontology:human_readable(connectome_data_standardization, "Connectome Data Standardization Constraint").
narrative_ontology:topic_domain(connectome_data_standardization, "neuroscience/computational_biology/data_infrastructure").

domain_priors:requires_active_enforcement(connectome_data_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(connectome_data_standardization, data_platform_operators).
narrative_ontology:constraint_beneficiary(connectome_data_standardization, early_adopter_labs).
narrative_ontology:constraint_victim(connectome_data_standardization, field_interoperability).
narrative_ontology:constraint_victim(connectome_data_standardization, resource_constrained_labs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESOURCE-CONSTRAINED LAB (SNARE) — Faces overwhelming barriers to participation in standardization ecosystem. High equipment costs, staff expertise requirements, and format conversion overhead create extraction without coordination benefit. Cannot exit: publishing connectome data requires conformance to emerging de facto standards controlled by well-funded platforms.
constraint_indexing:constraint_classification(connectome_data_standardization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER RESEARCH GROUP (TANGLED ROPE) — Constrained by adaptation costs but benefits from standardization's coordination function: shared tools reduce development burden, standard formats enable collaboration. Mixed experience — genuine coordination benefit alongside asymmetric costs of format migration and tool lock-in.
constraint_indexing:constraint_classification(connectome_data_standardization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DATA PLATFORM OPERATOR (ROPE) — Institutional beneficiary with arbitrage options. Experiences standardization as pure coordination: operating a hub for standardized connectome data solves legitimate collective action problem of fragmentation. Net beneficiary through data aggregation monopoly and ecosystem lock-in.
constraint_indexing:constraint_classification(connectome_data_standardization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STANDARDS ORGANIZATION (SCAFFOLD) — Organized coalition (INCF, international consortia) frames standardization as temporary coordination support with explicit sunset: as tool maturity increases and adoption broadens, community should converge on self-organizing standards. Theater ratio moderate (some performative standards-setting meetings) but genuine coordination function visible. Exit path exists through ecosystem maturation.
constraint_indexing:constraint_classification(connectome_data_standardization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY FORMAT COMMUNITY (PITON) — Traditional connectome formats (custom HDF5, lab-specific schemas) persist through institutional inertia despite acknowledged limitations. Community recognizes its own formats as degraded — performative maintenance of legacy systems because switching costs are high and path dependency is entrenched. Theater dominates function.
constraint_indexing:constraint_classification(connectome_data_standardization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, standardization exhibits both genuine coordination (fragmenting formats create barrier to science) and asymmetric extraction (early movers capture ecosystem advantage, late movers bear adaptation costs). The constraint persists because switching costs are sufficiently high that some labs remain trapped even if coordination benefits are clear.
constraint_indexing:constraint_classification(connectome_data_standardization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(connectome_data_standardization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(connectome_data_standardization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(connectome_data_standardization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(connectome_data_standardization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(connectome_data_standardization, TR),
    TR >= 0.70.

:- end_tests(connectome_data_standardization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and increasing over the measurement interval (0.28 → 0.52). Initial extractiveness is low because standardization is genuinely solving coordination problems and adoption is voluntary. As pressure from data platforms and funding agencies intensifies, extractiveness increases — non-adopters face publication barriers, collaboration impediments, and reputational cost. The trend reflects increasing coercion to conform, not increasing coordination value. Suppression (0.58): Moderate-high. Barriers to non-adoption include: high format conversion costs, specialized tool requirements, staff expertise gaps, and—critically—publication bias (journals increasingly require standardized data deposition). These barriers are particularly acute for under-resourced labs. Unlike trapped agents (material impossibility to exit), these labs are constrained (high but surmountable costs), but the suppression level (0.58) reflects that suppression operates through institutional mechanisms (publication requirements) rather than just material barriers. Theater ratio (0.64): Moderate-high and increasing. Performative elements include lengthy standards working group meetings, multiple competing format specifications, compliance documentation that diverges from actual practice, and public commitment to standards adoption with private maintenance of legacy systems. Theater increases over time because the gap between declared standardization and actual ecosystem fragmentation widens — many labs nominally adopt standards while maintaining legacy formats for internal use.
 *
 * PERSPECTIVAL GAP:
 *   The resource-constrained lab perceives snare (pure extraction with minimal coordination benefit — they get excluded from collaborative networks if they don't conform). The data platform operator perceives rope (pure coordination — standardization solves fragmentation and enables the aggregation platform). The mid-tier group perceives tangled rope (real coordination benefit of shared tools and standards, mixed with real extraction through adaptation costs and ecosystem lock-in). The standards organization perceives scaffold (temporary coordination problem being solved by maturing tools and broadening adoption — sunset horizon visible). The legacy format community perceives piton (their own systems are degraded but maintained through inertia). The analytical observer perceives tangled rope at civilizational scope — the full structure is visible: both genuine coordination function AND systematic extraction concentrated on resource-constrained actors. The perspectival gap reveals that single-position analysis (from platform operator or resource-constrained lab alone) would misclassify the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. Data platform operators (beneficiaries with arbitrage options) experience low d values — they benefit from standardization and can exit the constraint if ecosystem dynamics shift (they have alternatives). Resource-constrained labs (victims with trapped exit options) experience high d values — they bear costs and cannot feasibly exit (the scientific field increasingly requires standardized data sharing). Mid-tier groups (moderate power with constrained exit) experience middle d values — they have some agency (can invest in tools, hire staff) but significant costs remain. The power atom assignment (powerless vs moderate vs institutional) reflects constraint-specific structural position, not global institutional standing. A well-funded lab with prestigious affiliations is 'powerless' relative to this constraint if ecosystem lock-in leaves it no exit — conversely, a smaller lab may be 'moderate' or 'organized' if it has alternative publishing venues or collaboration networks not dependent on standardization.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that standardization IS a tangled rope: both coordination and extraction are structurally real. The error to avoid is (1) naturalizing standardization as pure coordination (rope) — this ignores the systematic extraction concentrated on resource-constrained labs — or (2) dismissing standardization as pure extraction (snare) — this ignores the genuine coordination value that early adopters and platform operators experience and that the field collectively benefits from. The constraint persists because the coordination function is real enough to attract support and the extraction mechanism is distributed enough across actors that organizing resistance is difficult. Resource-constrained labs experience snare (trapped, no alternatives), but they are individually powerless to resist. Data platform operators experience rope (coordination solves fragmentation), so they advocate for standardization. Mid-tier groups experience tangled rope (mixed benefit and cost) and have some negotiating power but insufficient incentive to organize opposition. The scaffold perspective from the standards organization is strategically important — it claims a sunset horizon (as tool maturity and adoption broaden, the constraint should fade). This claim is testable: if adoption rates plateau and ecosystem continues to fragment, the sunset claim is falsified and the constraint hardens into permanent snare for late movers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    format_optimization_direction,
    'Are the standardization pressures driven by genuine scientific need (interoperability for integrative analysis) or by platform-imposed format requirements (technical convenience of aggregators)?',
    'Comparative analysis: do labs adopting standards report improved scientific productivity or primarily report administrative burden reduction? Do platform operators generate exclusive value from format lock-in or from genuine scale effects?',
    'If scientifically driven: classification softens toward Rope (legitimate coordination dominates). If platform-driven: classification hardens toward Snare (extraction dominates for late movers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(format_optimization_direction, empirical, 'Whether standardization is scientifically motivated or platform-extraction motivated').

omega_variable(
    adaptation_cost_distribution,
    'Are format conversion and tool adoption costs distributed equitably across labs by funding level, or do costs concentrate on resource-constrained groups?',
    'Cost accounting by lab size and resources; analysis of which labs adopt standards earliest vs latest; correlation between lab resources and adoption timeline.',
    'If equitable: suppression values lower (barriers exist but are not disproportionate). If concentrated: suppression values higher and snare classification strengthens (extraction mechanism is systematic).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_cost_distribution, empirical, 'Whether standardization costs concentrate on resource-constrained labs').

omega_variable(
    collaborative_benefit_realization,
    'In practice, do standardized connectome datasets enable new collaborative discoveries, or does standardization primarily benefit data platform operators through aggregation?',
    'Citation analysis of papers using standardized vs non-standardized datasets; measurement of collaboration patterns across labs pre- and post-standardization adoption.',
    'If strong collaborative benefit: tangled rope classification is robust (coordination function is real). If minimal benefit: extractiveness increases (players bear costs without proportional benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collaborative_benefit_realization, empirical, 'Whether standardization enables collaborative scientific gains').

omega_variable(
    escape_velocity_threshold,
    'Is there a tipping point at which alternative standards emerge that fragment the market further, causing labs to abandon the dominant standard and create competing ecosystems?',
    'Historical precedent from bioinformatics (FASTA, GenBank, XML variants); monitoring of emerging connectome formats (NWB, GraphML variants) and adoption trajectories.',
    'If tipping point exists below 70% adoption: the constraint is more fragile than suppression metrics suggest, and scaffold perspective is stronger. If no tipping point: lock-in is deep and snare perspective hardens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escape_velocity_threshold, empirical, 'Whether market-fragmenting alternative standards can emerge').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(connectome_data_standardization, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cds_tr_t0, connectome_data_standardization, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cds_tr_t3, connectome_data_standardization, theater_ratio, 3, 0.51).
narrative_ontology:measurement(cds_tr_t6, connectome_data_standardization, theater_ratio, 6, 0.6).
narrative_ontology:measurement(cds_tr_t9, connectome_data_standardization, theater_ratio, 9, 0.64).

% Extraction over time
narrative_ontology:measurement(cds_be_t0, connectome_data_standardization, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cds_be_t3, connectome_data_standardization, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(cds_be_t6, connectome_data_standardization, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(cds_be_t9, connectome_data_standardization, base_extractiveness, 9, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(connectome_data_standardization, information_standard).
narrative_ontology:affects_constraint(connectome_data_standardization, connectome_data_access_equity).
narrative_ontology:affects_constraint(connectome_data_standardization, neuroscience_tool_ecosystem_lock_in).

% DUAL FORMULATION NOTE:
% Connectome standardization is upstream of data access constraints but represents a distinct extraction mechanism. Parallel constraint stories track (1) the technical standardization problem (this story), (2) the resource equity consequences (downstream), and (3) tool ecosystem dependencies (parallel mechanism). Each has its own epsilon and perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(connectome_data_standardization, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
