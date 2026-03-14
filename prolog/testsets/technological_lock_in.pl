% ============================================================================
% CONSTRAINT STORY: technological_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technological_lock_in, []).

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
 *   constraint_id: technological_lock_in
 *   human_readable: Technological Lock-In: Path Dependency and Switching Costs
 *   domain: technology/economics/infrastructure
 *
 * SUMMARY:
 *   Technological lock-in occurs when users or organizations become unable or
 *   unwilling to switch from an incumbent technology platform despite the
 *   availability of superior or equivalent alternatives, due to switching
 *   costs, network effects, data incompatibility, or ecosystem dependencies.
 *   This constraint exhibits the full spectrum of DR classifications
 *   depending on observer position. The same structural phenomenon — network
 *   effects that prevent switching — appears as a coordination mechanism
 *   solving the platform bootstrap problem (rope from the incumbent
 *   perspective), a genuine hybrid of coordination and asymmetric extraction
 *   (tangled rope from organizational and rival perspectives), an immutable
 *   property of how technologies scale (false mountain from the analytical
 *   perspective), a temporary problem being solved by interoperability
 *   standards (scaffold from the coalition perspective), or pure extraction
 *   with no exit (snare from individual trapped users). The constraint's
 *   extractiveness has increased over time (0.30→0.58) as network effects
 *   mature and switching costs accumulate, while theater_ratio has also
 *   increased (0.35→0.55), indicating that as the lock-in solidifies,
 *   institutional actors increasingly perform exploration of alternatives
 *   rather than genuinely pursuing exit.
 *
 * KEY AGENTS:
 *   - Locked-In Users: Primary victims (powerless/trapped) — face catastrophic switching costs including data conversion, workflow retraining, lost ecosystem integrations, incompatibility with collaborators' systems
 *   - Organizational Adopters: Secondary victims and partial beneficiaries (moderate/constrained) — experience coordination benefits from ecosystem maturity but also face extraction through vendor lock-in pricing and limited switching options
 *   - Incumbent Technology Provider: Primary beneficiary (institutional/arbitrage) — captures network effects and switching costs; can set prices and control ecosystem evolution with minimal threat of displacement
 *   - Rival Technology Competitors: Strategic actors (powerful/mobile) — must overcome coordination barriers while acknowledging legitimate network effects; invest in alternative ecosystems to break lock-in
 *   - Open Standards Coalition: Organized agents (organized/constrained) — industry consortia, open-source communities, standards bodies pursuing interoperability and data portability as lock-in escape mechanisms
 *   - Legacy Institutional Holdouts: Inertial actors (institutional/analytical) — continue using deprecated technology due to prior investment, regulatory constraints, or capability gaps; perform evaluation of alternatives without executing migration
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements (API design choices, data formats, business models) as immutable properties of technological networks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technological_lock_in, 0.58).
domain_priors:suppression_score(technological_lock_in, 0.65).
domain_priors:theater_ratio(technological_lock_in, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technological_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(technological_lock_in, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(technological_lock_in, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technological_lock_in, tangled_rope).
narrative_ontology:human_readable(technological_lock_in, "Technological Lock-In: Path Dependency and Switching Costs").
narrative_ontology:topic_domain(technological_lock_in, "technology/economics/infrastructure").

domain_priors:requires_active_enforcement(technological_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technological_lock_in, incumbent_technology_provider).
narrative_ontology:constraint_beneficiary(technological_lock_in, network_effects_beneficiary).
narrative_ontology:constraint_victim(technological_lock_in, trapped_users).
narrative_ontology:constraint_victim(technological_lock_in, superior_alternative_technologies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED USER (SNARE) — Individual users or organizations cannot exit without catastrophic switching costs: data incompatibility, lost productivity during migration, retraining burden, ecosystem dependencies. The constraint extracts maximum value from this agent with minimal coordination benefit. No meaningful alternative exists at acceptable cost. The user bears the full burden of path dependency.
constraint_indexing:constraint_classification(technological_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ORGANIZATIONAL ADOPTER (TANGLED ROPE) — Organizations experience both genuine coordination benefits (ecosystem maturity, workforce familiarity, supplier integration) and asymmetric extraction (vendor lock-in, upgrade costs, limited negotiating power). Exit is theoretically possible but at substantial cost. The constraint both solves collective action problems AND extracts rent.
constraint_indexing:constraint_classification(technological_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT PROVIDER (ROPE) — The dominant platform provider experiences the constraint as pure coordination: network effects solve the chicken-and-egg problem of ecosystem adoption. Users adopt because others use it; suppliers integrate because users are there. This perspective sees lock-in as a successful coordination mechanism that justifies their privileged position. Effective exit (to another platform) is costless for the provider.
constraint_indexing:constraint_classification(technological_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RIVAL COMPETITOR (TANGLED ROPE) — A well-resourced competitor sees the dominant lock-in as both a coordination mechanism that establishes market legitimacy AND a barrier to entry that must be overcome through coordinated alternative ecosystem investment. They must simultaneously acknowledge the incumbent's coordination function while fighting asymmetric extraction that benefits the incumbent. Mobile exit options but high transition cost for all ecosystem participants.
constraint_indexing:constraint_classification(technological_lock_in, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN STANDARDS COALITION (SCAFFOLD) — Industry consortia, open-source projects, and standards bodies (organized agents) view lock-in as a temporary coordination failure being addressed through interoperability standards, API standardization, and data portability mandates. These agents see a sunset clause: as standards mature and tool ecosystems become interoperable, switching costs decline. Theater is moderate because the coalition's solution (standards development) requires performative consensus-building but produces genuine technical artifacts.
constraint_indexing:constraint_classification(technological_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY INSTITUTIONAL HOLDOUT (PITON) — Some institutional actors (regulatory bodies, legacy enterprises, government agencies) are formally locked into older technology by policy, inertia, or prior investment but continue to maintain symbolic commitment to alternatives they will never actually adopt. The constraint persists through institutional theater despite degraded function — the institution performs evaluation and exploration of alternatives while remaining committed to the lock-in. High theater ratio indicates performative rather than genuine exit possibility.
constraint_indexing:constraint_classification(technological_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some degree of path dependency in technology adoption is inherent to network effects: the coordination benefit of shared standards naturally prevents switching, regardless of institutional design. This perspective risks naturalizing what is actually contingent institutional lock-in as an immutable property of how technologies scale. The natural law framing masks extractive surplus extraction that depends on particular choices about interoperability, data portability, and API openness.
constraint_indexing:constraint_classification(technological_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technological_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(technological_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(technological_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(technological_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(technological_lock_in, TR),
    TR >= 0.70.

:- end_tests(technological_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. Lock-in begins with genuine coordination benefit (network effects solve the platform bootstrap problem), but as adoption reaches critical mass, the incumbent captures surplus value through pricing power, limited feature innovation, and vendor lock-in. The increasing trajectory (0.30→0.58) reflects accumulation of switching costs over the technology's lifecycle. Extraction is not maximal (0.58 not 0.75+) because the incumbent still must provide competitive baseline features and face periodic threat from rival ecosystems, particularly for new deployment decisions. Suppression (0.65): Moderate-high. Multiple barriers prevent switching: technical incompatibilities, data lock-in, ecosystem dependencies, workflow familiarity, organizational inertia, switching cost uncertainty, and network effects. However, suppression is not total because some users do successfully migrate (to open-source alternatives, new platforms, or interoperable competitors), and regulatory interventions (interoperability mandates, data portability laws) are lowering barriers. Theater ratio (0.55): Moderate. The constraint includes both genuine coordination functions (ecosystem maturity providing real benefits) and performative elements (institutional actors exploring alternatives they never adopt, standards committees performing consensus while incumbents maintain de facto control). Theater has increased over time as path dependency solidifies and exit becomes more performative (institutions study alternatives) than real (actual migration).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence from a single set of structural parameters. The incumbent provider sees pure coordination (Rope) — network effects are solving a genuine collective action problem. Organizational adopters see mixed benefits and extraction (Tangled Rope) — they receive coordination benefits but pay extraction tax. Locked-in individual users see pure extraction with no exit (Snare) — switching costs are insurmountable relative to their resources. Organized coalitions see a temporary problem with sunset (Scaffold) — interoperability standards and open-source alternatives are building exit paths. Legacy institutions see degraded ritual (Piton) — they continue using the technology due to inertia while symbolically evaluating alternatives. The civilizational observer risks seeing natural law (false Mountain) — but the structural data reveals this as a naturalization of contingent choices about API design, data formats, and business models.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from each agent's structural relationship to the lock-in mechanism. Trapped users with no arbitrage options derive d ≈ 0.95 (maximum target status) producing high f(d) ≈ 1.42 and high chi. Institutional beneficiaries with arbitrage options (the incumbent) derive d ≈ 0.05 (full beneficiary status) producing low f(d) ≈ -0.12 and negative chi (they experience the constraint as subsidy). Organizational adopters with constrained exit options derive d ≈ 0.60 (moderate target) producing moderate f(d) ≈ 0.75. The scaffold coalition with organized status and constrained but visible exit options derives d ≈ 0.40, reflecting that they can drive change but face incumbent resistance. These directionality values drive the perspectival gap: the same ε value (0.58) produces radically different χ values depending on d, explaining why the beneficiary perceives rope while victims perceive snare.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy in technological lock-in is resolved by recognizing that the constraint genuinely combines coordination and extraction functions. The incumbent provider is solving a real bootstrap problem (network effects are necessary for platform viability); simultaneously, the incumbent is extracting surplus through switching costs and reduced competitive pressure. Both functions are real, not one masking the other. The tangled_rope classification is correct, not a failure of the framework to distinguish pure types. The mandatrophy appears when observers ask 'is this cooperation or coercion?' The answer is: both, structurally. The coordination function (solving network effects bootstrap) is real and valuable. The extraction function (leveraging accumulated network effects to limit competition) is also real and costly. The policy question is not whether to eliminate the lock-in entirely (which would destroy the coordination benefit) but whether to reduce extraction while preserving coordination (interoperability, data portability, API standards, rival ecosystem support). The false mountain perspective (this is inherent to how technology works) naturalizes contingent design choices (closed APIs, proprietary data formats) as immutable laws. Unmasking this false summit clarifies that lock-in's severity is policy-sensitive, not technology-determined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_measurement_ambiguity,
    'Are measured switching costs technical (genuine incompatibilities) or institutional (policy/business model choices that could be changed)?',
    'Counterfactual analysis: comparison of switching costs under different API policies, data portability laws, or interoperability mandates; historical examples of forced migration (regulatory mandate, acquisition)',
    'If mostly technical: lock-in is closer to mountain (inherent property of technology). If mostly institutional: lock-in is extractive snare (policy choice that concentrates extraction). Classification could shift from tangled_rope to snare or to pure rope depending on ratio.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_measurement_ambiguity, empirical, 'Technical vs institutional components of switching costs').

omega_variable(
    network_effects_coordination_vs_extraction,
    'Do network effects primarily generate genuine coordination benefits (lower cost for all users) or primarily concentrate extraction (incumbent monopoly rent)?',
    'Cost-benefit analysis: comparison of user welfare under lock-in vs hypothetical fragmented ecosystem; empirical study of price trajectories and feature innovation after lock-in reaches critical mass',
    'If coordination-dominated: classification remains tangled_rope (genuine benefit + extraction). If extraction-dominated: classification shifts toward snare (minimal coordination, maximum extraction). If benefit marginal: shifts toward pure piton (theater with degraded function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_coordination_vs_extraction, empirical, 'Whether network effects primarily benefit users or concentrate incumbent rents').

omega_variable(
    interoperability_sunset_reality,
    'Is the open standards / interoperability sunset clause (scaffold perspective) structurally achievable or aspirational theater?',
    'Technical feasibility analysis of proposed interoperability standards; timeline projection based on historical standards adoption rates; monitoring of incumbent resistance to API standardization',
    'If achievable: scaffold classification confirmed, theater_ratio justified, lock-in has genuine exit path. If not achievable: scaffold is aspirational theater, and true classification is snare with no exit. Mandatrophy turns on this question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_sunset_reality, empirical, 'Whether interoperability standards sunset is technically and politically feasible').

omega_variable(
    incumbent_provider_intentionality,
    'Is the incumbent''s lock-in mechanism an emergent consequence of coordination incentives or an intentional extraction strategy?',
    'Historical analysis of API design choices, documentation accessibility, migration tool provision; comparison with incumbents that chose high-interoperability strategies; testimony from internal product/platform decisions',
    'Intentionality affects mandatrophy framing but not classification: tangled_rope remains appropriate regardless. However, intentionality affects whether the constraint should be classified as snare (deliberate trap) or rope+extraction byproduct (coordination with side effects). This determines narrative framing and policy response.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incumbent_provider_intentionality, empirical, 'Whether lock-in emerges from coordination or deliberate extraction strategy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technological_lock_in, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(techlock_tr_t0, technological_lock_in, theater_ratio, 0, 0.35).
narrative_ontology:measurement(techlock_tr_t3, technological_lock_in, theater_ratio, 3, 0.45).
narrative_ontology:measurement(techlock_tr_t6, technological_lock_in, theater_ratio, 6, 0.52).
narrative_ontology:measurement(techlock_tr_t9, technological_lock_in, theater_ratio, 9, 0.55).

% Extraction over time
narrative_ontology:measurement(techlock_be_t0, technological_lock_in, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(techlock_be_t3, technological_lock_in, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(techlock_be_t6, technological_lock_in, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(techlock_be_t9, technological_lock_in, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technological_lock_in, global_infrastructure).
narrative_ontology:affects_constraint(technological_lock_in, platform_ecosystem_power_imbalance).
narrative_ontology:affects_constraint(technological_lock_in, interoperability_standards_fragmentation).
narrative_ontology:affects_constraint(technological_lock_in, data_portability_barriers).

% DUAL FORMULATION NOTE:
% Technological lock-in decomposes into at least three distinct constraints with different ε values: (1) network_bootstrap_coordination (ε≈0.15, pure Rope) — the genuine coordination problem lock-in solves; (2) switching_cost_accumulation (ε≈0.58, Tangled Rope) — the extraction mechanism that emerges from matured network effects; (3) incumbent_rent_extraction (ε≈0.75, Snare) — pure extraction when switching costs are weaponized through pricing. Each has different measurement properties and policy interventions. This story models the composite constraint system where all three interact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technological_lock_in, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
