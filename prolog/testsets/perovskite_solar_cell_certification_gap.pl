% ============================================================================
% CONSTRAINT STORY: perovskite_solar_cell_certification_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_perovskite_solar_cell_certification_gap, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: perovskite_solar_cell_certification_gap
 *   human_readable: Perovskite Solar Cell IEC Certification Gap
 *   domain: renewable_energy/materials_science
 *
 * SUMMARY:
 *   Perovskite solar cells have achieved laboratory efficiencies exceeding
 *   30%, surpassing conventional crystalline silicon (22-23%), yet remain
 *   absent from large-scale grid deployment. The primary structural barrier
 *   is not physics but institutional: certification. IEC 61646 (photovoltaic
 *   modules — safety qualification) and IEC 61730 (photovoltaic safety) were
 *   designed for silicon's failure modes (bulk defects, surface passivation,
 *   mechanical brittleness). These same protocols are applied to perovskites,
 *   whose failure mechanisms are fundamentally different (ion migration,
 *   A-site cation and halide vacancy dynamics, moisture ingress, thermal
 *   degradation under environmental stress). The result is a certification
 *   regime that measures the wrong properties for perovskite reliability.
 *   This constraint exhibits genuine coordination function (standards enable
 *   safety and market confidence) alongside asymmetric extraction: silicon
 *   manufacturers and incumbent certification bodies benefit from the status
 *   quo; perovskite researchers bear compliance costs; and the global
 *   decarbonization timeline bears the opportunity cost of deployment delay.
 *   The theater_ratio (0.68) reflects that much of the standards application
 *   is performative — testing protocols are followed because they are
 *   formally mandated, not because they predict perovskite long-term
 *   reliability. The constraint has a credible sunset: perovskite-specific
 *   standards (IEC TS 63312, ISO emerging standards) are in active
 *   development with target completion 5-7 years. This makes the
 *   classification scaffold-adjacent but the constraints on deployment are
 *   real enough today (extractiveness 0.52, suppression 0.58) that it
 *   functions as tangled_rope from most perspectives.
 *
 * KEY AGENTS:
 *   - Perovskite Research Community: Primary victim (moderate/constrained) — bears high testing costs designed for silicon; expensive expertise required to navigate incumbent certification bodies
 *   - Silicon Manufacturers: Primary beneficiary (institutional/arbitrage) — monopoly on grid-eligible solar cells maintained by incumbent standards; can adapt if market pressure forces change
 *   - Incumbent Certification Bodies (TÜV, UL, Eurofins at institutional level): Secondary beneficiary (institutional/arbitrage) — testing monopoly maintained by silicon-designed protocols; high barrier to entry for alternative certification
 *   - Materials Science Standards Coalition: Organized agents building exit pathway (organized/constrained) — Materials Research Society, NREL, international consortiums, progressive certification bodies drafting perovskite-specific standards with 5-7 year sunset
 *   - Grid Decarbonization Timeline: Primary victim (powerless/trapped) — cannot exit certification requirement; bears cost of 3-5 year deployment delay representing billions of metric tons of deferred CO2 reduction
 *   - IEC Standards Body: Institutional actor maintaining degraded authority (institutional/arbitrage) — lost functional grip on perovskite physics (theater dominates) but maintains formal legitimacy through precedent and incumbent alignment
 *   - Analytical Observer: Risks naturalizing contingent choice as immutable law (analytical/analytical) — the assumption that standards bodies must be conservative and certification always lags innovation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(perovskite_solar_cell_certification_gap, 0.52).
domain_priors:suppression_score(perovskite_solar_cell_certification_gap, 0.58).
domain_priors:theater_ratio(perovskite_solar_cell_certification_gap, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(perovskite_solar_cell_certification_gap, extractiveness, 0.52).
narrative_ontology:constraint_metric(perovskite_solar_cell_certification_gap, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(perovskite_solar_cell_certification_gap, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(perovskite_solar_cell_certification_gap, tangled_rope).
narrative_ontology:human_readable(perovskite_solar_cell_certification_gap, "Perovskite Solar Cell IEC Certification Gap").
narrative_ontology:topic_domain(perovskite_solar_cell_certification_gap, "renewable_energy/materials_science").

domain_priors:requires_active_enforcement(perovskite_solar_cell_certification_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(perovskite_solar_cell_certification_gap, 'c950460c-5ba5-4b5e-a693-9ba9fc9ae056').
narrative_ontology:cs_kernel_codification('c950460c-5ba5-4b5e-a693-9ba9fc9ae056', formalized).
narrative_ontology:cs_authority_grounding('c950460c-5ba5-4b5e-a693-9ba9fc9ae056', extraction).
narrative_ontology:cs_interpretation_layer_present('c950460c-5ba5-4b5e-a693-9ba9fc9ae056').
narrative_ontology:cs_reference_frame('c950460c-5ba5-4b5e-a693-9ba9fc9ae056', silicon_cell_performance_standardization).
narrative_ontology:cs_drift_state('c950460c-5ba5-4b5e-a693-9ba9fc9ae056', perovskite_commercial_readiness, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c950460c-5ba5-4b5e-a693-9ba9fc9ae056', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(perovskite_solar_cell_certification_gap, silicon_manufacturers).
narrative_ontology:constraint_beneficiary(perovskite_solar_cell_certification_gap, incumbent_certification_bodies).
narrative_ontology:constraint_victim(perovskite_solar_cell_certification_gap, perovskite_research_groups).
narrative_ontology:constraint_victim(perovskite_solar_cell_certification_gap, grid_decarbonization_timeline).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GRID DECARBONIZATION TIMELINE (SNARE) — Cannot exit the certification requirement; bears full cost of deployment delay. Global net-zero commitments depend on rapid renewable capacity scaling. The constraint locks perovskite's superior efficiency out of the grid despite technical readiness. Maximum extraction: 3-5 year deployment delay translates to billions of metric tons of avoided CO2 displaced to conventional sources.
constraint_indexing:constraint_classification(perovskite_solar_cell_certification_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PEROVSKITE RESEARCH COMMUNITY (TANGLED ROPE) — Constrained by expensive testing protocols designed for silicon (thermal cycling, humidity stress, mechanical durability) that do not map to perovskite failure modes. Testing costs are prohibitive for small groups. But the community also benefits from existing certification infrastructure (safety protocols, standardized testing language, regulatory legitimacy). Extraction is significant but not maximal — some research groups have arbitrage options (licensing to industrial partners, collaborating with certified labs).
constraint_indexing:constraint_classification(perovskite_solar_cell_certification_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SILICON MANUFACTURERS & CERTIFICATION BODIES (ROPE) — Benefits from the certification regime. Silicon holds monopoly on grid-eligible cells; certification bodies hold monopoly on validation of grid compliance. The constraint is experienced as coordination: testing standards enable quality assurance and market confidence. Net beneficiary — the certification structure channels legitimacy and capital toward incumbents. Exit option is arbitrage: if market pressure forces new standards, incumbents can adapt certification protocols (expensive but feasible).
constraint_indexing:constraint_classification(perovskite_solar_cell_certification_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: MATERIALS SCIENCE STANDARDS COALITION (SCAFFOLD) — Organized actors (Materials Research Society, NREL, international perovskite consortiums, progressive certification bodies like TÜV, Eurofins) are actively building perovskite-specific certification protocols (IEC TS 63312, emerging ISO standards). The coalition sees the silicon-based standards as a temporary bottleneck with a real sunset: perovskite certification standards are in draft form (technical specification stage) and expected to mature within 5-7 years. Low effective extraction because the organized coalition has agency and an exit timeline.
constraint_indexing:constraint_classification(perovskite_solar_cell_certification_gap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: IEC STANDARDS BODY (PITON) — The IEC has lost functional grip on perovskite physics but maintains its formal authority through institutional inertia. The IEC 61646/61730 standards are applied to materials (perovskites) whose failure modes (ion migration, hysteresis, moisture ingress, thermal instability) are structurally different from silicon's (bulk defects, surface passivation). The IEC's testing protocols no longer measure what matters for perovskite reliability — the theater persists (regulatory legitimacy, incumbent manufacturer alignment) while the functional content has atrophied. Theater ratio high, experienced extraction low because the standards are widely recognized as inadequate.
constraint_indexing:constraint_classification(perovskite_solar_cell_certification_gap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the analytical view, the certification bottleneck might appear as an immutable feature of industrial standardization: all disruptive materials face certification lag before market entry; standards bodies are inherently conservative; institutional friction is unavoidable in technology transition. This perspective risks naturalizing what is actually a contingent political choice — the decision to apply silicon standards to perovskites rather than creating new ones. The false summit detection engine will flag this as constructed, not natural law.
constraint_indexing:constraint_classification(perovskite_solar_cell_certification_gap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(perovskite_solar_cell_certification_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(perovskite_solar_cell_certification_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(perovskite_solar_cell_certification_gap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(perovskite_solar_cell_certification_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(perovskite_solar_cell_certification_gap, TR),
    TR >= 0.70.

:- end_tests(perovskite_solar_cell_certification_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, with time-dependent growth. Initial extractiveness (0.35) reflected genuine uncertainty about perovskite reliability — conservative caution was warranted. Over the 10-year interval, perovskite stability has improved substantially (lab data now shows >5000-hour accelerated testing performance, 5+ year field data from pilot installations). Yet the certification requirements have not adapted proportionally, indicating extractive mechanism (maintaining barrier) rather than cautious coordination. The growth trajectory from 0.35 to 0.52 models the point at which caution becomes extraction. Suppression (0.58): Moderate-high and stable. Testing costs are prohibitive for research groups without industrial partnerships (~€50k-200k per certification cycle); regulatory time delays add 2-3 years to deployment pathway; publication bias toward positive results in certified-pathway literature creates epistemic barriers to alternative architectures. This suppression is structural (real barriers) not epistemic (easily overcome with better communication). Theater ratio (0.68): High and growing. The silicon-based testing protocols are increasingly recognized as inadequate for perovskites; yet they persist because they provide legitimacy and incumbent protection. The theater has grown because the contradiction between testing protocol and actual perovskite physics has become obvious, yet formal application continues. This is diagnostic of piton dynamics applied to standards.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates six distinct classifications from identical structural data. Silicon manufacturers experience the regime as rope (coordination enabling quality assurance); perovskite researchers experience it as tangled_rope (coordination function exists but extraction dominates); the standards body itself experiences it as piton (performative theater persisting through inertia); organized standards coalition sees it as scaffold (temporary, with sunset); the grid decarbonization timeline experiences it as snare (trapped, no exit option); the analytical observer risks seeing it as mountain (certification lag is inevitable and natural) — but the structural data reveals this as false summit. The gap between snare and rope is particularly diagnostic: the same constraint appears as maximum extraction to the powerless agent (grid timeline) and as beneficial coordination to the institutional beneficiary (silicon manufacturers). This perspectival inversion is the defining feature of tangled_rope constraints where the coordination function is real but distribution is asymmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from their structural relationship to the certification requirement. Silicon manufacturers: beneficiary status + arbitrage exit = low d (circa 0.15), producing negative effective extractiveness — they experience the constraint as protective, not extractive. Perovskite researchers: victim status + constrained exit = high d (circa 0.65), producing high effective extractiveness — they bear the cost and cannot easily circumvent testing requirements. Grid timeline: victim status + trapped exit = maximum d (circa 0.95), producing maximum effective extractiveness — the timeline cannot negotiate or arbitrage, only delay. Certification bodies: beneficiary status + arbitrage exit = low d, experiencing constraint as coordination. Standards coalition: organized agent with constrained exit (must work within standards process) produces moderate d (circa 0.50), producing moderate experienced extraction offset by collective agency. The analytical observer at universal scope produces d circa 0.72 (typical for analytical observers), experiencing the constraint as intermediate extraction — high enough to see the problem, positioned well enough to analyze without being trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by disambiguating genuine coordination (real function for safety assurance) from extractive distribution (asymmetric benefit to incumbents). The silicon-based protocols DO enable useful coordination: they provide standardized safety testing, allow markets to price quality, prevent unsafe hardware from reaching grids. This genuine coordination function is why the classification is tangled_rope, not pure snare. However, the standards are applied to a material whose failure modes they do not measure. The theater_ratio documents this: testing is performed and results are reported (coordination theater exists), but the testing does not predict actual perovskite reliability. The coordination function could be preserved while lowering extraction by adopting perovskite-specific protocols. The constraint's mandate — 'all solar cells must be tested for grid safety' — is legitimate. Its implementation — 'perovskites must pass silicon tests' — is extractive. The mandatrophy is resolved by separating the legitimate mandate (safety verification required) from its contingent implementation (silicon standards applied). The scaffold classification for the standards coalition documents that this separation is actively happening: perovskite-specific standards are being built to preserve the coordination function while removing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    perovskite_failure_mode_independence,
    'Are perovskite failure mechanisms (ion migration, moisture ingress, thermal instability under load cycling) sufficiently distinct from silicon failure modes that silicon-based testing protocols systematically mismeasure perovskite reliability?',
    'Correlation analysis between IEC 61646/61730 test results and in-situ field degradation data for perovskites; comparison of failure prediction accuracy for silicon vs. perovskite across thermal cycling, humidity, and mechanical stress protocols',
    'If failure modes are truly distinct: IEC standards are category error — testing the wrong property. Certification gap is structural (Snare/Tangled Rope dominate). If modes overlap substantially: gap is merely methodological friction and silicon standards have some validity. Tangled Rope classification holds; Snare classification weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(perovskite_failure_mode_independence, empirical, 'Whether perovskite failures are independent from silicon failure modes').

omega_variable(
    silicon_incumbent_competitive_threat,
    'Is the certification gap maintained by incumbent silicon interests as a deliberate extraction mechanism, or does it reflect genuine conservative caution by standards bodies?',
    'Historical analysis of certification body decision-making; examination of industrial representation on IEC technical committees; timeline of perovskite standard proposals and responses; comparison with historical standards evolution for other emerging PV materials',
    'If deliberately maintained: constraint is pure extraction (Snare classification strengthens, Rope classification for incumbents confirmed). If genuine caution: constraint is primarily coordination friction (Tangled Rope classification strengthens, theater ratio interpretation shifts).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(silicon_incumbent_competitive_threat, empirical, 'Whether certification gap is deliberately maintained as competitive barrier').

omega_variable(
    tandem_perovskite_silicon_accelerator,
    'Will perovskite-silicon tandem cells (which can be tested using modified silicon protocols) accelerate certification and market deployment, or will incumbents apply similar friction to tandem certification?',
    'Timeline tracking of perovskite-silicon tandem certification proposals; analysis of whether modified silicon protocols are accepted for tandems; deployment rate comparison between perovskite-only and tandem architectures once certification pathways clarify',
    'If tandems accelerate: the pure-perovskite bottleneck is real but time-limited (Scaffold sunset strengthens). If incumbents block tandem certification too: the extraction mechanism is more robust than material-specific standards (suggests Snare deepens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tandem_perovskite_silicon_accelerator, empirical, 'Whether perovskite-silicon tandems provide accelerated certification pathway').

omega_variable(
    bespoke_testing_protocol_sufficiency,
    'Can perovskite-specific testing protocols (IEC TS 63312, emerging ISO standards) realistically achieve performance-predictive validity without decades of field data?',
    'Comparison of accelerated testing predictions with 5-10 year field degradation data for first-generation perovskite installations; analysis of whether emerging protocols capture the full failure envelope',
    'If protocols are sufficient: scaffold sunset is credible and perovskite deployment can follow (Scaffold timeline confirmed). If insufficient: certification bodies will require extensive field data before grid approval (Snare extraction extends, Scaffold sunset becomes aspirational).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bespoke_testing_protocol_sufficiency, empirical, 'Whether perovskite-specific protocols can predict long-term reliability').

omega_variable(
    false_summit_institutional_naturalization,
    'Is the certification gap presented as an immutable feature of how standards bodies work, when it is actually a contingent choice to apply silicon protocols to perovskites?',
    'Discourse analysis of standards body communications; examination of whether alternative certification pathways (material-agnostic, efficiency-based, performance-based) were actively considered and rejected vs. never proposed',
    'If choice was contingent: the Mountain perspective is a false summit (naturalization of politics). If alternatives were genuinely infeasible: some element of Mountain classification is warranted. Impact on engine classification: FSM flag and potential reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_institutional_naturalization, conceptual, 'Whether the certification bottleneck is natural or naturalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(perovskite_solar_cell_certification_gap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pero_cert_tr_t0, perovskite_solar_cell_certification_gap, theater_ratio, 0, 0.55).
narrative_ontology:measurement(pero_cert_tr_t5, perovskite_solar_cell_certification_gap, theater_ratio, 5, 0.65).
narrative_ontology:measurement(pero_cert_tr_t10, perovskite_solar_cell_certification_gap, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(pero_cert_be_t0, perovskite_solar_cell_certification_gap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pero_cert_be_t5, perovskite_solar_cell_certification_gap, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(pero_cert_be_t10, perovskite_solar_cell_certification_gap, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(pero_cert_su_t0, perovskite_solar_cell_certification_gap, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(pero_cert_su_t5, perovskite_solar_cell_certification_gap, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(pero_cert_su_t10, perovskite_solar_cell_certification_gap, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(perovskite_solar_cell_certification_gap, resource_allocation).
narrative_ontology:affects_constraint(perovskite_solar_cell_certification_gap, silicon_supply_chain_resilience).
narrative_ontology:affects_constraint(perovskite_solar_cell_certification_gap, perovskite_tandem_cell_manufacturing).
narrative_ontology:affects_constraint(perovskite_solar_cell_certification_gap, grid_scale_pv_financing_barriers).

% DUAL FORMULATION NOTE:
% The certification gap is decomposable into two structurally distinct constraints: (1) the silicon-specific testing protocol regime itself (institutional standardization, ε≈0.45), and (2) the financial and timeline cost structure these protocols impose on perovskite commercialization (economic barrier, ε≈0.52). Both are captured in this single story because they are causally coupled — the testing regime produces financial barriers, and the financial barriers maintain the testing regime (as incumbents profit from extended compliance costs). If perovskite-specific standards replace silicon standards, both constraints degrade simultaneously. Decomposition is not warranted; they are unified by the certification mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(perovskite_solar_cell_certification_gap, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
