% ============================================================================
% CONSTRAINT STORY: ground_based_astronomy_interference
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ground_based_astronomy_interference, []).

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
 *   constraint_id: ground_based_astronomy_interference
 *   human_readable: Ground-Based Astronomy Interference: RFI, Light Pollution, and Atmospheric Degradation
 *   domain: astronomy/environmental/regulatory
 *
 * SUMMARY:
 *   Ground-based astronomy faces cumulative interference from three sources:
 *   radio frequency interference (RFI) from telecommunications and power
 *   systems, light pollution from urbanization and satellite
 *   mega-constellations, and atmospheric degradation from emissions. The
 *   constraint exhibits structural tension between benefits to commercial
 *   actors (telecommunications carriers, satellite operators, urban
 *   developers) and costs to the astronomical community and the scientific
 *   commons. This is a canonical Tangled Rope scenario: genuine coordination
 *   functions exist (ITU spectrum coordination, dark sky preservation
 *   initiatives) alongside asymmetric extraction (spectrum allocated to
 *   telecommunications, light pollution externalized onto astronomy). The
 *   extractiveness has grown over 50 years from 0.28 to 0.58 as spectrum
 *   demands increased and satellite mega-constellations deployed. The theater
 *   ratio shows governance becoming increasingly performative — regulatory
 *   mechanisms (ITU allocations, dark sky ordinances) persist but lack
 *   enforcement teeth as economic pressures mount.
 *
 * KEY AGENTS:
 *   - Radio Astronomy Community: Distributed epistemic commons (powerless/trapped) — cannot exit expanding RFI without abandoning capability; bears full cost of spectrum encroachment
 *   - Optical Astronomy Community: Distributed scientific user group (powerless/trapped) — cannot exit expanding light pollution without relocating to remote sites or space platforms; bears cost of satellite mega-constellations
 *   - Telecommunications Industry: Institutional beneficiary (institutional/arbitrage) — benefits from broad spectrum access; has exit options through frequency shifting, power reduction, interference mitigation
 *   - Satellite Mega-Constellation Operators: Institutional beneficiary (organized/constrained) — benefits from launch capabilities; constrained by deployment decisions and investment scale but not trapped
 *   - National Radio Quiet Zones (e.g., Green Bank WV, Arecibo PR): Organized institutional actors (organized/constrained) — coordinate within protected zones but experience spectrum creep from adjacent areas and mobile service expansion
 *   - International Regulatory Bodies (ITU, IUCN): Governance institutions (institutional/arbitrage) — coordinate spectrum allocation and dark sky preservation; have enforcement limitations
 *   - Urban Development Interests: Distributed beneficiaries (powerful/mobile) — benefit from light fixtures and telecommunications infrastructure; mobile in that they can shift development patterns if required
 *   - Future Generations: Abstract victim collective (powerless/trapped) — inherit reduced dark sky and narrower electromagnetic observation windows
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ground_based_astronomy_interference, 0.58).
domain_priors:suppression_score(ground_based_astronomy_interference, 0.65).
domain_priors:theater_ratio(ground_based_astronomy_interference, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ground_based_astronomy_interference, extractiveness, 0.58).
narrative_ontology:constraint_metric(ground_based_astronomy_interference, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ground_based_astronomy_interference, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ground_based_astronomy_interference, tangled_rope).
narrative_ontology:human_readable(ground_based_astronomy_interference, "Ground-Based Astronomy Interference: RFI, Light Pollution, and Atmospheric Degradation").
narrative_ontology:topic_domain(ground_based_astronomy_interference, "astronomy/environmental/regulatory").

domain_priors:requires_active_enforcement(ground_based_astronomy_interference).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ground_based_astronomy_interference, telecommunications_industry).
narrative_ontology:constraint_beneficiary(ground_based_astronomy_interference, commercial_satellite_operators).
narrative_ontology:constraint_beneficiary(ground_based_astronomy_interference, urban_development_interests).
narrative_ontology:constraint_beneficiary(ground_based_astronomy_interference, rural_power_grid_operators).
narrative_ontology:constraint_victim(ground_based_astronomy_interference, radio_astronomy_community).
narrative_ontology:constraint_victim(ground_based_astronomy_interference, optical_astronomy_community).
narrative_ontology:constraint_victim(ground_based_astronomy_interference, dark_sky_preservation).
narrative_ontology:constraint_victim(ground_based_astronomy_interference, future_generations_scientific_capability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RADIO ASTRONOMY EPISTEMIC COMMONS (SNARE) — The radio astronomy community occupies increasingly narrow electromagnetic windows and cannot exit the interference regime without abandoning observational capability entirely. RFI suppression by telecommunications expansion creates a zero-sum extraction: the spectrum gained by wireless carriers is spectrum lost to radio astronomers. Trapped exit option with powerless agent status produces maximum experienced extraction.
constraint_indexing:constraint_classification(ground_based_astronomy_interference, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DARK SKY AS SHARED RESOURCE (SNARE) — Light pollution from urbanization and satellite mega-constellations reduces observable universe for optical astronomers and the public. The resource is consumed by urban sprawl and commercial interests with no compensation mechanism. Future generations inherit reduced scientific capacity. No exit available at the collective level.
constraint_indexing:constraint_classification(ground_based_astronomy_interference, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: RADIO ASTRONOMY RESEARCH INSTITUTION (TANGLED ROPE) — Individual observatories benefit from coordination through radio quiet zones and RFI mitigation protocols (genuine coordination function). Simultaneously, they experience extraction through regulatory constraints on facility placement and ongoing spectrum encroachment. Exit costs are high (relocation, international collaboration requirements) but not infinite — institutions can migrate to remote sites or partner across borders. Constrained exit with moderate power produces mixed experience.
constraint_indexing:constraint_classification(ground_based_astronomy_interference, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TELECOMMUNICATIONS INDUSTRY (ROPE) — Experiences the constraint as coordination: standardization of frequency allocations, power limits, and geographic safeguards enables simultaneous operation of wireless networks and radio observatories in some configurations. Arbitrage exit available — the industry can relocate operations, use alternative frequencies, implement interference mitigation technology, or negotiate spectrum licenses. Net beneficiary from the constraint structure.
constraint_indexing:constraint_classification(ground_based_astronomy_interference, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SATELLITE MEGA-CONSTELLATION OPERATORS (TANGLED ROPE) — Genuine coordination function: international treaties and frequency coordination protocols enable coexistence of satellite networks and ground observatories. Simultaneously, constellation deployment extracts from optical astronomers through light pollution. Organized agents with exit options (deployment orbit adjustment, laser link alternatives, international coordination mechanisms) but constrained by investment scale and regulatory requirements. Mixed coordination-extraction experience.
constraint_indexing:constraint_classification(ground_based_astronomy_interference, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL REGULATORY GOVERNANCE (SCAFFOLD) — ITU Radio Regulations and IUCN Dark Sky preservation initiatives function as temporary coordination scaffolding with explicit sunset mechanisms. Radio quiet zones have designated protection windows. Dark sky sanctuaries are designated with resource limitations. These mechanisms have sunset clauses: they require active renewal and are vulnerable to rollback as economic pressures mount. Low effective extraction because organized regulatory actors see explicit exit paths through renegotiation and sunset clauses.
constraint_indexing:constraint_classification(ground_based_astronomy_interference, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: PLANETARY ELECTROMAGNETIC ENVIRONMENT (PITON) — From a civilizational timescale, the electromagnetic spectrum is a fundamental physical resource. Its allocation was historically treated as a coordination problem (international frequency allocations through ITU). Over decades, allocation has become increasingly performative: spectrum is allocated to telecommunications on paper while ground-based astronomy occupies narrower and narrower windows. The coordination mechanism persists through institutional inertia despite degraded function. Theater ratio for spectrum allocation governance is high — regulatory processes produce allocation documents that lack enforcement mechanisms against interference creep.
constraint_indexing:constraint_classification(ground_based_astronomy_interference, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the analytical/civilizational perspective, the constraint appears to reflect a natural law: electromagnetic spectrum is finite, and competing uses create unavoidable tradeoffs. Radio frequencies allocated to telecommunications cannot be used for radio astronomy; photons from satellites degrade optical observations. This perspective treats the interference as an immutable physical limit rather than a contingent institutional choice. The analytical observer risks false summit classification — naturalizing what is actually a policy choice (spectrum allocation priority, satellite deployment approval, urbanization patterns) as inherent physical constraint.
constraint_indexing:constraint_classification(ground_based_astronomy_interference, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ground_based_astronomy_interference_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ground_based_astronomy_interference, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ground_based_astronomy_interference, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ground_based_astronomy_interference, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ground_based_astronomy_interference, TR),
    TR >= 0.70.

:- end_tests(ground_based_astronomy_interference_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High and rising. Telecommunications carriers and satellite operators extract value through spectrum allocation priority and deployment freedom. The extraction is asymmetric: the cost of interference (degraded observation capability, lost science) cannot be monetized by astronomy, while the beneficiaries (telecom, satellite ops) extract clear commercial value. Over 50 years, spectrum encroachment has increased from 0.28 to 0.58 as mobile networks expanded and satellite mega-constellations deployed. Suppression (0.65): High. Multiple barriers prevent radio astronomers from exiting: equipment is place-based (can't relocate telescopes without massive cost), international collaboration has coordination costs, spectroscopic observations at specific frequencies cannot be substituted. RFI and light pollution suppress alternative arrangements. Theater ratio (0.48): Moderate. ITU spectrum allocation processes produce detailed regulatory documents and enforcement mechanisms, but enforcement erodes over time as economic pressure mounts. Dark sky preservation initiatives are explicit and functional, but face constant rollback pressure. The constraint is not purely performative (real coordination mechanisms exist) but theatricality is increasing as formal mechanisms lose enforcement capacity.
 *
 * PERSPECTIVAL GAP:
 *   The powerless/trapped perspective (radio astronomy as epistemic commons) classifies as Snare with maximum experienced extraction. The institutional/arbitrage perspective (telecommunications industry) classifies as Rope, experiencing the same constraint as a coordination mechanism. This gap reflects real structural difference: the beneficiary with exit options experiences the constraint as enabling (rope), while the trapped victim experiences it as extractive (snare). The organized/constrained perspective (satellite operators) produces Tangled Rope — they coordinate through international protocols while externalizing light pollution costs. The regulatory perspective produces Scaffold — explicit sunset clauses and renegotiation mechanisms. The analytical/civilizational perspective risks producing Mountain — naturalizing allocation priority as physical law rather than policy. The perspectival range from Snare to Mountain on the same structural data reveals that the constraint's 'true nature' depends on observer position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation flows from beneficiary/victim declarations and exit options. Telecommunications industry agents are beneficiaries with arbitrage exits — they derive low directionality (d ≈ 0.15) from the derivation chain, experiencing negative effective extraction. Radio astronomers are victims with trapped exits — they derive high directionality (d ≈ 0.95) from the derivation chain, experiencing maximum effective extraction. Satellite operators are mixed beneficiaries (light pollution externality) and agents with constrained exits — their directionality sits in the middle range (d ≈ 0.55-0.65). Regulatory bodies are institutional beneficiaries nominally, but their enforcement capacity has eroded — overridden directionality would reflect captured regulatory status (d ≈ 0.40 rather than canonical institutional 0.00). The gap between beneficiary and victim directionality is the core of the extraction mechanism: spectrum/light are allocated to high-exit-option actors; costs are borne by trapped actors.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR OF COORDINATION VS EXTRACTION HYBRID: The constraint resolves mandatrophy by showing that ITU spectrum coordination is genuine (real coexistence mechanisms exist) while simultaneously being asymmetric (benefits concentrated on beneficiaries with exit options, costs concentrated on trapped agents). This is the defining signature of Tangled Rope: the coordination function is real enough that dismantling it would worsen outcomes for all parties, but the distribution of benefits is highly asymmetric. Neither pure coordination (Rope) nor pure extraction (Snare) capture the structure. The measurement trajectory (extractiveness rising from 0.28 to 0.58) shows how Tangled Rope can drift toward Snare over time as enforcement erodes and extraction accumulates. The theater ratio (0.48, moderate) indicates that regulatory mechanisms retain some functional content — this is not yet a Piton where theater dominates function. The mandatrophy is resolved by recognizing that the constraint is structurally a hybrid: coordination enables coexistence that neither side could achieve unilaterally, but the coordination terms are dictated by the more powerful actor (telecommunications industry), producing asymmetric extraction alongside genuine coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spectrum_allocation_priority_reversibility,
    'Is spectrum allocation priority (telecommunications favored over radio astronomy) a technical necessity or a reversible policy choice?',
    'Historical analysis of spectrum allocation decisions; comparison of technical requirements vs political/economic influence in allocation proceedings; modeling of coexistence scenarios with different allocation priorities',
    'If necessity: constraint approaches Mountain. If policy choice: constraint is Tangled Rope or Snare depending on enforcement and victim exit options.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spectrum_allocation_priority_reversibility, preference, 'Whether spectrum allocation priority is technically necessary or policy-determined').

omega_variable(
    satellite_optical_mitigation_feasibility,
    'Can optical satellite mega-constellations be deployed with light-suppression technology sufficient to preserve dark sky for ground-based optical astronomy?',
    'Engineering analysis of reflectance reduction technologies; constellation deployment trials with coated vs uncoated satellites; ground-based photometric measurements of deployed constellations',
    'If feasible at reasonable cost: satellite operators have arbitrage exit option, tangled rope classification strengthened. If infeasible: snare classification strengthened — light pollution is structural extraction with no mitigation path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(satellite_optical_mitigation_feasibility, empirical, 'Whether satellite light pollution can be mitigated with coatings').

omega_variable(
    radio_quiet_zone_enforcement_sustainability,
    'Can radio quiet zones maintain protected status as economic pressures increase and spectrum demand grows?',
    'Longitudinal analysis of FCC and ITU enforcement actions; tracking of spectrum encroachment over time in established quiet zones; modeling of future spectrum demand vs protected bandwidth',
    'If enforceable long-term: scaffold sunset mechanism is real. If enforcement erodes: scaffold degradation into piton, then snare as quiet zones become nominal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(radio_quiet_zone_enforcement_sustainability, empirical, 'Long-term sustainability of radio quiet zone protection').

omega_variable(
    space_based_astronomy_substitutability,
    'Can space-based observatories (JWST-type, radio interferometers in lunar orbit) fully substitute for ground-based capability, or do ground-based and space-based astronomy occupy irreducibly different scientific niches?',
    'Analysis of scientific capability gaps; comparison of observation costs (space vs ground); assessment of complementarity in observing programs across ground and space platforms',
    'If substitutable: radio astronomy community has arbitrage exit option (move to space), reducing experienced extraction. If complementary: ground-based capability is irreplaceable, snare classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(space_based_astronomy_substitutability, conceptual, 'Whether space-based astronomy can substitute for ground-based capability').

omega_variable(
    urbanization_momentum_reversibility,
    'Is light pollution from urbanization reversible through policy intervention (dark sky ordinances, lighting standards), or does it represent cumulative path-dependent infrastructure investment?',
    'Case studies of dark sky ordinance implementation and effectiveness; analysis of light pollution trends in jurisdictions with vs without intervention; modeling of infrastructure replacement cycles',
    'If reversible: dark sky constraint has policy levers and sunset mechanism. If path-dependent: light pollution extraction is structural and cumulative, snare classification strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(urbanization_momentum_reversibility, empirical, 'Reversibility of light pollution through policy intervention').

omega_variable(
    epistemic_value_commensurability,
    'How do we weigh the epistemic value of ground-based astronomical capability against the economic value of telecommunications and commercial satellite services?',
    'Framework analysis of epistemic vs economic value; case studies of allocation decisions that reveal priority structures; stakeholder preference elicitation',
    'If astronomy valued highly: current allocation represents pure extraction (snare). If valued equally: constraint represents genuine coordination problem (rope). If valued lower: current allocation is efficient (no extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_value_commensurability, preference, 'Commensurability of epistemic value of astronomy with commercial value').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ground_based_astronomy_interference, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gba_tr_t0, ground_based_astronomy_interference, theater_ratio, 0, 0.32).
narrative_ontology:measurement(gba_tr_t15, ground_based_astronomy_interference, theater_ratio, 15, 0.4).
narrative_ontology:measurement(gba_tr_t30, ground_based_astronomy_interference, theater_ratio, 30, 0.48).
narrative_ontology:measurement(gba_tr_t45, ground_based_astronomy_interference, theater_ratio, 45, 0.55).

% Extraction over time
narrative_ontology:measurement(gba_be_t0, ground_based_astronomy_interference, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gba_be_t15, ground_based_astronomy_interference, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(gba_be_t30, ground_based_astronomy_interference, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(gba_be_t45, ground_based_astronomy_interference, base_extractiveness, 45, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ground_based_astronomy_interference, resource_allocation).
narrative_ontology:affects_constraint(ground_based_astronomy_interference, satellite_mega_constellation_deployment).
narrative_ontology:affects_constraint(ground_based_astronomy_interference, electromagnetic_spectrum_allocation).
narrative_ontology:affects_constraint(ground_based_astronomy_interference, light_pollution_urban_development).

% DUAL FORMULATION NOTE:
% Ground-based astronomy interference decomposes into three structurally distinct constraints: RFI from telecommunications (frequency-specific, high suppression, high extractiveness), light pollution from satellites (spatially distributed, medium suppression, rising extractiveness), and atmospheric degradation from emissions (chemical/physical, low extractiveness but affects all observations). Each has distinct ε values and should be tracked separately. This story captures the unified phenomenon; downstream stories track specific interference mechanisms. Network relationships show this constraint upstream of specific observatory capability loss constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ground_based_astronomy_interference, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
