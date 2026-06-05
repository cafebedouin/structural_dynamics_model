% ============================================================================
% CONSTRAINT STORY: school_safety_zone_definition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_school_safety_zone_definition, []).

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
 *   constraint_id: school_safety_zone_definition
 *   human_readable: School Safety Zone Definition and Enforcement
 *   domain: policy/enforcement/urban_governance
 *
 * SUMMARY:
 *   School safety zone definitions create a structural constraint operating
 *   across multiple institutional levels: school systems, municipal
 *   governments, property markets, and enforcement agencies. The constraint
 *   coordinates genuine child safety infrastructure (police presence, design
 *   standards, pedestrian protections) while functioning as an enforcement
 *   mechanism that excludes and displaces homeless and low-income populations
 *   from designated areas. The extractiveness has increased over the
 *   measurement interval (0.35 to 0.58) as enforcement scope expanded and
 *   zone definitions became more expansive. Theater ratio has similarly
 *   increased (0.48 to 0.62), indicating that the performative function of
 *   safety zones — signaling that authorities are 'doing something' about
 *   school safety — has grown relative to evidence-based safety coordination.
 *   The constraint exhibits all structural signatures of Tangled Rope:
 *   genuine coordination of school site planning and police resources
 *   alongside asymmetric extraction targeting those unable to comply with or
 *   challenge zone definitions.
 *
 * KEY AGENTS:
 *   - Low-income Residents and Homeless Populations: Primary victims (powerless/trapped) — bear enforcement costs and displacement; cannot exit via relocation or legal challenge
 *   - Community Coalition: Organized moderate agents (moderate/constrained) — benefit from safety infrastructure while experiencing moral/political costs of enforcement targeting vulnerable populations
 *   - Municipal Zoning Authority: Institutional beneficiary (institutional/arbitrage) — coordinates land use planning and enforcement; experiences low extraction cost relative to control benefits
 *   - Property Developers: Powerful institutional actor (powerful/arbitrage) — benefit from development premium in safe-zoned areas; constrained by enforcement expansion that increases compliance costs
 *   - Police/Enforcement Agencies: Institutional actor (institutional/arbitrage) — coordinate with schools and municipal authorities; execute enforcement that extracts from powerless agents
 *   - Harm Reduction Coalition: Organized agents (organized/constrained) — recognize zone enforcement as temporary phase in shift toward housing-first and restorative justice models
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(school_safety_zone_definition, 0.58).
domain_priors:suppression_score(school_safety_zone_definition, 0.65).
domain_priors:theater_ratio(school_safety_zone_definition, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(school_safety_zone_definition, extractiveness, 0.58).
narrative_ontology:constraint_metric(school_safety_zone_definition, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(school_safety_zone_definition, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(school_safety_zone_definition, tangled_rope).
narrative_ontology:human_readable(school_safety_zone_definition, "School Safety Zone Definition and Enforcement").
narrative_ontology:topic_domain(school_safety_zone_definition, "policy/enforcement/urban_governance").

domain_priors:requires_active_enforcement(school_safety_zone_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(school_safety_zone_definition, property_developers).
narrative_ontology:constraint_beneficiary(school_safety_zone_definition, transit_operators).
narrative_ontology:constraint_beneficiary(school_safety_zone_definition, municipal_zoning_authorities).
narrative_ontology:constraint_victim(school_safety_zone_definition, low_income_residents).
narrative_ontology:constraint_victim(school_safety_zone_definition, homeless_populations).
narrative_ontology:constraint_victim(school_safety_zone_definition, small_business_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED RESIDENT (SNARE) — Caught between enforcement that excludes them from established spaces and inability to relocate due to economic constraints. Lacks resources to challenge zoning decisions or find alternative housing within safe walking distance. Zero exit options; maximum extraction and suppression.
constraint_indexing:constraint_classification(school_safety_zone_definition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: COMMUNITY COALITION (TANGLED ROPE) — Organized local groups benefit from genuine child safety coordination infrastructure (police presence, design review) while bearing costs of criminalization and enforcement targeting homeless and low-income populations. Real coordination function exists alongside asymmetric extraction.
constraint_indexing:constraint_classification(school_safety_zone_definition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MUNICIPAL AUTHORITY (ROPE) — Coordinates school site planning, police patrols, and design standards across jurisdictions. Sees the zone definition as a coordination mechanism for legitimate safety goals. Experiences low extraction relative to benefits of standardized enforcement.
constraint_indexing:constraint_classification(school_safety_zone_definition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: PROPERTY DEVELOPER (TANGLED ROPE) — Zone definitions create coordinated land use planning that enables profitable development (school-adjacent residential/commercial projects benefit from safety premium and regulation clarity). Also coordinates with municipal infrastructure. But extraction mechanism: zone enforcement can expand to impose development restrictions that increase costs or reduce profitability. Asymmetric relationship with municipal authority.
constraint_indexing:constraint_classification(school_safety_zone_definition, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — School safety zone regulations, inherited from 1990s-2000s crime prevention orthodoxy, persist through institutional inertia despite contested effectiveness. Enforcement theater (police presence, restricted zones) maintains symbolic function of 'taking safety seriously' even as evidence for zone-based crime reduction grows thin. Theater ratio high; functional coordination modest.
constraint_indexing:constraint_classification(school_safety_zone_definition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: HARM REDUCTION COALITION (SCAFFOLD) — Organized actors (housing advocates, public health agencies, alternative safety models) see school safety zones as a temporary enforcement-based approach that excludes vulnerable populations. Alternatives exist: restorative justice, housing-first models, community-embedded safety. This perspective sees a sunset clause: as evidence accumulates that enforcement-based zones displace rather than deter crime, and housing/mental health interventions show superior outcomes, the zone-based model loses policy legitimacy. Estimated transition: 10-15 years to policy shift away from enforcement-centric zones.
constraint_indexing:constraint_classification(school_safety_zone_definition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(school_safety_zone_definition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(school_safety_zone_definition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(school_safety_zone_definition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(school_safety_zone_definition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(school_safety_zone_definition, TR),
    TR >= 0.70.

:- end_tests(school_safety_zone_definition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts through displacement (powerless agents excluded from public spaces) and through enforcement (disproportionate policing and citations targeting homeless and low-income populations). Early in the interval (t=0), zones were narrower and more closely tied to school safety data, producing lower extractiveness (0.35). As zones expanded and enforcement intensified, extractiveness rose to 0.58 by t=10. This trajectory indicates that the constraint is accumulating extraction over time — zones grow larger and enforcement becomes more expansive, while actual safety gains plateau or become unverifiable. Suppression (0.65): High. Structural barriers to resistance include: municipal authority monopoly on land-use definitions, legal barriers to challenging enforcement (homeless cannot afford legal representation), no alternative spaces to retreat to (economic constraints), and police power to enforce. Theater ratio (0.62): Moderate-high. School zones are highly visible — signs, police presence, design features — creating strong performative signal that authorities are protecting children. But evidence that zones actually reduce crime vs displace it remains contested. The theater function (reassuring parents, signaling state capacity) may exceed the actual safety function.
 *
 * PERSPECTIVAL GAP:
 *   The municipal authority and developers perceive the constraint as coordination (Rope/Tangled Rope) — legitimate land-use planning with clear rules and mutual benefits. Enforcement agencies perceive coordination plus necessary order-maintenance (Rope). Community coalitions perceive mixed benefits and costs (Tangled Rope) — genuine safety infrastructure exists but enforced against vulnerable populations. Displaced residents perceive pure extraction and suppression (Snare) — they bear costs of enforcement with no exit option. The harm reduction coalition perceives a temporary phase with a visible sunset (Scaffold) — alternative models (housing-first, restorative justice) are building policy traction and will eventually replace enforcement-based zones. The regulatory framework itself (viewed civilizationally across long time) is Piton — an inherited enforcement model that persists through institutional inertia and performative safety theater, even as evidence questions its effectiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by the agent's structural position relative to the constraint. Low-income residents and homeless populations have d ≈ 0.95 (full targets of extraction) — no exit options (trapped), no beneficiary status, enforced against directly. Municipal authority has d ≈ 0.10 (beneficiary with arbitrage) — controls definitions, coordinates enforcement, experiences low extraction cost. Developers have d ≈ 0.35-0.40 (mixed — benefit from zone-adjacent development premium but constrained by enforcement expansion). Community coalition has d ≈ 0.55 (moderate — mixed benefits and moral costs). Organized harm reduction actors have d ≈ 0.65 (high but with exit path — organized actors see sunset clause). The rising extractiveness over time (0.35→0.58) reflects that d values shift as zones expand: residents become more trapped, municipal authority becomes more powerful relative to them, developer constraints increase. The beneficiary/victim declarations map to real structural extraction flow: developers and authorities benefit; low-income populations and homeless groups are victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that school safety zone definitions coordinate genuine child safety infrastructure (design standards, police coordination, pedestrian safety measures) while simultaneously functioning as an extraction and displacement mechanism. The constraint is NOT 'purely extractive' (which would justify dismissing all safety concerns) NOR 'purely coordinative' (which would ignore enforcement asymmetries). The Tangled Rope classification captures both functions: active enforcement keeps the zones functional as coordination mechanisms; the same enforcement extracts from powerless agents who cannot comply or exit. The rising theater ratio (0.48→0.62) indicates that over time, the performative function (appearing to be 'tough on crime,' reassuring parents) has grown relative to evidence-based safety gains — this is classic Goodhart drift. The scaffold perspective introduces the temporal resolution: as alternative safety models (housing-first, restorative justice, evidence-based community policing) accumulate evidence and policy traction, the enforcement-based zone model will sunset. The transition is already visible in forward-looking jurisdictions (some cities piloting housing-first school-zone approaches). The constraint's current classification as Tangled Rope with rising theater is therefore temporally accurate: it is Tangled Rope in the enforcement-based era, and it is transitioning toward Scaffold as harm-reduction alternatives demonstrate superior outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zone_effectiveness_ambiguity,
    'Do school safety zones actually reduce crime and injury around schools, or do they displace activity to adjacent areas?',
    'Longitudinal crime data analysis comparing zones with and without enforcement; spatial analysis of crime displacement patterns; comparison of injury rates in enforced vs non-enforced zones controlling for socioeconomic factors',
    'If genuinely effective: zone definition is justified coordination mechanism (Rope becomes more prominent). If displacing: zones are largely extractive enforcement with theater cover (Snare becomes more prominent; Piton classification confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(zone_effectiveness_ambiguity, empirical, 'Whether safety zones reduce crime or displace activity').

omega_variable(
    definition_discretion_capture,
    'Are zone definitions set primarily by child safety data and school input, or do developer/municipal interests drive expansion and scope?',
    'Analysis of zone definition decision-making: compare zones justified by incident data vs zones that align with development incentives; audit frequency of zone expansion relative to safety incidents; interview school principals vs municipal planners on definition rationale',
    'If safety-driven: coordination function dominates (Rope becomes more accurate). If interest-driven: extractive function dominates (Snare and Tangled Rope confirmed); developer perspective changes from Tangled Rope to beneficiary classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_discretion_capture, empirical, 'Whether zone definitions are driven by safety data or development interests').

omega_variable(
    enforcement_target_disparity,
    'Is enforcement applied equally across socioeconomic contexts, or does it disproportionately target homeless and low-income populations?',
    'Enforcement audit: arrest/citation rates by enforcement action (loitering, vagrancy) within zones vs outside; demographic analysis of enforcement recipients; comparison of enforcement intensity in affluent vs low-income school zones',
    'If equal enforcement: suppression is structural but not asymmetrically applied (Tangled Rope confirmed). If disparate: enforcement is extractive mechanism targeting powerless agents (Snare classification confirmed; victim designation validated).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_target_disparity, empirical, 'Whether enforcement targets disproportionately affect homeless and low-income populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(school_safety_zone_definition, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ssz_tr_t0, school_safety_zone_definition, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ssz_tr_t5, school_safety_zone_definition, theater_ratio, 5, 0.58).
narrative_ontology:measurement(ssz_tr_t10, school_safety_zone_definition, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(ssz_be_t0, school_safety_zone_definition, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ssz_be_t5, school_safety_zone_definition, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ssz_be_t10, school_safety_zone_definition, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(school_safety_zone_definition, resource_allocation).
narrative_ontology:affects_constraint(school_safety_zone_definition, street_level_discretion_enforcement).
narrative_ontology:affects_constraint(school_safety_zone_definition, spatial_exclusion_mechanisms).
narrative_ontology:affects_constraint(school_safety_zone_definition, development_property_value_premium).

% DUAL FORMULATION NOTE:
% School safety zone definition has two structurally distinct constraint components: (1) school site planning coordination (genuine multi-stakeholder safety coordination), (2) enforcement-based displacement (extraction targeting homeless/low-income populations). The unified constraint story treats these as interdependent mechanisms. Alternative decomposition would create separate stories: safety_zone_planning_coordination (ε≈0.15, Rope) and enforcement_displacement_mechanism (ε≈0.72, Snare), linked as upstream/downstream. The current single-story approach treats them as one Tangled Rope reflecting institutional reality: municipal authorities use genuine safety coordination as justification for enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(school_safety_zone_definition, powerful, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
