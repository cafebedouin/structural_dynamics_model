% ============================================================================
% CONSTRAINT STORY: temporal_decay_gradient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temporal_decay_gradient, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: temporal_decay_gradient
 *   human_readable: Temporal Decay Gradient in Tsunami Stone Commitment Maintenance
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   The Aneyoshi tsunami stone ('do not build below this line') provides a
 *   rare empirical test of commitment system durability across a 78-year
 *   non-catastrophe interval. Erected after the 1933 Sanriku tsunami, the
 *   stone directive governed settlement patterns through three generations
 *   who had no direct experience of the hazard. The 2011 Tōhoku tsunami
 *   functioned as a natural D5 catastrophe-theorem experiment: Aneyoshi
 *   village (population ~60) survived with zero casualties while neighboring
 *   settlements were destroyed, validating that the behavioral constraint
 *   remained operationally live despite nearly eight decades without
 *   reinforcement. The constraint exhibits a temporal decay gradient —
 *   theater ratio and extractiveness both increased over the interval as the
 *   directive's commemorative function grew relative to its coordination
 *   function, but the 2011 empirical test reversed the trend, demonstrating
 *   that the kernel had not fully decayed to symbol. This case is diagnostic
 *   for commitment system theory: it isolates the variable of
 *   catastrophe-driven validation and tests whether a purely
 *   lineage-transmitted directive (minimal active enforcement, no legal
 *   codification, no institutional enforcement) can survive generational
 *   turnover when the founding problem recurs on a century timescale.
 *
 * KEY AGENTS:
 *   - Aneyoshi Residents (post-1933 generation): Primary beneficiaries (powerless/constrained at individual level, but collectively organized through lineage transmission) — inherited the constraint and adhered to it despite no direct tsunami experience; bore the cost of land-use restriction but gained catastrophe protection
 *   - Regional Development Authority: Secondary beneficiary (moderate/mobile) — could have overridden the directive through zoning but chose coordination over extraction; gained disaster preparedness alignment without enforcement cost
 *   - Commitment Transmission Lineage: Institutional beneficiary (organized/constrained) — village elders, family heads, and community leaders who maintained the oral tradition and pointed new residents to the stone; constrained by obligation to transmit accurately
 *   - Post-2011 Heritage Industry: Secondary actor (institutional/arbitrage) — entered after empirical vindication; benefits from commemorative function but does not operationalize the directive (piton perspective)
 *   - Analytical Observer: Sees the constraint as pure coordination validated by catastrophe-theorem test
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temporal_decay_gradient, 0.18).
domain_priors:suppression_score(temporal_decay_gradient, 0.25).
domain_priors:theater_ratio(temporal_decay_gradient, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temporal_decay_gradient, extractiveness, 0.18).
narrative_ontology:constraint_metric(temporal_decay_gradient, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(temporal_decay_gradient, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temporal_decay_gradient, rope).
narrative_ontology:human_readable(temporal_decay_gradient, "Temporal Decay Gradient in Tsunami Stone Commitment Maintenance").
narrative_ontology:topic_domain(temporal_decay_gradient, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temporal_decay_gradient, 'f07e820d-2729-4a12-9754-56fa95e7f215').
narrative_ontology:cs_kernel_codification('f07e820d-2729-4a12-9754-56fa95e7f215', formalized).
narrative_ontology:cs_authority_grounding('f07e820d-2729-4a12-9754-56fa95e7f215', lineage).
narrative_ontology:cs_interpretation_layer_present('f07e820d-2729-4a12-9754-56fa95e7f215').
narrative_ontology:cs_reference_frame('f07e820d-2729-4a12-9754-56fa95e7f215', post_1933_direct_survivor_transmission).
narrative_ontology:cs_drift_state('f07e820d-2729-4a12-9754-56fa95e7f215', pre_2011_third_generation, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('f07e820d-2729-4a12-9754-56fa95e7f215', '2025-01-09T00:00:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temporal_decay_gradient, aneyoshi_residents).
narrative_ontology:constraint_beneficiary(temporal_decay_gradient, commitment_transmission_lineage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temporal_decay_gradient, post_1933_generation_residents).
narrative_ontology:constraint_beneficiary(temporal_decay_gradient, regional_development_authority).
narrative_ontology:constraint_beneficiary(temporal_decay_gradient, post_2011_heritage_industry).
narrative_ontology:constraint_victim(temporal_decay_gradient, post_1933_generation_residents).
narrative_ontology:constraint_vindicates(temporal_decay_gradient, intergenerational_memory_efficacy).
narrative_ontology:constraint_vindicates(temporal_decay_gradient, physical_kernel_durability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents born after the 1933 tsunami who inherited the stone's directive through family transmission and village elders. They bear the cost of land-use restriction (cannot build in economically optimal coastal locations; steeper terrain increases construction costs and limits agricultural land). But they also benefit from catastrophic risk coordination: the directive aligns individual settlement decisions with collective safety without requiring centralized enforcement. Exit is constrained by family ties, land ownership inheritance, and relocation costs, but not impossible. Their dual role (payer + beneficiary) reflects genuine coordination rather than extraction: the cost they bear is reciprocated by the safety benefit they receive.
narrative_ontology:constraint_stakeholder(temporal_decay_gradient, post_1933_generation_residents, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(temporal_decay_gradient, post_1933_generation_residents, beneficiary).

% Municipal and prefectural planning bodies with zoning authority. They could override the stone's directive through policy but choose not to, treating it as a coordination device that aligns local practice with regional disaster preparedness mandates. They benefit by saving enforcement costs (the stone self-enforces through social transmission) and by achieving public safety goals without active regulation. Exit is mobile: they have institutional capacity to rezone or ignore the directive, but doing so would require justifying why they are overriding a locally maintained safety practice.
narrative_ontology:constraint_stakeholder(temporal_decay_gradient, regional_development_authority, beneficiary,
    moderate, generational, mobile, regional).

% Village elders, family heads, and community leaders who maintain the oral tradition and ensure new residents understand the stone's directive. They set the agenda by pointing to the stone, narrating the 1933 disaster, and transmitting the behavioral rule to the next generation. Constrained exit: their role is a social obligation that binds through family and community responsibility, not a position they can freely abandon. They do not collect material benefits from this role (no rents, no formal authority), but they are load-bearing for the coordination function: without their active transmission, the stone would decay to a monument.
narrative_ontology:constraint_stakeholder(temporal_decay_gradient, commitment_transmission_lineage, agenda_setter,
    organized, generational, constrained, local).

% Tourism operators, heritage NGOs, and UNESCO designation bodies that entered after the 2011 empirical vindication. They benefit from the stone's commemorative function: disaster tourism pilgrimage, international media attention, heritage site designation funding. Arbitrage exit: they freely enter and exit the heritage economy with no binding obligation. They perform disaster memory (guided tours, plaques, annual ceremonies) without operationalizing the stone's directive for their own settlement decisions. The stone is a revenue-generating historical artifact for this group, not a behavioral constraint.
narrative_ontology:constraint_stakeholder(temporal_decay_gradient, post_2011_heritage_industry, beneficiary,
    institutional, generational, arbitrage, national).

% Villages within 5-10 km of Aneyoshi that lacked equivalent physical memory systems and were destroyed in 2011 with significant loss of life. They are excluded in the sense that they did not participate in the Aneyoshi stone's coordination system (different village, different lineage, no shared kernel). Their absence from the coordination arrangement is not by choice but by geographic and institutional boundary: disaster memory was localized to Aneyoshi's oral tradition. Post-2011, their destruction became the counterfactual that validated Aneyoshi's commitment, but they derived no benefit from the stone during the 78-year interval.
narrative_ontology:constraint_stakeholder(temporal_decay_gradient, neighboring_settlements, excluded,
    powerless, biographical, constrained, local).

% Commitment systems theorist or disaster anthropologist studying the stone as an empirical test case. Observes the constraint from outside the coordination system. Neither pays costs (does not face the land-use restriction) nor collects benefits (does not gain safety from the directive). Analytical seat: measures the constraint's structure without participating in its operation.
narrative_ontology:constraint_stakeholder(temporal_decay_gradient, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone directive solves a multi-generational information transmission problem: how to coordinate safe settlement patterns against a catastrophic risk whose return period (50-100 years) exceeds direct experience. Without the stone, each generation would have to relearn tsunami risk through catastrophe. The coordination function is: align individual land-use decisions with collective safety knowledge across generational turnover.
% TRANSFER_FUNCTION: The constraint transfers land-use flexibility and economic opportunity from residents (who forgo optimal coastal locations) to future disaster resilience (stored as higher-ground settlement that pays off only during rare catastrophe events). The transfer is intertemporal rather than between contemporary parties: today's generation bears the restriction cost, and a future generation (possibly several generations away) receives the survival benefit. Minimal material transfer between contemporary agents: no party collects rents from the stone's operation.
% ABSENT_VOICES: Neighboring settlements that lacked equivalent memory systems are absent from this coordination arrangement — not by exclusion from a decision-making process, but by the accident of geographic and institutional boundaries. Disaster memory was hyper-localized to Aneyoshi's lineage; nearby villages had different oral traditions (or none). Post-2011, their destruction validates the stone's function, but during 1933-2011 they had no mechanism to adopt Aneyoshi's kernel without transplanting the entire social transmission structure. The absent voice is: communities at equivalent tsunami risk without physical memory infrastructure.
% DISAPPEARANCE_RATIONALE: If the stone disappeared overnight (physically removed, or its directive forgotten), settlement patterns would rearrange. Without the kernel, residents would face rational economic pressure to build in lower, flatter coastal areas with better access and lower construction costs. The constraint removal would shift land use toward coastal concentration, which is economically optimal in the absence of catastrophe but catastrophically risky when the hazard recurs. The world rearranges because real agent behavior depends on the stone's directive — it is not a natural law that would persist without the artifact.
% FOUNDING_PROBLEM: The founding problem was: the 1933 Sanriku tsunami killed thousands across the region, including significant casualties in the Aneyoshi area. Survivors recognized that coastal settlement made the community structurally vulnerable to recurrence, and that future generations without direct tsunami experience would not internalize this risk. The stone was erected to solve the intergenerational knowledge transmission problem: ensure that descendants 50-100 years later would still coordinate settlement decisions around tsunami risk.
% FOUNDING_PROBLEM_CORROBORATION: The 2011 Tōhoku tsunami empirically corroborated that the founding problem is live: tsunami risk on this coastline recurs on a 50-100 year cycle, and the stone's directive remains operationally necessary. Corroboration comes from (1) geological evidence (tsunami deposit studies confirm recurring events over centuries), (2) the 2011 survival-vs-destruction outcome (Aneyoshi survived with zero casualties while neighboring settlements were destroyed, demonstrating that the stone's coordination function was not obsolete), and (3) disaster preparedness researchers who study the case as a validated model of physical kernel durability. The problem is live because the hazard is live, and no alternative coordination mechanism (modern building codes, hazard maps, seawalls) has yet fully replaced the stone's intergenerational transmission function for this specific community.
narrative_ontology:disappearance_verdict(temporal_decay_gradient, world_rearranges).
narrative_ontology:founding_problem_status(temporal_decay_gradient, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POST-1933 GENERATION RESIDENTS (ROPE) — Residents born after the 1933 tsunami who inherited the constraint through lineage transmission. Constrained exit (relocation is possible but costly; family ties and land ownership bind). Biographical horizon (lives within the commitment's span). The stone directive coordinates safe settlement patterns against a low-probability catastrophic risk. Minimal extraction: the constraint costs land-use flexibility but provides genuine safety coordination. Classification as rope reflects that the coordination function remained live across 78 years of non-catastrophe.
constraint_indexing:constraint_classification(temporal_decay_gradient, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: REGIONAL DEVELOPMENT AUTHORITY (ROPE) — Municipal and prefectural planning bodies that could have overridden the stone's directive through zoning policy but chose not to. Mobile exit (authority to rezone, no binding constraint on their institutional capacity). Generational horizon (planning across multiple administrations). The stone functions as a coordination device that aligns local practice with regional disaster preparedness without requiring active enforcement. Low extraction: respecting the stone saves enforcement costs and aligns with public safety mandates.
constraint_indexing:constraint_classification(temporal_decay_gradient, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: POST-2011 DISASTER MEMORY INSTITUTIONALIZATION (SCAFFOLD) — National and international disaster preparedness organizations that codified the Aneyoshi case as a template for physical memory systems after 2011. Organized agents (NGOs, UNESCO heritage designations, disaster studies institutes) treating the stone as a transitional model: the goal is to systematize and then supersede ad-hoc physical markers with standardized hazard mapping and building codes. Constrained exit (institutional mandates bind but can be revised). Civilizational horizon (designing for multi-century risk cycles). Scaffold logic: the stone's vindication in 2011 justifies temporary reliance on physical kernel methods while modern systems are built out. Estimated sunset: 50-100 years as GPS-based hazard mapping and enforced building codes replace localized physical memory.
constraint_indexing:constraint_classification(temporal_decay_gradient, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TOURISM AND HERITAGE MANAGEMENT (PITON) — Post-2011 heritage designation transformed the stone into a pilgrimage site and disaster tourism destination. Arbitrage exit (industry actors freely enter and exit the heritage economy). The stone's commemorative function now substantially outweighs its behavioral force for this constituency: visitors photograph the stone, tour guides narrate its history, but the industry does not coordinate settlement decisions around the directive. Theater ratio is moderate for this perspective: the heritage designation performs disaster memory without operationalizing it. Piton classification reflects the atrophied function — what remains is primarily performance.
constraint_indexing:constraint_classification(temporal_decay_gradient, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — The constraint is a pure coordination mechanism from the civilizational analytical perspective. The stone solves a genuine collective action problem: how to transmit catastrophic risk knowledge across generations when direct experience is separated by 50-100 year intervals. Zero extraction: no party collects rents from the stone's operation. The 2011 empirical test validates the coordination function: Aneyoshi survived while neighboring settlements without equivalent memory systems were destroyed. The analytical classification matches the residents' lived experience, confirming the constraint as rope rather than naturalized extraction.
constraint_indexing:constraint_classification(temporal_decay_gradient, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temporal_decay_gradient_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(temporal_decay_gradient, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(temporal_decay_gradient, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(temporal_decay_gradient, TR),
    TR >= 0.70.

:- end_tests(temporal_decay_gradient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low but non-zero. The constraint costs land-use flexibility — residents forgo economically optimal coastal locations and accept steeper construction costs on higher ground. But the extraction is genuinely reciprocal: the constraint provides catastrophic risk mitigation with no identifiable beneficiary capturing rents. The modest extractiveness reflects the real opportunity cost of the restriction, not asymmetric rent collection. The value increased over the 78-year interval as economic pressure on coastal land grew (0.10 in 1933 to 0.25 in 2011) but dropped post-catastrophe (0.18 in 2013) as the coordination benefit was empirically validated. Suppression (0.25): Low. The directive has no legal enforcement mechanism and no institutional sanctions for violation. Suppression is entirely social — community disapproval, family obligation, respect for ancestors. The stone itself is the enforcement: physical presence and narrative transmission maintain behavioral force without coercion. Theater ratio (0.15): Low overall but exhibiting temporal gradient. In 1933 (0.05), the stone was purely functional — direct tsunami survivors inscribed their experience. By 2011 (0.22), commemorative rituals (annual memorial services, school field trips, tourist visits) had accumulated around the stone, and some performative maintenance appeared (cleaning, repainting, signage). Post-2011 (0.15), the theater ratio dropped as the stone's functional validation eclipsed its symbolic role. The gradient maps the decay-and-revival cycle: function → symbol → function-restored-by-empirical-test.
 *
 * PERSPECTIVAL GAP:
 *   The residents and the analytical observer both classify the constraint as rope (pure coordination), confirming that the experienced reality matches the structural analysis. The Heritage Industry sees piton (degraded function maintained as performance) because their relationship to the stone is purely commemorative — they narrate the history without operationalizing the directive. The post-2011 institutionalization coalition sees scaffold (temporary reliance on physical kernel methods while modern hazard mapping systems are built). There is no snare perspective because no agent is trapped by the constraint in a way that produces asymmetric extraction — the residents are constrained but benefit, and all other actors have exit. The gap is between those who operationalize the directive (rope) and those who perform it (piton), with the scaffold perspective representing the institutional view that physical kernels are a transitional solution.
 *
 * DIRECTIONALITY LOGIC:
 *   Aneyoshi residents are declared beneficiaries because they gain catastrophic risk coordination. Their d-value is derived from (powerless, constrained, beneficiary) → moderate d → moderate f(d) → low-to-moderate chi. The constraint costs them flexibility but provides genuine safety, and they lack both the power to change it and the exit options to avoid it easily. The Regional Development Authority has (moderate, mobile, beneficiary) → low d → low f(d) → low chi: they could override the stone but experience the constraint as low-cost coordination rather than extraction. The Heritage Industry has (institutional, arbitrage, beneficiary) → very low d → negative f(d) → negative chi: they collect from the stone's commemorative function without bearing its costs. The analytical observer sees zero extraction: no agent captures rents from the stone's operation, and the 2011 test confirmed the coordination function is real. The modest base extractiveness (0.18) reflects opportunity cost distributed across the resident population, not concentrated extraction by a beneficiary class.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that a directive can remain coordination (rope) across a 78-year interval where its founding problem (catastrophic tsunami) does not recur. The decay gradient (rising theater ratio and extractiveness) suggests the commitment was under pressure — commemorative drift was accumulating — but the 2011 empirical test validated that the behavioral constraint had not collapsed. The stone is not a naturalized mountain (no claim of physical necessity), not a snare (no asymmetric extraction), and not purely a piton (the function was operationally live despite performative accretion). It is a rope with a temporal decay gradient: coordination erodes toward symbol over generational time, but empirical vindication restores functional status. The mandatrophy question 'Is this coordination or extraction?' is answered by the catastrophe test: Aneyoshi survived, neighbors did not, and no party captured rents from the outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decay_threshold_uncertainty,
    'At what point in the 78-year non-catastrophe interval (if any) did the stone''s behavioral force decay to primarily commemorative function, and was it revived only retrospectively after 2011?',
    'Longitudinal ethnographic evidence: settlement pattern analysis at decade intervals 1933-2011; oral history interviews about land-use decisions; correlation between proximity to stone and construction dates. If building patterns respected the line continuously, decay was minimal. If violations accumulated pre-2011 and were only corrected post-2011, the commitment had substantially decayed and the 2011 vindication was retrospective rather than continuous.',
    'If decay was minimal: rope classification holds across the interval. If substantial decay occurred: piton or scaffold classification is more accurate for the 1980-2011 period, with rope status restored only after empirical vindication.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_threshold_uncertainty, empirical, 'Whether the stone''s behavioral force decayed during the non-catastrophe interval or remained operationally live').

omega_variable(
    counterfactual_transmission_mechanism,
    'Would an equivalent directive transmitted through oral tradition alone (without physical kernel) have retained behavioral force across 78 years, or is the stone''s durability essential?',
    'Comparative analysis of disaster memory transmission across communities with and without physical markers; controlled comparison of oral-only vs. physical-kernel memory systems in similar risk contexts; experimental studies of multi-generational information retention.',
    'If oral transmission suffices: the stone is incidental (coordination function is carried by social practice, not physical artifact). If physical kernel is essential: the stone''s material durability is load-bearing for the coordination function, and the constraint is tightly coupled to the artifact''s presence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_transmission_mechanism, empirical, 'Whether physical kernel is necessary or oral transmission alone would suffice').

omega_variable(
    replication_boundary_conditions,
    'What are the boundary conditions for replicating this coordination mechanism in other disaster contexts (flood plains, volcanic zones, earthquake faults)?',
    'Systematic study of physical disaster memory systems globally; identification of successful and failed cases; isolation of necessary conditions (community size, hazard return period, governance stability, literacy rates, land tenure systems).',
    'If boundary conditions are narrow (requires specific cultural context, low mobility, stable governance): the Aneyoshi case is a local success not generalizable. If boundary conditions are broad: physical kernel methods are a viable disaster preparedness strategy at scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(replication_boundary_conditions, empirical, 'Boundary conditions for replicating the physical kernel coordination mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temporal_decay_gradient, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tdg_theater_1933, temporal_decay_gradient, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tdg_theater_1953, temporal_decay_gradient, theater_ratio, 20, 0.08).
narrative_ontology:measurement(tdg_theater_1973, temporal_decay_gradient, theater_ratio, 40, 0.12).
narrative_ontology:measurement(tdg_theater_1993, temporal_decay_gradient, theater_ratio, 60, 0.18).
narrative_ontology:measurement(tdg_theater_2011_pre, temporal_decay_gradient, theater_ratio, 78, 0.22).
narrative_ontology:measurement(tdg_theater_2013_post, temporal_decay_gradient, theater_ratio, 80, 0.15).

% Extraction over time
narrative_ontology:measurement(tdg_extract_1933, temporal_decay_gradient, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(tdg_extract_1953, temporal_decay_gradient, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(tdg_extract_1973, temporal_decay_gradient, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(tdg_extract_1993, temporal_decay_gradient, base_extractiveness, 60, 0.2).
narrative_ontology:measurement(tdg_extract_2011_pre, temporal_decay_gradient, base_extractiveness, 78, 0.25).
narrative_ontology:measurement(tdg_extract_2013_post, temporal_decay_gradient, base_extractiveness, 80, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temporal_decay_gradient, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is a single-kernel case. No decomposition is required: the stone's directive has one stable extractiveness value across all observables (settlement location, land use, construction patterns). The decay gradient is temporal variation in a single constraint, not multiple constraints requiring decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
