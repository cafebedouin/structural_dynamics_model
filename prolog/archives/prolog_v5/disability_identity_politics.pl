% ============================================================================
% CONSTRAINT STORY: disability_identity_politics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_disability_identity_politics, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: disability_identity_politics
 *   human_readable: Disability Identity Politics Constraint
 *   domain: social/political/identity
 *
 * SUMMARY:
 *   Disability identity politics refers to the constraint through which
 *   disabled people's access to material resources (income support, workplace
 *   accommodations, healthcare, legal protections) becomes contingent on
 *   adopting and performing a unified disability identity aligned with
 *   dominant disability movement narratives. This constraint generates a
 *   fundamental tension: the disability movement coordinates genuine
 *   collective action (legal victories, accessibility standards, community
 *   solidarity) while simultaneously extracting compliance with political
 *   identity orthodoxy, suppressing internal disagreement, and gatekeeping
 *   authentic voice through professional intermediaries. The constraint
 *   exhibits all six DR types from different perspectives. To a homeless
 *   disabled person dependent on SSI/SSDI, it is a snare — identity politics
 *   gatekeeps material survival. To an employed disabled person, it is
 *   tangled rope — genuine coordination benefits (workplace protections,
 *   community) alongside extraction (pressure to perform approved
 *   consciousness). To a disabled person whose identity is constituted
 *   through disability activism, it is identity-locked tangled rope —
 *   structurally mobile but cognitively bound by identity fusion. To
 *   disability advocacy organizations, it is rope — pure coordination
 *   function. To disability bureaucracy, it is piton — performative medical
 *   gatekeeping persisting through inertia. To organized disabled workers
 *   building class analysis, it is tangled rope with labor coordination
 *   function. To emerging disability justice movements, it is scaffold with
 *   sunset logic — intersectionality is decomposing identity politics into
 *   multiple liberation frameworks. The analytical observer risks seeing
 *   immutable identity law (mountain) while the structural data reveals
 *   contingent institutional arrangements. The constraint's theater ratio
 *   (0.68) reflects that significant disability activism involves
 *   performative identity validation rituals (testimony,
 *   consciousness-raising, approved narrative deployment) rather than
 *   material or policy change. The extractiveness value (0.58) reflects
 *   moderate but significant extraction: material gatekeeping is real but not
 *   total; voice suppression is institutional but not absolute; coordination
 *   benefits are genuine even as they come at identity conformity cost.
 *
 * KEY AGENTS:
 *   - Economically Dependent Disabled Person: Primary victim (powerless/trapped) — income support contingent on disability identity, no exit without survival risk
 *   - Working Disabled Person: Secondary victim (moderate/constrained) — workplace accommodations require movement legitimacy, discrimination risk if dissenting
 *   - Disabled Person Identity-Locked in Movement: Victim through identity fusion (moderate/identity_locked) — structurally mobile but cognitively bound; cannot exercise exit options
 *   - Disability Advocacy Organization: Primary beneficiary (institutional/arbitrage) — gatekeeps authentic voice, controls funding, manages movement messaging
 *   - Disability Services Bureaucracy: Secondary institutional actor (institutional/constrained) — maintains medical model gatekeeping through inertia; cannot exit without losing administrative role
 *   - Organized Disabled Workers Coalition: Mixed agent (organized/constrained) — benefits from coordination but bears extraction costs from movement gatekeeping
 *   - Disability Justice Movement: Emergent decomposer (organized/constrained) — building sunset logic through intersectional frameworks
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent arrangements as identity laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(disability_identity_politics, 0.58).
domain_priors:suppression_score(disability_identity_politics, 0.62).
domain_priors:theater_ratio(disability_identity_politics, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(disability_identity_politics, extractiveness, 0.58).
narrative_ontology:constraint_metric(disability_identity_politics, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(disability_identity_politics, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(disability_identity_politics, tangled_rope).
narrative_ontology:human_readable(disability_identity_politics, "Disability Identity Politics Constraint").
narrative_ontology:topic_domain(disability_identity_politics, "social/political/identity").

domain_priors:requires_active_enforcement(disability_identity_politics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(disability_identity_politics, disability_advocacy_organizations).
narrative_ontology:constraint_beneficiary(disability_identity_politics, disability_studies_academics).
narrative_ontology:constraint_beneficiary(disability_identity_politics, professional_intermediaries).
narrative_ontology:constraint_victim(disability_identity_politics, disabled_individuals_economic_access).
narrative_ontology:constraint_victim(disability_identity_politics, disabled_individuals_voice_silencing).
narrative_ontology:constraint_victim(disability_identity_politics, cross_disability_solidarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECONOMICALLY DEPENDENT DISABLED PERSON (SNARE) — Bears maximum extraction. Requires income support (SSI/SSDI) administered through disability bureaucracy controlled by advocacy gatekeepers and medical professionals. Cannot exit without risking survival. Experiences identity politics as constraint on authentic voice: must adopt approved disability identity and rhetoric to maintain material access. No alternatives.
constraint_indexing:constraint_classification(disability_identity_politics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKING DISABLED PERSON (TANGLED ROPE) — Constrained by workplace discrimination risk and social isolation if rejecting disability identity politics. Benefits from genuine coordination: shared legal protections, accessibility standards, community solidarity. Significant extraction: must navigate approved narratives to access workplace accommodations; professional consequences for dissenting from disability movement orthodoxy. Mixed experience — real benefits alongside real costs.
constraint_indexing:constraint_classification(disability_identity_politics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DISABLED PERSON TRAPPED IN MOVEMENT IDENTITY (TANGLED ROPE) — Structurally mobile (could leave disability activism, could adopt alternative frames) but identity-locked: professional identity, relational identity within movement, and self-concept are constituted through disability politics commitment. Cannot exercise structural mobility because exit would require abandoning identity. Experiences genuine coordination (movement provides community, validates experience, fights for access) alongside extraction (must suppress doubts, internalize orthodox positions, perform correct disability consciousness). The identity lock is the binding mechanism.
constraint_indexing:constraint_classification(disability_identity_politics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 4: DISABILITY ADVOCACY ORGANIZATION (ROPE) — Primary institutional beneficiary. Experiences identity politics as coordination function: manages collective action problems (resource pooling, unified messaging, legislative coordination), maintains funding streams, builds movement infrastructure. Has arbitrage options: can shift coalitional priorities, rebrand, access foundation funding. Extraction runs toward this agent — they benefit from gatekeeping authentic disability voice and controlling narrative legitimacy.
constraint_indexing:constraint_classification(disability_identity_politics, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DISABILITY SERVICES BUREAUCRACY (PITON) — Maintains medical model classification infrastructure (ICD-10, diagnostic categories, eligibility gatekeeping) through institutional inertia. Functionally degraded: medical model actively contradicts social model disability theory but persists because bureaucratic categories, funding formulas, and professional licensing are built on it. Theater ratio high: extensive documentation and verification ritual performed to justify benefit access, but actual assessment bears minimal relationship to functional support or autonomy. Cannot exit because alternative (direct cash transfers, no categorization) would eliminate administrative role.
constraint_indexing:constraint_classification(disability_identity_politics, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ORGANIZED DISABLED WORKERS COALITION (TANGLED ROPE) — Coalition of disabled labor organizers experiences constraint as hybrid: genuine coordination function (collective bargaining for workplace accommodations, wage equity, benefits protection) alongside extraction (pressure to subordinate class analysis to identity politics, conflicts with non-disabled worker coalitions, organizational resources captured by advocacy gatekeepers). Constrained by need for mainstream disability movement legitimacy; significant agency through labor organization. Bidirectional extraction — both benefiting from and bearing costs of identity politics.
constraint_indexing:constraint_classification(disability_identity_politics, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: DISABILITY JUSTICE MOVEMENT (SCAFFOLD) — Emergent framework that intersects disability with race, class, gender, queerness. Sees identity politics constraint as temporary: the political identity fusion required in earlier movement phases (consolidation, legal victories) is being decomposed into multiple simultaneous liberation frameworks. Theater declining as movement shifts from representative politics to direct action and mutual aid. Sunset logic: as intersectional frameworks mature, the need for unified disability identity as political bargaining chip decreases. Constrained by mainstream movement resistance; moderate agency through direct action.
constraint_indexing:constraint_classification(disability_identity_politics, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, identity politics might appear immutable: disability identity seems like a natural fact, identity fusion seems psychologically inevitable, the tension between individual authenticity and collective mobilization seems like an inherent feature of all social movements. However, the structural data contradicts this — the constraint is contingent on institutional arrangements (benefit gatekeeping, advocacy organization power, medical bureaucracy), not on natural laws. The mountain classification is a false summit that naturalizes what is actually managed extraction.
constraint_indexing:constraint_classification(disability_identity_politics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(disability_identity_politics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(disability_identity_politics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(disability_identity_politics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(disability_identity_politics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(disability_identity_politics, TR),
    TR >= 0.70.

:- end_tests(disability_identity_politics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts compliance with identity politics from disabled people in exchange for material access and legal protection. The extraction is significant but not absolute — disabled people retain some choice about degree of movement participation, and many find genuine value in community. The 0.58 value reflects that extraction is structural (material gatekeeping, voice suppression) but not totalizing (alternatives exist, benefits are real). Suppression (0.62): Moderate-high. Structural barriers include: medical bureaucracy gatekeeping, professional intermediary control over legitimate voice, social sanctions against dissent, organizational retaliation against heresy. But suppression is not absolute — disabled people do dissent, alternative frames (disability justice, disability-centered labor, neurodiversity) are emerging. Theater ratio (0.68): High. Significant portion of disability activism involves performative identity validation (testimony rituals, narrative conferences, approved consciousness deployment) rather than material change. Theater increased as movement professionalized and advocacy organizations grew relative to grassroots organizing. Theater is declining in disability justice spaces where direct action and mutual aid replace testimonial politics.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence. The snare perspective (economically dependent) experiences pure extraction and survival risk. The rope perspective (advocacy organization) experiences pure coordination and benefit. The tangled rope perspectives (workers, workers with labor organization) experience mixed extraction and coordination. The identity-locked perspective reveals that a constraint can be structurally mobile (the agent could leave) but cognitively immobilized (the agent cannot perceive leaving from within their identity frame). The piton perspective reveals that disability services bureaucracy has atrophied — it persists not because the medical gatekeeping works but because administrative roles depend on it. The scaffold perspective suggests a sunset — as disability justice and intersectional frameworks mature, the unified disability identity constraint is being decomposed into multiple simultaneous liberation frameworks. The mountain perspective is almost certainly a false summit — the analytical observer risks naturalizing identity politics as immutable while the structural data reveals it as an institutional contingency. No other constraint in this corpus produces as wide perspectival divergence from a single set of base metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's extraction is directional: it flows from economically dependent disabled people toward disability advocacy organizations and professional intermediaries. The directionality chain computes this through: (1) beneficiary/victim declarations (advocacy orgs are beneficiaries, disabled people are victims), (2) exit options (advocacy orgs have arbitrage exit, dependent disabled people have trapped exit), (3) power atoms (organizations are institutional, dependent people are powerless). The sigmoid f(d) amplifies the difference: beneficiaries with arbitrage exit get low d → negative f(d) → they see rope or coordination. Victims with trapped exit get high d → high f(d) → they see snare or extraction. The identity-locked perspective is analytically crucial because it shows that even a cognitively sophisticated agent (someone aware of the structure, often academically trained) can experience high d not because of external barriers but because their identity frame makes exit unthinkable. The identity lock is part of the extraction mechanism itself. Directionality overrides are not needed — the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint requires mandatrophy resolution because extractiveness (0.58) is above the 0.46 threshold for high-extraction constraints. The mandatrophy is: 'Is this a genuine coordination mechanism (rope) with some extraction, or is it a pure extraction mechanism (snare) using coordination language as cover?' The perspectives resolve this by showing that it is genuinely both — the constraint exhibits real coordination (legal protections, accessibility standards, community solidarity are real goods) alongside real extraction (gatekeeping, identity conformity pressure, voice suppression). The resolution is tangled rope: a hybrid mechanism where coordination and extraction are structurally inseparable. The disability movement genuinely solved collective action problems (litigation, legislation, cultural change) that benefited disabled people. But the organizational form that solved those problems also created gatekeeping power, and that gatekeeping power is now being used to enforce identity conformity. The coordination function does not justify the extraction, but it does explain why the extraction persists — dismantling the movement's gatekeeping power would also risk dissolving the coordination function that produced legal victories. This is the classic tangled rope mandatrophy: the mechanism that provides coordination benefits is the same mechanism that enables extraction. The scaffold perspective suggests a path forward (disability justice decomposing unified identity into multiple simultaneous frameworks), but this path faces resistance from organizations whose power depends on controlled identity politics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_threshold,
    'What constitutes genuine disability identity versus identity imposed by political necessity?',
    'Longitudinal interviews with disabled people asking what identity frames they would adopt absent political/material pressure; analysis of identity shifts correlating with access to material resources outside movement channels',
    'If identity is primarily imposed: extraction is higher, suppression is higher, snare perspective becomes dominant. If identity is primarily authentic: extraction lower, coordination function more genuine, rope perspective gains legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authenticity_threshold, conceptual, 'Threshold between authentic disability identity and politically-imposed identity').

omega_variable(
    identity_lock_mechanism,
    'For disabled people who are identity-locked in disability movement: is the lock primarily professional (career investment), relational (social bonds within movement), ideological (worldview that makes exit unthinkable), or some combination?',
    'Qualitative analysis of exits from disability movement; documentation of identity dissolution patterns; study of disabled people who maintain disability identity after leaving organized movement',
    'If primarily professional: constraint could decompose if employment alternatives existed. If relational: constraint persists as long as movement structures remain. If ideological: constraint persists even after exit. Different mechanisms require different interventions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Mechanisms of identity lock for disabled people in disability movement').

omega_variable(
    material_access_alternative,
    'Could disabled people access material resources (income support, accommodations, healthcare) through channels independent of disability identity politics?',
    'Policy modeling: comparison of direct cash transfer systems vs. needs-tested categorical benefits; analysis of disability access in different welfare regimes; study of disabled people''s material security under different institutional frameworks',
    'If yes: snare perspective could shift to tangled_rope or rope (material survival not contingent on identity politics, reducing extraction). If no: snare perspective accurate — material dependency is structural, extraction is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_access_alternative, empirical, 'Whether material access could exist independent of disability identity politics').

omega_variable(
    intersectionality_decomposition,
    'Does disability justice framework actually decompose the identity politics constraint (scaffold perspective), or does it merely add layers without resolving the underlying gatekeeping?',
    'Comparative analysis of resource distribution, voice amplification, and decision-making power before and after disability justice framework adoption; measurement of how much organizational capacity flows to non-Black disabled people, working-class disabled people, disabled immigrants',
    'If genuinely decomposing: scaffold sunset is real, constraint will weaken. If merely adding layers: theater increases, suppression persists, extraction replicates across intersectional axes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersectionality_decomposition, empirical, 'Whether disability justice actually decomposes identity politics constraint').

omega_variable(
    solidarity_cost_measurement,
    'What is the actual cost (in material resources, voice suppression, broken relationships) that disabled people pay for cross-disability solidarity enforcement?',
    'Survey research measuring opportunity cost (earnings forgone, time invested) in movement activities; documentation of incidents where disabled people faced sanctions for breaking solidarity; analysis of resource flows within movement by disability type',
    'If costs are high and unevenly distributed: extraction is higher than base measurement suggests, suppression is structural. If costs are low and evenly distributed: coordination function is more genuine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(solidarity_cost_measurement, empirical, 'Actual cost of cross-disability solidarity maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(disability_identity_politics, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(disid_tr_t0, disability_identity_politics, theater_ratio, 0, 0.52).
narrative_ontology:measurement(disid_tr_t10, disability_identity_politics, theater_ratio, 10, 0.62).
narrative_ontology:measurement(disid_tr_t20, disability_identity_politics, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(disid_be_t0, disability_identity_politics, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(disid_be_t10, disability_identity_politics, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(disid_be_t20, disability_identity_politics, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(disability_identity_politics, identity_coordination).
narrative_ontology:boltzmann_floor_override(disability_identity_politics, 0.12).
narrative_ontology:affects_constraint(disability_identity_politics, medical_gatekeeping_bureaucracy).
narrative_ontology:affects_constraint(disability_identity_politics, accessibility_standard_enforcement).

% DUAL FORMULATION NOTE:
% Disability identity politics is upstream of both medical gatekeeping (which uses disability identity categories for benefit eligibility) and accessibility enforcement (which uses unified disability movement voice for standard-setting). All three constraints are linked through the institutional control of disability categorization and representation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(disability_identity_politics, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
