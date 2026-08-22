% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__council_communist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__council_communist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: manifesto_revolutionary_method__council_communist_reading
 *   human_readable: Council Communist Revolutionary Method (Workers' Councils as Federated Assemblies)
 *   domain: political/revolutionary
 *
 * SUMMARY:
 *   This constraint story models the council communist reading of
 *   revolutionary method: workers' councils (soviets) as federated workplace
 *   assemblies that directly exercise power, replacing both the capitalist
 *   state and any vanguard party. The reading emerged from the German-Dutch
 *   left (Pannekoek, Gorter, Rühle) and Russian Workers' Opposition
 *   (Kollontai, Shlyapnikov) as a critique of Bolshevik substitutionism. The
 *   constraint's internal coordination function is genuine — councils solve
 *   the problem of large-scale direct democracy through mandated delegates
 *   and federation — but its historical persistence required active defense
 *   against both capitalist restoration and Bolshevik party-state
 *   suppression. The low internal extractiveness (0.25) reflects the council
 *   form's coordination purity; the high suppression (0.78) reflects the
 *   constraint's structural vulnerability to rival revolutionary readings
 *   that treat councils as either dangerous (vanguard) or unnecessary
 *   (gradualism).
 *
 * KEY AGENTS:
 *   - autonomous_worker_collectives: Primary beneficiaries (powerless/identity_locked) — exercise direct power through councils
 *   - factory_committees: Primary beneficiaries (organized/identity_locked) — workplace-level organs of council power
 *   - neighborhood_assemblies: Secondary beneficiaries (organized/constrained) — territorial federation nodes
 *   - state_bureaucrats: Primary victims (institutional/constrained) — displaced by council administration
 *   - vanguard_party_officials: Primary victims (institutional/constrained) — displaced as revolutionary leadership
 *   - capitalist_owners: Victims (powerful/trapped) — expropriated by council power
 *   - council_communist_theorists: Observers (analytical/analytical) — analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__council_communist_reading, 0.25).
domain_priors:suppression_score(manifesto_revolutionary_method__council_communist_reading, 0.78).
domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Council Communist Revolutionary Method (Workers' Councils as Federated Assemblies)").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political/revolutionary").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__council_communist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, 'fbf7f9e8-907b-4d53-ac5e-0a9a5912a174').
narrative_ontology:cs_kernel_codification('fbf7f9e8-907b-4d53-ac5e-0a9a5912a174', distributed).
narrative_ontology:cs_authority_grounding('fbf7f9e8-907b-4d53-ac5e-0a9a5912a174', practice).
narrative_ontology:cs_interpretation_layer_present('fbf7f9e8-907b-4d53-ac5e-0a9a5912a174').
narrative_ontology:cs_reading_relation('fbf7f9e8-907b-4d53-ac5e-0a9a5912a174', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('fbf7f9e8-907b-4d53-ac5e-0a9a5912a174', manifesto_revolutionary_method__democratic_gradualism_reading, coexists_with).
narrative_ontology:cs_axiom('fbf7f9e8-907b-4d53-ac5e-0a9a5912a174', foundational, working_class_self_emancipation).
narrative_ontology:cs_axiom_status(working_class_self_emancipation, holdable).
narrative_ontology:cs_axiom_grounding('fbf7f9e8-907b-4d53-ac5e-0a9a5912a174', working_class_self_emancipation, deontological).
narrative_ontology:cs_axiom('fbf7f9e8-907b-4d53-ac5e-0a9a5912a174', foundational, rejection_of_substitutionism).
narrative_ontology:cs_axiom_status(rejection_of_substitutionism, holdable).
narrative_ontology:cs_axiom_grounding('fbf7f9e8-907b-4d53-ac5e-0a9a5912a174', rejection_of_substitutionism, deontological).
narrative_ontology:cs_reference_frame('fbf7f9e8-907b-4d53-ac5e-0a9a5912a174', council_power_1917).
narrative_ontology:cs_drift_state('fbf7f9e8-907b-4d53-ac5e-0a9a5912a174', post_spanish_revolution_1937, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('fbf7f9e8-907b-4d53-ac5e-0a9a5912a174', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, factory_committees).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, neighborhood_assemblies).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, capitalist_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers organized in workplace assemblies that send mandated, recallable delegates to higher federations. They directly decide production, distribution, and social policy. Their identity is fused with the council form — exit means ceasing to be a collective subject. They bear the labor of self-administration but receive the full product of their associated labor.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, beneficiary,
    powerless, biographical, identity_locked, local).

% Elected committees managing day-to-day workplace operations under assembly sovereignty. They set the agenda for workplace decisions and coordinate with other committees through federation congresses. Their power derives from the assembly; they can be recalled at any time. Exit is identity-locked — the committee form is the workers' organized expression.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, factory_committees, beneficiary,
    organized, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__council_communist_reading, factory_committees, agenda_setter).

% Territorial councils federating workplace delegates with community residents. They coordinate distribution, defense, and social reproduction beyond the factory gate. More permeable exit than workplace collectives — residents can disengage without losing productive identity — but constrained by material dependence on council-distributed resources.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, neighborhood_assemblies, beneficiary,
    organized, biographical, constrained, regional).

% Officials of the capitalist or party-state apparatus displaced by council administration. Their skills (administration, planning, technical expertise) are partially transferable to council technical commissions, but their authority position is abolished. Exit is constrained: they can join councils as workers/technicians but lose status, salary, and decision-making monopoly.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats, payer,
    institutional, biographical, constrained, national).

% Professional revolutionaries whose leadership role is negated by the council form's rejection of substitutionism. Their organizational capital (cadre networks, propaganda apparatus, international connections) becomes counter-revolutionary by the council reading's lights. Exit is constrained: they can dissolve into the working class, but the party form itself is structurally incompatible with council power.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials, payer,
    institutional, biographical, constrained, national).

% Owners of means of production expropriated by council power. Their property rights are abolished; their exit options are flight (capital strike, emigration) or counter-revolution. Within the constraint's spatial scope, they are trapped — the council form recognizes no legitimate property claim to negotiate.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, capitalist_owners, payer,
    powerful, biographical, trapped, global).

% Theorists (Pannekoek, Gorter, Mattick, Dauvé, Camatte) who analyze the council form as a historical and theoretical object. They do not administer councils, collect rents, or bear extraction. Their seat is analytical: they map the constraint's structure across historical instances and theoretical elaborations.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, council_communist_theorists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the scale problem of direct democracy: how can millions of workers exercise power without representatives becoming a substitute ruling class? Federated mandated delegates, recallable at any time, coordinate workplace and territorial assemblies into a unified decision-making structure.
% TRANSFER_FUNCTION: Moves decision-making authority from state bureaucrats and party officials to worker assemblies; moves control of means of production from capitalist owners to federated councils; moves the product of labor from capital accumulation to direct social distribution.
% ABSENT_VOICES: Peasant communities (in agrarian societies) who are neither industrial workers nor capitalist owners — their relation to councils was ambiguous (ally, subordinate, or independent). Women's reproductive labor was largely invisible in early council formations. National minorities whose self-determination claims conflicted with centralizing federation logic. These voices were structurally excluded from the classic council form.
% DISAPPEARANCE_RATIONALE: If the council form vanished overnight, the working class would lose its only historically demonstrated organ of direct self-government. Power would revert to either vanguard party dictatorship, parliamentary mediation, or capitalist restoration — all three rival readings would fill the vacuum. The world rearranges because the constraint is the institutional form of working-class power itself.
% FOUNDING_PROBLEM: How can the working class exercise revolutionary power without creating a new ruling class (substitutionism)? The Bolshevik party substituted itself for the class; parliament substitutes representatives for the people. The council form was built to solve this by making delegates mandated, recallable, and federated.
% FOUNDING_PROBLEM_CORROBORATION: Council communist theorists (Pannekoek, Mattick) and autonomist Marxists (Negri, Holloway, the Invisible Committee) attest the substitutionism problem persists — every successful revolution has reproduced a new managerial class. Vanguardists (Lenin, Trotsky, contemporary Leninist parties) attest the problem is solved by democratic centralism. Social democrats attest it is solved by electoral accountability. No consensus exists outside the council communist tradition itself.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__council_communist_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__council_communist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(manifesto_revolutionary_method__council_communist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__council_communist_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__council_communist_reading_tests).
:- end_tests(manifesto_revolutionary_method__council_communist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed_type is tangled_rope because the constraint has a genuine coordination function (federated direct democracy solving the scale problem of worker self-organization) AND asymmetric extraction (state bureaucrats and party officials lose their positions and privileges, capitalist owners lose property). Active enforcement is required because the council form does not self-sustain against rival power centers — it must be defended militarily and politically. The metrics are authored at the interval endpoint (1937, end of Spanish Revolution) where the constraint's historical trajectory shows rising extraction and theater as councils are forced into war communism and internal policing. The 0.25 base extractiveness is the reading's own assessment of the council form's internal operation; the engine will compute effective extraction per seat from the beneficiary/victim structure and exit options.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes per-seat types from structural data: autonomous worker collectives (beneficiary, powerless, identity_locked) should compute as rope/mountain (low χ); state bureaucrats and party officials (victims, institutional, constrained) should compute as snare/tangled_rope (high χ); capitalist owners (victims, powerful, trapped) compute as snare. The council communist reading claims the constraint is a coordination mechanism for the working class; the vanguard reading experiences it as an existential threat to party authority. This perspectival divergence is the measurement, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: autonomous worker collectives, factory committees, neighborhood assemblies — these groups directly exercise power through the council form, receive the gains of self-administration, and have identity_locked exit (their self-concept is constituted through the council relation). Victims: state bureaucrats and vanguard party officials lose their structural position as administrators/leaders; they are constrained rather than trapped because they can join the councils as workers (though identity loss is severe). Capitalist owners are trapped — expropriation is total and exit is impossible within the constraint. The reading's own ε (0.25) assesses internal council operation; external suppression by rival readings is a separate dynamic captured in suppression (0.78) and the temporal series.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how can the working class exercise power without a substitute leadership — remains live (founding_problem_status: contested). The council form was built to solve the substitutionism problem; it persists in historical memory and contemporary autonomist/municipalist movements as a live alternative to both vanguardism and parliamentarism. No single party captures the gains (gain_flow: diffuse); fixing cost is prohibitive because the constraint requires a revolutionary rupture to instantiate and a civil war to defend. The mandatrophy declaration (mandatrophy_resolved: false) reflects that the arrangement's function has not been superseded by a non-extractive alternative — the problem it solved persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct revolutionary method or one reading of a shared kernel (manifesto_revolutionary_method) contested by vanguard_rupture and democratic_gradualism readings?',
    'Structural comparison of beneficiary/victim sets and coordination mechanisms across the three readings; if they share a referent (the problem of revolutionary transformation) but instantiate mutually incompatible arrangements, they are sibling readings of a kernel.',
    'If confirmed as kernel reading, classification must be reading-indexed; the committer structure (which kernel, which reading, sibling relations) routes to omega and cs_structure rather than standard fields.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this constraint is one reading of the manifesto_revolutionary_method kernel.').

omega_variable(
    internal_vs_external_extraction,
    'Does the low internal extractiveness (0.25) within councils represent genuine non-extractive coordination, or does it mask extraction that appears only when rival readings suppress the council form?',
    'Historical analysis of council periods (1917-1921 Russia, 1918-1919 Germany, 1936-1937 Spain): measure extraction inside councils when they hold power vs. when they are suppressed by Bolsheviks/social democrats/fascists.',
    'If extraction rises only under external suppression, the constraint''s ε is context-dependent — the engine''s ε-invariance test would require decomposition into ''councils_when_sovereign'' and ''councils_under_siege'' as separate constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_vs_external_extraction, empirical, 'Whether internal coordination is genuinely low-extraction or only appears so absent rival power.').

omega_variable(
    party_vs_state_victim_distinction,
    'Are vanguard party officials and state bureaucrats structurally distinct victim groups under this reading, or do they fuse into a single ''substitutionist apparatus'' victim class?',
    'Analyze council communist texts (Pannekoek, Gorter, Mattick, Dauvé) for whether they distinguish party dictatorship from state bureaucracy as separate extraction mechanisms, or treat them as unified.',
    'If fused, the victims array should declare one group (substitutionist_apparatus); if distinct, the current two-group declaration is correct and the engine computes separate directionalities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(party_vs_state_victim_distinction, conceptual, 'Whether party and state victims are structurally distinct or unified under council communist theory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 1917, 1937).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_tr_t1917, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1917, 0.05).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_tr_t1918, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1918, 0.1).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_tr_t1919, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1919, 0.15).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_tr_t1920, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1920, 0.22).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_tr_t1921, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1921, 0.3).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_tr_t1936, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1936, 0.08).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_tr_t1937, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1937, 0.25).

% Extraction over time
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_be_t1917, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1917, 0.18).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_be_t1918, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1918, 0.22).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_be_t1919, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1919, 0.28).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_be_t1920, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1920, 0.35).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_be_t1921, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1921, 0.42).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_be_t1936, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1936, 0.2).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_be_t1937, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1937, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_su_t1917, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1917, 0.45).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_su_t1918, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1918, 0.6).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_su_t1919, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1919, 0.72).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_su_t1920, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1920, 0.8).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_su_t1921, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1921, 0.88).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_su_t1936, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1936, 0.5).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_su_t1937, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1937, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__council_communist_reading, 0.08).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__democratic_gradualism_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, war_communism_policy).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, new_economic_policy).

% DUAL FORMULATION NOTE:
% This constraint decomposes the 'manifesto revolutionary method' kernel into three structurally distinct readings. The council_communist_reading instantiates a low-internal-extraction, high-external-suppression tangled_rope. The vanguard_rupture_reading instantiates a high-internal-extraction snare (party apparatus extracts from working class). The democratic_gradualism_reading instantiates a scaffold (electoral reform with sunset logic via institutionalization). All three are linked via affects_constraints; each has its own ε and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__council_communist_reading, institutional, 0.85).
constraint_indexing:directionality_override(manifesto_revolutionary_method__council_communist_reading, powerful, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
