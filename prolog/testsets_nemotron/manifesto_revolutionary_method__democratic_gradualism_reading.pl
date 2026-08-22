% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__democratic_gradualism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__democratic_gradualism_reading, []).

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
 *   constraint_id: manifesto_revolutionary_method__democratic_gradualism_reading
 *   human_readable: Democratic Gradualist Path to Socialism
 *   domain: political_philosophy/revolutionary_theory/historical_materialism
 *
 * SUMMARY:
 *   This constraint story instantiates the democratic_gradualism_reading of
 *   the manifesto_revolutionary_method kernel. It describes the claim that
 *   socialism can be achieved through democratic electoral majorities and
 *   gradual institutional reform within the continuity of liberal democratic
 *   structures. The constraint operates as a tangled rope: it genuinely
 *   coordinates working-class political power into an effective institutional
 *   vehicle (the coordination function), but it simultaneously extracts
 *   revolutionary energy, suppresses alternative revolutionary forms, and
 *   concentrates organizational resources in social democratic parties and
 *   unions that become structurally invested in the method's persistence (the
 *   extraction function). The engine will compute per-seat types from this
 *   structural data; the claimed_type of tangled_rope reflects the authoring
 *   seat's assessment that both coordination and asymmetric extraction are
 *   structurally present.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.55).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic Gradualist Path to Socialism").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political_philosophy/revolutionary_theory/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, 'be554dba-2827-4ebc-b7ce-5d5ddb968458').
narrative_ontology:cs_kernel_codification('be554dba-2827-4ebc-b7ce-5d5ddb968458', fixed_text).
narrative_ontology:cs_authority_grounding('be554dba-2827-4ebc-b7ce-5d5ddb968458', lineage).
narrative_ontology:cs_interpretation_layer_present('be554dba-2827-4ebc-b7ce-5d5ddb968458').
narrative_ontology:cs_reading_relation('be554dba-2827-4ebc-b7ce-5d5ddb968458', manifesto_revolutionary_method__vanguard_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('be554dba-2827-4ebc-b7ce-5d5ddb968458', manifesto_revolutionary_method__council_communist_reading, coexists_with).
narrative_ontology:cs_axiom('be554dba-2827-4ebc-b7ce-5d5ddb968458', foundational, electoral_mandate_sufficient_for_socialism).
narrative_ontology:cs_axiom_status(electoral_mandate_sufficient_for_socialism, holdable).
narrative_ontology:cs_axiom_grounding('be554dba-2827-4ebc-b7ce-5d5ddb968458', electoral_mandate_sufficient_for_socialism, conventional).
narrative_ontology:cs_axiom('be554dba-2827-4ebc-b7ce-5d5ddb968458', foundational, institutional_continuity_preserves_democratic_legitimacy).
narrative_ontology:cs_axiom_status(institutional_continuity_preserves_democratic_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('be554dba-2827-4ebc-b7ce-5d5ddb968458', institutional_continuity_preserves_democratic_legitimacy, deontological).
narrative_ontology:cs_axiom('be554dba-2827-4ebc-b7ce-5d5ddb968458', secondary, revolutionary_rupture_is_adventurist_and_counterproductive).
narrative_ontology:cs_axiom_status(revolutionary_rupture_is_adventurist_and_counterproductive, holdable).
narrative_ontology:cs_axiom_grounding('be554dba-2827-4ebc-b7ce-5d5ddb968458', revolutionary_rupture_is_adventurist_and_counterproductive, instrumental).
narrative_ontology:cs_reference_frame('be554dba-2827-4ebc-b7ce-5d5ddb968458', second_international_erfurt_program).
narrative_ontology:cs_drift_state('be554dba-2827-4ebc-b7ce-5d5ddb968458', contemporary_post_cold_war, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('be554dba-2827-4ebc-b7ce-5d5ddb968458', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, trade_unions).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, parliamentary_majority_coalitions).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, council_communist_organizers).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, vanguard_party_activists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Contest elections, form governments, and legislate reforms within the existing constitutional framework. They derive organizational resources, patronage, and political legitimacy from the democratic gradualist method. Exit means abandoning their electoral vehicle and organizational base, which is structurally costly but legally unconstrained.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, beneficiary).

% Negotiate collective bargaining agreements, influence labor law through political allies, and mobilize members for electoral support. They gain legal recognition, bargaining rights, and welfare state expansions under this method. Exit is constrained by the need to maintain legal status and member services; wildcat alternatives risk repression and loss of recognition.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, trade_unions, beneficiary,
    organized, biographical, constrained, national).

% Assemble legislative majorities to pass incremental reforms, control state apparatuses, and distribute resources. They benefit from the stability and predictability of the institutional path. Exit is mobile: coalitions can shift, defect, or dissolve without existential penalty to individual parties.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, parliamentary_majority_coalitions, beneficiary,
    powerful, biographical, mobile, national).

% Advocate for rupture, direct action, and extra-parliamentary organization. They are labeled 'adventurist', 'ultra-left', or 'counter-revolutionary' by the gradualist establishment, face marginalization within the workers' movement, and may face state repression enabled by gradualist legitimacy. Exit requires abandoning their revolutionary identity and political commitments, which is existentially difficult.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants, payer,
    powerless, biographical, identity_locked, national).

% Build workplace assemblies and federated councils as alternative power structures. They are excluded from the official labor movement, denied access to institutional resources, and their autonomous organizations are often suppressed or co-opted. Exit means dissolving the council form, which negates their entire political project.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, council_communist_organizers, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, council_communist_organizers, excluded).

% Organize for revolutionary seizure of power, often operating clandestinely or semi-legally. They face bans, surveillance, and ideological condemnation from both the state and the gradualist left. Exit is constrained by organizational discipline and the risk of isolation, but less identity-fused than councilists or militants.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, vanguard_party_activists, payer,
    moderate, biographical, constrained, national).

% Provide the constitutional framework, electoral machinery, judiciary, and bureaucratic apparatus within which gradualism operates. They set the procedural rules (election law, party registration, legislative procedure) that gradualists must follow and that constrain the speed and scope of transformation. They neither benefit nor pay directly but administer the constraint.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_institutions, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_institutions, observer).

% Vote for social democratic parties, join unions, and receive welfare reforms, labor protections, and public services. They bear the cost of slow transformation — continued exploitation, alienation, and the risk of co-optation — while gaining concrete improvements. Exit from the gradualist strategy means either political disengagement or shifting to more radical alternatives that are structurally marginalized.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate, payer).

% Analyze the constraint from outside: whether the gradualist method can actually achieve socialism, whether it betrays the revolutionary telos, and how it structures the field of left politics. They hold no institutional power within the constraint but provide the analytical vocabulary for its classification.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, historical_materialist_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the working class's political expression through a single, recognized institutional channel: elections, legislation, and state administration. Solves the problem of fragmented resistance by concentrating worker power into a parliamentary majority that can wield state apparatuses for reform.
% TRANSFER_FUNCTION: Moves political authority, state resources, and legislative agenda-setting from the bourgeoisie (or their representatives) to social democratic parties and unions via the electoral mandate. The transfer is gradual, legal, and reversible; it moves the levers of the existing state rather than replacing the state form.
% ABSENT_VOICES: The excluded voices are the revolutionary militants and council communists who would object that the gradualist path reproduces capitalism by preserving its state form and integrating the working class into its management. They are absent from the official labor movement's decision-making bodies, from parliamentary debate, and from the recognized negotiation table — kept out by the same democratic rules that legitimate the gradualist method.
% DISAPPEARANCE_RATIONALE: If the democratic gradualist constraint vanished overnight, the institutional channel for working-class power would collapse. Social democratic parties would lose their method and their claim to legitimacy; unions would lose their legal framework; the working class electorate would lose its recognized vehicle. The field would reorganize around either revolutionary rupture (vanguard or council forms) or fascist/reactionary capture of the disorganized masses. The world rearranges because the constraint IS the organizing form for a major historical actor.
% FOUNDING_PROBLEM: How can the working class achieve socialism without the chaos, bloodshed, and authoritarianism of revolutionary rupture? The founding problem was the perceived impossibility of insurrection in advanced capitalist democracies with strong states, and the desire to use the bourgeoisie's own democratic institutions against them.
% FOUNDING_PROBLEM_CORROBORATION: The gradualist founding problem is attested by the Second International's Erfurt Program (1891), the SPD's theoretical tradition (Kautsky, Bernstein), and post-war social democratic manifestos (Godesberg 1959). It is contested by the Communist International's 21 Conditions (1920), council communist critiques (Pannekoek, Mattick), and the historical record of social democratic governments managing capitalism rather than transcending it. No corroboration exists outside the gradualist tradition itself that the problem is 'solved' — the corroboration for 'contested' comes from the sibling readings' persistent existence and the historical failure of gradualist parties to achieve socialism.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__democratic_gradualism_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__democratic_gradualism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(manifesto_revolutionary_method__democratic_gradualism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).
:- end_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.40) is moderate because the constraint does not directly expropriate value in the market sense, but it extracts revolutionary potential — the time, energy, and political imagination of militants who are channeled into electoralism and then marginalized when they resist co-optation. Suppression (0.55) reflects the active marginalization of revolutionary alternatives: bans on communist parties, exclusion of councilists from unions, ideological policing within social democratic organizations, and the use of state apparatuses (which gradualists help legitimize) against extra-parliamentary movements. Theater ratio (0.35) captures the gap between the revolutionary rhetoric of early social democracy and the administrative reality of managing capitalism; it peaks in 1914 (war credits betrayal) and 1989 (end of history triangulation) but never reaches piton levels because the coordination function (winning reforms) remains real. Accessibility collapse (0.50) and resistance (0.60) reflect that alternatives (vanguard rupture, council communism) persist and resist despite marginalization — the constraint does not fully collapse the imaginative space, and the suppressed do not disappear.
 *
 * PERSPECTIVAL GAP:
 *   From the social democratic party seat (agenda_setter, institutional power, arbitrage exit), the constraint is a rope: it coordinates workers effectively and the extraction is the necessary cost of democratic legitimacy. From the revolutionary militant seat (victim, powerless, identity_locked exit), it is a snare: the coordination is a trap that neutralizes rupture, and the suppression is existential. From the council communist seat (victim/excluded, powerless, identity_locked), it is a snare that actively destroys the alternative form. From the working class electorate seat (beneficiary/payer, organized, constrained), it is a tangled rope: real gains, real costs, real foreclosure of the revolutionary horizon. The engine computes this divergence; the authoring declares the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (social democratic parties, unions, parliamentary coalitions, working class electorate) sit at d ≈ 0.15–0.35: they collect reforms, resources, and legitimacy. The constraint subsidizes their organizational existence. Victims (revolutionary militants, council communists, vanguard activists) sit at d ≈ 0.75–0.95: they bear the cost of marginalization, repression, and identity foreclosure. Their exit is identity_locked — leaving the revolutionary commitment is not a strategic choice but an existential rupture. The liberal democratic institutions (agenda_setter/observer) sit at d ≈ 0.5: they administer the constraint without direct gain or loss. The theorist (observer, analytical) sits at d = 0.5 by definition. The directionality derivation follows from these structural positions; no overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (achieve socialism through democratic reform) has partially atrophied: the founding problem (how to achieve socialism without rupture) remains contested, not solved. The constraint persists because it solves a *different* problem — how to integrate the working class into capitalist democracy — which benefits the beneficiaries. This is the mandatrophy signature: the declared mandate drifts from the actual function. The classification prevents mislabeling this as pure coordination (rope) because the asymmetric extraction (suppression of revolutionaries, concentration of resources in gradualist apparatuses) is structural, not incidental. It prevents mislabeling as pure extraction (snare) because the coordination function (winning welfare states, labor rights, democratic rights) is real and valued by the working class electorate. Tangled rope captures the hybrid truth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_kernel_democratic_gradualism,
    'Is the democratic_gradualism_reading a distinct constraint with its own ε, or a contextual measurement of the manifesto_revolutionary_method kernel under specific historical conditions?',
    'Test ε-invariance: if the constraint''s extractiveness changes when evaluated from the vanguard or council communist reading''s lights, then it is not a single constraint but a kernel with multiple readings. The engine''s per-seat classification already captures this — the reading is the constraint from one seat.',
    'If the kernel has no ε-invariant core, then ''the Manifesto''s method'' is not a constraint but a contested label. The constraint family structure (three linked stories) is the correct modeling choice. If a single ε could be defined, the family would be a false decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_kernel_democratic_gradualism, conceptual, 'Whether the kernel/reading decomposition respects ε-invariance or reifies a linguistic confusion.').

omega_variable(
    gradualism_coordination_vs_extraction_boundary,
    'Where exactly does the coordination function (winning reforms for workers) end and the extraction function (suppressing revolutionary alternatives to preserve the gradualist apparatus) begin? Are they separable in practice?',
    'Historical counterfactual: in moments when social democratic parties broke with gradualism (e.g., SPD 1918, Allende 1970, Mitterrand 1981), did the coordination function survive the extraction function''s crisis? If coordination collapses when extraction is challenged, they are structurally fused.',
    'If fused, the constraint is a snare with a coordination cover story. If separable, it is a genuine tangled rope where the coordination could survive without the extraction. The engine''s classification will reflect the authored metrics; this omega marks the interpretive boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gradualism_coordination_vs_extraction_boundary, empirical, 'Whether the coordination and extraction components are structurally separable or fused.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of revolutionary militants primarily structural (party bans, union exclusion, state repression) or internalized (militants self-censor because the gradualist frame defines ''responsible'' politics)?',
    'Post-exit trajectory: when militants leave gradualist organizations, do they regain revolutionary effectiveness, or do they carry the gradualist frame with them? If the latter, suppression has an internalized component that the structural metric undercounts.',
    'If internalized, the effective suppression is higher than 0.55 — the constraint shapes the subjectivity of its victims, not just their options. This would push the classification toward snare for the victim seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Structural vs. internalized suppression mechanism for revolutionary victims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 1890, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t1890, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1890, 0.15).
narrative_ontology:measurement(mani_tr_t1914, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1914, 0.5).
narrative_ontology:measurement(mani_tr_t1918, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1918, 0.3).
narrative_ontology:measurement(mani_tr_t1945, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(mani_tr_t1973, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1973, 0.35).
narrative_ontology:measurement(mani_tr_t1989, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1989, 0.45).
narrative_ontology:measurement(mani_tr_t2008, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 2008, 0.4).
narrative_ontology:measurement(mani_tr_t2024, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(mani_be_t1890, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1890, 0.2).
narrative_ontology:measurement(mani_be_t1914, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1914, 0.35).
narrative_ontology:measurement(mani_be_t1918, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1918, 0.45).
narrative_ontology:measurement(mani_be_t1945, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1945, 0.38).
narrative_ontology:measurement(mani_be_t1973, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1973, 0.42).
narrative_ontology:measurement(mani_be_t1989, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1989, 0.4).
narrative_ontology:measurement(mani_be_t2008, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 2008, 0.38).
narrative_ontology:measurement(mani_be_t2024, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t1890, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1890, 0.3).
narrative_ontology:measurement(mani_su_t1914, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1914, 0.7).
narrative_ontology:measurement(mani_su_t1918, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1918, 0.65).
narrative_ontology:measurement(mani_su_t1945, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1945, 0.45).
narrative_ontology:measurement(mani_su_t1973, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1973, 0.5).
narrative_ontology:measurement(mani_su_t1989, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1989, 0.55).
narrative_ontology:measurement(mani_su_t2008, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 2008, 0.5).
narrative_ontology:measurement(mani_su_t2024, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__democratic_gradualism_reading, 0.1).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__council_communist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three in the manifesto_revolutionary_method constraint family. The kernel 'Communist Manifesto revolutionary method' decomposes into three readings with distinct ε, beneficiary/victim structures, and claimed types. democratic_gradualism_reading (this story): ε=0.40, tangled_rope, beneficiaries=social democratic parties/unions, victims=revolutionaries. vanguard_rupture_reading: ε~0.65, snare/tangled_rope, beneficiaries=vanguard party apparatus, victims=working class autonomy/councilists. council_communist_reading: ε~0.35, rope/scaffold, beneficiaries=workers' councils, victims=vanguard party/state apparatus. All three stories link to each other via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
