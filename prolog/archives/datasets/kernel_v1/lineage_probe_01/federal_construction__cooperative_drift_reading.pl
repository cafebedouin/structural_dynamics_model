% ============================================================================
% CONSTRAINT STORY: federal_construction__cooperative_drift_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federal_construction__cooperative_drift_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federal_construction__cooperative_drift_reading
 *   human_readable: German Cooperative Federalism: Joint Tasks and Entangled Financing
 *   domain: legal/constitutional/federalism
 *
 * SUMMARY:
 *   German federalism underwent a structural drift from competitive toward
 *   cooperative decision-making across the postwar period. The Basic Law
 *   (1949) established a federal system, but the constitutional design —
 *   particularly the Bundesrat's role in federal legislation, shared tax
 *   revenues (Steuerbund), and the expansion of 'joint tasks'
 *   (Gemeinschaftsaufgaben) — created interlocking incentives for both
 *   federal and Land governments to coordinate rather than compete. By the
 *   1970s, joint tasks and shared financing covered education,
 *   infrastructure, regional economic development, and research. No
 *   government could govern alone; each required the others' consent. This
 *   entanglement solved genuine collective-action problems but also
 *   suppressed the competitive federalism alternative: a system where federal
 *   and Land governments would compete on policy innovation, where voters
 *   could hold each level separately accountable, and where clearer task
 *   separation would speed reform. The 'joint-decision trap' (Scharpf's term)
 *   emerged as a structural feature: the apparatus designed to coordinate was
 *   simultaneously the apparatus preventing unilateral action. This
 *   constraint instantiates ONE reading of a contested kernel about federal
 *   construction. The sibling readings — the Bundesrat entanglement reading
 *   and the Länder cultural sovereignty reading — emphasize different
 *   structural mechanisms and victim sets. The cooperative drift reading
 *   emphasizes extractiveness through diffused accountability and suppression
 *   of competitive innovation.
 *
 * KEY AGENTS:
 *   - Consensus Machinery (Bundesrat, joint commissions, inter-governmental coordination structures): Institutional beneficiary (institutional/arbitrage) — experiences the constraint as its function; the constraint IS the coordination apparatus itself
 *   - Reform-Blocked Voter: Primary victim (powerless/trapped) — cannot attribute outcomes, cannot exit through electoral voice; bears maximum experienced extraction
 *   - Land Executive (Länder governments): Secondary beneficiary and victim (moderate/constrained) — gains veto power over federal initiatives but loses autonomous policy space; mixed extraction
 *   - Federal Government: Secondary victim (institutional/constrained) — loses legislative autonomy, depends on Bundesrat approval; sees its own power diffused
 *   - Reform Coalition (constitutional reformers, efficiency advocates): Organized agent (organized/mobile) — sees the joint-decision trap as solvable; represents exit pathway (scaffold perspective)
 *   - Historical Legitimacy Apparatus (doctrinal framing of 'cooperative federalism as democratic virtue'): Institutional actor (institutional/arbitrage) — maintains theater through inherited legitimation ritual; increasingly performative
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing cooperative federalism as immutable feature of complex federalism rather than specific institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federal_construction__cooperative_drift_reading, 0.52).
domain_priors:suppression_score(federal_construction__cooperative_drift_reading, 0.58).
domain_priors:theater_ratio(federal_construction__cooperative_drift_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federal_construction__cooperative_drift_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(federal_construction__cooperative_drift_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(federal_construction__cooperative_drift_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federal_construction__cooperative_drift_reading, tangled_rope).
narrative_ontology:human_readable(federal_construction__cooperative_drift_reading, "German Cooperative Federalism: Joint Tasks and Entangled Financing").
narrative_ontology:topic_domain(federal_construction__cooperative_drift_reading, "legal/constitutional/federalism").

domain_priors:requires_active_enforcement(federal_construction__cooperative_drift_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federal_construction__cooperative_drift_reading, '66929ae1-d58e-4da9-9fc0-dbcbc5b99c32').
narrative_ontology:cs_kernel_codification('66929ae1-d58e-4da9-9fc0-dbcbc5b99c32', formalized).
narrative_ontology:cs_authority_grounding('66929ae1-d58e-4da9-9fc0-dbcbc5b99c32', lineage).
narrative_ontology:cs_interpretation_layer_present('66929ae1-d58e-4da9-9fc0-dbcbc5b99c32').
narrative_ontology:cs_reading_relation('66929ae1-d58e-4da9-9fc0-dbcbc5b99c32', federal_construction__bundesrat_entanglement, coexists_with).
narrative_ontology:cs_reading_relation('66929ae1-d58e-4da9-9fc0-dbcbc5b99c32', federal_construction__lander_cultural_sovereignty, influences).
narrative_ontology:cs_axiom('66929ae1-d58e-4da9-9fc0-dbcbc5b99c32', foundational, federalism_requires_accountability_separation).
narrative_ontology:cs_axiom_status(federalism_requires_accountability_separation, holdable).
narrative_ontology:cs_axiom_grounding('66929ae1-d58e-4da9-9fc0-dbcbc5b99c32', federalism_requires_accountability_separation, deontological).
narrative_ontology:cs_axiom('66929ae1-d58e-4da9-9fc0-dbcbc5b99c32', secondary, competitive_federalism_enables_policy_innovation).
narrative_ontology:cs_axiom_status(competitive_federalism_enables_policy_innovation, holdable).
narrative_ontology:cs_axiom_grounding('66929ae1-d58e-4da9-9fc0-dbcbc5b99c32', competitive_federalism_enables_policy_innovation, empirically_contingent).
narrative_ontology:cs_reference_frame('66929ae1-d58e-4da9-9fc0-dbcbc5b99c32', competitive_federalism_original_intent).
narrative_ontology:cs_drift_state('66929ae1-d58e-4da9-9fc0-dbcbc5b99c32', post_joint_tasks_expansion_2000s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('66929ae1-d58e-4da9-9fc0-dbcbc5b99c32', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(federal_construction__cooperative_drift_reading, federal_construction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federal_construction__cooperative_drift_reading, consensus_machinery).
narrative_ontology:constraint_beneficiary(federal_construction__cooperative_drift_reading, executive_coordination_apparatus).
narrative_ontology:constraint_victim(federal_construction__cooperative_drift_reading, reform_speed).
narrative_ontology:constraint_victim(federal_construction__cooperative_drift_reading, electoral_accountability).
narrative_ontology:constraint_victim(federal_construction__cooperative_drift_reading, competitive_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE REFORM-BLOCKED VOTER (SNARE) — Electorally trapped in a system where diffused accountability prevents clear causal attribution of outcomes to any single government. A voter cannot punish failure because both federal and Land authorities can blame each other. The joint-decision trap suppresses exit through diffusion of responsibility. Maximum experienced extraction: electoral voice becomes performative.
constraint_indexing:constraint_classification(federal_construction__cooperative_drift_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE LAND EXECUTIVE (TANGLED ROPE) — Constrained by resource dependency on federal revenue-sharing and co-financing requirements, but also benefits from shared-decision structures that provide veto power over federal initiatives affecting Land competencies. Can form a Land government coalition with limited autonomy; faces high coordination costs but gains leverage in federal negotiations. Mixed extraction and coordination.
constraint_indexing:constraint_classification(federal_construction__cooperative_drift_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE CONSENSUS MACHINERY (ROPE) — The Bundesrat, joint commissions, and interlocking tax-revenue systems experience this constraint as pure coordination. The shared-decision apparatus solves the collective-action problem of managing a complex federal system with cross-cutting externalities. Consensus is labor-intensive but functional. The machinery itself benefits from the entanglement — it has no exit option and experiences the constraint as its raison d'être.
constraint_indexing:constraint_classification(federal_construction__cooperative_drift_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE FEDERAL GOVERNMENT (TANGLED ROPE) — Can initiate legislation but depends on Bundesrat approval for any statute affecting Land interests (which is most statutes). Gains coordination benefit from forced consensus-building but bears extraction cost of reduced legislative autonomy and increased veto points. Federal executive sees its own power diffused through the system.
constraint_indexing:constraint_classification(federal_construction__cooperative_drift_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE REFORM COALITION (SCAFFOLD) — Organized political forces (reform parties, governance efficiency advocates, fiscal federalism scholars) see the joint-decision trap as a temporary institutional configuration solvable through federalism reform. Constitutional amendments like the 2006 Föderalismusreform I and II represent sunset mechanisms: they explicitly reduced joint tasks and re-clarified Land/federal boundaries. Reform is possible if coalitions align. Low effective extraction because the exit path is visible and partially realized.
constraint_indexing:constraint_classification(federal_construction__cooperative_drift_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: THE HISTORICAL LEGITIMACY APPARATUS (PITON) — The invocation of 'federalism as power-sharing' and 'cooperative federalism as consensus' persists as the dominant legitimating narrative despite evidence that consensus machinery is increasingly performative and that entanglement prevents rather than enables reform. The legitimacy ritual (framing shared decision-making as democratic virtue rather than institutional sclerosis) is largely theater. The apparatus sees itself as degraded but maintains the frame through doctrinal inertia.
constraint_indexing:constraint_classification(federal_construction__cooperative_drift_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, federalism's fundamental structure — coordinating multiple autonomous jurisdictions with overlapping competencies and shared resources — inherently requires some degree of joint decision-making and accountability diffusion. This perspective sees cooperative federalism as a natural law of complex polities. However, this reading risks naturalizing what is a specific institutional choice: other federal systems (Australia, the United States pre-New Deal, Canada in some domains) organize federalism as competitive rather than cooperative. The natural law framing masks that German federalism is a contingent design, not an immutable structural feature.
constraint_indexing:constraint_classification(federal_construction__cooperative_drift_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federal_construction__cooperative_drift_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federal_construction__cooperative_drift_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federal_construction__cooperative_drift_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federal_construction__cooperative_drift_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(federal_construction__cooperative_drift_reading, TR),
    TR >= 0.70.

:- end_tests(federal_construction__cooperative_drift_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint diffuses accountability across multiple governments, preventing voters from assigning electoral consequences to specific policy failures. This is significant extraction from the voter's perspective, but not maximal — voters retain some ability to vote out individual Land or federal governments, even if causal attribution is blurred. The measurement trajectory (0.28 in 1945 → 0.52 in 2000) reflects the gradual expansion of joint tasks and revenue-sharing from modest early coordination to an entrenched system. Suppression (0.58): Moderate-high. Multiple veto points (Bundesrat approval required for most statutes, Land participation in joint-task decisions, necessity of consensus in federal-Land commissions) create structural barriers to unilateral reform. However, suppression is not total — the Föderalismusreform I & II (2006, 2009) did achieve some task clarification and reduced joint-decision requirements, demonstrating that competitive alternatives are not foreclosed, only suppressed. Theater ratio (0.68): Moderate-high. Much of the machinery's function is increasingly performative: joint commissions produce agreements that are pre-negotiated among bureaucratic networks; the public presentation of 'cooperative' decision-making obscures that the real decisions have already been negotiated behind closed doors. The rising theater trajectory (0.42 → 0.68) reflects that the coordination function (which was once substantive) has become increasingly routinized and ritualized while remaining necessary to the appearance of legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the voter's snare perception (trapped, cannot attribute accountability) and the consensus machinery's rope perception (solves collective action problem) is the crux of the constraint's classification. The Land executive and federal government occupy the middle ground (tangled_rope) — they benefit from veto power but lose autonomous authority. The reform coalition (scaffold) sees an exit path through constitutional amendment, arguing that task clarification and reduced joint-decision requirements are achievable and would unblock reform. The historical legitimacy apparatus (piton) maintains the frame of 'cooperative federalism as democratic virtue' despite rising evidence that consensus machinery is sclerotic. The analytical observer risks collapsing this gap by treating cooperative federalism as an immutable feature of complex federalism (mountain), when in fact it is a contingent institutional choice — Australia, the US, and Canada have organized federalism differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (consensus machinery, executive coordination apparatus) occupy institutional positions with arbitrage exit options: they can work the system, derive rent from mediating coordination, and have no incentive to exit (the constraint IS their function). Their derived d-values are low (~0.15-0.20), producing negative or near-zero effective extractiveness from their perspective. Victims (voters, reform advocates) are trapped by diffused accountability and high consensus requirements; their d-values are high (~0.85-0.95), producing high effective extractiveness. Land executives and federal governments occupy mixed positions (constrained exit, some coordination benefit, some extraction cost); their d-values sit in the middle (0.45-0.55), producing moderate effective extractiveness. The directionality overrides are not needed here — the structural derivation from beneficiary/victim declarations produces the correct perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that tangled_rope is the appropriate classification at the institutional level: the system IS a hybrid of genuine coordination function (joint tasks solve externality problems, reduce inter-jurisdictional spillovers, enable burden-sharing) and asymmetric extraction (benefits the consensus machinery and coordination apparatus, extracts accountability from voters and reform speed from the political system). The mandatrophy does not dissolve into a single type because the constraint genuinely contains both elements. The perspectival distribution (snare for voters, rope for consensus machinery, scaffold for reformers) shows that the type varies by position, but the base classification (tangled_rope at the analytical/institutional level) accurately captures the hybrid mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reform_velocity_threshold,
    'What threshold of reform bottleneck duration distinguishes legitimate consensus-building from extractive immobilism?',
    'Comparative analysis of time-to-reform across federal systems with different decision structures; correlation between consensus requirements and policy change lag in response to exogenous shocks',
    'If threshold < 4 years: joint-decision systems function as intended (coordination dominates). If threshold > 8 years: extraction through immobilism dominates (victim of competitive innovation moves from abstract to concrete).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_velocity_threshold, empirical, 'Threshold distinguishing coordination from immobilism via reform velocity').

omega_variable(
    accountability_attribution_decomposability,
    'Can voters or observers decompose causal attribution of policy outcomes across federal and Land governments, or does diffusion create irreducible attribution failure?',
    'Survey evidence on voter causal attribution (can citizens attribute policy outcomes to specific governments?); comparison of electoral volatility and issue-voting strength across federal and unitary systems; analysis of ballot initiative outcomes in Länder where direct democracy provides control test',
    'If decomposable: diffusion is a coordination benefit (transparency gate passes). If irreducible: diffusion is extractive (snare classification confirmed for voter perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_attribution_decomposability, empirical, 'Whether accountability can be attributed across government levels').

omega_variable(
    competitive_federalism_counterfactual,
    'What would German federalism look like under a competitive rather than cooperative structure, and what would be gained/lost?',
    'Comparative institutional analysis of US federal system (particularly pre-1937, pre-Civil Rights Act), Australian federalism, and Swiss federalism; counterfactual modeling of German policy outcomes under clearer federal-Land task separation',
    'If competitive structure would improve reform speed without harming consensus: cooperative drift is extractive. If competitive structure would increase policy failure and externality spillover: cooperative drift is protective coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competitive_federalism_counterfactual, conceptual, 'Counterfactual outcome of competitive federalism vs. cooperative drift').

omega_variable(
    reading_contest_in_kernel,
    'Is the drift toward cooperative federalism a naturalizable feature of complex federalism (mountain reading), an entanglement of administrations that prevents elected executives from governing (Bundesrat reading), or a protection of Land cultural sovereignty against federal homogenization (sovereignty reading)?',
    'This omega documents that this constraint is one reading of a contested kernel. The sibling readings declare different structural mechanisms (administrative entanglement vs. cultural protection) and different victim sets (voter accountability vs. cultural diversity vs. competitive innovation). The kernel contest is not resolvable by empirical data alone — it depends on which structural mechanism the observer emphasizes. See cs_structure.reading_relations and cs_structure.axioms for the relations between readings.',
    'The classification of this constraint depends on the reading. Under cooperative_drift_reading: tangled_rope dominates, beneficiary is consensus machinery, victim is reform speed. Under bundesrat_entanglement: administrative imprisonment dominates, victim is democratic governance. Under lander_cultural_sovereignty: federalism is protective, beneficiary is cultural diversity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_in_kernel, conceptual, 'Kernel contest: competing readings of federal construction').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of competitive federalism structural (external barriers to reform in the form of veto points and consensus requirements) or internalized (ideology of cooperation absorbed into elite and voter preferences)?',
    'Analysis of reform discourse: are political actors who advocate competitive federalism (clearer task separation, reduced joint-decision requirements) blocked by external veto points or by ideological consensus? Comparison of reform attempts (Föderalismusreform I & II) across decades — do they consistently fail due to institutional barriers or ideological opposition?',
    'If structural: suppression is a material property of the constraint (high suppression gate passes; tangled_rope classification appropriate). If internalized: suppression is lower and the constraint is more like rope (consensual coordination); indicates identity_locked dynamics in elite preferences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of competitive federalism is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federal_construction__cooperative_drift_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fedcoop_theater_1945, federal_construction__cooperative_drift_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fedcoop_theater_1975, federal_construction__cooperative_drift_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement(fedcoop_theater_2000, federal_construction__cooperative_drift_reading, theater_ratio, 55, 0.68).

% Extraction over time
narrative_ontology:measurement(fedcoop_extract_1945, federal_construction__cooperative_drift_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fedcoop_extract_1975, federal_construction__cooperative_drift_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(fedcoop_extract_2000, federal_construction__cooperative_drift_reading, base_extractiveness, 55, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(fedcoop_suppress_1945, federal_construction__cooperative_drift_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(fedcoop_suppress_1975, federal_construction__cooperative_drift_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(fedcoop_suppress_2000, federal_construction__cooperative_drift_reading, suppression_requirement, 55, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federal_construction__cooperative_drift_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federal_construction__cooperative_drift_reading, federal_construction__bundesrat_entanglement).
narrative_ontology:affects_constraint(federal_construction__cooperative_drift_reading, federal_construction__lander_cultural_sovereignty).

% DUAL FORMULATION NOTE:
% The federal construction kernel has three structurally distinct constraint readings with different ε values. The cooperative_drift_reading (this file) emphasizes extractiveness through diffused accountability and suppressed competitive federalism (ε ≈ 0.52). The bundesrat_entanglement reading (downstream, higher ε) emphasizes administrative imprisonment via legislative entanglement (ε ≈ 0.62). The lander_cultural_sovereignty reading emphasizes protection rather than entanglement (ε ≈ 0.15, rope). These are not the same constraint viewed from different angles — they have substantially different ε values because they measure different structural mechanisms. The cooperation_drift reading measures accountability diffusion; the entanglement reading measures executive autonomy loss; the sovereignty reading measures cultural boundary protection. All three affect each other (network coupling) but are distinct constraint stories with distinct ε-invariance profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
