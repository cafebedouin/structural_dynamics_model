% ============================================================================
% CONSTRAINT STORY: ancient_constitutionalism__athenian_democratic_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ancient_constitutionalism__athenian_democratic_constitution, []).

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
 *   constraint_id: ancient_constitutionalism__athenian_democratic_constitution
 *   human_readable: Athenian Democratic Constitution: Citizen Self-Rule via Sortition and Rotation
 *   domain: political/historical/constitutional
 *
 * SUMMARY:
 *   The Athenian politeia is celebrated in Western constitutional tradition
 *   as the paradigm of citizen self-rule, a system that used sortition,
 *   rotation, and the assembled demos to prevent government from hardening
 *   into a separate ruling class. This narrative frames the constraint as a
 *   pure coordination mechanism that solved the central problem of political
 *   order: how to aggregate preferences and make collective decisions without
 *   creating permanent power monopolies. However, the structural reading
 *   reveals a tangled_rope: the constraint's actual mechanics depend entirely
 *   on the systematic extraction of labor and political agency from women,
 *   metics (resident foreigners), and the enslaved population. The constraint
 *   does prevent one form of hardening—the accumulation of magisterial
 *   office—but only by displacing extraction onto excluded populations whose
 *   labor finances the leisure that enables citizen participation. This story
 *   analyzes the Athenian democratic constitution as one reading of the
 *   ancient_constitutionalism kernel, distinct from the Roman republican
 *   reading, and traces how the constraint distributes costs and benefits
 *   across the citizen/non-citizen divide.
 *
 * KEY AGENTS:
 *   - Adult Male Citizens (Demos): Organized collective (organized/mobile) — primary beneficiary of sortition/rotation mechanism; experience constraint as coordination function solving tyranny problem
 *   - Women: Powerless/trapped — excluded from political participation; provide unpaid domestic and reproductive labor that sustains citizen leisure; bear suppression through legal status prohibition
 *   - Metics (Resident Foreigners): Powerless/trapped — produce wealth through commerce and manufacture; taxed, restricted in property ownership and political participation; legally subordinate despite economic contribution
 *   - Enslaved Population: Powerless/trapped — directly finance democratic institutions through labor in mines (Laurion silver), agriculture, and household service; subject to torture and death without legal protection; bear maximum suppression and extraction
 *   - Wealthy Landowners and Merchants: Powerful/constrained — benefit from extractive labor system but constrained by democratic participation requirements (liturgies, eisphorai); experience mixed coordination/extraction
 *   - Democratic Institutional Apparatus (Ekklesia, Dikasteria, Boule): Institutional actors (institutional/constrained) — genuine coordination function but subject to suppressive pressure from demos (graphe paranomon, penalties for dereliction); rapid turnover prevents accumulation but creates institutional instability
 *   - Later Democratic Tradition: Institutional observers (institutional/arbitrage) — cite Athens as legitimating narrative but have abandoned sortition/rotation for representative systems; maintain piton through rhetorical inheritance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ancient_constitutionalism__athenian_democratic_constitution, 0.62).
domain_priors:suppression_score(ancient_constitutionalism__athenian_democratic_constitution, 0.75).
domain_priors:theater_ratio(ancient_constitutionalism__athenian_democratic_constitution, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ancient_constitutionalism__athenian_democratic_constitution, extractiveness, 0.62).
narrative_ontology:constraint_metric(ancient_constitutionalism__athenian_democratic_constitution, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ancient_constitutionalism__athenian_democratic_constitution, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ancient_constitutionalism__athenian_democratic_constitution, tangled_rope).
narrative_ontology:human_readable(ancient_constitutionalism__athenian_democratic_constitution, "Athenian Democratic Constitution: Citizen Self-Rule via Sortition and Rotation").
narrative_ontology:topic_domain(ancient_constitutionalism__athenian_democratic_constitution, "political/historical/constitutional").

domain_priors:requires_active_enforcement(ancient_constitutionalism__athenian_democratic_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ancient_constitutionalism__athenian_democratic_constitution, '59861065-972e-41cf-9aef-6b20cfac262a').
narrative_ontology:cs_kernel_codification('59861065-972e-41cf-9aef-6b20cfac262a', formalized).
narrative_ontology:cs_authority_grounding('59861065-972e-41cf-9aef-6b20cfac262a', lineage).
narrative_ontology:cs_interpretation_layer_present('59861065-972e-41cf-9aef-6b20cfac262a').
narrative_ontology:cs_reading_relation('59861065-972e-41cf-9aef-6b20cfac262a', ancient_constitutionalism__roman_republican_constitution, coexists_with).
narrative_ontology:cs_axiom('59861065-972e-41cf-9aef-6b20cfac262a', foundational, sortition_prevents_permanent_rule).
narrative_ontology:cs_axiom_status(sortition_prevents_permanent_rule, holdable).
narrative_ontology:cs_axiom_grounding('59861065-972e-41cf-9aef-6b20cfac262a', sortition_prevents_permanent_rule, empirically_contingent).
narrative_ontology:cs_axiom('59861065-972e-41cf-9aef-6b20cfac262a', secondary, citizen_leisure_requires_excluded_labor).
narrative_ontology:cs_axiom_status(citizen_leisure_requires_excluded_labor, holdable).
narrative_ontology:cs_axiom_grounding('59861065-972e-41cf-9aef-6b20cfac262a', citizen_leisure_requires_excluded_labor, empirically_contingent).
narrative_ontology:cs_reference_frame('59861065-972e-41cf-9aef-6b20cfac262a', democratic_self_rule_via_sortition_and_assembly).
narrative_ontology:cs_drift_state('59861065-972e-41cf-9aef-6b20cfac262a', hellenistic_and_roman_imperial_erosion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('59861065-972e-41cf-9aef-6b20cfac262a', '').
narrative_ontology:cs_kernel_id(ancient_constitutionalism__athenian_democratic_constitution, ancient_constitutionalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ancient_constitutionalism__athenian_democratic_constitution, adult_male_citizens).
narrative_ontology:constraint_victim(ancient_constitutionalism__athenian_democratic_constitution, women).
narrative_ontology:constraint_victim(ancient_constitutionalism__athenian_democratic_constitution, metics).
narrative_ontology:constraint_victim(ancient_constitutionalism__athenian_democratic_constitution, enslaved_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ADULT MALE CITIZENS (ROPE) — Organized collective with mobile exit (emigration, ostracism as voluntary exile). Sortition and rotation prevent accumulation of power in any faction; the constraint solves the coordination problem of self-governance without professionalizing a ruling elite. Genuine coordination function with minimal coercive overhead from the perspective of the beneficiary population. The constraint appears as pure coordination because it serves the citizens' explicit interest in preventing tyranny.
constraint_indexing:constraint_classification(ancient_constitutionalism__athenian_democratic_constitution, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: WOMEN, METICS, AND THE ENSLAVED (SNARE) — Trapped populations with no exit. The Athenian democratic constitution extracts maximum value from these groups: women's unpaid domestic and reproductive labor sustains the leisure that enables male citizen participation; metics' economic production generates the wealth that funds democratic institutions and military capacity; enslaved persons' labor in mines (Laurion silver), agriculture, and domestic service directly finances the ekklesia, dikasteria, and fleet. The constraint has no coordination function for these groups — it is pure extraction masked by the ideology of democratic freedom. Suppression operates through legal prohibition (women and metics excluded from political participation), economic dependency (metics taxed and restricted), and violence (enslaved population subject to torture and death without legal recourse).
constraint_indexing:constraint_classification(ancient_constitutionalism__athenian_democratic_constitution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: WEALTHY LANDOWNERS AND MERCHANTS (TANGLED ROPE) — Constrained by democratic participation requirements (liturgies: expensive public service obligations; eisphorai: emergency wealth taxes). Sortition and rotation prevent these actors from dominating magistracies, but the wealth system (timarchy) gives them significant structural advantage. They benefit from the labor extraction of enslaved persons and metics (which generates the surplus that their property depends on), but the democratic framework constrains their ability to convert wealth into permanent political authority. Mixed experience: genuine coordination (liturgical system distributes the burden of state funding among the wealthy) combined with extraction (the demos can vote to redistribute wealth or ostracize the powerful).
constraint_indexing:constraint_classification(ancient_constitutionalism__athenian_democratic_constitution, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: DEMOCRATIC INSTITUTIONAL APPARATUS (TANGLED ROPE) — The ekklesia, dikasteria, boule, and magistracies have genuine coordination function: they aggregate preferences, deliberate, and allocate resources. But sortition and rotation create constant institutional instability — amateur governance with rapid turnover means no permanent bureaucracy accumulates expertise or can institutionalize procedural power. The apparatus experiences this as a mixed constraint: genuine coordination function (the machinery must work to reach decisions), but also suppressive pressure from the demos (penalties for dereliction, graphe paranomon lawsuits against improper proposals). The constraint maintains institutional flux to prevent hardening into permanent rule, but this flux is experienced as extractive pressure by the institutional actors themselves.
constraint_indexing:constraint_classification(ancient_constitutionalism__athenian_democratic_constitution, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the Athenian constitution might appear as an exemplification of a universal principle: that sortition and rotation are the only institutional mechanisms that prevent the hardening of power into a permanent ruling class. This is the implicit claim of the source material's framing — that the Athenian politeia discovered a natural law of politics. However, the structural data reveals this as a false summit: the constraint depends entirely on the labor extraction from women, metics, and the enslaved. Remove the extraction (give legal status to excluded populations), and the constraint collapses. The 'natural law' framing naturalizes the contingent institutional extraction.
constraint_indexing:constraint_classification(ancient_constitutionalism__athenian_democratic_constitution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: LATER DEMOCRATIC TRADITION (PITON) — Modern democracies claim inheritance from Athens but have abandoned sortition and rotation entirely, replacing them with competitive elections, professional bureaucracies, and party systems. The rituals of 'Athenian democracy' persist in constitutional discourse and civic education as a legitimating narrative, but the functional mechanism (lot and rotation) has atrophied and been replaced by representative institutions that exhibit exactly what Athens supposedly prevented: accumulation of office, professional ruling classes, and hardened power hierarchies. The theatrical citation of Athens in modern democratic ideology is a piton: the mechanism is no longer operative, yet the constraint persists in discourse and institutional identity, maintained by institutional inertia rather than function.
constraint_indexing:constraint_classification(ancient_constitutionalism__athenian_democratic_constitution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ancient_constitutionalism__athenian_democratic_constitution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ancient_constitutionalism__athenian_democratic_constitution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ancient_constitutionalism__athenian_democratic_constitution, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ancient_constitutionalism__athenian_democratic_constitution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ancient_constitutionalism__athenian_democratic_constitution, TR),
    TR >= 0.70.

:- end_tests(ancient_constitutionalism__athenian_democratic_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.62): Moderate-high. The constraint extracts from excluded populations at maximum intensity (women, metics, enslaved) but coordinates rather than extracts from citizen-beneficiaries. The aggregate extractiveness reflects that the system is genuinely coordinating for one group while being purely extractive for another. The measure reflects the weighted average: citizens represent ~12-15% of Athenian population; excluded populations represent ~85-88%. Even weighting for political impact (citizens are the pivot group), the net extractiveness is skewed toward high extraction because the mechanism's entire function depends on sustained labor appropriation. SUPPRESSION (0.75): High. Suppression operates through multiple channels: legal prohibition (women and metics excluded from political participation by law; enslaved population classified as property), economic dependency (metics taxed and restricted from property ownership; enslaved population given minimal subsistence), and violence (enslaved population subject to judicial torture—basianos—without protection; women's movement restricted by social norm enforced through honor/shame mechanisms; metics lack legal recourse for injury). The suppression is not total (metics can emigrate; citizens can choose exile through ostracism), but barriers are substantial. THEATER_RATIO (0.35): Low-moderate. The Athenian democratic apparatus is functionally operative—sortition genuinely prevents accumulation, assembly decisions have real force, dikasteria truly deliberate and decide. The theater is not the main operation. The theater component emerges in the later tradition (piton perspective) where democratic rhetoric persists without functional sortition.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces the sharpest perspectival gap in the corpus between beneficiary and victim populations. From the citizen perspective (organized/mobile), the constraint appears as rope: sortition and rotation solve the genuine coordination problem of preventing tyranny without concentrating power. From the excluded perspective (powerless/trapped), the constraint appears as snare: pure extraction masked by democratic ideology. The analytical observer at civilizational scope risks committing a false summit by treating 'sortition prevents permanent ruling class' as a natural law of political order, when the constraint is actually contingent on labor extraction from excluded populations. If labor extraction were eliminated (citizenship extended to all adults regardless of gender or legal status), the constraint would collapse in its current form, requiring either different coordination mechanisms or revealing the underlying extraction that sortition was concealing.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by each group's structural relationship to the extraction flow. Citizens (beneficiaries with organized power and mobile exit) derive d ≈ 0.40, yielding negative or near-zero f(d), producing low chi—they experience coordination, not extraction. Excluded populations (victims with powerless positions and trapped exit) derive d ≈ 0.90, yielding f(d) ≈ 1.28, producing high chi—they experience maximum extraction. Wealthy actors (both beneficiaries and subject to redistribution pressure) derive d ≈ 0.55, yielding tangled_rope characteristics. The piton perspective (institutional actors citing Athens in later tradition) derives d ≈ 0.50 from arbitrage exit (can exit the Athens mythology without consequence), experiencing low chi because the mechanism is no longer operative—the extraction is theatrical rather than structural.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that this constraint has two separable functions: (1) internal coordination among citizens (preventing citizen-faction tyranny via sortition/rotation), which is genuine coordination (rope); and (2) external extraction from excluded populations (women, metics, enslaved), which is pure extraction (snare). The constraint is NOT a snare from the citizen perspective; it is NOT rope from the enslaved perspective. The tangled_rope classification at the aggregate level (extractiveness 0.62, suppression 0.75, requires_active_enforcement true) is the correct synthesis. The mandatrophy-as-posed—'is this coordination or extraction?'—has no single answer because the answer depends on which population you measure. The constraint coordinates for citizens and extracts from non-citizens. The resolution is to declare both populations as victims AND beneficiaries of the SAME constraint, with directionality computations differentiating their experiences.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    slavery_extraction_structural_necessity,
    'Was the labor extraction from enslaved populations a structural necessity for the Athenian democratic system, or could democratic self-governance have functioned without slavery?',
    'Comparative analysis of non-slave democratic systems; economic modeling of Athenian citizen leisure requirements and alternative labor sources; historical counterfactual of free labor replacing enslaved labor',
    'If structural necessity: the constraint is inseparable from exploitation (extractiveness remains 0.62+ and suppression remains 0.75+). If contingent: the constraint could be reclassified as pure coordination (rope) if labor extraction were eliminated, revealing that the extraction was institutional choice, not structural requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slavery_extraction_structural_necessity, empirical, 'Whether slavery was structurally necessary for Athenian democracy').

omega_variable(
    metic_exclusion_justification,
    'What structural logic justified the exclusion of metics from political participation in a system nominally based on universal democratic principle?',
    'Analysis of metic legal status across time; correlation between metic economic contribution and suffrage debates; examination of inclusion/exclusion rationales in Athenian sources',
    'If justified by military/kinship logic: exclusion is rationally bounded within the reading''s internal framework. If arbitrary: exclusion reveals the constraint as a vehicle for an in-group''s power monopoly disguised as democratic principle, reclassifying it toward snare for all non-citizen perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metic_exclusion_justification, conceptual, 'Justification structure for metic political exclusion').

omega_variable(
    sortition_preservation_vs_wealth_hierarchy,
    'How did the Athenian system maintain the claim that sortition defeated power accumulation while simultaneously operating within a timarchy (wealth-based political influence)?',
    'Quantitative analysis of magistrate selection: correlation between wealth and lottery outcomes; examination of whether timarchs could circumvent sortition through patronage or prior nomination control; evidence of wealth-based de facto monopolization despite formal lot mechanism',
    'If sortition genuinely randomized: the constraint functioned as described (rope/tangled rope). If wealth corrupted the lot mechanism: the constraint is rhetoric concealing wealth-based rule (piton for the institutional perspective, snare for the broader view).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sortition_preservation_vs_wealth_hierarchy, empirical, 'Whether sortition was corrupted or circumvented by wealth hierarchy').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Does the Athenian democratic reading logically foreclose the Roman republican reading, or do they coexist as structurally viable alternatives within different political contexts?',
    'Comparative constitutional logic: does the claim ''sortition prevents permanent rule'' require rejecting the Roman claim ''distributed magistracies prevent permanent rule''? Or are both mechanisms valid responses to the same political problem (preventing tyranny), chosen based on different structural constraints (direct vs representative, slave vs client labor systems)?',
    'If forecloses: only one anti-tyranny mechanism can be normatively correct (one reading ejects the other from legitimacy). If coexists: both readings are live political choices dependent on labor, scale, and participation scope. This determines the cs_structure.reading_relations value and reveals whether the kernel contest is logical or contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Logical relationship between Athenian democratic and Roman republican readings').

omega_variable(
    democratic_legitimacy_grounding,
    'Is the Athenian democratic constitution''s legitimacy grounded in the intrinsic right of citizens to self-rule, or in the empirical claim that sortition/rotation functionally prevent tyranny?',
    'Textual analysis of Athenian democratic discourse (Pericles'' Funeral Oration, Assembly speeches, legal arguments); categorization of legitimacy claims by type (rights-based vs consequence-based vs procedural). Test whether the constraint persists if the empirical claim fails (i.e., if sortition demonstrably fails to prevent tyranny or if rotation empirically produces worse outcomes than stable leadership).',
    'If deontological (rights-based): the axiom is foreclosed only by value-change (society rejecting self-rule as intrinsic right). If empirically_contingent: the axiom is foreclosed by evidence showing sortition/rotation fail their stated purpose. This determines axiom grounding_type and feeds T17 abductive triggers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_legitimacy_grounding, conceptual, 'Legitimacy grounding of democratic constitution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ancient_constitutionalism__athenian_democratic_constitution, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(athenian_theater_t0, ancient_constitutionalism__athenian_democratic_constitution, theater_ratio, 0, 0.2).
narrative_ontology:measurement(athenian_theater_t1, ancient_constitutionalism__athenian_democratic_constitution, theater_ratio, 1, 0.28).
narrative_ontology:measurement(athenian_theater_t2, ancient_constitutionalism__athenian_democratic_constitution, theater_ratio, 2, 0.35).

% Extraction over time
narrative_ontology:measurement(athenian_extractiveness_t0, ancient_constitutionalism__athenian_democratic_constitution, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(athenian_extractiveness_t1, ancient_constitutionalism__athenian_democratic_constitution, base_extractiveness, 1, 0.6).
narrative_ontology:measurement(athenian_extractiveness_t2, ancient_constitutionalism__athenian_democratic_constitution, base_extractiveness, 2, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(athenian_suppression_t0, ancient_constitutionalism__athenian_democratic_constitution, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(athenian_suppression_t1, ancient_constitutionalism__athenian_democratic_constitution, suppression_requirement, 1, 0.73).
narrative_ontology:measurement(athenian_suppression_t2, ancient_constitutionalism__athenian_democratic_constitution, suppression_requirement, 2, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ancient_constitutionalism__athenian_democratic_constitution, identity_coordination).
narrative_ontology:affects_constraint(ancient_constitutionalism__athenian_democratic_constitution, ancient_constitutionalism__roman_republican_constitution).

% DUAL FORMULATION NOTE:
% The Athenian democratic constitution and the Roman republican constitution are sibling readings of the ancient_constitutionalism kernel, not separate constraints. However, they have different ε values (Athenian ≈ 0.62 tangled_rope due to embedded extraction; Roman ≈ 0.48 tangled_rope due to class balance with different suppression mechanisms) and different constraint families. The structural delta is the labor-extraction mechanism: Athens displaces extraction onto gender/legal status exclusion; Rome displaces extraction onto client-patronage hierarchies. These are distinct constraints with distinct beneficiary/victim structures, linked by the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
