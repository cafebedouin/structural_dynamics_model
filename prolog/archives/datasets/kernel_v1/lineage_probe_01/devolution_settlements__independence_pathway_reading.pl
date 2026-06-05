% ============================================================================
% CONSTRAINT STORY: devolution_settlements__independence_pathway_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_devolution_settlements__independence_pathway_reading, []).

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
 *   constraint_id: devolution_settlements__independence_pathway_reading
 *   human_readable: Devolution Settlements as Independence Pathway (Constitutional-Route Reading)
 *   domain: constitutional_law/sovereignty_doctrine
 *
 * SUMMARY:
 *   The devolution settlement (Scotland Act 1998) was designed as a
 *   constitutional closure: devolve significant domestic power, satisfy the
 *   demand for home rule and national recognition, eliminate the independence
 *   question by giving Scotland institutional voice within the union. But the
 *   institutional structures created — a national Scottish Parliament with
 *   direct democratic legitimacy, a referendum precedent established in 2014,
 *   and a standing mandate argument that reads electoral pro-independence
 *   majorities as claims to another referendum — transformed the settlement
 *   into an independence pathway. This reading treats the constraint not as a
 *   finality device (which is how unionists read it) but as a structural
 *   mechanism that renews the independence question with every electoral
 *   cycle. The constraint exhibits asymmetric extraction: the independence
 *   movement benefits from the institutional platform and renewable electoral
 *   claims; the unionist finality claim becomes the victim, unable to enforce
 *   the settlement's closure. This is one reading of a contested kernel — the
 *   devolution settlements themselves — that also admits reserved-powers and
 *   Sewel-strain readings. Each reading instantiates a different constraint
 *   with different beneficiaries and victims. This reading takes the
 *   independence pathway as the core institutional dynamic: devolution built
 *   the exit's anteroom.
 *
 * KEY AGENTS:
 *   - Constitutional-Route Independence Movement: Primary beneficiary (organized/constrained) — benefits from Scottish Parliament platform, electoral cycles as renewal mechanism, standing mandate argument as legitimacy claim
 *   - Unionist Constitutional Finality Claims: Primary victim (powerless/trapped) — the settlement's design goal (to provide permanent closure) becomes unenforceable; finality is contested with each election
 *   - Scottish Parliament: Institutional actor (institutional/constrained) — operates within the settlement's powers but also serves as platform for independence claims; constrained by Westminster's retained supremacy
 *   - Westminster Parliament: Institutional actor (institutional/arbitrage) — retains legal right to refuse referendum, can arbitrage between granting and refusing consent, but facing accumulated pressure from elected majorities in subordinate parliament
 *   - Unionist Political Establishment: Powerful actor (powerful/constrained) — benefits from settlement in principle (Scotland within union) but faces extraction mechanism (renewed independence demands) that they cannot eliminate without revoking devolution
 *   - Electoral Cycles (Scottish Parliament): Meta-agent representing the institutional mechanism — every election with pro-independence majority renews the mandate claim and the extractive cycle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(devolution_settlements__independence_pathway_reading, 0.58).
domain_priors:suppression_score(devolution_settlements__independence_pathway_reading, 0.48).
domain_priors:theater_ratio(devolution_settlements__independence_pathway_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(devolution_settlements__independence_pathway_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(devolution_settlements__independence_pathway_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(devolution_settlements__independence_pathway_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(devolution_settlements__independence_pathway_reading, tangled_rope).
narrative_ontology:human_readable(devolution_settlements__independence_pathway_reading, "Devolution Settlements as Independence Pathway (Constitutional-Route Reading)").
narrative_ontology:topic_domain(devolution_settlements__independence_pathway_reading, "constitutional_law/sovereignty_doctrine").

domain_priors:requires_active_enforcement(devolution_settlements__independence_pathway_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(devolution_settlements__independence_pathway_reading, '88954834-0158-4b43-b801-81b06950b4a9').
narrative_ontology:cs_kernel_codification('88954834-0158-4b43-b801-81b06950b4a9', formalized).
narrative_ontology:cs_authority_grounding('88954834-0158-4b43-b801-81b06950b4a9', lineage).
narrative_ontology:cs_interpretation_layer_present('88954834-0158-4b43-b801-81b06950b4a9').
narrative_ontology:cs_reading_relation('88954834-0158-4b43-b801-81b06950b4a9', devolution_settlements__reserved_powers_model_reading, coexists_with).
narrative_ontology:cs_reading_relation('88954834-0158-4b43-b801-81b06950b4a9', devolution_settlements__sewel_strain_reading, influences).
narrative_ontology:cs_axiom('88954834-0158-4b43-b801-81b06950b4a9', foundational, devolution_enables_constitutional_revision).
narrative_ontology:cs_axiom_status(devolution_enables_constitutional_revision, holdable).
narrative_ontology:cs_axiom_grounding('88954834-0158-4b43-b801-81b06950b4a9', devolution_enables_constitutional_revision, instrumental).
narrative_ontology:cs_axiom('88954834-0158-4b43-b801-81b06950b4a9', secondary, mandate_renewal_through_electoral_cycles).
narrative_ontology:cs_axiom_status(mandate_renewal_through_electoral_cycles, holdable).
narrative_ontology:cs_axiom_grounding('88954834-0158-4b43-b801-81b06950b4a9', mandate_renewal_through_electoral_cycles, conventional).
narrative_ontology:cs_reference_frame('88954834-0158-4b43-b801-81b06950b4a9', devolution_as_subordinate_permanent_structure).
narrative_ontology:cs_drift_state('88954834-0158-4b43-b801-81b06950b4a9', contemporary_post_2016_brexit_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('88954834-0158-4b43-b801-81b06950b4a9', '').
narrative_ontology:cs_kernel_id(devolution_settlements__independence_pathway_reading, devolution_settlements).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(devolution_settlements__independence_pathway_reading, constitutional_route_independence_movement).
narrative_ontology:constraint_victim(devolution_settlements__independence_pathway_reading, unionist_constitutional_finality_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNIONIST CONSTITUTIONAL FINALITY (SNARE) — The devolution settlement was designed to provide permanent constitutional closure: devolve power, satisfy the demand for home rule, eliminate the independence question. But the institutional structures created (Scottish Parliament, referendum precedent, electoral cycles with standing mandate argument) make that finality impossible to enforce. Trapped in the constraint because the victim set (the claim that the union was settled) cannot exit or organize; the extraction is the renewable contestation of what was meant to be final.
constraint_indexing:constraint_classification(devolution_settlements__independence_pathway_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CONSTITUTIONAL-ROUTE INDEPENDENCE MOVEMENT (TANGLED ROPE) — Benefits from the institutional structures the settlement created: a national parliament with electoral legitimacy, a referendum precedent (2014) that established the mechanism, and the standing mandate argument that turns every Scottish Parliament election with pro-independence majority into a claim to hold another referendum. Also faces constraints: UK Parliament can refuse, Westminster's legal supremacy remains, electoral cycles constrain timing. Mixed: genuine coordination function (elections as legitimate expression of preference) plus asymmetric extraction (movement renews the independence question each cycle, wearing down unionist closure).
constraint_indexing:constraint_classification(devolution_settlements__independence_pathway_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: WESTMINSTER PARLIAMENT (ROPE) — From Westminster's immediate/institutional view, the settlement is coordination: devolve domestic policy, retain foreign affairs and fiscal authority, manage Scotland as a subordinate legislature. The 2014 referendum was a safety valve, a coordination mechanism for processing demands. Westminster can arbitrage: grant or withhold referendum consent depending on political calculation. Experiences the constraint as managing a devolved polity, not as extraction.
constraint_indexing:constraint_classification(devolution_settlements__independence_pathway_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: RESERVED POWERS DOCTRINE (PITON) — From a civilizational/institutional view, the reserved-powers framing (everything not reserved is devolved) is itself a performative commitment. Westminster formally reserves sovereignty but has incrementally devolved power over 25 years. The doctrine persists through institutional inertia — it is how the UK imagines constitutional restraint — but the actual restraint has eroded. Theater ratio captures this: the formal doctrine remains, but the practice has drifted toward something closer to federalism without ever admitting the shift. Classification reflects institutional maintenance of outdated framing.
constraint_indexing:constraint_classification(devolution_settlements__independence_pathway_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: UNIONIST POLITICAL ESTABLISHMENT (TANGLED ROPE) — Benefit from the settlement: maintaining the union, managing Scotland from the center, using devolution to prevent independence. Face constraints: cannot reverse devolution without triggering crisis, cannot prevent electoral cycles from generating pro-independence majorities, cannot stop referendum demand renewal. Classification reflects both the coordination function (devolving to stabilize) and the extraction mechanism (denying consent when it would be granted, using electoral legitimacy to refuse legitimacy).
constraint_indexing:constraint_classification(devolution_settlements__independence_pathway_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal legal perspective, the constraint appears as an immutable feature of constitutional federalism: when power is devolved, the subordinate unit's legitimacy to challenge the partition is established de jure. The devolving parliament cannot prevent this without revoking devolution. This reading treats the constraint as a logical law of constitutional design itself. However, the structural data contradicts the mountain classification — this is a false summit, naturalizing the contingent institutional choice (to devolve) as if it were a law of nature.
constraint_indexing:constraint_classification(devolution_settlements__independence_pathway_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(devolution_settlements__independence_pathway_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(devolution_settlements__independence_pathway_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(devolution_settlements__independence_pathway_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(devolution_settlements__independence_pathway_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(devolution_settlements__independence_pathway_reading, TR),
    TR >= 0.70.

:- end_tests(devolution_settlements__independence_pathway_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the asymmetric benefit distribution and the renewable nature of the extraction mechanism. The independence movement benefits from institutional platform and legitimacy to make independence claims; unionists bear the cost of constitutional unsettlement and the inability to enforce finality. Extractiveness is not static but cycles with elections — it rises when pro-independence majorities emerge, falls when unionist majorities gain power, but the underlying structure (the platform + the precedent + the mandate argument) remains. Suppression (0.48): Moderate. The barrier to independence is Westminster's legal supremacy and retained veto power — structural and real, but not total. Elections are free and fair; the independence movement can organize, campaign, and win parliamentary majorities. Suppression operates through constitutional hierarchy, not through coercion. Theater ratio (0.62): Moderate-high. The standing mandate argument is performative in a specific sense: elections are framed as referenda on independence, but the mechanism for acting on that mandate (a second referendum) is blocked by Westminster. The mandate claim becomes theatrical — it asserts authority without the mechanism to implement it. The performativity is not in the elections themselves (which are functional) but in the claim that electoral results authorize something (another referendum) that Westminster can refuse.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. Unionists see the settlement as designed closure — their reading (reserved_powers_model_reading) emphasizes Westminster's retained sovereignty and the devolved parliament's subordinate status. From that reading, the independence pathway is a misinterpretation or deliberate distortion of the settlement's intent. The independence movement reads the settlement as a structural mechanism — the existence of a national parliament with democratic legitimacy, the precedent of the 2014 referendum, and the standing mandate argument combine to establish a procedural pathway to independence. These are incompatible readings of the same kernel. The analytical observer risks naturalizing one reading over the other by treating one as 'the correct' constitutional law and the other as 'misreading' or 'pushing the boundary.' In fact, both readings are structurally available within the kernel's ambiguous text. The kernel (the Scotland Acts, the devolution settlement) underdetermines which reading is correct. This perspectival gap is not a difference in empirical belief but a difference in constitutional framing: whether the settlement's purpose was finality (unionist reading) or whether its structure enables renewal (independence reading).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality (d) is derived from beneficiary/victim declarations plus exit options. The independence movement is declared as beneficiary with exit_options: constrained — they benefit from the platform and the institutional mechanism, but they face Westminster's legal veto and cannot unilaterally exercise the right to independence. The unionist finality claims are declared as victim — they are the thing that the extraction mechanism targets. The beneficiary's d is low (they benefit, so extraction runs toward them); the victim's d is high (they bear costs, so extraction runs toward and through them). The constraint exhibits the core structure of a tangled rope: genuine coordination function (Scottish Parliament, elections, democratic expression) plus asymmetric extraction (the mechanism for independence is built in, but finality is rendered impossible). From Westminster's institutional perspective (immediate/arbitrage), d is derived from beneficiary status (Westminster retains control) plus high exit options (Westminster can grant or withhold referendum, can arbitrage between elections). From the unionist establishment's perspective (powerful/constrained), d is higher — they face the extraction mechanism (renewed demands) but cannot exit (cannot eliminate devolution without triggering larger crisis).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_threshold_ambiguity,
    'What threshold of pro-independence electoral representation constitutes a ''standing mandate'' for a new referendum? A single-election majority? Sustained majority? Super-majority?',
    'Examination of precedent (2014 setup); comparison with other constitutional democracies'' secession thresholds; Westminster vs Scottish Parliament''s competing interpretations of democratic legitimacy',
    'If threshold is single election: mandate renews every 5 years, extraction mechanism is continuous. If threshold requires sustained majority or super-majority: extraction is periodic and higher-barrier, weaker alternating power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_threshold_ambiguity, conceptual, 'Definition of ''standing mandate'' for referendum demand').

omega_variable(
    scottish_parliament_legitimacy_status,
    'Is the Scottish Parliament a subordinate legislature (devolved from Westminster) or a co-equal parliament (part of a federal structure)?',
    'Constitutional framing analysis; comparison of actual powers and independence of revenue-raising; legal text (Scotland Act definitions) vs practice (fiscal autonomy, welfare devolution trajectory); international constitutional law precedent on co-equal vs subordinate legislatures',
    'If subordinate: Westminster can deny referendum indefinitely (Snare from independence perspective). If co-equal: Scotland''s self-determination claim has independent standing (Rope from independence perspective). This distinction determines whether the constraint is extractive or coordinative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scottish_parliament_legitimacy_status, conceptual, 'Legitimacy status of Scottish Parliament: subordinate vs co-equal').

omega_variable(
    referendum_precedent_binding_status,
    'Does the 2014 referendum set a binding constitutional precedent that Westminster must honor future referendum requests when a mandate is demonstrated, or is it a one-time exceptional event?',
    'Analysis of the 2014 agreement (Edinburgh Agreement) language; Westminster statements before and after 2014; current legal challenges to referendum authority; comparison with other precedent-setting constitutional moments (Brexit referendum)',
    'If binding precedent: the independence movement has a legitimate claim to renewal (Rope/Tangled Rope from their perspective). If one-time exception: Westminster can refuse indefinitely, and the precedent is performative theater (Piton/Snare for the movement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(referendum_precedent_binding_status, conceptual, 'Whether 2014 referendum sets binding constitutional precedent').

omega_variable(
    suppression_mechanism_locus,
    'Is suppression of independence renewal structural (Westminster''s legal supremacy and veto power) or internalized (Scottish electorate''s normative commitment to parliamentary process and acceptance of Westminster''s sovereignty)?',
    'Post-referendum behavior analysis: if Westminster refuses consent and compliance is maintained without mass civil disobedience, suppression is partly internalized. Survey data on Scottish public''s acceptance of Westminster''s right to refuse. Comparison with other subordinate polities'' compliance patterns.',
    'If structural: suppression should decline with power transfer or legal change (Scottish independence referendum legally enabled would reduce suppression to near-zero). If internalized: suppression persists even with formal power transfer (normative acceptance of Westminster''s right to decide).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_locus, empirical, 'Whether suppression of independence is structural or internalized').

omega_variable(
    devolution_asymmetry_trajectory,
    'Is devolution converging toward federalism (equal partners in a dual polity) or diverging (Westminster retaining ultimate authority, Scotland increasingly frustrated)?',
    'Measurement of fiscal autonomy, welfare authority, and constitutional language over time; polling on Scottish identity and UK attachment; Westminster''s rhetorical framing (subordinate devolution vs federal language); comparative analysis with actual federal systems',
    'If converging to federalism: extraction mechanism will weaken (coexistence of equal legitimacies reduces asymmetry). If diverging: extraction mechanism will strengthen (frustration + blocked pathways → radicalization). This determines whether the constraint is stabilizing or destabilizing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devolution_asymmetry_trajectory, empirical, 'Trajectory of devolution toward federalism vs subordinate equilibrium').

omega_variable(
    kernel_reading_underdetermination,
    'Is the independence pathway reading a legitimate reading of the devolution kernel, or is it a distortion of the kernel''s original intent?',
    'Historical analysis of the 1997-1999 devolution debates; statements by architects (Secretary of State for Scotland, constitutional framers); comparative legal analysis of reserved-powers structures elsewhere; interpretation of the Scotland Acts'' language in context of original debates vs contemporary legal doctrine',
    'If legitimate reading: the independence pathway is built into the kernel''s structure (institutional path to challenge the settlement). If distortion: the independence movement is misinterpreting institutional structures designed to prevent independence (the settlement was meant to foreclose independence, but the movement interprets it as providing a mechanism for independence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether independence pathway is a legitimate or distorted reading of devolution kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(devolution_settlements__independence_pathway_reading, 1997, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(devol_indep_tr_t0, devolution_settlements__independence_pathway_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(devol_indep_tr_t10, devolution_settlements__independence_pathway_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(devol_indep_tr_t15, devolution_settlements__independence_pathway_reading, theater_ratio, 15, 0.68).
narrative_ontology:measurement(devol_indep_tr_t20, devolution_settlements__independence_pathway_reading, theater_ratio, 20, 0.62).

% Extraction over time
narrative_ontology:measurement(devol_indep_be_t0, devolution_settlements__independence_pathway_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(devol_indep_be_t5, devolution_settlements__independence_pathway_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(devol_indep_be_t10, devolution_settlements__independence_pathway_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(devol_indep_be_t15, devolution_settlements__independence_pathway_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(devol_indep_be_t20, devolution_settlements__independence_pathway_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(devol_indep_be_t25, devolution_settlements__independence_pathway_reading, base_extractiveness, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(devolution_settlements__independence_pathway_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(devolution_settlements__independence_pathway_reading, 0.25).
narrative_ontology:affects_constraint(devolution_settlements__independence_pathway_reading, devolution_settlements__reserved_powers_model_reading).
narrative_ontology:affects_constraint(devolution_settlements__independence_pathway_reading, devolution_settlements__sewel_strain_reading).
narrative_ontology:affects_constraint(devolution_settlements__independence_pathway_reading, scottish_independence_referendum_precedent).
narrative_ontology:affects_constraint(devolution_settlements__independence_pathway_reading, westminster_devolution_authority_limit).

% DUAL FORMULATION NOTE:
% The independence_pathway_reading is one of three structurally distinct constraints that decompose the contested devolution_settlements kernel. The reserved_powers_model_reading emphasizes Westminster's retained sovereignty (ε≈0.25, Mountain from analytical perspective, coordination from unionist perspective). The sewel_strain_reading focuses on the Brexit stress-test and reveals Westminster's practical enforcement of retained authority (ε≈0.52, Tangled Rope). This reading (independence_pathway_reading) treats the independence renewal mechanism as the core extraction dynamic (ε≈0.58, Tangled Rope). All three share the same underlying institutional structures (the Scotland Acts) but instantiate different constraints by foregrounding different structural dynamics. The three readings are not compatible as a single constraint; each is a complete, ε-invariant story. Declare all three as separate JSON files and link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(devolution_settlements__independence_pathway_reading, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
