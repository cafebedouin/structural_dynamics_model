% ============================================================================
% CONSTRAINT STORY: soviet_constitution_1936__federal_fiction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_soviet_constitution_1936__federal_fiction, []).

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
 *   constraint_id: soviet_constitution_1936__federal_fiction
 *   human_readable: Soviet Federation: The 1936 Constitution's Secession Clause as Structural Trap
 *   domain: political/historical/constitutional
 *
 * SUMMARY:
 *   The 1936 Soviet Constitution presents a federal structure with explicit
 *   rights of secession that no republic could invoke without strategic
 *   suicide. This is the federal_fiction reading: the constitution names an
 *   exit option (Article 17: union republics possess the right to secede)
 *   while the apparatus makes that exit lethal. The constraint extracts from
 *   republics and autonomous regions through suppression of a named exit. The
 *   beneficiary is the All-Union center's ability to present the USSR as a
 *   voluntary federation while operating an apparatus that forecloses
 *   voluntarism. The reading treats the federal form as intentional theater —
 *   a legitimacy mechanism that requires the text/apparatus split to
 *   function. The secession clause is not a failed promise; it is a designed
 *   trap that makes suppression appear as the republic's own irrational
 *   choice rather than the center's coercion.
 *
 * KEY AGENTS:
 *   - Union Republics (Ukraine, Byelorussia, Transcaucasian SFSR, etc.): Primary victims (powerless/trapped) — named as federal entities with constitutions and borders, but secession right is structurally lethal
 *   - Autonomous Republics and Autonomous Oblasts: Secondary victims (moderate/constrained) — lower formal autonomy status, similar suppression apparatus, implicit foreclosure of any autonomy assertion
 *   - Moscow Center / All-Union Leadership: Primary beneficiary (institutional/arbitrage) — gains legitimacy from federal form, coordination apparatus, and suppression machinery all in one structure
 *   - Communist Party Apparatus: Institutional co-beneficiary (organized/arbitrage) — operates the suppression machinery parallel to the constitutional text, maintaining duality that prevents republics from appealing to either structure
 *   - Soviet State (Legitimacy Apparatus): Institutional beneficiary (institutional/arbitrage) — the federal form solves the legitimacy problem of post-imperial succession; the text provides credentials, the apparatus provides control
 *   - Constitutional Text as Artifact: Piton perspective (institutional/arbitrage) — maintained through institutional inertia despite functional degradation; persists in successive constitutions (1977, 1991) because removing it is administratively disruptive
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(soviet_constitution_1936__federal_fiction, 0.68).
domain_priors:suppression_score(soviet_constitution_1936__federal_fiction, 0.92).
domain_priors:theater_ratio(soviet_constitution_1936__federal_fiction, 0.88).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(soviet_constitution_1936__federal_fiction, extractiveness, 0.68).
narrative_ontology:constraint_metric(soviet_constitution_1936__federal_fiction, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(soviet_constitution_1936__federal_fiction, theater_ratio, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(soviet_constitution_1936__federal_fiction, snare).
narrative_ontology:human_readable(soviet_constitution_1936__federal_fiction, "Soviet Federation: The 1936 Constitution's Secession Clause as Structural Trap").
narrative_ontology:topic_domain(soviet_constitution_1936__federal_fiction, "political/historical/constitutional").

domain_priors:requires_active_enforcement(soviet_constitution_1936__federal_fiction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(soviet_constitution_1936__federal_fiction, '84bf46f7-9851-418a-9950-125141640130').
narrative_ontology:cs_kernel_codification('84bf46f7-9851-418a-9950-125141640130', formalized).
narrative_ontology:cs_authority_grounding('84bf46f7-9851-418a-9950-125141640130', extraction).
narrative_ontology:cs_interpretation_layer_present('84bf46f7-9851-418a-9950-125141640130').
narrative_ontology:cs_reading_relation('84bf46f7-9851-418a-9950-125141640130', soviet_constitution_1936__party_state_duality, influences).
narrative_ontology:cs_reading_relation('84bf46f7-9851-418a-9950-125141640130', soviet_constitution_1936__rights_catalog_facade, coexists_with).
narrative_ontology:cs_reading_relation('84bf46f7-9851-418a-9950-125141640130', soviet_constitution_1936__terror_coincidence, coexists_with).
narrative_ontology:cs_axiom('84bf46f7-9851-418a-9950-125141640130', foundational, secession_clause_intentional_apparatus_trap).
narrative_ontology:cs_axiom_status(secession_clause_intentional_apparatus_trap, holdable).
narrative_ontology:cs_axiom_grounding('84bf46f7-9851-418a-9950-125141640130', secession_clause_intentional_apparatus_trap, empirically_contingent).
narrative_ontology:cs_axiom('84bf46f7-9851-418a-9950-125141640130', secondary, federal_form_legitimacy_extraction_coupling).
narrative_ontology:cs_axiom_status(federal_form_legitimacy_extraction_coupling, holdable).
narrative_ontology:cs_axiom_grounding('84bf46f7-9851-418a-9950-125141640130', federal_form_legitimacy_extraction_coupling, instrumental).
narrative_ontology:cs_reference_frame('84bf46f7-9851-418a-9950-125141640130', voluntary_federal_union_framing).
narrative_ontology:cs_drift_state('84bf46f7-9851-418a-9950-125141640130', contemporary_cold_war_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('84bf46f7-9851-418a-9950-125141640130', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(soviet_constitution_1936__federal_fiction, soviet_constitution_1936).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(soviet_constitution_1936__federal_fiction, moscow_center).
narrative_ontology:constraint_beneficiary(soviet_constitution_1936__federal_fiction, nationality_form_legitimacy).
narrative_ontology:constraint_victim(soviet_constitution_1936__federal_fiction, union_republics).
narrative_ontology:constraint_victim(soviet_constitution_1936__federal_fiction, autonomous_republics).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNION REPUBLIC (SNARE) — A republic reading Article 17 sees an explicit constitutional right of secession. In practice, invoking it is suicide: economic blockade, military intervention, execution of leadership. The clause exists as pure theater — naming an exit that is structurally foreclosed. The republic is trapped: the text grants exit while suppression machinery makes exit lethal. Maximum experienced extraction — the constraint extracts the republic's sovereignty while the constitution claims to guarantee it.
constraint_indexing:constraint_classification(soviet_constitution_1936__federal_fiction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: AUTONOMOUS REPUBLIC / MINORITY NATION (SNARE) — Lower formal status than union republics but still nominally possessing territorial autonomy and internal self-governance. The secession clause does not formally apply, but the implicit message is clear: attempting autonomy assertion or exit would meet the same apparatus as a union republic's secession attempt. Constrained rather than trapped — autonomous republics have less explicit legal basis for exit, but the experience of suppression is comparable. High extraction with slightly lower perceived lethality creates 'constrained' exit assessment.
constraint_indexing:constraint_classification(soviet_constitution_1936__federal_fiction, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: MOSCOW CENTER (TANGLED ROPE) — From the perspective of All-Union leadership, the federal structure coordinates genuine functions: resource allocation across republics, national defense, industrial planning at scale. The federation is not pure extraction — it does solve coordination problems. But the suppression of exit options (the secession clause is a trap, not a real option) generates asymmetric extraction. Moscow benefits from both coordination and the credibility of 'voluntary union' without the cost of actual voluntarism. Institutional/arbitrage actors experience this as mixed coordination and controlled asymmetry — they can exit the coordinating relationship (through Moscow's power to reallocate or restructure) while others cannot.
constraint_indexing:constraint_classification(soviet_constitution_1936__federal_fiction, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: SOVIET STATE / LEGITIMACY APPARATUS (ROPE) — At the generational timescale, the 1936 Constitution serves its primary function: the Soviet system needs legitimacy credentials in a post-imperial, post-monarchical world. A federal structure with enumerated rights appears to solve this — the USSR looks like a modern constitutional state rather than a colonial empire. From this legitimacy perspective, the federal form IS the coordination mechanism. The extraction (the suppression of actual exit) is secondary to the coordination problem it solves: presenting the USSR as a voluntary union of equal republics. The state apparatus experiences this primarily as successful legitimacy work, not as extraction apparatus.
constraint_indexing:constraint_classification(soviet_constitution_1936__federal_fiction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: CONSTITUTIONAL TEXT AS PITON (PITON) — Over civilizational time, the 1936 Constitution becomes an institutionalized artifact maintained by inertia. The secession clause persists in every iteration (Stalin Constitution, Brezhnev Constitution, even the 1991 Russian Federation constitution contains vestigial federalism language). The text is theater maintaining itself through ritual — nobody believes Article 17 is real, but removing it would be administratively disruptive and symbolically explicit. The theater ratio is extreme (0.88) — the entire federal form is performative, but it persists because replacing it requires political work. At civilizational scale, Piton classification captures the degradation of federal function into institutional theater.
constraint_indexing:constraint_classification(soviet_constitution_1936__federal_fiction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZING VIEW (MOUNTAIN) — From a civilizational/universal perspective, one reading is that all large states require some fiction of voluntary association; the gap between constitutional text and suppressive practice is inherent to state capacity at continental scale. Federal forms are an unavoidable feature of governing large territories with distinct populations. This perspective risks naturalizing the contradiction as a law of political organization — 'federalism always requires some theater, some suppression of exit, some contradiction between text and apparatus.' The engine's false summit detector identifies this as naturalization of what is actually a contingent extractive choice, not an immutable property of federalism.
constraint_indexing:constraint_classification(soviet_constitution_1936__federal_fiction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(soviet_constitution_1936__federal_fiction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(soviet_constitution_1936__federal_fiction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(soviet_constitution_1936__federal_fiction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(soviet_constitution_1936__federal_fiction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(soviet_constitution_1936__federal_fiction, TR),
    TR >= 0.70.

:- end_tests(soviet_constitution_1936__federal_fiction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The All-Union center extracts the republics' sovereignty through the structure of named/foreclosed exit. The measurement trajectory (0.58 → 0.64 → 0.68 over 1936-1940 interval) shows extractiveness increasing as the Terror intensifies and the practical enforcement of the secession clause's lethality becomes undeniable. The 0.68 value reflects that extractiveness is substantial but not maximum (0.95+) — some republics retain nominal autonomy, some cultural institutions persist, some coordination functions are genuine. But the core extraction is absolute: republics cannot exercise the named right. Suppression (0.92): Extreme. The apparatus makes the named exit lethal through execution of leadership, economic blockade, military occupation, and the purge. The measurement trajectory (0.85 → 0.90 → 0.92 over 1936-1940) captures the intensification of the Terror as the regime's response to any perceived threat to the federation's integrity. At 0.92, suppression is nearly total — the only way to 'exit' is through death or exile. Theater ratio (0.88): Very high. The entire federal form is performative. The elaborate constitutional structure — named republics, borders, constitutions, secession rights — exists primarily to present the USSR as a voluntary federation rather than to enable actual federal functions. The ritual of constitutional acclamation (December 1936), the ceremonial roles of republics in propaganda, and the persistent invocation of 'Soviet federalism' in international diplomacy are pure theater. The 0.88 value reflects that most of the federal apparatus is theater maintained through institutional inertia; some genuine coordination of resources and defense does occur, but it is subordinate to the legitimacy function.
 *
 * PERSPECTIVAL GAP:
 *   The federal_fiction reading generates a maximal perspectival gap. The union republic sees a trap (Snare) — the text grants exit while suppression makes exit lethal. Moscow center sees coordination with controlled asymmetry (Tangled Rope) — genuine federation functions alongside suppression of alternatives. The legitimacy apparatus sees successful constitutional form (Rope) — the federal structure solves the post-imperial legitimacy problem. The constitutional text at civilizational scale appears as degraded institution (Piton) — the form persists through inertia despite functional hollowing. The analytical observer risks seeing this as an inherent property of federalism (Mountain) — that all large states require some fiction of voluntary association. The engine's false summit detector identifies this naturalization: the federal trap is not inherent to federalism, but rather a specific extractive choice made by the Soviet regime.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's power position, exit options, and relationship to the extraction flow. The union republic (powerless/trapped) experiences maximum directionality toward victimhood (d ≈ 0.95): trapped with no alternatives, bearing full cost of suppression. Moscow center (institutional/arbitrage) experiences beneficiary directionality (d ≈ 0.10): can reallocate structure, command apparatus, and exit from any coordination commitment. The autonomous republic (moderate/constrained) experiences high-victim directionality (d ≈ 0.85): lower formal autonomy, but similarly trapped by suppression apparatus. The legitimacy apparatus (institutional/arbitrage) experiences beneficiary directionality: the constitutional form is their tool, not their constraint. The analytical observer (analytical/analytical) neutralizes directionality — sees the structure from outside all power relationships, risking false equanimity in the face of extreme asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The Snare classification resolves the mandatrophy by identifying the constraint as pure extraction with minimal coordination function. The federal form appears to coordinate (it does allocate resources, command defense, regulate commerce) but these functions are subordinate to the extraction mechanism (suppression of the named exit). The Tangled Rope perspective from Moscow center suggests genuine coordination, but the structural data reveals asymmetry: Moscow can reallocate or exit the coordinating relationship; republics cannot. The Rope perspective from the legitimacy apparatus suggests the federal form is pure coordination, but the suppression (0.92) reveals extraction. The false summit (Mountain) perspective naturalizes the contradiction as inherent to federalism, but the federal_fiction reading exposes it as a designed apparatus. The Piton perspective captures the constraint's degradation into pure theater at civilizational scale. No single type 'dominates' — the structure is truly a snare from the perspective of those suppressed, mixed from the perspective of those benefiting, and degraded theater from the perspective of the institutional artifact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secession_clause_intentionality,
    'Was the secession clause in Article 17 a genuine offer (text predating the apparatus) or intentional theater (drafted as trap)?',
    'Historical analysis of drafting debates (1935-1936); comparison with Lenin''s 1924 Constitution secession language; interviews with surviving drafters; archival records on Molotov/Stalin revisions of the clause',
    'If genuine offer: the constraint is a failed coordination mechanism (rope degraded to snare through subsequent apparatus expansion). If intentional theater: the constraint is predatory snare from inception. Classification remains Snare in both cases, but the narrative changes from ''betrayed federalism'' to ''designed trap.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secession_clause_intentionality, empirical, 'Whether the secession clause was genuine option or intentional trap from drafting').

omega_variable(
    republic_interpretation_variance,
    'Did different union republics (Ukraine, Georgia, Kazakhstan) interpret the secession clause with different perceived legitimacy or different thresholds for testing it?',
    'Comparative study of pre-purge elite communications and 1937-1938 purge patterns by republic; analysis of whether purge intensity correlated with republic''s proximity to actual secession rhetoric',
    'If variance exists: suppression is targeted selective terror (snare), not structural federation. If uniform: suppression is administered systematically across all republics (confirming snare as structural). Variance would identify which republics experienced the secession clause as highest-extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(republic_interpretation_variance, empirical, 'Variance in republic interpretation and suppression intensity').

omega_variable(
    competing_reading_foreclosure,
    'Do the party_state_duality and federal_fiction readings forecast each other, or can both coexist in a single analytical framework?',
    'Formal reconstruction: if federal form requires text/apparatus split (federal_fiction), does the apparatus''s parallel operation foreclose or require the party/state duality (party_state_duality)? Can the party operate ''from outside'' the federal state without the federal state already being an external apparatus-controlled theater?',
    'If readings foreclose: they are describing different constraints from decomposable mechanisms. If they coexist: they are capturing interdependent aspects of the same constraint. Affects network.affects_constraints linking strategy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_reading_foreclosure, conceptual, 'Whether federal_fiction and party_state_duality foreclosure or coexistence').

omega_variable(
    legitimacy_extraction_coupling,
    'What proportion of the extractiveness (0.68) is driven by suppression of exit (snare mechanism) versus the legitimacy extraction (using constitutional form to justify apparatus)?',
    'Decomposition: calculate extractiveness under counterfactual ''federal text without apparatus suppression'' (coordination only, pure Rope) vs. ''apparatus suppression without federal text'' (visible empire, no legitimacy cover). Compare to actual 0.68 to isolate coupling.',
    'If legitimacy coupling > 40%: the federal form is doing substantial extractive work via credibility provision. If coupling < 20%: suppression mechanism dominates extraction, and federal form is incidental theater. Affects whether boltzmann.coordination_type should emphasize legitimacy or material coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_extraction_coupling, conceptual, 'Proportion of extractiveness driven by legitimacy cover versus apparatus suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(soviet_constitution_1936__federal_fiction, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soviet_fed_theater_t0, soviet_constitution_1936__federal_fiction, theater_ratio, 0, 0.75).
narrative_ontology:measurement(soviet_fed_theater_t2, soviet_constitution_1936__federal_fiction, theater_ratio, 2, 0.82).
narrative_ontology:measurement(soviet_fed_theater_t5, soviet_constitution_1936__federal_fiction, theater_ratio, 5, 0.88).

% Extraction over time
narrative_ontology:measurement(soviet_fed_extract_t0, soviet_constitution_1936__federal_fiction, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(soviet_fed_extract_t2, soviet_constitution_1936__federal_fiction, base_extractiveness, 2, 0.64).
narrative_ontology:measurement(soviet_fed_extract_t5, soviet_constitution_1936__federal_fiction, base_extractiveness, 5, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(soviet_fed_suppress_t0, soviet_constitution_1936__federal_fiction, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(soviet_fed_suppress_t2, soviet_constitution_1936__federal_fiction, suppression_requirement, 2, 0.9).
narrative_ontology:measurement(soviet_fed_suppress_t5, soviet_constitution_1936__federal_fiction, suppression_requirement, 5, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(soviet_constitution_1936__federal_fiction, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(soviet_constitution_1936__federal_fiction, 0.12).
narrative_ontology:affects_constraint(soviet_constitution_1936__federal_fiction, soviet_constitution_1936__party_state_duality).
narrative_ontology:affects_constraint(soviet_constitution_1936__federal_fiction, soviet_constitution_1936__rights_catalog_facade).
narrative_ontology:affects_constraint(soviet_constitution_1936__federal_fiction, soviet_constitution_1936__terror_coincidence).

% DUAL FORMULATION NOTE:
% The soviet_constitution_1936 kernel has four distinct readings, each a separate constraint with different ε values and different perspectives. The federal_fiction reading (this file, ε=0.68, Snare) treats the secession clause as intentional trap. The party_state_duality reading (ε≈0.65, Tangled Rope) foregrounds the apparatus operating outside the constitutional text. The rights_catalog_facade reading (ε≈0.72, Snare) focuses on enumerated rights being negated in practice. The terror_coincidence reading (ε≈0.70, Snare) emphasizes temporal structure — constitution and purge as simultaneous events. All four readings affect each other: the federal form requires the party/state split (apparatus operating outside the text); the rights catalog requires the apparatus (guarantees negated by enforcement); the Terror enables all of them (makes contradiction unquestionable). They are linked by network.affects_constraints in all files. Each reading instantiates a single ε-invariant constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(soviet_constitution_1936__federal_fiction, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
