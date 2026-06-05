% ============================================================================
% CONSTRAINT STORY: roman_republican_constitution__popular_assemblies_and_tribunate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_republican_constitution__popular_assemblies_and_tribunate, []).

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
 *   constraint_id: roman_republican_constitution__popular_assemblies_and_tribunate
 *   human_readable: Roman Republic: Popular Assemblies and Tribunate
 *   domain: political/historical/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested Roman
 *   Republican constitution: the 'popular assemblies and tribunate'
 *   interpretation. The reading claims that the Republic's distinctive
 *   structural feature was the empowerment of plebeian assemblies (comitia)
 *   as law-passing bodies and the creation of the sacrosanct tribunate as an
 *   intercession veto protecting plebeians from magisterial overreach. This
 *   reading emphasizes sovereignty as vested in popular assemblies and
 *   protection as vested in the tribunes' power to say 'no' on behalf of the
 *   plebs. The constraint's extractiveness (0.38) reflects a middle-ground
 *   assessment: genuine coordination mechanisms (assembly law-passing,
 *   tribunician veto) exist alongside asymmetric extraction (plebeians bear
 *   mandatory military service and corvée labor; patrician creditors extract
 *   through debt-bondage). The suppression (0.42) captures the structural
 *   barriers to plebeian participation: economic dependency on patrician
 *   patrons, illiteracy, geographic distance, time opportunity-costs. The
 *   theater ratio (0.55) reflects that while the assembly and tribunate are
 *   functional institutions in the Early and Middle Republic, they degrade
 *   over time into performances of popular sovereignty that mask
 *   elite-negotiated outcomes. The measurement trajectory shows
 *   extractiveness rising from 0.28 to 0.52 and theater rising from 0.30 to
 *   0.78 as the Republic ages — the constraint's suppression of magisterial
 *   overreach weakens as elite factions learn to manipulate the formal
 *   mechanism.
 *
 * KEY AGENTS:
 *   - Plebeian Assembly (comitia): Primary claimed beneficiary (institutional/arbitrage) — vested with law-passing authority and magistrate election power; receives legitimacy from popular participation
 *   - Tribunes of the Plebs: Secondary beneficiary (institutional/arbitrage) — vested with intercession veto power; derive authority and career advancement from plebeian support
 *   - Plebeian Tenant-Farmers: Primary victim (powerless/trapped) — formal assembly rights obscured by economic dependency on patrician landlords; cannot exit patronage bonds
 *   - Urban Plebeian Craftsmen: Secondary victim (moderate/constrained) — bear mandatory military service and state requisitions; constrained but mobile within the city
 *   - Patrician Magistrates: Restrained beneficiary (powerful/constrained) — benefit from magisterial office but constrained by colleague veto and popular ratification requirements
 *   - Patrician Creditors: Unacknowledged beneficiary (powerful/arbitrage) — extract through debt-bondage (nexum) mechanism, partially insulated from assembly/tribunate constraints
 *   - Senate: Competing institutional actor (institutional/arbitrage) — authority is advisory (auctoritas) rather than legal, but exercises substantial influence over magistrates and finance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_republican_constitution__popular_assemblies_and_tribunate, 0.38).
domain_priors:suppression_score(roman_republican_constitution__popular_assemblies_and_tribunate, 0.42).
domain_priors:theater_ratio(roman_republican_constitution__popular_assemblies_and_tribunate, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_republican_constitution__popular_assemblies_and_tribunate, extractiveness, 0.38).
narrative_ontology:constraint_metric(roman_republican_constitution__popular_assemblies_and_tribunate, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(roman_republican_constitution__popular_assemblies_and_tribunate, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_republican_constitution__popular_assemblies_and_tribunate, tangled_rope).
narrative_ontology:human_readable(roman_republican_constitution__popular_assemblies_and_tribunate, "Roman Republic: Popular Assemblies and Tribunate").
narrative_ontology:topic_domain(roman_republican_constitution__popular_assemblies_and_tribunate, "political/historical/constitutional").

domain_priors:requires_active_enforcement(roman_republican_constitution__popular_assemblies_and_tribunate).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(roman_republican_constitution__popular_assemblies_and_tribunate, '9b3bd935-a88b-4cf9-a9ee-a51d6aa58060').
narrative_ontology:cs_kernel_codification('9b3bd935-a88b-4cf9-a9ee-a51d6aa58060', fixed_text).
narrative_ontology:cs_authority_grounding('9b3bd935-a88b-4cf9-a9ee-a51d6aa58060', lineage).
narrative_ontology:cs_interpretation_layer_present('9b3bd935-a88b-4cf9-a9ee-a51d6aa58060').
narrative_ontology:cs_reading_relation('9b3bd935-a88b-4cf9-a9ee-a51d6aa58060', roman_republican_constitution__crisis_machinery, coexists_with).
narrative_ontology:cs_reading_relation('9b3bd935-a88b-4cf9-a9ee-a51d6aa58060', roman_republican_constitution__legal_codification_twelve_tables, influences).
narrative_ontology:cs_reading_relation('9b3bd935-a88b-4cf9-a9ee-a51d6aa58060', roman_republican_constitution__magistracies_and_collegiality, coexists_with).
narrative_ontology:cs_reading_relation('9b3bd935-a88b-4cf9-a9ee-a51d6aa58060', roman_republican_constitution__senate_authority, coexists_with).
narrative_ontology:cs_axiom('9b3bd935-a88b-4cf9-a9ee-a51d6aa58060', foundational, popular_assembly_sovereignty).
narrative_ontology:cs_axiom_status(popular_assembly_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('9b3bd935-a88b-4cf9-a9ee-a51d6aa58060', popular_assembly_sovereignty, conventional).
narrative_ontology:cs_axiom('9b3bd935-a88b-4cf9-a9ee-a51d6aa58060', foundational, tribunician_veto_as_shield).
narrative_ontology:cs_axiom_status(tribunician_veto_as_shield, holdable).
narrative_ontology:cs_axiom_grounding('9b3bd935-a88b-4cf9-a9ee-a51d6aa58060', tribunician_veto_as_shield, conventional).
narrative_ontology:cs_reference_frame('9b3bd935-a88b-4cf9-a9ee-a51d6aa58060', early_republic_plebeian_victory).
narrative_ontology:cs_drift_state('9b3bd935-a88b-4cf9-a9ee-a51d6aa58060', late_republic_factional_manipulation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9b3bd935-a88b-4cf9-a9ee-a51d6aa58060', '').
narrative_ontology:cs_kernel_id(roman_republican_constitution__popular_assemblies_and_tribunate, roman_republican_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_republican_constitution__popular_assemblies_and_tribunate, plebeian_assembly).
narrative_ontology:constraint_beneficiary(roman_republican_constitution__popular_assemblies_and_tribunate, tribunes_of_plebs).
narrative_ontology:constraint_victim(roman_republican_constitution__popular_assemblies_and_tribunate, patrician_magistracies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLEBEIAN TENANT-FARMER (SNARE) — Bound to aristocratic patrons by debt bondage and clientage. Assembly participation is theoretically available but structurally constrained by economic dependency on patrician landlords. Exit is material impossibility. The plebeian experiences maximum suppression despite formal rights — cannot exercise assembly vote without incurring patron's displeasure.
constraint_indexing:constraint_classification(roman_republican_constitution__popular_assemblies_and_tribunate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: URBAN PLEBEIAN CRAFTSMAN (TANGLED ROPE) — Constrained but mobile within the city. Benefits from assembly and tribunate protections against arbitrary magisterial seizure of goods or forced military levies. Bears costs of mandatory assembly attendance and corvée labor obligations. Mixed experience: genuine coordination mechanism (assembly law-passing) paired with extraction (state requisitions).
constraint_indexing:constraint_classification(roman_republican_constitution__popular_assemblies_and_tribunate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: TRIBUNAL OF PLEBS (ROPE) — Primary institutional beneficiary. The tribunate extracts legitimacy and power from plebeian support while providing genuine coordination: the intercession veto mediates between plebeian grievances and magisterial authority. The tribunes experience the constraint as enabling their own role. They enjoy mobility and can leverage their position for political capital. Net beneficiary with genuine coordination function.
constraint_indexing:constraint_classification(roman_republican_constitution__popular_assemblies_and_tribunate, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORMING COALITION (SCAFFOLD) — Organized plebeian movement (c. 367 BCE) targeting specific structural barriers: patrician monopoly on consulship, unjust debt laws, expansion of assembly domains. The coalition perceives the constraint as temporary — solvable through legal reform (opening magistracies, recalibrating assembly votes). Theater is moderate because the coalition's demands are concrete and measurable. Sunset logic applies: as specific reforms pass, the motivation for continued organized pressure declines (unless new barriers emerge).
constraint_indexing:constraint_classification(roman_republican_constitution__popular_assemblies_and_tribunate, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PATRICIAN MAGISTRATE (TANGLED ROPE) — Benefits from collegial sharing of the imperium (each magister can veto peers, limiting any one person's power). Constrained by annual office term limits and by the tribunes' veto power. Experiences genuine coordination function (colleague check-and-balance) paired with extraction-feeling constraint (veto strip on personal authority). The plebeian assembly's ratification requirement checks magisterial power but also legitimates it — coordination mechanism with asymmetric burden-sharing.
constraint_indexing:constraint_classification(roman_republican_constitution__popular_assemblies_and_tribunate, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MYTHIC POPULAR SOVEREIGNTY (PITON) — The idealized narrative of 'the people rule' persists in Roman historical memory long after the actual mechanism becomes degraded. By the Late Republic (1st century BCE), assemblies are manipulated through violence and bribery, tribunes are instruments of elite factions rather than plebeian shields, and the formal popular apparatus is theatrical. The institutional reality has drifted far from the founding claim, yet the myth of popular sovereignty is maintained through ritual invocation. Theater ratio is high because the assembly meets but is not functionally decisive — performances of 'the people' deciding precede outcomes determined by elite negotiation.
constraint_indexing:constraint_classification(roman_republican_constitution__popular_assemblies_and_tribunate, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: NATURAL LAW VIEW (CYCLICAL CORRUPTION) — From a civilizational lens, some theorists (Roman and modern) see the degradation of the Republic as inevitable — a natural law of polities. The observation: all republics tend toward oligarchy; checks on power erode under elite pressure; formal constraints become theatrical. The constraint appears as an immutable structural tendency of human political organization. However, this perspective naturalizes what is actually a contingent historical choice by elite actors to subvert the constraint. The engine's false summit detector identifies this as naturalization.
constraint_indexing:constraint_classification(roman_republican_constitution__popular_assemblies_and_tribunate, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_republican_constitution__popular_assemblies_and_tribunate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_republican_constitution__popular_assemblies_and_tribunate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_republican_constitution__popular_assemblies_and_tribunate, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(roman_republican_constitution__popular_assemblies_and_tribunate, TR),
    TR >= 0.70.

:- end_tests(roman_republican_constitution__popular_assemblies_and_tribunate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.38): The base extractiveness reflects that genuine coordination mechanisms coexist with real extraction. The assembly passing laws is a coordination function: it establishes rules that bind all parties, including the elite. But the assembly's outcomes are constrained by patrician demographic advantage (rural voters must travel), property qualifications on some voting, and elite domination of rhetoric and persuasion. The extraction is not maximal because plebeians do have agency — they can elect tribunes who veto, they can vote on laws, they can organize faction pressure. But their agency is structurally constrained by economic dependency (debt-bondage), illiteracy, and time poverty. Extractiveness rises over the interval from 0.28 to 0.52 because elite factions increasingly learn to manipulate assembly procedures (violence, bribery, geographic manipulation of voting), while at the same time the plebeian base becomes more immobilized by imperial wars and land loss. The rising trajectory is the measurement signature of a constraint whose protective function erodes as elite actors learn countermeasures. SUPPRESSION (0.42): Barriers to plebeian participation are substantial but not total. Debt-bondage is a real barrier but not universal — urban plebeians can sometimes escape it. Geographic distance limits assembly attendance but does not prevent it. Illiteracy limits independent legal knowledge but tribunes and advocates provide interpretation. Suppression rises from 0.35 to 0.65 as elite violence increases and land loss forces more plebeians into permanent economic dependency. THEATER RATIO (0.55): Moderate in the Early Republic (0.30), reflecting genuine functional use of assemblies and tribunate. Rises to 0.55 in the Middle Republic and 0.78 in the Late Republic as elite factions learn to stage-manage assemblies and tribunes become factional tools rather than plebeian protectors. The theater signature indicates that the constraint's protective function is being hollowed out by institutional decay, not by its own logic.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates a wide perspectival gap across power and exit positions. The plebeian tenant-farmer sees a Snare (trapped, powerless, maximum extraction) because their participation is formal but their agency is nullified by debt-bondage. The urban plebeian craftsman sees a Tangled Rope (constrained but mobile, mixed coordination and extraction) because they genuinely benefit from assembly law-passing and tribunician veto but bear mandatory service costs. The tribune sees a Rope (benefits from the constraint, exercises power through it) because their authority is anchored in plebeian support and the constraint enables their role. The reforming coalition sees a Scaffold (organized agents with agency and a sunset path) — the constraint is temporary, solvable through specific structural reforms like opening magistracies to plebeians. The patrician magistrate sees a Tangled Rope (constrained by colleague veto and assembly ratification, but benefiting from collegial power-sharing and legitimacy conferral). The mythic narrative of popular sovereignty becomes Piton as theater rises and actual decision-making power shifts to elite negotiation. The civilizational analytical observer risks seeing natural law (republics always tend toward oligarchy) but the measurements show that the degradation is not inevitable — it results from deliberate elite subversion of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for each perspective follows from the agent's power level, exit options, and beneficiary/victim relationship to THIS specific constraint. The plebeian tenant-farmer is a victim (bears extraction costs) with trapped exit (cannot leave patronage bonds), deriving d ≈ 0.95, f(d) ≈ 1.42 (powerless experienced extractiveness). The tribunal of plebs is a beneficiary (derives authority and power) with arbitrage exit (can leverage position), deriving d ≈ 0.05, f(d) ≈ -0.12 (institutional beneficiary experienced extractiveness). The reforming coalition is organized, has constrained but real exit options, and benefits from the assembly/tribunate mechanism while suffering from structural barriers — deriving d ≈ 0.50-0.60, consistent with organized agent experiencing moderate extraction. Each perspective's chi value (effective extractiveness) is then scaled by the scope modifier σ(S): local scope (σ=0.8) dampens chi for peasant-level extraction; national scope (σ=1.0) sets the baseline for republic-level institutional constraint. The perspectival gap is diagnostic: if all perspectives produced the same chi, the differentiated exit options would not matter. The gap is real because agent power and exit structurally differ across the contexts.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint does not exhibit mandatrophy because it is not a Snare masquerading as a Rope. The constraint is correctly classified as Tangled Rope at the analytical level: it genuinely includes both coordination function (assemblies passing laws, tribunes mediating) and asymmetric extraction (plebeians bear military and tax burdens disproportionately). The mandatrophy test asks: if we removed the supposed 'coordination' would the extraction disappear or persist? Answer: the extraction (magisterial power, patrician economic advantage, debt-bondage) persists in alternative forms (direct aristocratic rule, private armed retainers, slavery expansion). The coordination function (assemblies and tribunate) does real work — it constrains magisterial unilateralism and provides plebeian grievance channels. Both are structurally present. The perspectival gap across types (Snare for the trapped victim, Rope for the tribune, Piton for the degraded myth, Tangled Rope for the analytical observer) is not paradox — it is the signature of a hybrid constraint seen from different positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    plebeian_assembly_real_decisiveness,
    'Did the plebeian assembly make substantively independent legislative decisions, or did it ratify outcomes negotiated by elite factions (Senate and magistrates)?',
    'Comparative analysis of recorded votes: correlation between assembly outcomes and prior magisterial/senatorial preferences; reconstruction of evidence from law texts and historical narratives showing whether assemblies changed the form of proposed laws or merely approved them',
    'If truly independent: constraint is genuine Tangled Rope (mixed coordination and extraction). If consistently ratifying elite preferences: constraint is Piton or Snare (theater masking extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plebeian_assembly_real_decisiveness, empirical, 'Whether assemblies made independent legislative decisions or ratified elite-negotiated outcomes').

omega_variable(
    tribunate_independence_from_plebeian_base,
    'Were tribunes representatives who needed to maintain plebeian support, or did they become elite factional instruments manipulating plebeian voters?',
    'Historical analysis of tribune behavior: voting records, alliance patterns across generations; evidence of bribing or coercing tribune electoral support; correlation between tribune actions and explicit plebeian petitions vs elite factional interest',
    'If tribunes were accountable to plebs: constraint is genuine Tangled Rope with real beneficiary protection. If tribunes became elite tools: constraint shifts toward Piton or Snare (formal protections corrupted into theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tribunate_independence_from_plebeian_base, empirical, 'Whether tribunes remained accountable to plebeian base or became elite factional instruments').

omega_variable(
    extraction_flow_direction_debt_bondage,
    'Was debt-bondage (nexum) of plebeians a consequence of patrician extraction through the constraint, or was it a pre-constitutional economic condition the assembly and tribunate addressed?',
    'Chronological reconstruction of when debt-bondage became severe vs when assembly/tribunate protections were established; analysis of whether plebeian debt increased or decreased following tribunate reforms (Lex Poetelia on debt slavery, debt relief measures); identification of whether patrician creditors used debt as a mechanism of political control',
    'If debt-bondage was extraction consequence: victim set expands, suppression increases, constraint becomes more snare-like. If pre-constitutional and addressed: constraint is genuine protective mechanism (more rope-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_flow_direction_debt_bondage, empirical, 'Whether debt-bondage was a consequence of patrician extraction or a pre-constitutional condition the assembly addressed').

omega_variable(
    contested_kernel_committer_frame,
    'Which reading of the Roman Republican constitution captures the true structural mechanism: popular assemblies, magistracies and collegiality, senate authority, the twelve tables, or crisis machinery?',
    'This is a committer-axis ambiguity, not resolvable empirically. Different historical interpretations emphasize different constitutional elements as primary. Modern historians privilege different readings (Brunt emphasizes popular sovereignty; Scullard emphasizes senatorial authority; Lintott emphasizes magistracies). No single reading is ''correct'' — each captures a real structural element. Resolution mechanism is interpretive framework choice, not empirical discovery.',
    'Selection of reading determines which constraint story is authored. This story instantiates the ''popular assemblies and tribunate'' reading. Sibling readings instantiate different constraint stories with different ε values and different beneficiary/victim sets.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contested_kernel_committer_frame, conceptual, 'Which constitutional element is primary: assemblies, magistracies, senate, tables, or crisis machinery?').

omega_variable(
    tribunate_veto_effectiveness_scope,
    'Was the tribunes'' veto power (intercession) effective across all categories of action, or were there domains (military command, religious authority, emergency measures) where the veto could be circumvented or nullified?',
    'Textual and historical evidence: whether tribunes could veto military levies, emergency dictatorships, religious decisions; documented instances of magistrates acting despite tribunician veto or refusal to act; scope of tribunician jurisdiction in law vs practice',
    'If veto is comprehensive: tribunes have real power, constraint is protective Tangled Rope. If veto scope is limited: tribunes have symbolic power in restricted domains, constraint becomes Piton (performative in excluded domains).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tribunate_veto_effectiveness_scope, empirical, 'Whether tribunician veto was effective across all action domains or limited to specific jurisdictions').

omega_variable(
    assembly_voting_structure_patron_influence,
    'Did the assembly''s voting structure (voting by tribe, voting by centuriate rank, voting by individual) systematically advantage or disadvantage plebeian majorities relative to patrician preferences?',
    'Quantitative reconstruction of voting distribution: by tribe (geographic), by centuriate rank (property-based), by individual count; modeling how different voting rules would have changed outcomes on contested laws; historical evidence of deliberate voting structure changes responding to assembly outcomes',
    'If structure favored patrician minorities: assembly is theater/extraction mechanism (Snare/Piton despite formal democracy). If structure allowed true majorities: assembly is functional coordination (Rope/Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(assembly_voting_structure_patron_influence, empirical, 'Whether assembly voting structure systematically advantaged patrician minorities or enabled plebeian majorities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_republican_constitution__popular_assemblies_and_tribunate, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rom_theater_early_republic, roman_republican_constitution__popular_assemblies_and_tribunate, theater_ratio, 0, 0.3).
narrative_ontology:measurement(rom_theater_middle_republic, roman_republican_constitution__popular_assemblies_and_tribunate, theater_ratio, 150, 0.55).
narrative_ontology:measurement(rom_theater_late_republic, roman_republican_constitution__popular_assemblies_and_tribunate, theater_ratio, 250, 0.78).

% Extraction over time
narrative_ontology:measurement(rom_extract_early_republic, roman_republican_constitution__popular_assemblies_and_tribunate, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(rom_extract_middle_republic, roman_republican_constitution__popular_assemblies_and_tribunate, base_extractiveness, 150, 0.38).
narrative_ontology:measurement(rom_extract_late_republic, roman_republican_constitution__popular_assemblies_and_tribunate, base_extractiveness, 250, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(rom_suppress_early_republic, roman_republican_constitution__popular_assemblies_and_tribunate, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(rom_suppress_middle_republic, roman_republican_constitution__popular_assemblies_and_tribunate, suppression_requirement, 150, 0.42).
narrative_ontology:measurement(rom_suppress_late_republic, roman_republican_constitution__popular_assemblies_and_tribunate, suppression_requirement, 250, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_republican_constitution__popular_assemblies_and_tribunate, enforcement_mechanism).
narrative_ontology:affects_constraint(roman_republican_constitution__popular_assemblies_and_tribunate, roman_republican_constitution__crisis_machinery).
narrative_ontology:affects_constraint(roman_republican_constitution__popular_assemblies_and_tribunate, roman_republican_constitution__legal_codification_twelve_tables).
narrative_ontology:affects_constraint(roman_republican_constitution__popular_assemblies_and_tribunate, roman_republican_constitution__magistracies_and_collegiality).
narrative_ontology:affects_constraint(roman_republican_constitution__popular_assemblies_and_tribunate, roman_republican_constitution__senate_authority).

% DUAL FORMULATION NOTE:
% The Roman Republican constitution is a contested kernel with five sibling readings, each instantiating a different constraint. This story models the 'popular assemblies and tribunate' reading (ε=0.38, Tangled Rope). Sibling readings have different ε values and different beneficiary/victim structures. The crisis_machinery reading (ε varies by moment, includes emergency dictatorship) influences this reading by establishing the constitutional framework in which assemblies and tribunate operate. The magistracies_and_collegiality reading (ε=0.30-0.45, Rope-dominant) captures an alternative primary element (magistrates as constraint-bearers rather than assemblies as constraint-bearers). The senate_authority reading (ε=0.35-0.50, Rope-Tangled Rope) emphasizes advisory authority rather than formal law-passing. All five readings coexist as live historical interpretations. Their relationships are coexists_with (different scholars emphasize different readings) and influences (each reading's emphasis shapes what constitutional element is foregrounded for students and policymakers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
