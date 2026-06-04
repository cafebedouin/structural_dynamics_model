% ============================================================================
% CONSTRAINT STORY: ancient_constitutionalism__roman_republican_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ancient_constitutionalism__roman_republican_constitution, []).

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
 *   constraint_id: ancient_constitutionalism__roman_republican_constitution
 *   human_readable: Roman Republican Constitution: Balanced Government Through Mutual Veto
 *   domain: political/historical/constitutional
 *
 * SUMMARY:
 *   The Roman Republican constitution is one reading of the kernel 'ancient
 *   constitutionalism' — a contested claim about how political power should
 *   be distributed. This reading asserts that the paradigm of balanced
 *   government distributes power among magistrates, Senate, and assemblies
 *   through mutual veto (collegiality, tribunician intercession) such that no
 *   single order could rule alone. The sibling reading (Athenian democratic
 *   constitution) asserts instead that the paradigm relies on sortition,
 *   rotation, and the direct assembled demos to prevent the hardening of
 *   government into a separate ruling class. These readings coexist as live
 *   positions in the history of political theory but operate from
 *   fundamentally different premises about what constitutes legitimate
 *   authority and how to prevent tyranny. The Roman reading grounds itself in
 *   the authority of the aristocratic Senate checking magistratic power
 *   through collegiality; the Athenian reading grounds itself in the rotation
 *   of lots among the mass citizenry. The structural delta for the Roman
 *   reading: suppression operates through mutual veto and the threat of
 *   tribunal intercession (coercive negation), rather than through explicit
 *   force. Beneficiary is the senatorial order operating the machinery.
 *   Victim set includes the provinces (extracted through taxation and
 *   conscription with no veto mechanism) and the unpropertied plebs and
 *   enslaved populations (legally excluded from the mechanism).
 *   Extractiveness is high at the imperial periphery (no constitutional
 *   constraint), contested at the center (veto structures create real
 *   constraint for the property-qualified orders but are weak against
 *   organized senatorial consensus). The measurements show rising
 *   extractiveness and suppression over the interval of the Republic's
 *   existence: the early Republic (founding) maintains lower extractiveness
 *   because new provincial conquest is incomplete and inter-senatorial power
 *   competition is high; by the Gracchi period (mid-interval) extractiveness
 *   rises as the empire expands and suppression mechanisms tighten against
 *   agrarian reformers; by the late Republic (end of interval) extractiveness
 *   is severe as the constraint breaks down into civil war, requiring maximal
 *   suppression to maintain the facade of constitutional procedure.
 *
 * KEY AGENTS:
 *   - Senatorial Order: Primary beneficiary (institutional/arbitrage) — operates the machinery of mutual veto; uses collegiality and tribunician intercession to prevent rival magnates from monopolizing power while collectively controlling the state and provinces. Net extractor.
 *   - Provinces: Primary victim (powerless/trapped) — extracted through taxation, conscription, and administrative appropriation; no constitutional mechanism constrains the Senate's exploitative capacity at the periphery. Experience pure Snare from this reading.
 *   - Unpropertied Plebs: Secondary victim (powerless/trapped) — legally excluded from magistratic office; rely on tribunes for intercession, but tribunes are constrained by property qualifications and senatorial agenda-setting. Experience Snare with some Tangled Rope relief through organized tribune action.
 *   - Tribunes of the Plebs: Organized victims (organized/constrained) — granted veto power by the constitution but constrained by sacrosanctity requirement, property qualifications, and Senate control of the agenda. Experience Tangled Rope: they provide some genuine coordination (block decrees that would eliminate plebeian interests entirely) but face severe extraction through agenda constraints.
 *   - Patrician Magnates: Powerful beneficiaries (powerful/mobile) — compete within the senatorial order using mutual-veto mechanisms to prevent rivals from eliminating them. Experience Tangled Rope: coordination is necessary for your own security, but zero-sum extraction occurs through blocking of rival initiatives.
 *   - Consular Magistrates: Institutional beneficiaries (institutional/arbitrage) — co-consulship with veto power is theoretically a check on tyranny but increasingly performs ceremonial rather than constraining function. Experience Piton: the office persists through institutional inertia even as its functional power degrades.
 *   - Enslaved Populations: Structurally invisible victims (powerless/trapped) — entirely outside the constitutional framework; the constitution's balance-of-power is a mechanism for distributing their extraction and that of conquered territories among the property-qualified orders. Experience maximum extraction with no constitutional recourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ancient_constitutionalism__roman_republican_constitution, 0.58).
domain_priors:suppression_score(ancient_constitutionalism__roman_republican_constitution, 0.62).
domain_priors:theater_ratio(ancient_constitutionalism__roman_republican_constitution, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ancient_constitutionalism__roman_republican_constitution, extractiveness, 0.58).
narrative_ontology:constraint_metric(ancient_constitutionalism__roman_republican_constitution, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ancient_constitutionalism__roman_republican_constitution, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ancient_constitutionalism__roman_republican_constitution, tangled_rope).
narrative_ontology:human_readable(ancient_constitutionalism__roman_republican_constitution, "Roman Republican Constitution: Balanced Government Through Mutual Veto").
narrative_ontology:topic_domain(ancient_constitutionalism__roman_republican_constitution, "political/historical/constitutional").

domain_priors:requires_active_enforcement(ancient_constitutionalism__roman_republican_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ancient_constitutionalism__roman_republican_constitution, 'f98234b1-f882-4c1f-8db7-6d607ce31685').
narrative_ontology:cs_kernel_codification('f98234b1-f882-4c1f-8db7-6d607ce31685', formalized).
narrative_ontology:cs_authority_grounding('f98234b1-f882-4c1f-8db7-6d607ce31685', lineage).
narrative_ontology:cs_interpretation_layer_present('f98234b1-f882-4c1f-8db7-6d607ce31685').
narrative_ontology:cs_reading_relation('f98234b1-f882-4c1f-8db7-6d607ce31685', ancient_constitutionalism__athenian_democratic_constitution, coexists_with).
narrative_ontology:cs_axiom('f98234b1-f882-4c1f-8db7-6d607ce31685', foundational, balanced_power_through_tripartite_veto).
narrative_ontology:cs_axiom_status(balanced_power_through_tripartite_veto, holdable).
narrative_ontology:cs_axiom_grounding('f98234b1-f882-4c1f-8db7-6d607ce31685', balanced_power_through_tripartite_veto, conventional).
narrative_ontology:cs_axiom('f98234b1-f882-4c1f-8db7-6d607ce31685', secondary, property_qualification_for_political_capacity).
narrative_ontology:cs_axiom_status(property_qualification_for_political_capacity, holdable).
narrative_ontology:cs_axiom_grounding('f98234b1-f882-4c1f-8db7-6d607ce31685', property_qualification_for_political_capacity, deontological).
narrative_ontology:cs_reference_frame('f98234b1-f882-4c1f-8db7-6d607ce31685', aristocratic_balance_through_collegiality).
narrative_ontology:cs_drift_state('f98234b1-f882-4c1f-8db7-6d607ce31685', late_republic_civil_war, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f98234b1-f882-4c1f-8db7-6d607ce31685', '').
narrative_ontology:cs_kernel_id(ancient_constitutionalism__roman_republican_constitution, ancient_constitutionalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ancient_constitutionalism__roman_republican_constitution, senatorial_order).
narrative_ontology:constraint_beneficiary(ancient_constitutionalism__roman_republican_constitution, patrician_families).
narrative_ontology:constraint_beneficiary(ancient_constitutionalism__roman_republican_constitution, propertied_citizens).
narrative_ontology:constraint_victim(ancient_constitutionalism__roman_republican_constitution, provinces).
narrative_ontology:constraint_victim(ancient_constitutionalism__roman_republican_constitution, unpropertied_plebs).
narrative_ontology:constraint_victim(ancient_constitutionalism__roman_republican_constitution, enslaved_populations).
narrative_ontology:constraint_victim(ancient_constitutionalism__roman_republican_constitution, subject_allies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROVINCIAL SUBJECT (SNARE) — Trapped within imperial periphery, extracted through taxation, conscription, and administrative appropriation. The balanced constitution applies only to citizens within the pomerium; provinces experience pure extraction with minimal suppressive need (they lack capacity to organize). Maximum experienced chi from this perspective.
constraint_indexing:constraint_classification(ancient_constitutionalism__roman_republican_constitution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: PROPERTIED PLEBEIAN (TANGLED ROPE) — Constrained by property qualifications and voting structure; participates in assemblies but faces vetoing magistrates and senatorial domination. Experiences both coordination (mutual veto prevents tyranny of one order) and extraction (senatorial oligarchy controls real power). Can exit through migration to provinces or alliance-building, but at cost.
constraint_indexing:constraint_classification(ancient_constitutionalism__roman_republican_constitution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SENATORIAL ORDER (ROPE) — Primary beneficiary. Operates the machinery of mutual veto; uses collegiality and tribunician intercession to prevent rival magnates from monopolizing power while collectively controlling the state. Experiences the constraint as pure coordination mechanism — mutual veto is how the patrician oligarchy perpetuates itself. Net beneficiary; can arbitrage between offices and alliances.
constraint_indexing:constraint_classification(ancient_constitutionalism__roman_republican_constitution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRIBUNES OF THE PLEBS (TANGLED ROPE) — Organized agents with constitutional veto power (intercession). Genuine coordination function: they block senatorial decrees that would eliminate plebeian interests entirely. But they are constrained by sacrosanctity rather than real power — the Senate controls the agenda, and tribunician veto is primarily reactive. Experience significant extraction despite legal parity.
constraint_indexing:constraint_classification(ancient_constitutionalism__roman_republican_constitution, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSULAR MAGISTRACY (PITON) — The co-consulship with veto power is structurally sound as a mutual-check mechanism, but operationally much of it is theatrical ritual by the late Republic. Consuls perform elaborate ceremonial functions that serve more to legitimize senatorial dominance than to constrain it. Theater ratio reflects the gap between the constitutional theory (balanced power) and practice (senatorial direction). The magistracy persists through institutional inertia rather than functional necessity.
constraint_indexing:constraint_classification(ancient_constitutionalism__roman_republican_constitution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational perspective, the Roman constitution might appear as a natural law of balanced government: any complex state with competing power centers will naturally develop mutual-veto mechanisms to prevent tyranny. This reads the constitution as an inevitable discovery of political science. However, the structural data contradicts the mountain classification — this is a false summit, as the engine will detect. The constitution is a contingent institutional arrangement that benefits the senatorial order and extracts from provinces. The 'natural law' framing naturalizes what is actually political capture.
constraint_indexing:constraint_classification(ancient_constitutionalism__roman_republican_constitution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: PATRICIAN MAGNATE (TANGLED ROPE) — Powerful individual actor operating within the senatorial class. The mutual-veto constitution both enables and constrains: enables because veto power prevents rivals from eliminating you; constrains because your rivals can veto your initiatives. This agent experiences genuine coordination (you need alliances to block rivals; you need colleagues to enforce your power) but also zero-sum extraction (advancement means blocking others). Mobile within the Roman elite but constrained by the need for coalition-building.
constraint_indexing:constraint_classification(ancient_constitutionalism__roman_republican_constitution, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ancient_constitutionalism__roman_republican_constitution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ancient_constitutionalism__roman_republican_constitution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ancient_constitutionalism__roman_republican_constitution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ancient_constitutionalism__roman_republican_constitution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ancient_constitutionalism__roman_republican_constitution, TR),
    TR >= 0.70.

:- end_tests(ancient_constitutionalism__roman_republican_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting the core structural feature — the constitution genuinely constrains extraction within the property-qualified orders (hence not pure Snare at center) but enables unconstrained extraction at the periphery. The value reflects that the constraint operates asymmetrically: for provincial subjects and enslaved populations, it is extractive machinery; for the propertied orders competing within the Senate, it is coordination mechanism with contested power. The rising trajectory in measurements (0.38 → 0.52 → 0.68) models empire expansion: as provincial territory grows, the benefit of extraction grows, and the constitution's constraint (relevant only for central power distribution) becomes less relevant to average extractiveness across the entire system. Suppression (0.62): Moderate-high. The suppression takes the form of mutual veto and the threat of tribunician intercession within the propertied orders, but also manifest coercion (military enforcement of senatorial decisions, punishment of provincial resistance, enslavement as the ultimate suppression of the unpropertied). The rising trajectory (0.42 → 0.58 → 0.75) models the late-Republic crises: as the constitution breaks down, suppression mechanisms must intensify (culminating in the necessity for civil war to maintain pretense of constitutional procedure). Theater ratio (0.55): Moderate. The constitution contains both functional elements (mutual veto genuinely constrains certain magnates; tribunes can block certain decrees) and performative elements (consular ceremonial, elaborate voting procedures, ritual intercession). The rising trajectory (0.38 → 0.48 → 0.62) models the increasing theatricality of late-Republic procedure: as real power consolidates into fewer hands and the balance breaks down, the performative content of the constitutional ritual increases to maintain legitimacy claims.
 *
 * PERSPECTIVAL GAP:
 *   The Roman reading exhibits maximum perspectival divergence. The senatorial order sees Rope — a mechanism for managing competition among equals. The organized plebeians see Tangled Rope — genuine coordination benefit (the tribunes do prevent total senatorial domination) mixed with severe extraction (agenda control, property restrictions). The provincial subject sees Snare — pure extraction with no constraint. The patrician magnate sees Tangled Rope for themselves — mutual checks are necessary for their own security — but Snare for others. The consular magistracy sees Piton — the office is theater, though it was once functional. The analytical observer risks Mountain (natural law of balanced government) but the structural data contradicts it: beneficiaries are identifiable (the senatorial order), victims are identifiable (provinces, unpropertied), and extractiveness is high at the periphery. The false summit is especially strong in this reading because the constitution's self-description (balanced power, mutual constraint) is correct for the property-qualified center and entirely false for the periphery.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives its directionality from the agent's structural position within the reading. The senatorial order benefits from the mechanism (low d, approaching beneficiary status) and experiences the constraint as coordination. The provinces and unpropertied are trapped (high d approaching 1.0) and experience extraction. The tribunes occupy a middle position (d ≈ 0.6–0.7): they have veto power (some benefit from the mechanism, some ability to constrain others) but operate under senatorial agenda-setting (constrained, trapped relative to the overall framework). The patrician magnate occupies a precarious middle (d ≈ 0.5): the mutual-veto mechanism prevents you from dominating others but also prevents others from dominating you; you must invest heavily in coalition-building to exercise power. The analytical observer's d is set at 0.72 (canonical analytical value) but risks the false-summit classification: the mountain perspective treats the constitution as natural law (inevitable consequence of power distribution) rather than as a contingent institutional arrangement that extracts value and naturalizes hierarchy.
 *
 * MANDATROPHY ANALYSIS:
 *   The Roman Republican reading resolves the mandatrophy through structural asymmetry: the constitution is Tangled Rope at the center (genuine coordination mixed with asymmetric extraction among the property-qualified orders) and Snare at the periphery (extraction with no coordination function for provinces and unpropertied). The false summit (mountain perspective) naturalizes the center's balance as an inherent law of government, eliding the periphery's pure extraction. The true structure cannot be captured by a single type — it requires the decomposition of the observation site. The senatorial order's beneficiary status and institutional power derive d toward 0.05–0.20 (strong beneficiary); the provincial victim status and trapped exit derive d toward 0.95 (maximum victim); the organized tribune derives d toward 0.55–0.65 (constrained with some benefits from the veto mechanism). The perspectival gap is not merely different opinions about the same constraint — it reflects genuinely different structural relationships to the constraint. The mandate (constraint's claim) is contestable precisely because the reading works only for a subset of the population while claiming universality. The Athenian sibling reading resolves the mandatrophy differently: by rejecting the property qualification and the Senate's central role, and grounding legitimacy in sortition and assembly, it produces a different beneficiary/victim structure. This reading interaction is what the kernel contest captures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balanced_constitution_or_oligarchic_facade,
    'Is the Roman Republican constitution a genuine system of balanced power or a facade for oligarchic extraction disguised as constitutional constraint?',
    'Empirical analysis of veto usage: frequency of senatorial decrees blocked by tribunes vs frequency of tribunes'' actions blocked by magistrates; correlation between property class and policy outcomes; examination of how the constitution operated under stress (grain crisis, external threat, succession disputes).',
    'If genuinely balanced: classification across the index set leans toward Rope/Tangled Rope with lower effective extraction. If oligarchic facade: classifications shift toward Snare/Piton with higher realized extraction, especially for victims. The mountain perspective becomes a false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(balanced_constitution_or_oligarchic_facade, empirical, 'Whether the constitution is balanced or oligarchic facade').

omega_variable(
    veto_efficacy_at_periphery_versus_center,
    'Do the mutual-veto and collegiality mechanisms that constrain power at the center proportionally constrain extraction at the periphery, or does the constitution create a structural asymmetry where central balance-of-power enables peripheral extraction?',
    'Comparative analysis: rates of administrative extraction (taxation, conscription, arbitrary judgment) in territories governed by different constitutional periods and magistrate types; correlation between internal political competition (high during contested successions, low during hegemonic control) and peripheral extraction rates.',
    'If veto mechanisms constrain periphery equally: the constitution is Rope across the observation site (coordination benefit is shared). If periphery is decoupled: the constitution is Tangled Rope at center, Snare at periphery — a structural separation. This is the critical question for the expected structural delta.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_efficacy_at_periphery_versus_center, empirical, 'Whether veto mechanisms constrain extraction at the periphery proportionally to the center').

omega_variable(
    tribunician_intercession_real_veto_or_reactive_blocking,
    'Is the Tribune of the Plebs'' intercession power a genuine constraint on senatorial decision-making (proactive veto of harmful decrees), or primarily a reactive blocking mechanism that leaves the Senate with agenda-setting power?',
    'Historical case analysis: compare instances of tribunes blocking senatorial measures with instances of Senate rejecting or anticipating tribunes'' objections; examination of which party initiates most policy changes; measurement of policy distance between tribunes'' preferences and enacted law over time.',
    'If genuinely proactive: Tangled Rope classification for organized plebeians is correct (they have real power). If primarily reactive: they are constrained even more severely — classification might shift toward Snare for lack of ability to shape the agenda. Affects the functional extraction experienced by the plebeian coalition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tribunician_intercession_real_veto_or_reactive_blocking, empirical, 'Whether tribunician intercession is proactive veto or reactive blocking').

omega_variable(
    kernel_reading_contest,
    'Is the Roman Republican constitution a reading of ''how political balance is achieved'' that coexists with the Athenian democratic reading, or do these readings foreclose each other?',
    'Logical analysis: examine whether a single authority structure could instantiate both the Roman tripartite balance (executive/senate/assembly with property qualifications) and the Athenian sortition/rotation/direct democracy (without property qualifications, with mass assembly supremacy). If both can coexist in different states, relation is coexists_with. If implementing Roman-style balance prevents Athenian-style democracy or vice versa, relation is forecloses.',
    'Determines the reading_relations field in cs_structure. If coexists_with: both readings are live in the kernel-contest, representing genuine alternative political solutions. If forecloses: one reading''s core premise contradicts the other''s at the level of institutional design, and they cannot be held simultaneously by a single framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether Roman and Athenian readings foreclose each other or coexist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ancient_constitutionalism__roman_republican_constitution, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rom_rep_theater_founding, ancient_constitutionalism__roman_republican_constitution, theater_ratio, 0, 0.38).
narrative_ontology:measurement(rom_rep_theater_gracchi, ancient_constitutionalism__roman_republican_constitution, theater_ratio, 150, 0.48).
narrative_ontology:measurement(rom_rep_theater_late_republic, ancient_constitutionalism__roman_republican_constitution, theater_ratio, 300, 0.62).

% Extraction over time
narrative_ontology:measurement(rom_rep_extract_founding, ancient_constitutionalism__roman_republican_constitution, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(rom_rep_extract_gracchi, ancient_constitutionalism__roman_republican_constitution, base_extractiveness, 150, 0.52).
narrative_ontology:measurement(rom_rep_extract_late_republic, ancient_constitutionalism__roman_republican_constitution, base_extractiveness, 300, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(rom_rep_suppress_founding, ancient_constitutionalism__roman_republican_constitution, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(rom_rep_suppress_gracchi, ancient_constitutionalism__roman_republican_constitution, suppression_requirement, 150, 0.58).
narrative_ontology:measurement(rom_rep_suppress_late_republic, ancient_constitutionalism__roman_republican_constitution, suppression_requirement, 300, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ancient_constitutionalism__roman_republican_constitution, enforcement_mechanism).
narrative_ontology:affects_constraint(ancient_constitutionalism__roman_republican_constitution, ancient_constitutionalism__athenian_democratic_constitution).

% DUAL FORMULATION NOTE:
% The Roman Republican reading and the Athenian Democratic reading are sibling instantiations of the same contested kernel 'ancient constitutionalism'. They have structurally different epsilon values: the Roman reading (this constraint) has high extractiveness at the periphery (ε ≈ 0.58), while the Athenian reading has lower peripheral extractiveness but higher internal vulnerability to majority tyranny. The network link indicates that the two readings address the same constitutional problem (prevention of tyranny) through mutually exclusive institutional mechanisms. The readings coexist in political discourse but represent different commitments to authority grounding (lineage vs. demos) and legitimacy claims (ancestral wisdom vs. participatory self-governance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ancient_constitutionalism__roman_republican_constitution, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
