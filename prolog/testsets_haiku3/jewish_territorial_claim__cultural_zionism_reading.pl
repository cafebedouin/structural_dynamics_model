% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__cultural_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__cultural_zionism_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: jewish_territorial_claim__cultural_zionism_reading
 *   human_readable: Jewish Cultural and Spiritual Center in Palestine (Cultural Zionism Reading)
 *   domain: political_history/settler_colonialism/nationalism
 *
 * SUMMARY:
 *   This constraint instantiates the cultural Zionism reading of the
 *   contested kernel 'jewish territorial claim'. The reading specifies Jewish
 *   settlement in Palestine as primarily a project of cultural and
 *   intellectual autonomy—language revival, philosophical development, and
 *   institutional self-governance—without necessarily requiring political
 *   sovereignty or demographic majority. The constraint is claimed as rope
 *   (genuine coordination of dispersed cultural ambitions toward a shared
 *   territorial center) while authored metrics show moderate extraction
 *   (territorial allocation from Palestinian Arabs, suppression of competing
 *   claims in the foundational narrative) rising over time as political
 *   pressure mounts. The claim/metric divergence is deliberate and
 *   structural: the reading's own frame does not position Arab presence as
 *   threatening, yet the actual operation requires suppressing Arab voice in
 *   settlement decisions. This divergence is the measurement the constraint
 *   story exists to capture.
 *
 * KEY AGENTS:
 *   - jewish_diaspora_seeking_cultural_autonomy: primary beneficiary, organized power, global reach — seeks institutional autonomy for cultural development
 *   - hebrew_language_revivalists: beneficiary within coordinated project, moderate power, regional scope — focuses language reconstruction as central function
 *   - jewish_intellectual_community: beneficiary, organized power, biographical horizon — pursues philosophical and theological work in Hebrew context
 *   - palestinian_arab_population: excluded by framing, moderate power, constrained exit — territorial inhabitants whose claims remain unframed in the constraint narrative
 *   - palestinian_intellectual_community: excluded by framing, moderate power, constrained exit — parallel cultural project without voice in the arrangement
 *   - binational_framework_advocates: analytical observer, organized power — measures whether constraint can function without exclusion
 *   - political_zionist_establishment: analytical observer, institutional power — measures constraint against sovereignty requirement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, 0.38).
domain_priors:suppression_score(jewish_territorial_claim__cultural_zionism_reading, 0.42).
domain_priors:theater_ratio(jewish_territorial_claim__cultural_zionism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__cultural_zionism_reading, rope).
narrative_ontology:human_readable(jewish_territorial_claim__cultural_zionism_reading, "Jewish Cultural and Spiritual Center in Palestine (Cultural Zionism Reading)").
narrative_ontology:topic_domain(jewish_territorial_claim__cultural_zionism_reading, "political_history/settler_colonialism/nationalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__cultural_zionism_reading, '47bda78e-ccd9-4b03-8d33-ef1d65501343').
narrative_ontology:cs_kernel_codification('47bda78e-ccd9-4b03-8d33-ef1d65501343', distributed).
narrative_ontology:cs_authority_grounding('47bda78e-ccd9-4b03-8d33-ef1d65501343', lineage).
narrative_ontology:cs_interpretation_layer_present('47bda78e-ccd9-4b03-8d33-ef1d65501343').
narrative_ontology:cs_reading_relation('47bda78e-ccd9-4b03-8d33-ef1d65501343', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('47bda78e-ccd9-4b03-8d33-ef1d65501343', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('47bda78e-ccd9-4b03-8d33-ef1d65501343', jewish_territorial_claim__revisionist_zionism_reading, forecloses).
narrative_ontology:cs_axiom('47bda78e-ccd9-4b03-8d33-ef1d65501343', foundational, cultural_autonomy_without_territorial_dominance).
narrative_ontology:cs_axiom_status(cultural_autonomy_without_territorial_dominance, holdable).
narrative_ontology:cs_axiom_grounding('47bda78e-ccd9-4b03-8d33-ef1d65501343', cultural_autonomy_without_territorial_dominance, deontological).
narrative_ontology:cs_axiom('47bda78e-ccd9-4b03-8d33-ef1d65501343', foundational, arab_presence_not_inherently_threatening_to_jewish_culture).
narrative_ontology:cs_axiom_status(arab_presence_not_inherently_threatening_to_jewish_culture, holdable).
narrative_ontology:cs_axiom_grounding('47bda78e-ccd9-4b03-8d33-ef1d65501343', arab_presence_not_inherently_threatening_to_jewish_culture, empirically_contingent).
narrative_ontology:cs_reference_frame('47bda78e-ccd9-4b03-8d33-ef1d65501343', jewish_cultural_pluralism_in_shared_territory).
narrative_ontology:cs_drift_state('47bda78e-ccd9-4b03-8d33-ef1d65501343', late_twentieth_century_institutional_capture, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('47bda78e-ccd9-4b03-8d33-ef1d65501343', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, jewish_diaspora_seeking_cultural_autonomy).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, hebrew_language_revivalists).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, jewish_intellectual_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks a territorial space where Jewish cultural, spiritual, and intellectual life can flourish without coercive assimilation pressures from host societies. Views Palestine as a location where Hebrew language revival, Jewish philosophy, and religious study can develop without dependence on gentile permission structures. Benefits from the constraint's framing of settlement as cultural-spiritual project rather than political takeover.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, jewish_diaspora_seeking_cultural_autonomy, beneficiary,
    organized, generational, arbitrage, global).

% Actively reconstructs Hebrew as a living language and medium for modern cultural production. Views Palestine as a space where Hebrew can be the dominant social language without defensive response, enabling literary, educational, and intellectual work in the language. The constraint enables their project by providing territorial context where revival efforts gain institutional support.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, hebrew_language_revivalists, beneficiary,
    moderate, generational, mobile, regional).

% Engages in Jewish philosophy, Talmudic study, ethical thought, and theological reconstruction. Seeks a territorial center where this intellectual work is the primary social function—not a diaspora minority practice requiring accommodation in non-Jewish institutions. Views Palestine as enabling a center of Jewish thought oriented toward its own questions rather than external pressures.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, jewish_intellectual_community, beneficiary,
    organized, biographical, mobile, global).

% Inhabits the same territory and holds competing territorial and cultural claims. Under this reading's framework, their presence is not inherently threatening to the Jewish cultural project—the reading explicitly rejects demographic majority as a requirement. Their exclusion from the constraint's narrative is structural: the reading does not position Arab objections as part of its legitimacy claim, creating an asymmetry where their input on territorial arrangement remains unframed.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_population, excluded,
    moderate, generational, constrained, regional).

% Holds parallel intellectual and cultural projects in the same territory. Would likely object to any arrangement that subordinates Palestinian cultural autonomy, language revitalization, or intellectual space to Jewish primacy. Their absence from the constraint's framing—as participants in the negotiation over what 'cultural center' means and who decides—is the mark of their exclusion.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, palestinian_intellectual_community, excluded,
    moderate, biographical, constrained, regional).

% Argue for structures in which both Jewish and Arab cultural and political projects can coexist with negotiated autonomy and shared institutions. From this seat, the cultural_zionism reading is evaluated as potentially compatible with binationalism if stripped of exclusionary demographic or sovereignty requirements. They measure whether the constraint can function without suppressing Palestinian cultural claims.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, binational_framework_advocates, observer,
    organized, generational, analytical, regional).

% Pursues territorial sovereignty and Jewish demographic majority as the solution to diaspora vulnerability. Views the cultural_zionism reading as insufficient because it does not secure statehood or majority control. Observes the constraint from the position of those who believe cultural autonomy without political power is unstable and ultimately indefensible.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, political_zionist_establishment, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__cultural_zionism_reading, jewish_intellectual_community).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__cultural_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates institutional and territorial conditions for autonomous development of Jewish cultural, intellectual, and spiritual projects: Hebrew language as lived medium, Talmudic and philosophical study without diaspora constraints, community self-governance of cultural priorities, and Hebrew-medium literature and arts. Solves the coordination problem of Jewish intellectual work requiring social coherence around a shared language and set of priorities.
% TRANSFER_FUNCTION: Allocates territorial space and institutional autonomy from the Palestinian Arab population to the Jewish cultural project. Does not inherently transfer demographic control or political sovereignty—the reading explicitly rejects those requirements. The transfer is of social space and decision-making authority over cultural institutions, not of land rights or statehood.
% ABSENT_VOICES: Palestinian Arabs whose cultural and territorial claims are parallel to Jewish ones remain excluded from the framing of what 'cultural center' means, who decides settlement patterns, and how competing claims are negotiated. Palestinian intellectuals whose philosophical and literary projects would occupy the same space are structurally outside the constraint's legitimacy narrative. Anti-Zionist and post-Zionist Jewish voices questioning whether territorial settlement solves the stated problems are also unframed.
% DISAPPEARANCE_RATIONALE: Political Zionists argue that if the cultural_zionism reading were adopted without political sovereignty, Jewish cultural institutions would remain vulnerable to external pressure and state dissolution, making the project unsustainable—the world would rearrange away from Jewish institutional autonomy. Binational advocates argue that without disappearing, the constraint could be modified to require Palestinian co-design of shared institutions, leaving both cultural projects intact. Critics argue the constraint is already disappearing by being subordinated to political sovereignty demands, so the question is moot.
% FOUNDING_PROBLEM: Jewish cultural and intellectual life in diaspora is threatened by assimilation pressures, institutional subordination to non-Jewish frameworks, and loss of Hebrew as a living language. The project seeks a territorial space where Jewish culture develops autonomously according to its own intellectual and spiritual priorities, not as a minority practice requiring accommodation in host societies.
% FOUNDING_PROBLEM_CORROBORATION: Hebrew language revivalists and Jewish philosophers of the late 19th and early 20th centuries—Ahad Ha'am, Asher Ginsberg, and cultural Zionist thinkers—document the assimilation and cultural fragmentation in diaspora Jewish communities. Palestinian Arab intellectuals and postcolonial scholars contest whether territorial occupation is the only or correct solution to diaspora vulnerability, noting it creates new victims rather than solving the stated problem. No corroboration exists from Palestinian stakeholders affirming the founding problem as stated, only from external scholars analyzing the historical intellectual currents.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__cultural_zionism_reading, contested).
narrative_ontology:founding_problem_status(jewish_territorial_claim__cultural_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__cultural_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__cultural_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__cultural_zionism_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__cultural_zionism_reading_tests).
:- end_tests(jewish_territorial_claim__cultural_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins low (0.25 projected at t0) because the reading frames settlement as cultural self-determination without dispossession. It rises to 0.42 by t60 as territorial consolidation requires suppressing Palestinian claims and as political Zionist institutions capture the cultural project toward sovereignty aims. By t120 it recedes to 0.38 as binational and post-Zionist voices gain volume, destabilizing the framing and forcing acknowledgment of Palestinian exclusion. The suppression_requirement trajectory (0.18→0.48→0.42) tracks the mounting pressure required to maintain the exclusive framing against Palestinian objection and internal fracturing. Theater_ratio rises from 0.15 to 0.31 by t90 as the constraint's cultural-autonomy story increasingly fronts security and political concerns, then falls slightly to 0.28 at t120 as the performance becomes transparent. The rising early and partial decline at the end model the constraint's trajectory under mounting contestation: initially lightweight (genuine cultural coordination), intensifying as political stakes sharpen, then eroding as the excluded voices breach the framing. One shared time grid across all metrics ensures alignment.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (diaspora Jews, revivalists, intellectuals) perceive the constraint as enabling autonomous cultural development—a rope, a solution to diaspora fragmentation. The excluded seats (Palestinian Arabs and intellectuals) perceive it as territorial extraction and cultural erasure—a snare by their lights, though not authored as one here. The binational observer seat measures whether the reading's stated rejection of demographic majority could permit genuine coexistence; political Zionist observers measure it against sovereignty requirements they view as essential. The engine computes these divergent positions from stakeholder power, exit_options, and exclusion status; the authored claim does not adjudicate which perception is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jews and intellectuals are beneficiaries (d near 0.0–0.2): they collect the coordination benefit (cultural autonomy) without bearing suppression costs directly. Palestinian Arabs and intellectuals are excluded (not stakeholders with roles—their exclusion is structural): if they were seated, they would be payers or excluded entirely, with d near 0.8–1.0. Binational advocates and political Zionists are analytical observers (d = 0.5, symmetric): they neither collect nor pay, but measure the constraint against their own criteria. The asymmetry in beneficiary/victim structure is the key: the constraint's legitimacy narrative centers the Jewish cultural project and leaves Palestinian cultural autonomy unframed, creating structural directionality toward extraction even though the reading claims not to require it.
 *
 * MANDATROPHY ANALYSIS:
 *   The cultural Zionism reading faces a mandatrophy trap: the founding problem (Jewish diaspora cultural fragmentation) is real, but the solution (territorial settlement in Palestine) creates new foundational problems (Palestinian territorial and cultural claims). At t60–90, suppression peaks as the constraint requires maximal effort to suppress Palestinian counterclaims. By t120, the mandate has inverted: defenders of cultural Zionism find themselves defending territorial occupation and military suppression, which contradicts the reading's own premise that Arab presence is not inherently threatening. The constraint persists not because the founding problem remains acute (assimilation pressures have actually decreased in some diaspora communities by t90+), but because political Zionist institutional interests have captured the cultural project. This is textbook mandatrophy: the founding problem dies but the arrangement lives, now sustained by extraction rather than coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_autonomy_without_territorial_control,
    'Can Jewish cultural and intellectual autonomy be institutionally guaranteed without territorial control and demographic majority? Or does territorial control become functionally necessary to protect minority cultural projects from external pressure?',
    'Historical-comparative analysis: examine whether autonomous cultural institutions have persisted and flourished in shared territories (e.g., pluralist empires, federated systems, minority-rights frameworks) versus whether they required territorial control. Test cases: Palestinian binational proposals, minority cultural autonomy in Europe, Jewish community autonomy in diaspora contexts.',
    'If autonomy is institutionally achievable without control, the cultural_zionism reading remains coherent and the constraint might function as rope. If territorial control becomes functionally necessary, the reading collapses into political Zionism and the constraint reclassifies toward snare (suppression of Palestinian autonomy becomes inherent, not contingent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_autonomy_without_territorial_control, empirical, 'Whether cultural autonomy requires territorial control or can survive in shared institutional frameworks.').

omega_variable(
    palestinian_exclusion_structural_or_contingent,
    'Is the exclusion of Palestinian voice from the constraint''s founding narrative a contingent feature of how cultural Zionism was historically implemented, or is it structurally entailed by the reading itself?',
    'Analyze whether a binational version of the cultural_zionism reading—in which Palestinian cultural claims are framed as partners rather than threats—remains internally coherent or whether it requires revision of the reading''s core premises. Examine writings of Buber, Magnes, and other cultural Zionists on binationalism.',
    'If exclusion is contingent, the reading could be reformulated to include Palestinian cultural autonomy and the constraint might remain rope-classified. If structurally entailed, the reading''s claim of non-threatening Arab presence contradicts its actual operation and the constraint should reclassify toward tangled_rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(palestinian_exclusion_structural_or_contingent, conceptual, 'Whether Arab exclusion from the constraint''s frame is inherent to cultural Zionism or an implementation choice.').

omega_variable(
    founding_problem_displacement,
    'By t90–120, what is the founding problem the constraint actually solves? Is it still Jewish cultural fragmentation in diaspora, or has it shifted to political security and demographic sovereignty?',
    'Examine institutional priorities at each time point: what problems do settlement institutions actually allocate resources to (language, philosophy, security, territorial control)? Track rhetoric and resource allocation separately.',
    'If the founding problem has shifted to sovereignty and security, the constraint has experienced mandatrophy and should be reclassified; the constraint story should note the problem displacement and potentially spin off a new story for the sovereignty requirement under political_zionism_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_displacement, empirical, 'Whether the constraint''s founding problem persists or has been displaced by political objectives.').

omega_variable(
    suppression_internalization,
    'Is the rising suppression_requirement (0.18→0.48) primarily structural (external Palestinian resistance requiring active enforcement) or internalized (cultural Zionist institutions progressively adopting military/security ideology, suppressing their own anti-coercive members)?',
    'Post-institutional analysis: compare suppression mechanisms before and after institutional capture by political Zionism. Examine internal dissent within cultural institutions (e.g., Brit Shalom, binational advocates) and how it is handled.',
    'If suppression is structural, the constraint''s persistence depends on external coercion and reclassification toward snare becomes likely. If internalized, the suppression persists even if Palestinian resistance ceases—the constraint has colonized its own constituency and mandatrophy is irreversible without ideological rupture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether measured suppression reflects external resistance or internal ideological capture.').

omega_variable(
    reading_foreclosure_by_political_zionism,
    'Does the political_zionism reading logically foreclose the cultural_zionism reading within a single institutional framework, or do they coexist as competing values that institutional design could potentially honor?',
    'Analyze whether a federal or binational structure could allocate cultural autonomy to Jewish institutions while allocating political sovereignty to a joint or separate Palestinian state. Test: are there logically consistent frameworks holding both readings, or does one reading''s core premise negate the other''s?',
    'If foreclosure is real (one reading''s premise logically eliminates the other), the relation should be forecloses. If coexistence is possible within certain institutional designs, the relation should be coexists_with or influences. This affects the constraint family''s internal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_by_political_zionism, conceptual, 'Whether political and cultural Zionism readings are logically incompatible or merely compete institutionally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__cultural_zionism_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jewi_tr_t20, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(jewi_tr_t40, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(jewi_tr_t60, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(jewi_tr_t90, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 90, 0.31).
narrative_ontology:measurement(jewi_tr_t120, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 120, 0.28).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(jewi_be_t20, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(jewi_be_t40, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(jewi_be_t60, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(jewi_be_t90, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 90, 0.45).
narrative_ontology:measurement(jewi_be_t120, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 120, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(jewi_su_t20, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(jewi_su_t40, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(jewi_su_t60, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 60, 0.44).
narrative_ontology:measurement(jewi_su_t90, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 90, 0.48).
narrative_ontology:measurement(jewi_su_t120, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 120, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__cultural_zionism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__cultural_zionism_reading, 0.12).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint and its three sibling readings (political_zionism, labor_zionism, revisionist_zionism) constitute a constraint family instantiating four readings of a single contested kernel: jewish_territorial_claim. The ε-invariance principle requires separate stories because the four readings generate structurally different constraints with different referents, different beneficiary/victim structures, and different classifications. Cultural_zionism emphasizes cultural autonomy without sovereignty (this file); political_zionism emphasizes statehood as solution to antisemitism; labor_zionism emphasizes socialist transformation and settlement facts; revisionist_zionism emphasizes maximalist territorial claim and military compulsion. Each reading's constraint measures the structural cost of its own instantiation. Network links enable contamination-propagation analysis: if cultural autonomy (this reading) proves institutionally indefensible without political control, political_zionism's extraction costs rise; if political Zionism's sovereignty claims are undermined, cultural_zionism's viability as autonomous project increases.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
