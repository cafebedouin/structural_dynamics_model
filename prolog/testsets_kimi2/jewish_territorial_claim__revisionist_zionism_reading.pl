% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__revisionist_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__revisionist_zionism_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: jewish_territorial_claim__revisionist_zionism_reading
 *   human_readable: Revisionist Zionist Maximalist Territorial Claim (Both Banks)
 *   domain: political/historical
 *
 * SUMMARY:
 *   The Revisionist Zionist reading of the Jewish territorial claim asserts
 *   sovereignty over the entire historic Land of Israel on both banks of the
 *   Jordan River, rejecting Arab political consent as a prerequisite and
 *   relying on an 'Iron Wall' of military force to compel eventual
 *   acceptance. This constraint instantiates one reading of the contested
 *   jewish_territorial_claim kernel, distinguished from Political, Labor, and
 *   Cultural Zionist readings by its territorial maximalism, immediatism, and
 *   coercion-based epistemology. The constraint coordinates the Jewish
 *   nationalist movement around a unified territorial program while
 *   extracting land, sovereignty, and self-determination from the Palestinian
 *   Arab population through active military enforcement.
 *
 * KEY AGENTS:
 *   - jewish_revisionist_community: agenda_setter and beneficiary (organized/identity_locked) â sets the claim, enforces via paramilitaries, ideologically committed to territorial maximalism
 *   - palestinian_arab_population: payer (powerless/trapped) â bears extraction via displacement and military subjugation
 *   - zionist_labor_leadership: observer (institutional/constrained) â rival Zionist faction with competing reading
 *   - british_mandatory_authority: observer (institutional/constrained) â colonial administering power
 *   - international_community: observer (institutional/analytical) â external analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, 0.85).
domain_priors:suppression_score(jewish_territorial_claim__revisionist_zionism_reading, 0.9).
domain_priors:theater_ratio(jewish_territorial_claim__revisionist_zionism_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__revisionist_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__revisionist_zionism_reading, "Revisionist Zionist Maximalist Territorial Claim (Both Banks)").
narrative_ontology:topic_domain(jewish_territorial_claim__revisionist_zionism_reading, "political/historical").

domain_priors:requires_active_enforcement(jewish_territorial_claim__revisionist_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__revisionist_zionism_reading, '9f431ed2-3a72-4e37-af29-88310ba3d3be').
narrative_ontology:cs_kernel_codification('9f431ed2-3a72-4e37-af29-88310ba3d3be', fixed_text).
narrative_ontology:cs_authority_grounding('9f431ed2-3a72-4e37-af29-88310ba3d3be', lineage).
narrative_ontology:cs_interpretation_layer_present('9f431ed2-3a72-4e37-af29-88310ba3d3be').
narrative_ontology:cs_reading_relation('9f431ed2-3a72-4e37-af29-88310ba3d3be', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f431ed2-3a72-4e37-af29-88310ba3d3be', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f431ed2-3a72-4e37-af29-88310ba3d3be', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('9f431ed2-3a72-4e37-af29-88310ba3d3be', foundational, biblical_territorial_inheritance).
narrative_ontology:cs_axiom_status(biblical_territorial_inheritance, holdable).
narrative_ontology:cs_axiom_grounding('9f431ed2-3a72-4e37-af29-88310ba3d3be', biblical_territorial_inheritance, deontological).
narrative_ontology:cs_axiom('9f431ed2-3a72-4e37-af29-88310ba3d3be', foundational, iron_wall_coercion_imperative).
narrative_ontology:cs_axiom_status(iron_wall_coercion_imperative, holdable).
narrative_ontology:cs_axiom_grounding('9f431ed2-3a72-4e37-af29-88310ba3d3be', iron_wall_coercion_imperative, instrumental).
narrative_ontology:cs_reference_frame('9f431ed2-3a72-4e37-af29-88310ba3d3be', biblical_territorial_mandate).
narrative_ontology:cs_drift_state('9f431ed2-3a72-4e37-af29-88310ba3d3be', post_mandate_partition_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9f431ed2-3a72-4e37-af29-88310ba3d3be', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, jewish_revisionist_community).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_population).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, iron_wall_doctrine).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, jewish_historical_territorial_integrity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizes settlement, paramilitary activity, and political agitation to assert Jewish sovereignty over both banks of the Jordan. Bears ideological commitment to territorial maximalism; exit means abandoning the core Zionist vision as they define it.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, jewish_revisionist_community, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__revisionist_zionism_reading, jewish_revisionist_community, beneficiary).

% Lives under military threat, land confiscation, and political exclusion from the territorial claim. Subject to the 'Iron Wall' of force; alternatives such as independent statehood, return, or equal citizenship are structurally blocked by the enforcement of the claim.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_population, payer,
    powerless, biographical, trapped, regional).

% Competing Zionist faction advocating gradualism, socialist transformation, and partition. Opposes the maximalist military approach but shares the broader Zionist goal of Jewish statehood. Sees the revisionist claim as undermining diplomatic efforts.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, zionist_labor_leadership, observer,
    institutional, generational, constrained, national).

% Colonial administering power over Palestine. Caught between honoring the Balfour Declaration and managing Arab opposition. Periodically restricts Jewish immigration and land purchase, coming into direct tension with the maximalist claim.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, british_mandatory_authority, observer,
    institutional, immediate, constrained, national).

% League of Nations and later UN bodies observe the conflict. The mandate system nominally oversees the territory, and partition proposals emerge as alternatives to the maximalist claim, but enforcement capacity is limited.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, international_community, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__revisionist_zionism_reading, jewish_revisionist_community).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__revisionist_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Jewish nationalist movement around a unified territorial program spanning both banks of the Jordan, centralizing military and settlement efforts under a single maximalist sovereignty claim.
% TRANSFER_FUNCTION: Transfers territorial control, demographic dominance, and political sovereignty from Palestinian Arab inhabitants to the Jewish settler population through sustained military presence and compulsion.
% ABSENT_VOICES: Palestinian Arab political leadership and population are excluded from consent negotiations; moderate Zionist partitionists and minority international voices advocating binational or cultural solutions are marginalized within the revisionist framework.
% DISAPPEARANCE_RATIONALE: If the maximalist claim and its enforcement machinery vanished, Jewish settler territorial expansion would halt or reverse, Palestinian Arab political and territorial alternatives would become viable, and the regional arrangement would restructure around partition or binational frameworks rather than maximalist compulsion.
% FOUNDING_PROBLEM: Jewish statelessness, diaspora vulnerability, and the perceived impossibility of Jewish national survival without immediate territorial sovereignty over the full historic homeland.
% FOUNDING_PROBLEM_CORROBORATION: Revisionist Zionist ideologues and paramilitary leaders attest from within the beneficiary set. Palestinian Arab leaders, anti-Zionist Jewish intellectuals, and British colonial administrators from outside the beneficiary set contest that the problem required this specific territorial solution or method.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__revisionist_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__revisionist_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__revisionist_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__revisionist_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__revisionist_zionism_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the constraint systematically transfers territorial control and political sovereignty from one population to another by force. Suppression is very high (0.90) because the arrangement's persistence depends on actively suppressing Palestinian alternatives and militarily defeating Arab resistance. Theater ratio is moderate (0.45): early settlement and military activity had substantial functional content, but a growing share became performative demonstrations of irreversible presence intended to compel psychological acceptance. Accessibility collapse is high (0.70) for Palestinian alternatives once the claim is understood; resistance is high (0.85) because the constraint meets sustained violent and political opposition. The metrics are authored independently of the claimed type: the structural claim is tangled_rope because genuine coordination exists for the Jewish nationalist camp, but the metrics honestly report heavy extraction and suppression.
 *
 * PERSPECTIVAL GAP:
 *   The Jewish revisionist seat experiences the constraint as necessary national coordinationâa collective-action solution to statelessnessâwhile the Palestinian Arab seat experiences the identical structure as coercive extraction denying self-determination. The British and international observer seats see a settler-colonial claim requiring colonial enforcement. The engine computes these divergences from the structural data without reconciling them.
 *
 * DIRECTIONALITY LOGIC:
 *   The jewish_revisionist_community is both agenda_setter and beneficiary: they set the rules of territorial allocation and collect the sovereignty and land (low directionality). The palestinian_arab_population is the payer: they bear the costs of dispossession and military subjugation with no exit (high directionality). The zionist_labor_leadership occupies a middle positionâsharing some beneficiary features but opposing the maximalist enforcement mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than snare preserves the genuine coordination function the constraint serves for Jewish nationalistsâorganizing a dispersed people around a concrete territorial programâwhile registering that the same structure simultaneously extracts from Palestinian Arabs. A snare reading would erase the coordination function entirely; a rope reading would erase the extraction. Tangled_rope captures both. Mandatrophy is unresolved: the founding problem of Jewish statelessness remains contested, and the territorial solution has not achieved stable acceptance, suggesting the constraint persists beyond its solved founding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_foreclosure_ambiguity,
    'Does the ''Iron Wall'' framework permanently foreclose Arab political consent, or is it a temporary transitional mechanism that could eventually permit negotiated acceptance?',
    'Historical trajectory analysis: if military enforcement relaxes without mutual recognition, does the constraint collapse or convert to voluntary coordination?',
    'If permanent foreclosure, classification leans snare; if transitional coercion, scaffold or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_foreclosure_ambiguity, conceptual, 'Whether Iron Wall coercion is permanent or transitional').

omega_variable(
    territorial_scope_necessity,
    'Is sovereignty over both banks of the Jordan structurally necessary for Jewish national survival, or an ideological excess separable from the core coordination function?',
    'Comparative analysis with political Zionism: does statehood on one bank satisfy the founding problem?',
    'If separable, the extraction from Palestinian Arabs east of the Jordan is pure ideological rent, increasing epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_scope_necessity, preference, 'Whether territorial maximalism is necessary or excess').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of Palestinian alternatives primarily structural (military occupation and legal exclusion) or internalized (hegemonic acceptance of Jewish territorial supremacy)?',
    'Post-withdrawal or post-reform trajectory: do Palestinian claims re-emerge immediately or show persistent suppression scars?',
    'If internalized, effective suppression exceeds structural measure; if purely structural, resistance should spike when enforcement gaps appear.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__revisionist_zionism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jtc_rev_tr_t0, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jtc_rev_tr_t5, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(jtc_rev_tr_t10, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(jtc_rev_tr_t15, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(jtc_rev_tr_t20, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(jtc_rev_tr_t25, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement(jtc_rev_tr_t30, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(jtc_rev_be_t0, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(jtc_rev_be_t5, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(jtc_rev_be_t10, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(jtc_rev_be_t15, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 15, 0.74).
narrative_ontology:measurement(jtc_rev_be_t20, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement(jtc_rev_be_t25, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 25, 0.83).
narrative_ontology:measurement(jtc_rev_be_t30, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jtc_rev_su_t0, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(jtc_rev_su_t5, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(jtc_rev_su_t10, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(jtc_rev_su_t15, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 15, 0.82).
narrative_ontology:measurement(jtc_rev_su_t20, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 20, 0.86).
narrative_ontology:measurement(jtc_rev_su_t25, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 25, 0.89).
narrative_ontology:measurement(jtc_rev_su_t30, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__revisionist_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the jewish_territorial_claim kernel family. The epsilon-invariance principle requires separate stories for each reading because the structural claims, beneficiary/victim distributions, and epsilon values differ significantly across sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
