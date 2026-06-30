% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__indigenous_return_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__indigenous_return_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: jewish_self_determination__indigenous_return_reading
 *   human_readable: Jewish Indigenous Return: Historical Fact of Unbroken Connection
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents one reading of a deeply contested kernel:
 *   whether Jewish self-determination constitutes decolonization or
 *   colonization of the same territory. The indigenous-return reading asserts
 *   unbroken historical and genealogical connection to the Levantine
 *   territory, claiming that contemporary Jewish political sovereignty
 *   represents return to an ancestral homeland and therefore qualifies as
 *   indigenous self-determination, not settler colonialism. This reading
 *   competes with four other readings of the same underlying kernel
 *   (diasporist, liberal nationalist, religious covenant, settler colonial)
 *   that offer different framings of Jewish territorial claims. Each reading
 *   produces a different constraint with different beneficiaries, victims,
 *   and epsilon values. This story instantiates ONLY the indigenous-return
 *   reading as a structurally coherent constraint; the other readings are
 *   separate constraint stories (not authored here) linked through
 *   network.affects_constraints. The authored metrics describe substantial
 *   contestation and increasing enforcement burden over the interval — this
 *   reading is NOT presenting itself as an uncontested fact, but as a claim
 *   whose operation requires active defense and reframing of contrary
 *   evidence.
 *
 * KEY AGENTS:
 *   - jewish_claimants_to_ancestral_land (organized, identity-locked): the constituency for whom this reading's legitimacy is constitutive of collective identity and political agency
 *   - palestinian_presence_interpreters (organized, constrained): those whose territorial and historical claims are subordinated by this reading's operation
 *   - secular_historians_and_archaeologists (institutional, analytical): the evidentiary arbiters whose findings either support or undermine the reading's historical claims
 *   - religious_and_nationalist_interpreters (organized, identity-locked): the agenda-setters who construct and defend the reading's narrative
 *   - postcolonial_theorists (institutional, excluded): those whose analytical framework would classify this reading as a settler-colonial counter-narrative
 *   - diasporist_critics (organized, constrained): Jewish intellectuals whose alternative framework forecloses this reading's legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, 0.78).
domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, 0.82).
domain_priors:theater_ratio(jewish_self_determination__indigenous_return_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__indigenous_return_reading, mountain).
narrative_ontology:human_readable(jewish_self_determination__indigenous_return_reading, "Jewish Indigenous Return: Historical Fact of Unbroken Connection").
narrative_ontology:topic_domain(jewish_self_determination__indigenous_return_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:emerges_naturally(jewish_self_determination__indigenous_return_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__indigenous_return_reading, 'e3397258-e171-4fc4-bdbe-36269bc107a7').
narrative_ontology:cs_kernel_codification('e3397258-e171-4fc4-bdbe-36269bc107a7', fixed_text).
narrative_ontology:cs_authority_grounding('e3397258-e171-4fc4-bdbe-36269bc107a7', extraction).
narrative_ontology:cs_interpretation_layer_present('e3397258-e171-4fc4-bdbe-36269bc107a7').
narrative_ontology:cs_reading_relation('e3397258-e171-4fc4-bdbe-36269bc107a7', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_reading_relation('e3397258-e171-4fc4-bdbe-36269bc107a7', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3397258-e171-4fc4-bdbe-36269bc107a7', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3397258-e171-4fc4-bdbe-36269bc107a7', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_axiom('e3397258-e171-4fc4-bdbe-36269bc107a7', foundational, unbroken_jewish_connection_to_levantine_territory).
narrative_ontology:cs_axiom_status(unbroken_jewish_connection_to_levantine_territory, holdable).
narrative_ontology:cs_axiom_grounding('e3397258-e171-4fc4-bdbe-36269bc107a7', unbroken_jewish_connection_to_levantine_territory, empirically_contingent).
narrative_ontology:cs_axiom('e3397258-e171-4fc4-bdbe-36269bc107a7', foundational, indigenous_status_grants_self_determination_priority).
narrative_ontology:cs_axiom_status(indigenous_status_grants_self_determination_priority, holdable).
narrative_ontology:cs_axiom_grounding('e3397258-e171-4fc4-bdbe-36269bc107a7', indigenous_status_grants_self_determination_priority, deontological).
narrative_ontology:cs_axiom('e3397258-e171-4fc4-bdbe-36269bc107a7', secondary, zionism_as_decolonization_not_colonization).
narrative_ontology:cs_axiom_status(zionism_as_decolonization_not_colonization, holdable).
narrative_ontology:cs_axiom_grounding('e3397258-e171-4fc4-bdbe-36269bc107a7', zionism_as_decolonization_not_colonization, conventional).
narrative_ontology:cs_reference_frame('e3397258-e171-4fc4-bdbe-36269bc107a7', jewish_historical_continuity_and_indigenous_status).
narrative_ontology:cs_drift_state('e3397258-e171-4fc4-bdbe-36269bc107a7', contemporary_postcolonial_era_2020s, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e3397258-e171-4fc4-bdbe-36269bc107a7', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__indigenous_return_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__indigenous_return_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__indigenous_return_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__indigenous_return_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, ExtMetricName, E),
    domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(jewish_self_determination__indigenous_return_reading),
    narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(jewish_self_determination__indigenous_return_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as high (0.78 at interval end) not because the reading is false, but because its operation requires continuous narrative work to maintain historical claims against contrary evidence, subordinate alternative accounts, and suppress postcolonial framings that would recharacterize the constraint itself. The reading's extractiveness increases over the interval (0.42→0.78) as: (1) historical and archaeological findings accumulate that complicate claims of unbroken connection; (2) Palestinian historical narratives gain institutional credibility; (3) postcolonial theory becomes mainstream in universities. The constraint responds by increasing theater (narrative reframing, selective evidence use, institutional gatekeeping) and suppression (excluding contrary voices, reinterpreting findings, controlling academic narrative). Suppression is high (0.82) because the reading's persistence depends on actively constraining how evidence is interpreted and whose voices are heard in scholarly and political conversation. Theater ratio (0.41) reflects that an increasing share of effort is devoted to narrative maintenance rather than simply asserting the historical claim—this is Goodhart drift: the reading's original function (establish legitimate claim to territory) is supplemented by performative defense of the claim against evidence. Accessibility collapse is moderate (0.68): alternatives to the unbroken-connection reading are logically available and intellectually coherent, but adopting them carries high cost for Jewish constituencies identity-locked to this reading. Resistance is very high (0.79): the constraint meets substantial resistance from Palestinian historians, secular archaeologists, postcolonial scholars, and diasporist critics whose empirical and theoretical frameworks directly contradict it. The reading is NOT presented as a natural law; it is presented as a factual historical claim that is in fact highly contested.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (jewish_claimants) and the payer seat (palestinian_presence_interpreters) should compute radically different classifications. From the beneficiary seat, the constraint appears as restitution and indigenous self-determination—a rope solving a genuine coordination problem (how a diaspora population maintains collective identity and security). From the payer seat, the same constraint operates as enforced territorial subordination and recharacterization of historical presence—a snare whose persistence depends on suppressing alternative historical narratives and Palestinian self-determination claims. The agenda-setter seat (religious/nationalist interpreters) experiences the constraint as requiring active narrative defense and institutional gatekeeping to maintain legitimacy against contrary evidence. The observer seat (secular historians) experiences institutional pressure to align their findings with the reading while maintaining scholarly integrity. The excluded seats (postcolonial theorists, diasporist critics) experience the constraint as structurally foreclosing their positions within the conversation. This multi-seat divergence is exactly what the engine should detect from the structural data: same constraint, radically different effective types depending on where the agent sits.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation flows from beneficiary/victim structure and exit options. Jewish claimants are beneficiaries (the reading legitimates their territorial claim and political agency; d → 0.0 for a beneficiary seat). Palestinian presence is reframed as victim—but a victim whose victimization is narratively constructed as either 'later arrival' or 'subordinate co-indigenous claim' rather than dispossession. Under this reading's own terms, Palestinians are not victims of extraction but of historical population movement or legitimate subordination. Exit options for jewish_claimants are identity_locked: rejecting this reading would require severing collective identity constituted through genealogical and religious narrative. Exit for palestinian_presence_interpreters is constrained: they can refuse the reading's framing, but the reading's operational power (institutional gatekeeping, narrative authority, political legitimacy) makes exit costly. The derivation chain produces moderate-to-high d values for payer seats (0.6–0.8 range) reflecting constrained but not trapped exit, and low d values for beneficiary seat (0.2–0.3 range) reflecting identity-locked beneficiary status. This produces a moderate but significant directionality asymmetry, feeding the tangled_rope classification range.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish vulnerability and need for self-determination after the Holocaust) was historically live and remains contested in status. The reading's mandate was: provide a historical and political legitimacy framework that explains Jewish territorial claims as indigenous return rather than colonization. That mandate has not been abandoned, but its necessity is contested. Secular historians argue the founding problem is substantially solved through Holocaust restitution, international law, and UN recognition of Jewish peoplehood. Postcolonial theorists argue the reading itself has become the problem: it sustains territorial expansion and Palestinian subordination by continuously reframing dispossession as restoration. Diasporist critics argue the mandate was misconceived—Jewish security lies in pluralism and minority rights, not militarized sovereignty. The constraint has not lost its mandate, but the mandate's legitimacy and necessity are under sustained contestation. This is not yet mandatrophy (the reading is actively defended and reformed), but it is approaching the contested zone where the original function (legitimate Jewish self-determination) is increasingly supplemented by functional drift (justify territorial expansion, suppress Palestinian narratives, maintain ethnic-demographic advantage).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalism_vs_construction_ambiguity,
    'Is the unbroken Jewish connection to the land a natural historical fact (mountain status) or a constructed historical narrative that benefits contemporary Jewish claimants and requires suppression of alternative narratives to maintain (snare/tangled_rope status)?',
    'Independent scholarly assessment of archaeological evidence, demographic continuity records, and historical texts by scholars with no institutional stake in either reading. Examination of how evidence has been selected, interpreted, and presented by advocates of each reading. Post-exit suppression trajectory: if Palestinians gain territorial and political control, does the unbroken-connection narrative persist, or is it abandoned as instrumentally unnecessary?',
    'If the connection is a natural fact, the constraint is a mountain and classification should reflect that. If constructed, the constraint is a snare whose persistence depends on suppression of contrary evidence, and should classify accordingly. Current authored metrics suggest the constraint operates as snare/tangled_rope (high extractiveness, high suppression, high theater, high resistance), indicating the constructed reading is more empirically accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalism_vs_construction_ambiguity, empirical, 'Whether the unbroken-connection claim is objective historical fact or contingent narrative serving contemporary political interests.').

omega_variable(
    reading_contestation_and_foreclosure,
    'Does the indigenous_return_reading logically foreclose the settler_colonial_reading (they cannot both be true in a single framework), or do they coexist as competing narratives held by different parties with no possibility of resolution within the current institutional structure?',
    'Examine the logical structure of each reading''s core premises. Can a framework be constructed that holds both readings simultaneously (reading as matters of interpretation rather than fact), or does accepting one reading''s core premise necessarily reject the other''s? If they coexist without logical resolution, the constraint is perpetually contested; if they foreclose each other, one reading must ultimately prevail or be abandoned.',
    'If the readings foreclose each other, the contest is fundamentally about which reading''s truth claim will be institutionally recognized; this makes the constraint a winner-take-all battle over historical legitimacy. If they coexist, the constraint persists through institutional compartmentalization and different parties inhabiting different historical narratives—this is the perpetual contestation signature. Current corpus data suggests coexistence (both readings have sustained institutional backing, neither has been defeated), indicating the constraint is structurally unresolvable within current institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contestation_and_foreclosure, conceptual, 'Whether competing kernel readings are logically exclusive or can coexist indefinitely.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the authored suppression (0.82) primarily structural (institutional barriers, control of narrative spaces, gatekeeping in academia and media) or has it become internalized in Jewish constituencies such that suppression persists even when external barriers are removed?',
    'Examine Jewish communities in contexts where Palestinian narratives have gained institutional legitimacy (universities with strong postcolonial curricula, international forums with Palestinian participation): do Jewish constituencies continue to suppress or reframe alternative narratives, or do they engage them directly? Post-institutional-change trajectories: if Palestinian self-determination becomes institutionally recognized, does the indigenous-return reading persist due to identity-fusion, or is it abandoned as institutionally unnecessary?',
    'If suppression is primarily structural, removing institutional barriers (equal academic platforms for Palestinian historians, postcolonial curricula, diverse media ownership) could resolve the constraint. If internalized, suppression persists despite institutional change because rejection of the reading appears to threaten Jewish identity and security. Current measurements show rising theater ratio (narrative reframing work increasing), suggesting internalization is deepening.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Mechanism of suppression: external barriers vs. identity-fused internalization.').

omega_variable(
    beneficiary_expansion_and_drift,
    'The authored beneficiary set includes only jewish_claimants_to_ancestral_land. Are there other beneficiaries not named? Specifically: do settlers, military/security institutions, or state bureaucracies benefit from the constraint''s operation independently of whether they believe in the reading''s historical claims?',
    'Structural analysis of who materially benefits from the constraint''s operation (control of resources, territorial expansion, political power) versus who benefits from its legitimacy (those for whom the reading''s truth is identity-constitutive). Do all beneficiaries require the reading''s truth, or do some benefit from it regardless of its truth value?',
    'If additional beneficiaries exist who profit from the constraint without believing in it, the constraint is snare (some parties know it''s unjust but profit anyway) rather than rope (genuine coordination) or mountain (natural fact). This would indicate functional drift: the constraint originally served to legitimate Jewish territorial claims, but now serves to sustain territorial expansion and resource extraction regardless of legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_expansion_and_drift, empirical, 'Whether beneficiary set is complete or conceals profit-motivated actors who benefit regardless of the reading''s truth.').

omega_variable(
    competitive_alternatives_in_reading_ecology,
    'What structural prevents the settler_colonial_reading from displacing the indigenous_return_reading in institutional authority and political legitimacy? Is it superior evidence, institutional power, identity-lock on Jewish constituencies, or institutional stalemate where neither reading can overcome the other?',
    'Comparative institutional analysis: which reading has stronger evidentiary support from archaeology, demographics, and historiography? Which reading has institutional backing (universities, governments, media, international institutions)? Where do power asymmetries lie? Has the settler_colonial_reading been suppressed by the indigenous_return_reading''s institutional dominance, or does it coexist in an intellectual stalemate?',
    'If the indigenous_return_reading prevails through institutional power rather than evidentiary strength, the constraint is snare (power-based narrative enforcement). If the settler_colonial_reading would prevail in a fair-evidence comparison but is suppressed, that confirms snare classification. If neither reading has sufficient evidence to defeat the other, the constraint is a perpetual-contestation structure requiring active suppression to maintain the indigenous_return_reading''s authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competitive_alternatives_in_reading_ecology, empirical, 'Structural conditions explaining which reading maintains institutional legitimacy despite competing alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__indigenous_return_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__indigenous_return_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(jewi_tr_t10, jewish_self_determination__indigenous_return_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(jewi_tr_t20, jewish_self_determination__indigenous_return_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(jewi_tr_t35, jewish_self_determination__indigenous_return_reading, theater_ratio, 35, 0.31).
narrative_ontology:measurement(jewi_tr_t50, jewish_self_determination__indigenous_return_reading, theater_ratio, 50, 0.37).
narrative_ontology:measurement(jewi_tr_t75, jewish_self_determination__indigenous_return_reading, theater_ratio, 75, 0.41).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__indigenous_return_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(jewi_be_t10, jewish_self_determination__indigenous_return_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(jewi_be_t20, jewish_self_determination__indigenous_return_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(jewi_be_t35, jewish_self_determination__indigenous_return_reading, base_extractiveness, 35, 0.71).
narrative_ontology:measurement(jewi_be_t50, jewish_self_determination__indigenous_return_reading, base_extractiveness, 50, 0.76).
narrative_ontology:measurement(jewi_be_t75, jewish_self_determination__indigenous_return_reading, base_extractiveness, 75, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__indigenous_return_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(jewi_su_t10, jewish_self_determination__indigenous_return_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(jewi_su_t20, jewish_self_determination__indigenous_return_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(jewi_su_t35, jewish_self_determination__indigenous_return_reading, suppression_requirement, 35, 0.76).
narrative_ontology:measurement(jewi_su_t50, jewish_self_determination__indigenous_return_reading, suppression_requirement, 50, 0.8).
narrative_ontology:measurement(jewi_su_t75, jewish_self_determination__indigenous_return_reading, suppression_requirement, 75, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__indigenous_return_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__indigenous_return_reading, 0.12).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, palestinian_self_determination__indigenous_claims_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, territorial_settlement_policies__legitimacy_defense).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'jewish_self_determination'. Five structural readings decompose the kernel into separate constraints with different epsilon values, beneficiaries, and victim structures. The indigenous_return_reading claims mountain status (objective historical fact of unbroken connection) but operates with metrics consistent with snare/tangled_rope (high extractiveness 0.78, high suppression 0.82, high theater 0.41, high resistance 0.79)—this divergence is the diagnostic signature of a false summit. The settler_colonial_reading (separate constraint story) describes the same territory and claims structure but reverses beneficiary/victim assignments and claims snare status with similar epsilon range. These are not two perspectives on one constraint; they are two distinct constraints arising from the same kernel, with different structural properties, different empirical claims, and different operational mechanisms. They are linked through network.affects_constraints because the institutional dominance of one reading structurally constrains the other's legitimacy and resource availability. The indigenous_return_reading influences the settler_colonial_reading by controlling narrative authority and institutional gatekeeping; the settler_colonial_reading influences the indigenous_return_reading by continuously generating counter-evidence and alternative framings that must be suppressed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__indigenous_return_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
