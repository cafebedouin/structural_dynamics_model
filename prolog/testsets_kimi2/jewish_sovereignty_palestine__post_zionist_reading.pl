% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__post_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__post_zionist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__post_zionist_reading
 *   human_readable: Jewish Ethnic-National Framework in Israel/Palestine (Post-Zionist Reading)
 *   domain: political/nationalism/postcolonial
 *
 * SUMMARY:
 *   The post-Zionist reading of Jewish sovereignty in Israel/Palestine treats
 *   the Zionist project's success in achieving statehood as a historical fact
 *   that has transformed the ethical and structural status of the
 *   ethnic-national framework. Where Zionism once addressed Jewish
 *   statelessness and persecution, the post-Zionist reading holds that the
 *   continued institutionalization of Jewish ethnic privilegeâthrough the
 *   Law of Return, land regime asymmetries, military occupation, and
 *   constitutional Jewish identityânow extracts from Palestinian
 *   populations (both citizens and occupied) and obstructs civic equality and
 *   regional integration. This constraint story instantiates the post-Zionist
 *   reading of the contested kernel 'jewish_sovereignty_palestine'; sibling
 *   readings include liberal-nationalist, settler-colonial,
 *   religious-Zionist, and cultural-Zionist framings.
 *
 * KEY AGENTS:
 *   - jewish_israeli_citizens (beneficiary): Dominant demographic with institutional access and immigration privileges
 *   - palestinian_citizens_of_israel (payer): Marginalized citizens bearing structural inequality within the state
 *   - palestinians_under_occupation (payer): Stateless population under military administration
 *   - israeli_state_institutions (agenda_setter): Administrative and constitutional machinery of ethnic-national governance
 *   - international_human_rights_observers (observer): External analytical seat monitoring the gap between democratic claims and ethnic practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, 0.72).
domain_priors:suppression_score(jewish_sovereignty_palestine__post_zionist_reading, 0.78).
domain_priors:theater_ratio(jewish_sovereignty_palestine__post_zionist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__post_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__post_zionist_reading, "Jewish Ethnic-National Framework in Israel/Palestine (Post-Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__post_zionist_reading, "political/nationalism/postcolonial").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__post_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__post_zionist_reading, '9141d5a7-c08b-485b-8a18-fce705a57e9a').
narrative_ontology:cs_kernel_codification('9141d5a7-c08b-485b-8a18-fce705a57e9a', formalized).
narrative_ontology:cs_authority_grounding('9141d5a7-c08b-485b-8a18-fce705a57e9a', lineage).
narrative_ontology:cs_interpretation_layer_present('9141d5a7-c08b-485b-8a18-fce705a57e9a').
narrative_ontology:cs_reading_relation('9141d5a7-c08b-485b-8a18-fce705a57e9a', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9141d5a7-c08b-485b-8a18-fce705a57e9a', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9141d5a7-c08b-485b-8a18-fce705a57e9a', jewish_sovereignty_palestine__religious_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('9141d5a7-c08b-485b-8a18-fce705a57e9a', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('9141d5a7-c08b-485b-8a18-fce705a57e9a', foundational, ethnic_nationalism_obsolete_post_statehood).
narrative_ontology:cs_axiom_status(ethnic_nationalism_obsolete_post_statehood, holdable).
narrative_ontology:cs_axiom_grounding('9141d5a7-c08b-485b-8a18-fce705a57e9a', ethnic_nationalism_obsolete_post_statehood, empirically_contingent).
narrative_ontology:cs_axiom('9141d5a7-c08b-485b-8a18-fce705a57e9a', foundational, civic_equality_supersedes_ethnic_privilege).
narrative_ontology:cs_axiom_status(civic_equality_supersedes_ethnic_privilege, holdable).
narrative_ontology:cs_axiom_grounding('9141d5a7-c08b-485b-8a18-fce705a57e9a', civic_equality_supersedes_ethnic_privilege, deontological).
narrative_ontology:cs_reference_frame('9141d5a7-c08b-485b-8a18-fce705a57e9a', zionist_statehood_fulfilled).
narrative_ontology:cs_drift_state('9141d5a7-c08b-485b-8a18-fce705a57e9a', contemporary_ethno_national_entrenchment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9141d5a7-c08b-485b-8a18-fce705a57e9a', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_israeli_citizens).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, palestinians_under_occupation).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__post_zionist_reading, law_of_return_doctrine).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__post_zionist_reading, jewish_statehood_perpetuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive preferential immigration access under the Law of Return, dominant representation in state institutions, preferential land allocation through the Jewish National Fund and Israel Land Authority, and constitutional affirmation of Jewish national character. They sustain the constraint through electoral support and institutional participation, with emigration as a practical option though most do not exercise it.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_israeli_citizens, beneficiary,
    powerful, generational, mobile, national).

% Hold formal citizenship but face systematic inequality in land ownership, municipal funding, immigration rights (family reunification restrictions), and national service benefits. Constitute a demographic minority within the state's self-defined Jewish identity, with limited institutional leverage to alter the ethnic-national framework. Exit through emigration is possible but involves loss of indigenous homeland and community.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_of_israel, payer,
    moderate, generational, constrained, national).

% Live under military administration in the West Bank and Gaza Strip (or under blockade/occupation architecture), without citizenship in the sovereign power controlling their territory. Subject to movement restrictions, separate legal system (military law), land confiscation, and settlement expansion that benefits Jewish citizens. Exit is severely constrained by borders, permits, economic dependency, and lack of refugee absorption options.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinians_under_occupation, payer,
    powerless, immediate, trapped, regional).

% Administer the Law of Return, citizenship and nationality registers, land regimes, military occupation infrastructure, and constitutional Basic Laws affirming Jewish state character. The institutional matrix is locked into Zionist constitutional principles and coalition politics that make de-Zionization structurally difficult despite internal debates.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Monitor and report on discriminatory legislation, occupation practices, and civic inequality through UN bodies, international courts, and human rights organizations. They observe the structural gap between declared democratic norms and ethnic-national practice, producing comparative analysis and legal opinions that exert normative pressure but lack direct enforcement.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, international_human_rights_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__post_zionist_reading, jewish_israeli_citizens).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__post_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established sovereign state institutions providing collective self-determination, refuge, and political expression for Jews in their ancestral homeland following statelessness and persecution.
% TRANSFER_FUNCTION: Transfers land access, immigration rights under the Law of Return, state resources, and institutional dominance to Jewish citizens and immigrants, while Palestinian populations within the territory receive subordinate civic status, restricted land rights, and exclusion from the national definition.
% ABSENT_VOICES: Palestinian refugees displaced in 1948 and 1967 and their descendants are structurally excluded from territorial return and civic voice; Jewish anti-Zionist minorities and post-Zionist academics are marginal in mainstream Israeli discourse.
% DISAPPEARANCE_RATIONALE: If the Jewish ethnic-national framework dissolved overnight, the Law of Return would cease, land and resource allocation would shift toward civic equality, military occupation architecture would lose its demographic rationale, and the regional integration logic would transform from ethno-national conflict to civic-state normalization.
% FOUNDING_PROBLEM: Dispersal and statelessness of the Jewish people; vulnerability to persecution and genocide in Europe; need for collective self-determination and safe refuge.
% FOUNDING_PROBLEM_CORROBORATION: Post-Zionist historians and sociologists attest that the Zionist project succeeded in state-building and that the continued ethnic framework serves interests other than the original refuge function. Palestinian civil society organizations and international historians outside the Israeli Jewish beneficiary consensus corroborate the obsolescence reading; mainstream Zionist parties and state institutions contest it, asserting the threat persists.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__post_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__post_zionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__post_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__post_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__post_zionist_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is moderate-high because the ethnic-national framework systematically transfers land, immigration rights, and institutional dominance to Jewish citizens while subordinating Palestinian populations. Suppression (0.78) is high because the arrangement depends on active enforcement: military occupation, separate legal systems, citizenship and family reunification restrictions, and constitutional entrenchment of Jewish state character. Theater ratio (0.48) is moderate-high: democratic institutions performatively maintain liberal appearances while substantive civic equality is systematically undermined by ethnic architecture. Accessibility collapse (0.60) reflects that alternatives (civic state, binationalism, confederation) exist in discourse but are institutionally blocked. Resistance (0.55) captures sustained Palestinian opposition, periodic civil society campaigns, and growing international legal pressure. The measurement series tracks escalating extraction and theater from state consolidation (1948) through occupation (1967) to contemporary entrenchment.
 *
 * PERSPECTIVAL GAP:
 *   The Jewish Israeli citizen seat experiences the constraint as legitimate national self-expression and security architecture, with low effective extraction and possibly negative extraction (subsidy). The Palestinian seats experience high effective extraction through restricted rights, land dispossession, and political subordination. The state institutional seat experiences the constraint as a constitutional imperative with constrained exit options due to coalition politics and ideological lock-in. The engine computes this divergence from beneficiary/payer declarations and exit modulations.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish Israeli citizens are declared beneficiaries with mobile exit options, placing d near the subsidy end (low d). Palestinian citizens and occupied populations are declared victims with constrained/trapped exit options, placing d near the full-target end (high d). State institutions are agenda-setters with constrained exit, experiencing moderate d toward the target end because they bear the enforcement costs and ideological commitments of maintaining the framework. International observers sit at analytical exit with neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling as pure extraction by acknowledging the genuine historical coordination function: Zionism solved Jewish statelessness and established sovereign institutions. However, the post-Zionist reading classifies the current arrangement as Tangled Rope because that coordination function has atrophied into asymmetric extractionâthe ethnic-national framework persists beyond its founding problem and now primarily distributes privilege and obstructs equality. If the founding problem were still live and the ethnic framework were proportionate to it, the classification would shift toward Rope; if there were no genuine founding problem at all, it would shift toward Snare. The temporal measurements showing rising extractiveness and theater over time support the mandatrophy (mission-accomplished-but-persists) diagnosis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    post_zionist_kernel_position,
    'Does the post-Zionist reading of Jewish sovereignty constitute a distinct constraint from the settler-colonial reading, or does de-Zionization logically require decolonization?',
    'Comparative analysis of post-Zionist institutional proposals (civic state, cultural autonomy) versus settler-colonial proposals (return of refugees, abolition of Zionist property regimes) to determine structural separability.',
    'If inseparable, the post-Zionist reading collapses into the settler-colonial constraint; if separable, it remains a distinct civic-nationalist alternative with different beneficiary/victim dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_zionist_kernel_position, conceptual, 'Structural relationship between post-Zionist reform and settler-colonial abolition').

omega_variable(
    founding_problem_obsolescence,
    'Is the Zionist founding problem (Jewish statelessness and vulnerability) genuinely solved, or does ongoing anti-Semitism and regional hostility reactivate the need for an ethnic-national refuge?',
    'Empirical assessment of Jewish global security indicators, asylum-seeking patterns, and comparative minority-protection frameworks.',
    'If the founding problem remains live, the constraint reads as Rope or Scaffold rather than Tangled Rope; if obsolete, the ethnic framework is exposed as atrophied extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether Zionism''s founding problem is solved or persists').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is Palestinian subordination maintained primarily by structural legal mechanisms or by internalized political resignation?',
    'Behavioral response to structural reform experiments (e.g., repeal of discriminatory land laws) to test whether civic integration advances or meets latent social resistance.',
    'If internalized, effective suppression exceeds structural metrics and de-Zionization requires deeper cultural change; if purely structural, legal reform suffices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of Palestinian civic equality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__post_zionist_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsp_postzion_tr_t0, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jsp_postzion_tr_t15, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(jsp_postzion_tr_t30, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(jsp_postzion_tr_t45, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 45, 0.35).
narrative_ontology:measurement(jsp_postzion_tr_t60, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(jsp_postzion_tr_t75, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 75, 0.48).

% Extraction over time
narrative_ontology:measurement(jsp_postzion_be_t0, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(jsp_postzion_be_t15, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(jsp_postzion_be_t30, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(jsp_postzion_be_t45, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 45, 0.58).
narrative_ontology:measurement(jsp_postzion_be_t60, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(jsp_postzion_be_t75, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(jsp_postzion_su_t0, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(jsp_postzion_su_t15, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(jsp_postzion_su_t30, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(jsp_postzion_su_t45, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 45, 0.65).
narrative_ontology:measurement(jsp_postzion_su_t60, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(jsp_postzion_su_t75, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 75, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel jewish_sovereignty_palestine. The kernel decomposes into multiple structurally distinct constraints (readings) because the natural-language label 'Jewish sovereignty in Israel/Palestine' conflates claims with different epsilon values, beneficiary structures, and normative foundations. Each reading carries its own constraint_id and epsilon. Family members are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
