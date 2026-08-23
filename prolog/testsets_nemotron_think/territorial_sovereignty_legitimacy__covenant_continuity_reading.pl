% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__covenant_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__covenant_continuity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: territorial_sovereignty_legitimacy__covenant_continuity_reading
 *   human_readable: Covenant-Continuity Legitimacy Framework for Territorial Sovereignty
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story analyzes the covenant-continuity reading of
 *   territorial sovereignty legitimacy in Israel/Palestine. The reading
 *   claims sovereignty derives from an ancient divine covenant, continuous
 *   Jewish presence, and modern international recognition (Balfour, UN
 *   Partition, 1948). Structurally, the framework operates as a legitimating
 *   constraint: it authorizes Israeli state sovereignty over the territory
 *   while structurally excluding Palestinian collective self-determination.
 *   The proponents present it as a Mountain (divine promise = natural law),
 *   but the metric profile shows high extraction, active enforcement, and
 *   rising theater — a Tangled Rope with False Summit Mountain
 *   characteristics. The constraint has a genuine coordination function
 *   (providing international legal recognition for Jewish statehood) but
 *   couples it to asymmetric extraction (Palestinian dispossession,
 *   occupation, denial of return).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.78).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.82).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Covenant-Continuity Legitimacy Framework for Territorial Sovereignty").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, '47311970-d058-406c-b66b-4713fe52ab47').
narrative_ontology:cs_kernel_codification('47311970-d058-406c-b66b-4713fe52ab47', fixed_text).
narrative_ontology:cs_authority_grounding('47311970-d058-406c-b66b-4713fe52ab47', lineage).
narrative_ontology:cs_interpretation_layer_present('47311970-d058-406c-b66b-4713fe52ab47').
narrative_ontology:cs_reading_relation('47311970-d058-406c-b66b-4713fe52ab47', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_reading_relation('47311970-d058-406c-b66b-4713fe52ab47', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('47311970-d058-406c-b66b-4713fe52ab47', foundational, divine_covenant_grants_perpetual_title).
narrative_ontology:cs_axiom_status(divine_covenant_grants_perpetual_title, holdable).
narrative_ontology:cs_axiom_grounding('47311970-d058-406c-b66b-4713fe52ab47', divine_covenant_grants_perpetual_title, theological).
narrative_ontology:cs_axiom('47311970-d058-406c-b66b-4713fe52ab47', foundational, continuous_presence_maintains_sovereign_claim).
narrative_ontology:cs_axiom_status(continuous_presence_maintains_sovereign_claim, holdable).
narrative_ontology:cs_axiom_grounding('47311970-d058-406c-b66b-4713fe52ab47', continuous_presence_maintains_sovereign_claim, empirically_contingent).
narrative_ontology:cs_axiom('47311970-d058-406c-b66b-4713fe52ab47', secondary, international_recognition_confirms_preexisting_right).
narrative_ontology:cs_axiom_status(international_recognition_confirms_preexisting_right, holdable).
narrative_ontology:cs_axiom_grounding('47311970-d058-406c-b66b-4713fe52ab47', international_recognition_confirms_preexisting_right, conventional).
narrative_ontology:cs_reference_frame('47311970-d058-406c-b66b-4713fe52ab47', covenantal_sovereignty_framework).
narrative_ontology:cs_drift_state('47311970-d058-406c-b66b-4713fe52ab47', post_1967_occupation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('47311970-d058-406c-b66b-4713fe52ab47', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_people).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, settler_movement).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_people).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_palestinian_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, settler_movement).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, divine_covenant_grants_perpetual_title).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, continuous_presence_maintains_sovereign_claim).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_recognition_confirms_preexisting_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the sovereignty claim through military control, legal frameworks, and diplomatic apparatus. Sets the rules of citizenship, land allocation, and movement. Collects the primary benefits: territorial control, international recognition, resource extraction. Faces minimal exit pressure due to nuclear deterrent and great-power patronage.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state, agenda_setter,
    institutional, generational, arbitrage, global).

% Transnational collective that derives legitimate homeland status and refuge guarantee from the framework. The covenant narrative fuses religious identity with territorial claim, making exit from the framework existentially costly. Diaspora communities materially and politically support the arrangement; the framework resolves the historical condition of statelessness.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_people, beneficiary,
    organized, civilizational, identity_locked, global).

% Directly extracts land and resources through settlement expansion enabled by the legitimacy framework. Ideologically fused to the covenant narrative — exit would constitute theological and communal apostasy. Bears costs of international opprobrium, security risk, and demographic tension, but these are subordinated to the identity payoff.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, settler_movement, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, settler_movement, payer).

% Bears the extraction: territorial dispossession, military occupation, denial of self-determination, fragmented governance. The framework structurally excludes Palestinian collective agency — the covenant narrative precedes and overrides Palestinian presence. Exit options are near-zero: no sovereign territory, restricted movement, refugee status for millions. Resistance is continuous but constrained by power asymmetry.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_people, payer,
    organized, generational, trapped, national).

% Descendants of those displaced in 1948 and 1967, now numbering millions across diaspora and camps. The covenant-continuity framework explicitly denies their right of return as incompatible with Jewish demographic majority. They have no seat at any negotiating table; their objection is structurally silenced by the framework's premise that the land was promised to another people.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_refugees, excluded,
    powerless, generational, trapped, global).

% Citizens of the state but structurally subordinated by the framework's definition of the state as the nation-state of the Jewish people. Bear costs: land expropriation, discriminatory planning, symbolic exclusion. Exit is constrained — they remain in homeland but without collective equality. Their political voice is contained within a system that defines their belonging as conditional.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_palestinian_citizens, payer,
    moderate, biographical, constrained, national).

% Provides the modern recognition pillar (UN, Balfour, partition) that the framework claims as confirmation. States and institutions variously endorse, critique, or ignore the framework. Their analytical seat allows them to see the full structure: the coordination function (international legal order) and the extraction function (Palestinian dispossession). Some act to enforce the coordination (two-state diplomacy); others enable the extraction (unconditional support).
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_community, observer,
    institutional, generational, analytical, global).

% Document and challenge the extraction dimension: occupation practices, settlement illegality, apartheid findings. Their analytical output feeds international_community and palestinian_people seats but lacks enforcement power. They are structurally observers — they name the constraint but cannot alter its operation.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, human_rights_organizations, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legitimating framework that converts a contested territorial claim into recognized sovereign statehood within the international order. Solves the coordination problem of 'how does a people achieve recognized sovereignty over a specific territory?' by layering divine promise, historical continuity, and modern legal instruments into a single authoritative narrative that states and institutions can recognize.
% TRANSFER_FUNCTION: Moves territorial control, demographic majority, resource access, and political self-determination from the Palestinian people to the Israeli state and Jewish people. The covenant narrative legitimates the transfer; international recognition institutionalizes it; military enforcement secures it. The settlers receive land; the state receives sovereignty; the Palestinian people lose both.
% ABSENT_VOICES: Palestinian refugees (denied return, denied representation) and the pre-1948 Palestinian political leadership (erased by the Nakba) would reject the framework's premise that Jewish historical connection overrides Palestinian continuous residence. Their absence is structural: the framework's founding logic requires their exclusion, because their inclusion would falsify the claim that the land was 'empty' or 'promised' exclusively to one people.
% DISAPPEARANCE_RATIONALE: If the covenant-continuity framework vanished overnight, the legal and moral architecture justifying Israeli sovereignty over the whole territory would collapse. The international recognition pillar (UN Partition, 1948) would revert to its original terms: two states, not one. The settlement enterprise would lose its legitimating narrative. The Palestinian people would gain a structural opening for self-determination. The world would rearrange around a fundamentally different territorial-political settlement.
% FOUNDING_PROBLEM: The Jewish condition of statelessness, persecution, and existential vulnerability in diaspora, culminating in the Holocaust. The framework was built to solve: how does a persecuted people achieve permanent security and collective self-determination?
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and Israeli state discourse attest the problem remains live (rising antisemitism, Iran threat). Palestinian historiography and human rights organizations attest the founding problem has been substantially solved for Jewish people (sovereign state, nuclear deterrent, great-power alliance) but the framework persists as extraction. International legal scholars outside both parties (e.g., ICJ advisory opinions, UN special rapporteurs) corroborate the shifted-function reading: the framework now primarily manages occupation, not survival.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__covenant_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__covenant_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the framework transfers territory, resources, and political rights from Palestinians to Israeli/Jewish beneficiaries, and the transfer has expanded over time (1967 occupation, settlement growth). Suppression (0.82) is very high because the framework's persistence depends on military occupation, legal exclusion of refugees, and demographic engineering — not voluntary adherence. Theater ratio (0.45) is moderate and rising: the 'security' and 'peace process' performances increasingly cover settlement entrenchment. Accessibility collapse (0.65) reflects that alternatives (one state with equality, two states on 1967 lines, binationalism) are structurally blocked by the framework's premises. Resistance (0.72) is high: Palestinian resistance (intifadas, BDS, legal challenges, sumud) is continuous and met with escalating force.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter (israeli_state) and beneficiary (jewish_people, settler_movement) seats experience the constraint as coordination — it solves their existential security problem and provides international legitimacy. The payer seats (palestinian_people, refugees, israeli_palestinian_citizens) experience it as extraction enforced by military and legal apparatus. The observer seats (international_community, human_rights_orgs) see both dimensions but lack enforcement power to alter the structure. The engine computes this divergence from the declared power/exit/role data; the claimed_type (tangled_rope) reflects the generator's structural assessment, while the proponents' self-presentation as Mountain is documented in omegas.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state (institutional, arbitrage exit) sits at d≈0.1 (full beneficiary: collects sovereignty, resources, international cover). Jewish people (organized, identity_locked exit) sit at d≈0.2 (beneficiary with identity lock-in). Settler movement (organized, identity_locked) sits at d≈0.3 (beneficiary-payer dual: gains land but bears ideological/physical risk). Palestinian people (organized, trapped exit) sit at d≈0.9 (full target: bears dispossession with no exit). Palestinian refugees (powerless, trapped) sit at d≈1.0 (total extraction, zero voice). Israeli Palestinian citizens (moderate, constrained exit) sit at d≈0.7 (subordinate inclusion). International community (institutional, analytical) sits at d≈0.5 (symmetric: provides recognition but bears instability costs).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish statelessness and persecution) was substantially solved by 1948/1967: sovereign state achieved, nuclear deterrent established, great-power patron secured. The framework persists not because the founding problem remains live in its original form, but because it has been repurposed to manage the occupation and settlement project. The mandate has atrophied into extraction maintenance. The constraint shows classic mandatrophy signals: rising theater, expanding extraction, suppression of alternatives (two-state solution), and identity-locked beneficiaries who cannot conceive of the framework's end. The classification as tangled_rope (not pure snare) captures that the coordination function (international legitimacy for Jewish statehood) remains real and valued by beneficiaries — but it is now inextricably coupled to extraction that the coordination does not require.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct structural entity from its sibling readings, or a framing of the same underlying territorial settlement?',
    'Test ε-invariance: if measuring the constraint via covenant narrative yields ε=0.78 but measuring via self-determination narrative yields ε=0.35, they are different constraints. The ε-invariance principle requires separate stories for each reading.',
    'Confirms this JSON correctly instantiates one reading only. Sibling readings must be separate constraint stories linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commiter frame: this is one reading of kernel territorial_sovereignty_legitimacy, not the kernel itself.').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Does the divine covenant component function as a genuine natural law (Mountain) or a constructed legitimating narrative that benefits identifiable agents (False Summit)?',
    'Historical-critical analysis of covenant theology vs. political deployment; test whether the framework''s operation changes when the theological premise is bracketed. If extraction persists without the theological premise, the Mountain claim is false summit.',
    'If false summit, FSM signature triggers reclassification to tangled_rope. The declared beneficiaries (israeli_state, jewish_people, settler_movement) are the FSM trigger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'FSM candidate: Mountain claim with declared beneficiaries. The covenant narrative presents as natural law but operates as political extraction framework.').

omega_variable(
    continuous_presence_empirical_validity,
    'Is the claim of ''continuous Jewish presence'' empirically sufficient to sustain sovereign title across 2000 years of demographic minority?',
    'Demographic history: Jewish population was <10% in 1800, ~30% in 1947. The claim relies on symbolic/religious continuity, not demographic continuity. Compare to other title claims in international law.',
    'If empirically insufficient, the ''continuity'' pillar is performative — the framework rests on covenant + recognition only, making the continuity claim theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuous_presence_empirical_validity, empirical, 'Whether the historical continuity claim is empirically valid or a legitimating fiction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression structural (military occupation, legal barriers, geographic fragmentation) or internalized (Palestinian acceptance of framework''s legitimacy, collaboration, despair)?',
    'Post-exit suppression trajectory: if suppression persists after physical barriers are removed (e.g., Oslo Areas A/B), reclassify as partially internalized. Measure Palestinian Authority security cooperation as internalized suppression.',
    'If substantially internalized, effective suppression is higher than structural measure suggests — the target carries the constraint''s logic internally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the occupation framework.').

omega_variable(
    international_recognition_creates_vs_confirms,
    'Does modern international recognition (Balfour, UN Partition, 1948) create the sovereign right or merely confirm a pre-existing covenantal right?',
    'Legal history: Balfour/Partition were constitutive acts creating new legal status, not recognitions of pre-existing sovereignty. The reading''s claim that they ''confirm'' is a retrospective reinterpretation.',
    'If recognition creates rather than confirms, the framework''s temporal logic collapses: the modern legal pillar contradicts the ancient covenant pillar. The framework becomes incoherent rather than layered.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_recognition_creates_vs_confirms, conceptual, 'Whether the recognition pillar is constitutive or confirmatory in the framework''s internal logic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsl_cc_tr_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(tsl_cc_tr_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1967, 0.3).
narrative_ontology:measurement(tsl_cc_tr_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1993, 0.35).
narrative_ontology:measurement(tsl_cc_tr_t2000, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(tsl_cc_tr_t2010, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2010, 0.43).
narrative_ontology:measurement(tsl_cc_tr_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(tsl_cc_be_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(tsl_cc_be_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1967, 0.65).
narrative_ontology:measurement(tsl_cc_be_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1993, 0.7).
narrative_ontology:measurement(tsl_cc_be_t2000, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2000, 0.73).
narrative_ontology:measurement(tsl_cc_be_t2010, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2010, 0.76).
narrative_ontology:measurement(tsl_cc_be_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(tsl_cc_su_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(tsl_cc_su_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(tsl_cc_su_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1993, 0.75).
narrative_ontology:measurement(tsl_cc_su_t2000, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(tsl_cc_su_t2010, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(tsl_cc_su_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2024, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.08).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial 'Israeli legitimacy claim' into three structurally distinct readings with different ε values, beneficiary/victim structures, and temporal scopes. The covenant_continuity_reading (this story) has high ε (0.78) due to occupation extraction. The self_determination_reading would have lower ε for Jewish beneficiaries but high ε for Palestinian beneficiaries. The existential_matrix_reading has near-zero coordination function (zero-sum framing) and high ε for all parties. They are linked because each reading's proponents cite the others as the primary obstacle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__covenant_continuity_reading, organized, 0.25).
constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__covenant_continuity_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
