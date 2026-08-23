% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__cultural_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: jewish_territorial_claim__cultural_zionism_reading
 *   human_readable: Cultural Zionist Vision: Jewish Spiritual Center in Palestine without Political Sovereignty
 *   domain: political/historical/nationalism
 *
 * SUMMARY:
 *   The cultural Zionist reading of the Jewish territorial claim (exemplified
 *   by Ahad Ha'am) envisions Palestine as a Jewish spiritual and cultural
 *   center — a 'national home' in the cultural sense — without requiring
 *   political sovereignty or a Jewish demographic majority. It emphasizes
 *   quality of settlement over quantity, Hebrew cultural revival, and the
 *   possibility of binational coexistence with the Arab population. This
 *   reading was influential in early Zionist thought (pre-1917) and shaped
 *   the Yishuv's cultural institutions, but it was marginalized by the
 *   political Zionist drive for statehood after the Balfour Declaration. The
 *   constraint is the cultural Zionist vision itself as a coordinating ideal
 *   for Jewish life in Palestine.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, 0.15).
domain_priors:suppression_score(jewish_territorial_claim__cultural_zionism_reading, 0.25).
domain_priors:theater_ratio(jewish_territorial_claim__cultural_zionism_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__cultural_zionism_reading, rope).
narrative_ontology:human_readable(jewish_territorial_claim__cultural_zionism_reading, "Cultural Zionist Vision: Jewish Spiritual Center in Palestine without Political Sovereignty").
narrative_ontology:topic_domain(jewish_territorial_claim__cultural_zionism_reading, "political/historical/nationalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__cultural_zionism_reading, '2dfc603c-89ff-4ab8-9ad0-8d1e18120338').
narrative_ontology:cs_kernel_codification('2dfc603c-89ff-4ab8-9ad0-8d1e18120338', distributed).
narrative_ontology:cs_authority_grounding('2dfc603c-89ff-4ab8-9ad0-8d1e18120338', lineage).
narrative_ontology:cs_interpretation_layer_present('2dfc603c-89ff-4ab8-9ad0-8d1e18120338').
narrative_ontology:cs_reading_relation('2dfc603c-89ff-4ab8-9ad0-8d1e18120338', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('2dfc603c-89ff-4ab8-9ad0-8d1e18120338', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('2dfc603c-89ff-4ab8-9ad0-8d1e18120338', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('2dfc603c-89ff-4ab8-9ad0-8d1e18120338', foundational, jewish_spiritual_center_suffices_for_national_revival).
narrative_ontology:cs_axiom_status(jewish_spiritual_center_suffices_for_national_revival, holdable).
narrative_ontology:cs_axiom_grounding('2dfc603c-89ff-4ab8-9ad0-8d1e18120338', jewish_spiritual_center_suffices_for_national_revival, deontological).
narrative_ontology:cs_axiom('2dfc603c-89ff-4ab8-9ad0-8d1e18120338', foundational, binational_coexistence_possible_without_jewish_sovereignty).
narrative_ontology:cs_axiom_status(binational_coexistence_possible_without_jewish_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('2dfc603c-89ff-4ab8-9ad0-8d1e18120338', binational_coexistence_possible_without_jewish_sovereignty, empirically_contingent).
narrative_ontology:cs_reference_frame('2dfc603c-89ff-4ab8-9ad0-8d1e18120338', ahad_haam_cultural_center_paradigm).
narrative_ontology:cs_drift_state('2dfc603c-89ff-4ab8-9ad0-8d1e18120338', post_1948_statehood, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2dfc603c-89ff-4ab8-9ad0-8d1e18120338', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, jewish_cultural_nationalists).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, arab_population_palestine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, jewish_diaspora_communities).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, labor_zionist_pioneers).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, labor_zionist_pioneers).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__cultural_zionism_reading, jewish_cultural_revival_through_territorial_center).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__cultural_zionism_reading, binational_coexistence_feasibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Intellectuals and organizers (e.g., Ahad Ha'am, Bialik) who articulate and promote the vision of a Jewish spiritual center in Palestine. They set the cultural agenda, publish journals, and influence settlement priorities. Their exit is mobile: they can continue cultural work in diaspora or shift to other Zionist streams.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, jewish_cultural_nationalists, agenda_setter,
    organized, generational, mobile, global).

% Palestinian Arab communities who, under this vision, would not face displacement or demographic engineering. They benefit from a binational framework that recognizes their presence. Their exit is constrained by geography and political circumstance.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, arab_population_palestine, beneficiary,
    moderate, biographical, constrained, regional).

% Jewish communities worldwide who gain a cultural center for Hebrew revival and national culture without the burdens of statehood. They can engage selectively (funding, immigration, cultural exchange) and have high exit options.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, jewish_diaspora_communities, beneficiary,
    organized, generational, arbitrage, global).

% Leaders of the political Zionist stream (Herzl, Weizmann, Ben-Gurion) who seek a Jewish state with sovereignty and majority. They are excluded from the cultural Zionist framework because their goal requires political mechanisms this reading rejects. They can and do pursue their own parallel track.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, political_zionist_leadership, excluded,
    institutional, biographical, mobile, global).

% The mandatory administration that regulates immigration, land purchase, and communal autonomy. They observe the cultural Zionist project as one factor in Palestinian politics, neither fully endorsing nor suppressing it.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, british_mandatory_authorities, observer,
    institutional, immediate, analytical, regional).

% Socialist settlers (Second/Third Aliyah) who build the cultural infrastructure (kibbutzim, Hebrew schools, Histadrut) while also bearing the physical costs of settlement. They benefit from the cultural revival but pay with labor and risk.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, labor_zionist_pioneers, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__cultural_zionism_reading, labor_zionist_pioneers, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish cultural revival in Palestine — Hebrew language, literature, education, and communal institutions — as a spiritual center that sustains Jewish peoplehood without requiring a sovereign state or demographic majority.
% TRANSFER_FUNCTION: Moves cultural capital, immigration, philanthropic funding, and intellectual labor from the Jewish diaspora to Palestine to build the institutions of a national culture, without transferring political sovereignty or enforcing demographic transformation.
% ABSENT_VOICES: Palestinian Arab political leadership (who sought national self-determination, not binational cultural autonomy), revisionist Zionists (who demanded immediate sovereignty over both banks of the Jordan), and British imperial authorities (who ultimately imposed partition). These voices are absent because the cultural Zionist framework does not incorporate their political claims.
% DISAPPEARANCE_RATIONALE: If the cultural Zionist vision vanished, the Hebrew cultural institutions it built (universities, publishing, language revival) would likely persist because they became embedded in the Yishuv and later Israel. However, the binational coexistence paradigm it offered would be lost, and the historical record shows its absence did not prevent the emergence of a sovereign Jewish state — though some argue a cultural Zionist path might have altered the trajectory of Arab-Jewish relations.
% FOUNDING_PROBLEM: The crisis of Jewish assimilation and cultural decay in the diaspora: Ahad Ha'am argued that Jews needed a spiritual center in their historic homeland to revitalize Jewish culture and identity, not a state to solve the 'Jewish Question' politically.
% FOUNDING_PROBLEM_CORROBORATION: Ahad Ha'am's essays (e.g., 'The Jewish State and the Jewish Problem', 1897) and later scholars (Simon Rawidowicz, Hans Kohn) attest the cultural problem was real. However, the political Zionist leadership and subsequent Israeli historiography often treat the cultural problem as solved by statehood, while post-Zionist scholars argue the cultural crisis persists despite sovereignty.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__cultural_zionism_reading, contested).
narrative_ontology:founding_problem_status(jewish_territorial_claim__cultural_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__cultural_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__cultural_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__cultural_zionism_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.15) because the vision does not inherently extract resources from Arabs or demand demographic displacement; it seeks cultural production, not rent. Suppression is low (0.25) because the vision tolerates alternatives (political Zionism, Arab nationalism) and does not require silencing them — though in practice, the cultural institutions it built later became part of the state apparatus. Theater ratio is moderate (0.35) because the cultural infrastructure (schools, journals, Hebrew language) performed real coordination, but the binational rhetoric became increasingly performative as political Zionism dominated. Accessibility collapse is moderate (0.45): alternatives (diaspora cultural autonomy, binational state proposals) remained conceptually available but were politically marginalized. Resistance is moderate (0.55): the vision faced resistance from political Zionists who saw it as insufficient, from Arabs who distrusted any Zionist settlement, and from British authorities who managed competing claims.
 *
 * PERSPECTIVAL GAP:
 *   From the cultural Zionist seat, the constraint is a rope (pure coordination for cultural revival). From the political Zionist seat, the same cultural institutions appear as a scaffold for state-building (transitional). From the Arab seat, the cultural settlement appears as a tangled rope (coordination of Jewish community that also enables demographic change). The engine will compute these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish cultural nationalists are agenda-setters (d low, near beneficiary) — they define the vision and benefit from cultural authority. Arab population are beneficiaries (d low) in this reading's logic, though in practice they became payers under political Zionism. Jewish diaspora are beneficiaries with arbitrage exit. Political Zionists are excluded (d not computed) — they operate a rival constraint. British are observers. Labor Zionists are dual: beneficiaries of cultural revival, payers of settlement labor.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish cultural survival) remains contested — some say statehood solved it, others say cultural vitality requires more than sovereignty. The cultural Zionist arrangement (cultural center without statehood) is dead as a political program, but its institutions persist. This is not mandatrophy (the mandate didn't outlive its function; the function was overtaken by a rival program). The mandate was displaced, not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the cultural Zionist vision a distinct constraint from the political Zionist constraint, or a transitional phase of the same constraint?',
    'Trace whether the cultural institutions (Hebrew University, Hebrew language committee, cultural journals) were structurally subordinated to political sovereignty goals from the outset, or whether they maintained autonomous coordination logic.',
    'If distinct, the cultural Zionist constraint is a genuine rope with low extraction. If transitional, it is a scaffold whose sunset was political statehood, and its low extractiveness metrics reflect only its early phase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the cultural Zionist reading instantiates a separate constraint or a phase of the political Zionist constraint.').

omega_variable(
    binational_feasibility,
    'Was the binational coexistence vision structurally feasible given the demographic and political dynamics of the mandate period?',
    'Counterfactual analysis of Arab-Jewish communal autonomy proposals (e.g., 1920s-30s binationalist groups, 1947 UNSCOP minority report) and whether cultural Zionist institutions could have mediated conflict without state structures.',
    'If feasible, the reading''s low suppression and extractiveness are structurally honest. If infeasible, the vision functioned as a low-extraction cover for a settlement project that inevitably required political domination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(binational_feasibility, empirical, 'Whether the binational framework was a genuine coordination possibility or an illusion.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.25) structural (British policy, Arab opposition) or internalized (cultural Zionists self-censoring political demands)?',
    'Compare cultural Zionist writings before and after 1917 (Balfour) and 1929 (riots) — did they voluntarily limit their program, or were they constrained by external forces?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the agents carried the suppression with them into the political Zionist mainstream.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the cultural Zionist movement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__cultural_zionism_reading, 1880, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1880, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1880, 0.2).
narrative_ontology:measurement(jewi_tr_t1900, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1900, 0.25).
narrative_ontology:measurement(jewi_tr_t1920, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1920, 0.3).
narrative_ontology:measurement(jewi_tr_t1935, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1935, 0.35).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1948, 0.35).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1880, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1880, 0.05).
narrative_ontology:measurement(jewi_be_t1900, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1900, 0.1).
narrative_ontology:measurement(jewi_be_t1920, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1920, 0.12).
narrative_ontology:measurement(jewi_be_t1935, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1935, 0.15).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1948, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1880, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1880, 0.1).
narrative_ontology:measurement(jewi_su_t1900, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1900, 0.15).
narrative_ontology:measurement(jewi_su_t1920, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1920, 0.2).
narrative_ontology:measurement(jewi_su_t1935, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1935, 0.25).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1948, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__cultural_zionism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__cultural_zionism_reading, 0.08).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This reading decomposes the kernel 'jewish_territorial_claim' by separating the cultural coordination function (spiritual center, Hebrew revival) from the political extraction function (sovereignty, demographic majority). The political_zionism_reading extracts the latter; the cultural_zionism_reading isolates the former. They are linked because the cultural institutions built by this reading became the infrastructure for the political reading's state.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
