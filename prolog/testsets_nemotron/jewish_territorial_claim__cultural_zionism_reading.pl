% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__cultural_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Cultural Zionist Territorial Claim — Jewish Spiritual Center Without Sovereignty Requirement
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   The cultural Zionist reading of the Jewish territorial claim — associated
 *   with Ahad Ha'am, Martin Buber, Judah Magnes, and the Brit Shalom movement
 *   — asserts that Jewish national renaissance requires a spiritual and
 *   cultural center in Palestine, but explicitly does not require political
 *   sovereignty, a Jewish demographic majority, or displacement of the Arab
 *   population. The constraint is the claim that such a center can exist and
 *   flourish under a binational or multinational framework where Jewish
 *   cultural autonomy is secured without statehood. This reading was
 *   marginalized after 1948 but persists as a structural alternative to the
 *   sovereignty-requiring readings that dominate Israeli policy. The ε refers
 *   to the standing arrangement of a Jewish cultural center in Palestine
 *   without sovereignty, assessed from the cultural Zionist reading's own
 *   lights — not to the binational alternative it endorses (which would be a
 *   different constraint).
 *
 * KEY AGENTS:
 *   - jewish_cultural_institutions: Primary beneficiary (institutional/mobile) — gains cultural autonomy without sovereignty burdens
 *   - hebrew_language_revival_movement: Primary beneficiary (organized/mobile) — gains territorial base for language
 *   - arab_palestinian_population: Declared beneficiary (organized/constrained) — binational framework promises parity without displacement
 *   - political_zionist_institutions: Excluded agenda-setter (institutional/arbitrage) — captured the territorial claim for statehood project
 *   - brit_shalom_ihud_intellectuals: Observer/advocate (analytical/analytical) — articulate the reading's internal logic
 *   - mandate_british_authorities: Historical agenda_setter (institutional/analytical) — enabled then constrained the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, 0.15).
domain_priors:suppression_score(jewish_territorial_claim__cultural_zionism_reading, 0.08).
domain_priors:theater_ratio(jewish_territorial_claim__cultural_zionism_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__cultural_zionism_reading, rope).
narrative_ontology:human_readable(jewish_territorial_claim__cultural_zionism_reading, "Cultural Zionist Territorial Claim — Jewish Spiritual Center Without Sovereignty Requirement").
narrative_ontology:topic_domain(jewish_territorial_claim__cultural_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__cultural_zionism_reading, 'ff0e03d7-c3ef-4612-a88e-0a46b14bb888').
narrative_ontology:cs_kernel_codification('ff0e03d7-c3ef-4612-a88e-0a46b14bb888', distributed).
narrative_ontology:cs_authority_grounding('ff0e03d7-c3ef-4612-a88e-0a46b14bb888', lineage).
narrative_ontology:cs_interpretation_layer_present('ff0e03d7-c3ef-4612-a88e-0a46b14bb888').
narrative_ontology:cs_reading_relation('ff0e03d7-c3ef-4612-a88e-0a46b14bb888', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff0e03d7-c3ef-4612-a88e-0a46b14bb888', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff0e03d7-c3ef-4612-a88e-0a46b14bb888', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('ff0e03d7-c3ef-4612-a88e-0a46b14bb888', foundational, jewish_national_culture_requires_territorial_home_not_state).
narrative_ontology:cs_axiom_status(jewish_national_culture_requires_territorial_home_not_state, holdable).
narrative_ontology:cs_axiom_grounding('ff0e03d7-c3ef-4612-a88e-0a46b14bb888', jewish_national_culture_requires_territorial_home_not_state, deontological).
narrative_ontology:cs_axiom('ff0e03d7-c3ef-4612-a88e-0a46b14bb888', foundational, arab_presence_not_inherently_threatening_to_jewish_cultural_autonomy).
narrative_ontology:cs_axiom_status(arab_presence_not_inherently_threatening_to_jewish_cultural_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('ff0e03d7-c3ef-4612-a88e-0a46b14bb888', arab_presence_not_inherently_threatening_to_jewish_cultural_autonomy, deontological).
narrative_ontology:cs_axiom('ff0e03d7-c3ef-4612-a88e-0a46b14bb888', secondary, binational_parity_superior_to_sovereign_dominance).
narrative_ontology:cs_axiom_status(binational_parity_superior_to_sovereign_dominance, holdable).
narrative_ontology:cs_axiom_grounding('ff0e03d7-c3ef-4612-a88e-0a46b14bb888', binational_parity_superior_to_sovereign_dominance, instrumental).
narrative_ontology:cs_reference_frame('ff0e03d7-c3ef-4612-a88e-0a46b14bb888', ahad_haam_spiritual_center).
narrative_ontology:cs_drift_state('ff0e03d7-c3ef-4612-a88e-0a46b14bb888', post_1948_statehood, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ff0e03d7-c3ef-4612-a88e-0a46b14bb888', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, jewish_cultural_institutions).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, hebrew_language_revival_movement).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, arab_palestinian_population).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__cultural_zionism_reading, jewish_national_culture_requires_territorial_home).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__cultural_zionism_reading, arab_jewish_coexistence_possible_without_dominance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hebrew universities, literary societies, spiritual centers (e.g., Hebrew University, Bezalel Academy) that develop Jewish national culture in Palestine. They gain a territorial home for cultural production without the burdens of state administration. Their exit is mobile: cultural institutions can relocate or adapt, though with generational cost.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, jewish_cultural_institutions, beneficiary,
    institutional, generational, mobile, regional).

% The movement to revive Hebrew as a living spoken language. Palestine provides the territorial concentration necessary for language revival. The movement benefits from the cultural center's existence but does not require sovereignty — Hebrew thrived under Ottoman, British, and Israeli rule alike. Exit is mobile: the language could (and did) develop under multiple political frameworks.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, hebrew_language_revival_movement, beneficiary,
    organized, generational, mobile, regional).

% The indigenous Arab population of Palestine. Under the cultural Zionist reading's binational framework, they are declared beneficiaries: no displacement, no demographic engineering, parity in a multinational polity. Their exit is constrained: they cannot leave the territory without losing their homeland, but the reading promises they need not leave. The omega 'arab_palestinian_beneficiary_status' captures the ambiguity of whether this promise is structurally realizable.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, arab_palestinian_population, beneficiary,
    organized, generational, constrained, regional).

% The Zionist Executive, Jewish Agency, and later Israeli state institutions that captured the territorial claim for a sovereignty project. They are excluded from the cultural reading's framework because their agenda (Jewish state with Jewish majority) is logically incompatible with the cultural reading's binational non-sovereignty logic. They have arbitrage exit: they simply implemented their own reading instead, using their institutional power to make it the operating reality.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, political_zionist_institutions, excluded,
    institutional, biographical, arbitrage, national).

% Intellectuals (Ahad Ha'am, Martin Buber, Judah Magnes, Hugo Bergmann, Ernst Simon) who articulated and advocated the cultural reading. They hold the analytical seat: they perceive the full structure, advocate the reading, but do not administer it or bear its costs directly. Their exit is analytical: they can abandon the reading intellectually without material consequence.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, brit_shalom_ihud_intellectuals, observer,
    analytical, biographical, analytical, regional).

% British mandatory administration (1920–1948) that set the political parameters within which the cultural reading operated. They enabled Jewish immigration and institution-building (Balfour Declaration, Mandate terms) but also constrained it (White Papers, immigration limits). They administered the territory but did not embody the Jewish territorial claim. Their exit was analytical: they withdrew in 1948, ending their agenda-setting role.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, mandate_british_authorities, agenda_setter,
    institutional, immediate, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__cultural_zionism_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__cultural_zionism_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a territorial home for Jewish national culture and Hebrew language revival without requiring the machinery of state sovereignty, demographic engineering, or displacement of the existing population. Solves the coordination problem of 'where and how can Jewish culture develop autonomously in its historic homeland?' by answering: in Palestine, alongside and not instead of the Arab population.
% TRANSFER_FUNCTION: Moves minimal resources — primarily immigration certificates, land purchase rights, and cultural autonomy permissions — from the mandatory/administrative authority to Jewish cultural institutions. No systematic transfer from Arab Palestinians to Jews; the reading explicitly rejects 'conquest of labor' and land expropriation. The transfer is administrative permission, not resource extraction.
% ABSENT_VOICES: Palestinian Arab nationalist leadership (al-Husseini, Nashashibi factions) who rejected any Jewish national rights in Palestine — they would object to even a cultural center as a wedge for sovereignty. Also absent: Jewish masses in Eastern Europe whose desperation drove political Zionism's urgency — they were not consulted on the cultural reading's gradualism. Both absences are structural: the cultural reading had no mechanism to include rejectionist Arab nationalism or rescue-driven Jewish migration.
% DISAPPEARANCE_RATIONALE: If the cultural Zionist constraint vanished overnight, the intellectual alternative to sovereignty-based Zionism would disappear. The binational framework would lose its most coherent Jewish articulation. Israeli peace movements and Palestinian interlocutors who draw on the cultural reading's logic (parity, non-displacement, cultural autonomy) would lose a structural reference point. The political reading would face no internal Jewish alternative — the constraint's disappearance rearranges the discourse, not the territory.
% FOUNDING_PROBLEM: Jewish national culture in the Diaspora faces assimilation, spiritual stagnation, and physical vulnerability. A territorial center in Palestine where Hebrew culture can develop autonomously — without requiring state sovereignty or displacing the Arab population — would regenerate Jewish national life.
% FOUNDING_PROBLEM_CORROBORATION: Ahad Ha'am (1897, 'The Jewish State and Jewish Problem') attests the cultural problem is distinct from the political problem. Martin Buber (1939, 'A Jewish Commonwealth?') attests the problem remains live under any framework. Israeli scholars (Anita Shapira, 'Land and Power') corroborate from outside the benefiting parties that cultural Zionism diagnosed a real problem (Diaspora cultural decay) but its solution was politically marginalized. No non-beneficiary source attests the problem is 'dead' — Hebrew culture in Israel today is shaped by state institutions the cultural reading did not envision.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__cultural_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__cultural_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__cultural_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
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
 *   Extractiveness is low (0.15) because the cultural reading does not inherently extract from Arab Palestinians — its coordination function is Jewish cultural autonomy, not resource transfer. The slight positive ε reflects historical friction: British mandatory restrictions on immigration/land purchase, and later the reading's marginalization by sovereign state structures. Suppression is near-zero (0.08) because the reading explicitly rejects coercive enforcement; its failure mode is political irrelevance, not violent suppression. Theater ratio is low (0.12): the reading's advocates (Brit Shalom, Ihud) genuinely pursued binational parity, not performative coexistence. Accessibility collapse is moderate (0.35): alternatives (political sovereignty, labor settlement) were politically dominant but the cultural reading remained intellectually accessible. Resistance is low (0.22): the reading faced marginalization, not active violent resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish cultural institutions and Hebrew revival are beneficiaries (d ≈ 0.1) — they receive the coordination good (cultural autonomy) without bearing extraction costs. Arab Palestinians are declared beneficiaries (d ≈ 0.2) under the binational framework — they retain presence and parity. The cultural reading does not create a payer class; extraction is near-zero because the claim is non-exclusionary. Political Zionist institutions are excluded agenda-setters: they captured the territorial claim's political realization and redirected it toward sovereignty, but they do not bear costs from the cultural reading itself. The directionality derivation follows from the non-sovereignty logic: no state apparatus = no extraction machinery.
 *
 * MANDATROPHY ANALYSIS:
 *   The cultural reading avoids mandatrophy because its coordination function (Jewish cultural autonomy in Palestine) remains partially live — Hebrew culture continues to develop in the territory. However, the reading's political marginalization after 1948 means its original binational framework was displaced by the sovereign state. The constraint persists as an intellectual alternative, not as an operating arrangement. This is not a piton (theatrical maintenance of dead function) because the cultural function is genuinely alive; it is a rope whose political instantiation was foreclosed by a competing reading of the same kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (cultural_zionism_reading) of the contested kernel jewish_territorial_claim. What structural elements differ between this reading and its siblings (political_zionism_reading, labor_zionism_reading, revisionist_zionism_reading)?',
    'Compare the declared readings'' beneficiary/victim structures, spatial scope claims, and enforcement requirements. The cultural reading''s binational framework and quality-over-quantity settlement logic produce a fundamentally different extraction profile than sovereignty-requiring readings.',
    'If the kernel decomposition is valid, each reading gets its own ε and classification. The cultural reading''s low extractiveness and near-zero suppression reflect its non-sovereignty logic; political and revisionist readings would show high extraction on Arab Palestinian populations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commitment-system kernel decomposition: cultural_zionism_reading vs. political/labor/revisionist sibling readings').

omega_variable(
    arab_palestinian_beneficiary_status,
    'Is the Arab Palestinian population a genuine beneficiary of the cultural Zionist framework, or does the framework''s Jewish-cultural-centering inevitably produce structural exclusion regardless of intent?',
    'Trace the operational consequences of ''Jewish spiritual and cultural center'' as an organizing principle: does it create Hebrew-dominant public space that marginalizes Arabic? Does binational parity hold when one nation defines the cultural center?',
    'If structural exclusion is inevitable, the Arab Palestinian entry in beneficiaries is a false summit — the constraint would reclassify toward tangled_rope or snare for that seat. If genuine parity is achievable, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arab_palestinian_beneficiary_status, empirical, 'Whether ''quality over quantity'' settlement with binational framework genuinely includes Arab Palestinians as beneficiaries or produces soft exclusion').

omega_variable(
    founding_problem_live_vs_dead,
    'Does the founding problem — ''Jewish national culture requires a territorial home where it can develop autonomously'' — remain live, or was it solved by the establishment of Israeli state institutions that now serve that function (however imperfectly)?',
    'Assess whether Hebrew cultural institutions, language revival, and Jewish spiritual life in Israel/Palestine today still depend on the specific cultural Zionist arrangement, or whether state sovereignty has subsumed and transformed the function.',
    'If the founding problem is dead but the constraint persists, mandatrophy_resolved triggers. If live, the constraint retains its coordination function. The cultural reading''s status differs from political Zionism''s: the cultural problem may be live even where the sovereignty problem is resolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_live_vs_dead, conceptual, 'R5 genealogy: founding problem status for cultural Zionist reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__cultural_zionism_reading, 1897, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewish_territorial_claim_cultural_zionism_tr_t1897, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1897, 0.05).
narrative_ontology:measurement(jewish_territorial_claim_cultural_zionism_tr_t1920, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1920, 0.08).
narrative_ontology:measurement(jewish_territorial_claim_cultural_zionism_tr_t1948, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(jewish_territorial_claim_cultural_zionism_tr_t1967, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1967, 0.22).
narrative_ontology:measurement(jewish_territorial_claim_cultural_zionism_tr_t1993, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1993, 0.12).
narrative_ontology:measurement(jewish_territorial_claim_cultural_zionism_tr_t2024, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(jewish_territorial_claim_cultural_zionism_be_t1897, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1897, 0.08).
narrative_ontology:measurement(jewish_territorial_claim_cultural_zionism_be_t1920, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1920, 0.12).
narrative_ontology:measurement(jewish_territorial_claim_cultural_zionism_be_t1948, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1948, 0.18).
narrative_ontology:measurement(jewish_territorial_claim_cultural_zionism_be_t1967, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1967, 0.22).
narrative_ontology:measurement(jewish_territorial_claim_cultural_zionism_be_t1993, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1993, 0.15).
narrative_ontology:measurement(jewish_territorial_claim_cultural_zionism_be_t2024, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(jewish_territorial_claim_cultural_zionism_su_t1897, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1897, 0.02).
narrative_ontology:measurement(jewish_territorial_claim_cultural_zionism_su_t1920, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1920, 0.05).
narrative_ontology:measurement(jewish_territorial_claim_cultural_zionism_su_t1948, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1948, 0.12).
narrative_ontology:measurement(jewish_territorial_claim_cultural_zionism_su_t1967, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1967, 0.18).
narrative_ontology:measurement(jewish_territorial_claim_cultural_zionism_su_t1993, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1993, 0.08).
narrative_ontology:measurement(jewish_territorial_claim_cultural_zionism_su_t2024, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 2024, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__cultural_zionism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__cultural_zionism_reading, 0.08).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This is the cultural_zionism_reading of the jewish_territorial_claim kernel. The kernel decomposes into four constraint stories with divergent ε and beneficiary/victim structures. The cultural reading is upstream in intellectual history (Ahad Ha'am precedes political Zionism's dominance) but downstream in political realization — the sovereign state instantiated the political reading, marginalizing this one. The network edges reflect structural influence: the cultural reading's binational logic influenced later peace frameworks (Oslo, two-state parameters) but its core claim (cultural center without sovereignty) was displaced by the political reading's statehood.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
