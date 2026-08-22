% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__jewish_national_home_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__jewish_national_home_primacy, []).

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
 *   constraint_id: balfour_mandate_instruments__jewish_national_home_primacy
 *   human_readable: Balfour Mandate Instruments — Jewish National Home Primacy Reading
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint story captures the 'jewish_national_home_primacy' reading
 *   of the Balfour Mandate instruments kernel. The reading interprets the
 *   Mandate's 'national home' language as authorizing and directing
 *   demographic and territorial transformation toward Jewish sovereignty:
 *   Article 4 grants the Jewish Agency quasi-governmental status; land
 *   transfer from Arab to Jewish ownership is systematically facilitated
 *   through legal instruments; immigration quotas are managed to favor
 *   demographic transformation; Arab political representation is structurally
 *   downgraded relative to Jewish institutional representation. The
 *   constraint operates as a tangled_rope: it establishes genuine
 *   coordination infrastructure (Jewish Agency as public body, Hebrew
 *   education/health systems, labor federation) while simultaneously
 *   extracting land, political standing, and demographic future from the
 *   Palestinian Arab population through the same legal-administrative
 *   structure. Active enforcement by the British mandatory power is required
 *   to sustain the asymmetric arrangement.
 *
 * KEY AGENTS:
 *   - zionist_institutions: Primary beneficiary (institutional/arbitrage) — receives quasi-governmental recognition, land allocation, immigration control, resource channeling
 *   - jewish_migrants: Primary beneficiary (organized/mobile) — receives facilitated entry, institutional absorption, land access, political representation
 *   - palestinian_arab_landholders: Primary victim (powerless/trapped) — bears land transfer, dispossession, legal disability in land transactions
 *   - palestinian_arab_political_leadership: Primary victim (organized/constrained) — bears political subordination, exclusion from parallel institutional structures, demographic marginalization
 *   - british_mandatory_power: Agenda setter (institutional/arbitrage) — administers the constraint, holds interpretive discretion, enforces asymmetric structure
 *   - league_of_nations: Observer (institutional/analytical) — nominal supervisory authority, limited actual oversight
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, 0.82).
domain_priors:suppression_score(balfour_mandate_instruments__jewish_national_home_primacy, 0.78).
domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, extractiveness, 0.82).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Balfour Mandate Instruments — Jewish National Home Primacy Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, '5ea3721f-06bd-4f2f-be9f-81c3e873d0b3').
narrative_ontology:cs_kernel_codification('5ea3721f-06bd-4f2f-be9f-81c3e873d0b3', formalized).
narrative_ontology:cs_authority_grounding('5ea3721f-06bd-4f2f-be9f-81c3e873d0b3', extraction).
narrative_ontology:cs_interpretation_layer_present('5ea3721f-06bd-4f2f-be9f-81c3e873d0b3').
narrative_ontology:cs_reading_relation('5ea3721f-06bd-4f2f-be9f-81c3e873d0b3', balfour_mandate_instruments__dual_obligation_indigenous_rights, influences).
narrative_ontology:cs_reading_relation('5ea3721f-06bd-4f2f-be9f-81c3e873d0b3', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('5ea3721f-06bd-4f2f-be9f-81c3e873d0b3', foundational, jewish_national_home_requires_demographic_majority).
narrative_ontology:cs_axiom_status(jewish_national_home_requires_demographic_majority, holdable).
narrative_ontology:cs_axiom_grounding('5ea3721f-06bd-4f2f-be9f-81c3e873d0b3', jewish_national_home_requires_demographic_majority, instrumental).
narrative_ontology:cs_axiom('5ea3721f-06bd-4f2f-be9f-81c3e873d0b3', foundational, jewish_agency_as_proto_state_authority).
narrative_ontology:cs_axiom_status(jewish_agency_as_proto_state_authority, holdable).
narrative_ontology:cs_axiom_grounding('5ea3721f-06bd-4f2f-be9f-81c3e873d0b3', jewish_agency_as_proto_state_authority, conventional).
narrative_ontology:cs_reference_frame('5ea3721f-06bd-4f2f-be9f-81c3e873d0b3', balfour_declaration_1917).
narrative_ontology:cs_drift_state('5ea3721f-06bd-4f2f-be9f-81c3e873d0b3', mandate_termination_1947, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5ea3721f-06bd-4f2f-be9f-81c3e873d0b3', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__jewish_national_home_primacy, jewish_self_determination_in_palestine).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__jewish_national_home_primacy, mandate_as_proto_state_instrument).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives quasi-governmental recognition under Mandate Article 4 (Jewish Agency as 'public body'), operates land acquisition (JNF), immigration management, Hebrew education/health systems, and labor federation (Histadrut). Channels resources and administrative capacity from the Mandate structure to build proto-state institutions. Exit is arbitrage-grade: institutions could relocate or reconfigure but the Mandate provides unmatched legal-administrative leverage.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, beneficiary,
    institutional, generational, arbitrage, regional).

% Receives facilitated immigration entry (certificates, quotas managed by Jewish Agency), institutional absorption (Hebrew labor, kibbutz/moshav settlement, Histadrut), land access through JNF purchases, and political representation in the Yishuv's elected bodies. Exit is mobile: migrants could choose other destinations (US, etc.) but the Mandate structure lowers entry barriers and provides institutional support unmatched elsewhere.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants, beneficiary,
    organized, biographical, mobile, regional).

% Bears systematic land transfer to Jewish ownership through legal instruments (Land Transfer Regulations 1940, absentee property mechanisms, JNF purchases facilitated by Mandate land registration). Dispossession is often irreversible — land sold under economic pressure or legal disability cannot be recovered. Exit is trapped: land is immobile, legal remedies within the Mandate system are structurally foreclosed, and physical relocation means abandoning ancestral holdings.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders, payer,
    powerless, biographical, trapped, local).

% Bears political subordination: excluded from parallel legislative/executive structures granted to Jewish Agency; demographic marginalization through immigration quotas; suppression of national institutions (Arab Higher Committee outlawed 1937). Attempts to negotiate equal representation (1939 White Paper negotiations) are overridden by Mandate's primacy commitment. Exit is constrained: political leadership can resist, appeal to League/UN, or revolt, but the Mandate's legal-administrative structure structurally downgrades their standing.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, payer,
    organized, generational, constrained, regional).

% Administers and enforces the Mandate instruments: appoints High Commissioner, controls immigration quotas, enforces land regulations, suppresses Arab revolt (1936-39), holds interpretive discretion over 'national home' vs. 'dual obligation'. Bears imperial costs (military, administrative, diplomatic) but extracts strategic value (Suez access, imperial prestige, wartime mobilization). Exit is arbitrage-grade: Britain could (and did) refer the problem to UN and withdraw, but only after 25 years of active administration.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_power, agenda_setter,
    institutional, biographical, arbitrage, global).

% Nominal supervisory authority via Permanent Mandates Commission: receives annual reports, hears petitions, issues observations. Lacks enforcement capacity; British non-compliance with Commission recommendations (e.g., on Arab representation, land protection) goes unsanctioned. Exit is analytical: the League observes but cannot alter the constraint's operation; its dissolution (1946) ends even nominal oversight.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, league_of_nations, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes Jewish institutional capacity in Palestine through recognized quasi-governmental structure (Jewish Agency), Hebrew-language public systems (education, health, labor), and centralized immigration/land management — solving the coordination problem of building a national polity from a dispersed diaspora population.
% TRANSFER_FUNCTION: Moves land (from Arab owners to JNF/Jewish purchasers), immigration slots (from global pool to Jewish migrants via Agency certificates), political representation (from Arab majority to Jewish minority institutions), and administrative authority (from British to Jewish Agency) — all through Mandate legal-administrative machinery.
% ABSENT_VOICES: Palestinian Arab peasantry (fellahin) who worked the land but lacked formal title — structurally excluded from land registration systems and petition mechanisms. Palestinian Arab women — excluded from both Mandate and Yishuv political structures. Transjordanian Bedouin tribes — affected by Mandate border demarcation but not consulted. These voices would object to land dispossession and political exclusion but were never seated in the Mandate's governance architecture.
% DISAPPEARANCE_RATIONALE: If the Jewish national home primacy constraint vanished overnight (Mandate Article 4 recognition withdrawn, land transfer facilitation ended, immigration quotas equalized, Arab political representation upgraded), the Yishuv's proto-state institutions would lose their legal-administrative foundation, land markets would revert to Ottoman-era patterns, demographic trajectory would shift, and the 1947-49 war/partition trajectory would be fundamentally altered. The world rearranges because the constraint built the institutional and demographic preconditions for Israeli statehood.
% FOUNDING_PROBLEM: The Balfour Declaration (1917) committed Britain to 'the establishment in Palestine of a national home for the Jewish people' — a commitment made to the Zionist movement without consulting the Palestinian Arab majority. The Mandate (1922) operationalized this commitment through Article 4 (Jewish Agency recognition), Article 6 (Jewish immigration facilitation), and Article 11 (land settlement). The founding problem was: how to implement a British imperial commitment to Zionist state-building in a territory with an Arab majority, under League of Nations mandate supervision.
% FOUNDING_PROBLEM_CORROBORATION: Zionist institutions (Jewish Agency, WZO) attest the founding problem remains live: ongoing security needs, demographic competition, and incomplete ingathering justify continued primacy. Palestinian leadership (AHC, PLO) and international legal scholars (e.g., Henry Cattan, John Quigley) attest the founding problem is dead: Jewish sovereignty achieved (1948), continued extraction lacks founding justification. British archival records (Colonial Office, Cabinet papers) show Mandate officials increasingly treating 'national home' as requiring demographic transformation beyond the Declaration's text — corroboration from outside the beneficiary set that the constraint outran its founding mandate.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(balfour_mandate_instruments__jewish_national_home_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint systematically transfers land, political standing, and demographic trajectory from the Arab population to Zionist institutions and Jewish migrants through state-administrative machinery. Suppression (0.78) is high because the arrangement depends on British military-administrative enforcement to maintain land transfer mechanisms, immigration quotas, and political representation asymmetry — alternatives (binational state, equal representation, land tenure protection) are actively suppressed. Theater ratio (0.45) is moderate: the 'national home' framing and League of Nations mandate supervision provide legitimating cover, but the operational core is extractive transformation. Accessibility collapse (0.68) reflects that once the Mandate's legal-administrative structure is in place, alternative political futures (equal citizenship, binational federation, Arab sovereignty) become structurally difficult to access. Resistance (0.72) is high: the Arab population mounted sustained political, legal, and armed resistance throughout the period (1920-1947), including the 1936-39 revolt.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter (British mandatory power) experiences the constraint as administrative implementation of a League mandate — coordination with extraction as policy choice. The beneficiaries (Zionist institutions, Jewish migrants) experience it as liberation/coordination — gaining institutional capacity and demographic foothold. The victims (Palestinian Arab landholders and leadership) experience it as dispossession and political erasure — the same legal instruments that 'coordinate' Jewish institutional development extract their land and political future. The engine computes these seat divergences from the structural power/exit/beneficiary declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist institutions and Jewish migrants are declared beneficiaries: they collect institutional recognition, land, immigration slots, and political representation through the constraint. Palestinian Arab landholders and political leadership are declared victims: they bear land loss, political subordination, and demographic marginalization through the same instruments. The British mandatory power is the agenda_setter: it administers and enforces the asymmetric structure, holding interpretive discretion (see sibling reading 'mandatory_interpretive_discretion'). Directionality derives from these structural positions — beneficiaries near d=0.15 (subsidized by constraint), victims near d=0.85 (extracted by constraint), agenda_setter near d=0.3 (extracts administrative control but bears imperial cost).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establishing a Jewish national home in Palestine per Balfour Declaration) was live in 1922 but the constraint's extraction profile intensified well beyond the coordination requirements of that founding problem. By 1936-39, the extraction (land transfer, demographic engineering, political suppression) had become the constraint's primary operational logic, with coordination functions (Jewish Agency capacity) serving as infrastructure for extraction rather than as independent goods. The mandate's termination in 1947 did not resolve the mandatrophy — the constraint's successor structures (Israeli state institutions, military occupation, settlement regime) inherited and amplified the extraction logic. The founding problem is contested (status=contested): Zionist institutions claim it remains live (ongoing security/development needs); Palestinian leadership and international law scholars attest it is dead (sovereignty achieved, extraction continues without founding justification).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the contested kernel ''balfour_mandate_instruments'' or does it collapse into the kernel''s undifferentiated operation?',
    'Compare structural beneficiaries/victims, extractiveness profile, and institutional outcomes against sibling readings (dual_obligation_indigenous_rights, mandatory_interpretive_discretion). If extractiveness and suppression vectors diverge significantly, the reading is structurally distinct.',
    'If the reading is not distinct, it cannot carry its own ε and the kernel must be treated as a single constraint. If distinct, each reading requires separate classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the jewish_national_home_primacy reading constitutes a structurally independent constraint from the kernel''s other readings.').

omega_variable(
    coordination_extraction_separability,
    'Is the coordination function (establishing Jewish institutional capacity) structurally separable from the extraction function (land transfer, demographic displacement, political subordination of Arab population)?',
    'Analyze whether the Mandate''s Article 4 quasi-governmental recognition of the Jewish Agency could have operated without the land sales facilitation, immigration quotas, and representation downgrading. Historical counterfactual: what if Jewish institutional development proceeded without asymmetric extraction?',
    'If inseparable, the constraint is a snare with coordination cover. If separable, it remains a genuine tangled_rope where coordination and extraction are distinct structural layers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable or necessarily fused.').

omega_variable(
    mandate_article4_legal_character,
    'Does Mandate Article 4''s recognition of the Jewish Agency as a ''public body'' constitute a genuine coordination mechanism or an extraction enabler?',
    'Trace the Agency''s actual functions: land acquisition (JNF), immigration management, educational/health systems, labor federation (Histadrut). Assess which functions served collective coordination vs. which channeled resources/power asymmetrically.',
    'If primarily extraction-enabling, the constraint''s claimed coordination function is cover. If genuinely coordinative, the tangled_rope classification holds with real coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_article4_legal_character, empirical, 'The structural character of the Jewish Agency''s quasi-governmental status under Article 4.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 1922, 1947).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1922, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1922, 0.2).
narrative_ontology:measurement(balf_tr_t1929, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1929, 0.3).
narrative_ontology:measurement(balf_tr_t1936, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1936, 0.42).
narrative_ontology:measurement(balf_tr_t1939, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1939, 0.38).
narrative_ontology:measurement(balf_tr_t1947, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1947, 0.45).

% Extraction over time
narrative_ontology:measurement(balf_be_t1922, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1922, 0.45).
narrative_ontology:measurement(balf_be_t1929, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1929, 0.58).
narrative_ontology:measurement(balf_be_t1936, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1936, 0.71).
narrative_ontology:measurement(balf_be_t1939, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1939, 0.65).
narrative_ontology:measurement(balf_be_t1947, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1947, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1922, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1922, 0.35).
narrative_ontology:measurement(balf_su_t1929, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1929, 0.55).
narrative_ontology:measurement(balf_su_t1936, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1936, 0.75).
narrative_ontology:measurement(balf_su_t1939, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1939, 0.7).
narrative_ontology:measurement(balf_su_t1947, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1947, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(balfour_mandate_instruments__jewish_national_home_primacy, 0.12).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__dual_obligation_indigenous_rights).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__mandatory_interpretive_discretion).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, israeli_absentee_property_law).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_refugee_status_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the balfour_mandate_instruments kernel. The three readings form a constraint family: jewish_national_home_primacy (this story, high ε tangled_rope), dual_obligation_indigenous_rights (lower ε rope/scaffold reading emphasizing Arab rights protection), and mandatory_interpretive_discretion (meta-constraint on interpretive authority). The primacy reading structurally influences the dual_obligation reading by consuming the Mandate's administrative capacity for extraction rather than rights protection, and influences the interpretive_discretion reading by making British adjudication a site of contestation rather than neutral arbitration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__jewish_national_home_primacy, institutional, 0.3).
constraint_indexing:directionality_override(balfour_mandate_instruments__jewish_national_home_primacy, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
