% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__religious_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__religious_restoration_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: zionist_legitimacy_basis__religious_restoration_reading
 *   human_readable: Religious Zionist Restoration Reading (Post-1967)
 *   domain: political/religious/nationalism
 *
 * SUMMARY:
 *   This constraint instantiates the religious restoration reading of the
 *   Zionist legitimacy kernel: the claim that Zionism is not merely a
 *   national liberation or colonial project, but the fulfillment of a divine
 *   promise and an active messianic process. Post-1967, this reading was
 *   radicalized to treat territorial withdrawal as a violation of covenant
 *   and to mandate Jewish settlement of the entire biblical Land of Israel.
 *   The constraint operates as a commitment system: a fixed textual kernel
 *   (biblical promise) interpreted through rabbinic lineage, generating
 *   binding normative conclusions that override secular political
 *   considerations. It coordinates a large community around shared meaning
 *   while extracting land and sovereignty from Palestinian communities and
 *   political autonomy from secular Israeli Jews.
 *
 * KEY AGENTS:
 *   - Religious Zionist community: Primary beneficiary (organized/identity_locked) â receives meaning and legitimation
 *   - Rabbinic authority structure: Agenda-setter with beneficiary secondary role (organized/constrained) â interprets and enforces the theological mandate
 *   - Palestinian communities: Primary payer (powerless/trapped) â bears dispossession and occupation
 *   - Secular Israeli Jews: Secondary payer (moderate/constrained) â political options overridden by theological coalition dynamics
 *   - Israeli state apparatus: Agenda-setter with beneficiary secondary role (institutional/constrained) â enforces settlement expansion while losing strategic flexibility
 *   - International community: Analytical observer (analytical/analytical) â monitors and contests from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.72).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.78).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Religious Zionist Restoration Reading (Post-1967)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political/religious/nationalism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, 'ace54eec-cdb5-40c0-8988-bf41f9a6f1df').
narrative_ontology:cs_kernel_codification('ace54eec-cdb5-40c0-8988-bf41f9a6f1df', fixed_text).
narrative_ontology:cs_authority_grounding('ace54eec-cdb5-40c0-8988-bf41f9a6f1df', lineage).
narrative_ontology:cs_interpretation_layer_present('ace54eec-cdb5-40c0-8988-bf41f9a6f1df').
narrative_ontology:cs_reading_relation('ace54eec-cdb5-40c0-8988-bf41f9a6f1df', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('ace54eec-cdb5-40c0-8988-bf41f9a6f1df', zionist_legitimacy_basis__settler_colonial_reading, forecloses).
narrative_ontology:cs_axiom('ace54eec-cdb5-40c0-8988-bf41f9a6f1df', foundational, eretz_yisrael_divinely_promised).
narrative_ontology:cs_axiom_status(eretz_yisrael_divinely_promised, holdable).
narrative_ontology:cs_axiom_grounding('ace54eec-cdb5-40c0-8988-bf41f9a6f1df', eretz_yisrael_divinely_promised, theological).
narrative_ontology:cs_axiom('ace54eec-cdb5-40c0-8988-bf41f9a6f1df', foundational, human_agency_mandatory_in_redemption).
narrative_ontology:cs_axiom_status(human_agency_mandatory_in_redemption, holdable).
narrative_ontology:cs_axiom_grounding('ace54eec-cdb5-40c0-8988-bf41f9a6f1df', human_agency_mandatory_in_redemption, theological).
narrative_ontology:cs_reference_frame('ace54eec-cdb5-40c0-8988-bf41f9a6f1df', biblical_territorial_mandate).
narrative_ontology:cs_drift_state('ace54eec-cdb5-40c0-8988-bf41f9a6f1df', contemporary_israeli_policy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ace54eec-cdb5-40c0-8988-bf41f9a6f1df', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_community).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, rabbinic_authority_structure).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinian_communities).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_jews).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives collective meaning, theological purpose, and political legitimation from the narrative that Jewish settlement fulfills divine promise. Members organize around yeshivas, settlement councils, and religious political parties. Exit from this framework means abandoning a core communal and cosmological identity, not merely changing a political preference.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_community, beneficiary,
    organized, generational, identity_locked, national).

% Produces binding interpretations of biblical and Talmudic sources that mandate territorial maximalism and forbid withdrawal. Controls educational curricula, kosher certification leverage, and religious court appointments. Its authority depends on maintaining the exclusivity of the divine promise reading; departure from maximalism risks splintering its own legitimacy.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, rabbinic_authority_structure, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, rabbinic_authority_structure, beneficiary).

% Bear the direct costs of territorial expansion through land confiscation, settlement encirclement, military occupation, and denial of sovereignty. Their presence and claims are rendered illegitimate by the theological framework that designates the land as exclusively promised to the Jewish people. Exit options are blocked by military, legal, and diplomatic barriers.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_communities, payer,
    powerless, biographical, trapped, regional).

% Live within a state whose policy options are constrained by theological imperatives they do not share. Territorial compromise, secular constitutional reform, and separation of religion and state are blocked by coalition dynamics driven by the religious reading. They benefit from general state services but bear the costs of perpetual conflict and democratic deficit.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_jews, payer,
    moderate, biographical, constrained, national).

% Administrates settlement expansion, military occupation, and legal discrimination through civilian bureaucracies and security services. While formally sovereign, key ministries and coalition structures are captured by parties advancing the religious restoration reading. The apparatus gains territorial control and budget flows but loses strategic flexibility and international legitimacy.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_apparatus, beneficiary).

% Monitors and intermittently condemns settlement expansion through diplomatic statements, international law rulings, and sanctions threats. Does not experience the constraint directly but provides the discursive framework in which the religious reading is contested as a violation of international norms.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, international_community, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__religious_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the religious Zionist community around a shared eschatological narrative that provides cosmic meaning, collective purpose, and political mobilization in the context of modern Jewish statehood.
% TRANSFER_FUNCTION: Transfers land, sovereignty, and military-political resources from Palestinian communities and secular Israeli society to the religious Zionist settlement enterprise and its allied state institutions.
% ABSENT_VOICES: Palestinian theological counter-claims to the land (Islamic and Christian sacred geography) and secular anti-Zionist Jewish voices are structurally excluded from the normative framework. Their inclusion would rupture the exclusivity of the divine promise claim.
% DISAPPEARANCE_RATIONALE: If the theological mandate vanished, the settlement project would lose its primary legitimation, territorial compromise would become politically viable, and the religious Zionist community would face a crisis of meaning and collective purpose requiring fundamental reorganization.
% FOUNDING_PROBLEM: The theological crisis of Jewish statehood lacking divine sanctification, and the perceived danger that secular Zionism would abandon covenantal land and produce a state like any other rather than a redemptive instrument.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist theologians and historians attest the founding problem from within the benefiting community. Critical Israeli sociologists, Palestinian historians, and secular constitutional scholars attest that the problem was retroactively constructed to justify expansion, and that the arrangement functions as territorial acquisition rather than theological repair. Corroboration from outside the benefiting parties is mixed and politically divided.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__religious_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__religious_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__religious_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__religious_restoration_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the theological mandate drives zero-sum territorial expansion that is structurally insulated from cost-benefit calculation. Suppression (0.78) is high because the constraint actively delegitimizes alternatives: territorial compromise is treated as heresy, and Palestinian self-determination is theologically excluded. Theater ratio (0.45) is moderate because settlement activity involves genuine religious performance (sanctification through inhabitation) alongside political instrumentalization. Accessibility collapse (0.80) is high because once the divine promise framework is accepted, secular pragmatic alternatives become cognitively and normatively unavailable. Resistance (0.70) reflects sustained Palestinian opposition, intermittent secular Israeli protest, and international legal pressure. The temporal series tracks the 1967â2024 interval, showing intensifying extraction and enforcement as the settlement enterprise institutionalized and the religious Zionist political bloc captured coalition politics.
 *
 * PERSPECTIVAL GAP:
 *   The religious Zionist community experiences this constraint as sacred duty and cosmic participation (coordination, low effective extraction), while Palestinian communities experience it as a supremacist mechanism of dispossession (extraction, high effective extraction), and secular Israeli Jews experience it as a democratic deficit imposed by a minority theological veto. The engine computes this divergence from identical structural data by applying directionality and scope: identity-locked beneficiaries at national scope receive subsidy-like negative extraction, while trapped payers at regional scope receive amplified extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (religious_zionist_community, israeli_state_apparatus, rabbinic_authority_structure) drive directionality toward the beneficiary end, dampening effective extraction. Victim declarations (palestinian_communities, secular_israeli_jews) drive directionality toward the target end, amplifying effective extraction. The identity_locked exit of the religious community fuses it to the constraint, producing near-subsidy conditions. The trapped exit of Palestinian communities and constrained exit of secular Israelis place them in high-target positions. The institutional state apparatus, despite formal power, is captured by the theological constraint and sits in a mixed position due to its constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents the mandatrophy error of treating the constraint as either pure coordination (a rope) or pure extraction (a snare). The religious reading genuinely coordinates a community around meaning, eschatology, and collective action; calling it a snare would erase that coordination function. Conversely, the reading extracts asymmetrically from Palestinians and secular Israelis; calling it a rope would erase those costs. The active enforcement requirement (rabbinic rulings, state military and legal apparatus, educational indoctrination) is what holds the hybrid together: without enforcement, the coordination would splinter (secular drift) and the extraction would face reversal (territorial withdrawal).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_sincerity_vs_instrumentalization,
    'Is the religious Zionist commitment to territorial maximalism primarily sincere theological conviction or instrumental political ideology deployed to justify expansion?',
    'Longitudinal analysis of rabbinic responsa, private communications, and behavioral response to counterfactual scenarios such as settlements in strategically worthless areas.',
    'If instrumental, the constraint''s coordination function is cover for extraction, shifting classification toward snare; if sincere, the genuinely held belief sustains the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_sincerity_vs_instrumentalization, empirical, 'Tests whether the theological layer is authentic belief or justification.').

omega_variable(
    state_capture_or_instrumentalization,
    'Does the religious reading autonomously constrain state policy, or has the state apparatus instrumentalized the reading to legitimize independently desired territorial expansion?',
    'Process-tracing of policy formation to determine whether security and planning bodies initiate expansion and later seek rabbinic legitimation, or rabbinic rulings precede and direct state action.',
    'If the state instrumentalizes the reading, the state seat shifts toward concentrated beneficiary and the constraint''s extraction is more centralized; if the reading genuinely constrains the state, the state seat is structurally a payer and the constraint is more diffuse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capture_or_instrumentalization, conceptual, 'Direction of causality between theology and state expansion.').

omega_variable(
    kernel_foreclosure_validity,
    'Does the religious restoration reading''s core premise logically foreclose the settler-colonial reading, or do the readings merely politically exclude one another while remaining co-entertainable in a single framework?',
    'Formal analysis of whether any single coherent framework can consistently affirm both ''divinely promised restoration'' and ''European colonial displacement'' as the fundamental nature of the same historical process.',
    'If forecloses is valid, the engine''s axiom-contradiction detection will confirm; if not, the relation should be reclassified to influences, altering the kernel''s structural map.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_foreclosure_validity, conceptual, 'Structural relation between sibling readings of the Zionism kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'For Palestinian communities, is the measured suppression primarily structural (military occupation, legal discrimination, settlement encirclement) or internalized (acquiescence to permanent displacement as inevitable)?',
    'Post-opening suppression trajectory: measure political claims and resistance behavior following concrete territorial concessions or diplomatic openings to determine whether suppression persists after external barriers ease.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure, making extraction harder to reverse; if purely structural, removal of external barriers would rapidly shift the seat''s directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression for Palestinian payers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__religious_restoration_reading, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zionist_religious_tr_t0, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(zionist_religious_tr_t10, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(zionist_religious_tr_t20, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(zionist_religious_tr_t30, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(zionist_religious_tr_t40, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(zionist_religious_tr_t57, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 57, 0.58).

% Extraction over time
narrative_ontology:measurement(zionist_religious_be_t0, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(zionist_religious_be_t10, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(zionist_religious_be_t20, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(zionist_religious_be_t30, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(zionist_religious_be_t40, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement(zionist_religious_be_t57, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 57, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(zionist_religious_su_t0, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(zionist_religious_su_t10, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(zionist_religious_su_t20, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(zionist_religious_su_t30, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(zionist_religious_su_t40, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(zionist_religious_su_t57, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 57, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is the religious_restoration_reading of kernel zionist_legitimacy_basis. It is decomposed from the national_liberation_reading and settler_colonial_reading per the Îµ-invariance principle, as each reading instantiates a structurally distinct constraint with different beneficiary/victim structures, Îµ values, and classification types. The upstream kernel is the contested political-historical phenomenon of Zionism; the downstream readings are independent constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
