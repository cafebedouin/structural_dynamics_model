% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__bridge_pidginized
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__bridge_pidginized, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: hebrew_continuity__bridge_pidginized
 *   human_readable: Hebrew as Contact Language Bridge (Pidginized Reading)
 *   domain: sociolinguistics/language_revitalization
 *
 * SUMMARY:
 *   Hebrew is claimed by three structurally distinct readings as the kernel
 *   of Jewish linguistic continuity. This story instantiates the
 *   bridge_pidginized reading: Hebrew lives as a contact language for
 *   diaspora Jewish coordination, neither purely liturgical (the first
 *   sibling reading) nor fully native-generative (the third sibling reading).
 *   The pidginized reading treats Hebrew as instrumentally valuable for
 *   cross-diaspora communication, accepting high-register and low-register
 *   mixing, code-switching with heritage languages, and non-native speaker
 *   norms. The reading extracts from native speakers and linguistic purists
 *   through constant accommodation labor, and from non-Hebrew diaspora Jews
 *   through pressure to adopt Hebrew as the marker of Jewish identity. It
 *   benefits diaspora communities with a shared lingua franca and Israeli
 *   institutions with soft power. The constraint operates through
 *   institutional enforcement: Hebrew language bodies, educational curricula,
 *   and Israeli cultural diplomacy all reinforce the pidginized reading as
 *   the legitimate diaspora Hebrew. The measurement series tracks rising
 *   extractiveness (the institutional pressure to conform to diaspora Hebrew
 *   norms intensifies over time) and rising theater ratio (an increasing
 *   share of institutional activity defends the constraint's form rather than
 *   its function).
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities: primary beneficiary (gain shared lingua franca)
 *   - hebrew_language_institutions: agenda_setter (enforce norms, set curricula)
 *   - native_hebrew_speakers: primary payer (carry accommodation labor, linguistic dilution)
 *   - israel_state_apparatus: secondary agenda_setter (soft power through Hebrew promotion)
 *   - linguistic_purists: payer through correction labor and stigmatization work
 *   - non_hebrew_diaspora_jews: payer through language displacement pressure
 *   - liturgical_reading_adherents: excluded (their reading is structurally suppressed)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, 0.68).
domain_priors:suppression_score(hebrew_continuity__bridge_pidginized, 0.52).
domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, extractiveness, 0.68).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Hebrew as Contact Language Bridge (Pidginized Reading)").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistics/language_revitalization").

domain_priors:requires_active_enforcement(hebrew_continuity__bridge_pidginized).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, '941f9e53-8985-4992-9564-20f9bd58f8ea').
narrative_ontology:cs_kernel_codification('941f9e53-8985-4992-9564-20f9bd58f8ea', fixed_text).
narrative_ontology:cs_authority_grounding('941f9e53-8985-4992-9564-20f9bd58f8ea', lineage).
narrative_ontology:cs_interpretation_layer_present('941f9e53-8985-4992-9564-20f9bd58f8ea').
narrative_ontology:cs_reading_relation('941f9e53-8985-4992-9564-20f9bd58f8ea', hebrew_continuity__liturgical_preservation, coexists_with).
narrative_ontology:cs_reading_relation('941f9e53-8985-4992-9564-20f9bd58f8ea', hebrew_continuity__native_generative, influences).
narrative_ontology:cs_axiom('941f9e53-8985-4992-9564-20f9bd58f8ea', foundational, hebrew_instrumental_viability).
narrative_ontology:cs_axiom_status(hebrew_instrumental_viability, holdable).
narrative_ontology:cs_axiom_grounding('941f9e53-8985-4992-9564-20f9bd58f8ea', hebrew_instrumental_viability, instrumental).
narrative_ontology:cs_axiom('941f9e53-8985-4992-9564-20f9bd58f8ea', foundational, diaspora_hebrew_autonomy).
narrative_ontology:cs_axiom_status(diaspora_hebrew_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('941f9e53-8985-4992-9564-20f9bd58f8ea', diaspora_hebrew_autonomy, conventional).
narrative_ontology:cs_reference_frame('941f9e53-8985-4992-9564-20f9bd58f8ea', diaspora_lingua_franca_necessity).
narrative_ontology:cs_drift_state('941f9e53-8985-4992-9564-20f9bd58f8ea', contemporary_digital_globalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('941f9e53-8985-4992-9564-20f9bd58f8ea', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__bridge_pidginized, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, hebrew_language_institutions).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, secular_zionist_movements).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, native_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, linguistic_purists).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, non_hebrew_diaspora_jews).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, commercial_language_market).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, israel_state_apparatus).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, instrumental_language_utility_thesis).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, contact_language_viability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use Hebrew as a shared language across linguistic and cultural boundaries within global Jewish networks. Hebrew functions as the lingua franca that connects Russian-speaking Jews, French-speaking Jews, English-speaking Jews, and Arabic-speaking Jews in conversation, prayer, and coordination. They benefit from a language that does not privilege any single national or linguistic group and maintains Jewish collective identity across diaspora dispersion.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).

% Academic departments, summer immersion programs, diaspora community centers, and educational organizations that teach and standardize Hebrew as a shared language. They enforce standardized vocabulary and grammar norms, publish approved curricula, and exclude or stigmatize non-standard dialects and code-switching patterns. They extract institutional legitimacy and funding by maintaining Hebrew as the official language of Jewish continuity.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, hebrew_language_institutions, agenda_setter,
    institutional, generational, mobile, global).

% Use Hebrew as the primary instrument for constructing a shared Jewish political and cultural identity outside Israel. The constraint provides a neutral supranational language that instantiates Jewish peoplehood independent of territorial or religious claims. They benefit from Hebrew's symbolic weight as the national language of Israel, which they can invoke without full political commitment.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, secular_zionist_movements, beneficiary,
    institutional, generational, mobile, global).

% Native speakers (primarily in Israel but also in diaspora families) experience the constraint as dilution of native fluency norms. They pay through constant code-switching accommodation, explicit teaching labor (repeating explanations, slowing speech, providing translations), and identity fragmentation when non-native speakers' instrumental use diverges from native grammatical and phonological patterns. They cannot exit because their linguistic identity is constituted through Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, native_hebrew_speakers, payer,
    moderate, biographical, identity_locked, regional).

% Language purists, including some academic linguists and cultural conservatives, view the pidginized contact language as degraded Hebrew and a threat to linguistic integrity. They pay through the labor of correction, stigmatization, and teaching work aimed at raising diaspora Hebrew toward native standards. They cannot exit this role because their professional and cultural identity depends on maintaining language standards.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, linguistic_purists, payer,
    moderate, biographical, identity_locked, global).

% Jews from non-Hebrew diaspora backgrounds (Yiddish-speaking, Arabic-speaking, etc.) who are pressured to adopt Hebrew as the marker of Jewish peoplehood, often displacing native diaspora languages. They pay through the erosion of linguistic heritage and the cognitive load of language acquisition and maintenance. Their exit options are constrained: not learning Hebrew risks exclusion from institutional Jewish life and diaspora community coordination.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, non_hebrew_diaspora_jews, payer,
    moderate, biographical, constrained, global).

% Language-teaching industries, textbook publishers, software localization firms, and digital platforms benefit from the sustained demand for Hebrew instruction and simplified pedagogical materials. The constraint creates a steady market for 'diaspora-friendly' Hebrew education, designed for adult learners with limited immersion exposure.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, commercial_language_market, beneficiary,
    powerful, biographical, arbitrage, global).

% The Israeli government and affiliated cultural bodies set normative standards for 'correct' Hebrew, export educational programs through official channels, and derive soft power from Hebrew's status as the language of Jewish peoplehood globally. They enforce the constraint by controlling accreditation of Hebrew teachers, publishing official curricula, and using diplomatic channels to marginalize competing dialects.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, israel_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__bridge_pidginized, israel_state_apparatus, beneficiary).

% Communities and individuals who maintain Hebrew exclusively through liturgical and textual study (Orthodox yeshiva students, classical scholars, prayer-centered communities) are structurally excluded from the contact-language bridge: their Hebrew remains high-register, non-instrumental, and resistant to pidginization. They would argue that 'real Hebrew' is constituted through continuity with classical texts, not market-driven utility.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, liturgical_reading_adherents, excluded,
    moderate, generational, constrained, global).

% Academic observers and researchers studying language revitalization, contact linguistics, and the sociology of diaspora languages. They document the pidginization process, measure deviation from native norms, and provide independent analysis of whether the contact-language reading preserves or transforms the Hebrew kernel.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, linguistic_anthropologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__bridge_pidginized, hebrew_language_institutions).
narrative_ontology:fixing_cost_class(hebrew_continuity__bridge_pidginized, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared lingua franca that allows Jews from linguistically distinct diaspora backgrounds (Russian, French, Arabic, English, etc.) to communicate across communal and national boundaries without privileging any single heritage language. Solves the problem of Jewish collective coordination without requiring assimilation into any single national or linguistic majority.
% TRANSFER_FUNCTION: Transfers linguistic and educational labor from native speakers and purists (who must teach and accommodate non-native speakers) to diaspora communities (who gain access to Hebrew communication without full native fluency). Also transfers cultural authority from liturgical and textual traditions to institutional language bodies and the Israeli state, which set and enforce standardized norms for 'correct' diaspora Hebrew.
% ABSENT_VOICES: Speakers of displaced diaspora languages (Yiddish, Ladino, Arabic, Farsi) are structurally absent — they would argue that Hebrew displacement erases linguistic pluralism and minority heritage. Native Hebrew speakers who experience constant accommodation labor have limited voice in institutional decisions about diaspora Hebrew policy. Linguistic purists and textual scholars who reject the pidginized reading are partially excluded from defining 'legitimate' Hebrew use.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared, diaspora Jewish communities would reorganize around either (a) English as the primary lingua franca (with Hebrew relegated to liturgy), (b) a decentralized multilingual ecology where multiple heritage languages coexist, or (c) formal polyglot translation systems. The institutions invested in teaching diaspora Hebrew would reallocate resources; the Israeli state would lose a primary soft-power instrument; native speakers and purists would no longer carry the labor of accommodation.
% FOUNDING_PROBLEM: Early 20th-century Zionist and diaspora Jewish movements faced the problem of creating a shared linguistic and cultural identity across diaspora communities speaking mutually unintelligible languages (Yiddish, Ladino, Arabic, French, English). Reviving Hebrew as a shared symbolic language unified Jewish peoplehood without requiring conversion to any single national or cultural framework.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiographers and diaspora institutional leaders attest the founding problem was acute and Hebrew's revival solved it. Linguistic anthropologists and historians of diaspora Jewry confirm the coordination crisis existed in the early 20th century. However, contemporary scholars of language politics argue the founding problem has been substantially mitigated by the rise of English as a global lingua franca and by digital translation technologies, yet the constraint persists as institutional maintenance rather than functional necessity.
narrative_ontology:disappearance_verdict(hebrew_continuity__bridge_pidginized, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__bridge_pidginized, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__bridge_pidginized, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__bridge_pidginized, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__bridge_pidginized, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__bridge_pidginized_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__bridge_pidginized_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the constraint extracts visible labor from native speakers and purists (teaching, accommodation, constant correction) and extractive pressure on diaspora Jews (language acquisition burden, heritage erosion). Suppression is moderate (0.52) because the constraint does not fully prevent alternative readings — liturgical Hebrew persists, Yiddish and other diaspora languages continue in pockets — but the institutional apparatus actively marginalizes them from mainstream Jewish institutional life. Theater is moderate-to-high (0.41) because an increasing share of institutional Hebrew activity appears to be maintaining the language-bridge form (pedagogy, standardization, cultural promotion) rather than enabling diaspora coordination itself (which could increasingly run through English or digital translation). The measurement series on one shared grid shows extractiveness rising from 0.48 to 0.68 as institutional investment in diaspora Hebrew pedagogy intensifies, and theater rising from 0.22 to 0.41 as institutional activity shifts from coordination-enabling to form-maintenance. Suppression requirement rises from 0.38 to 0.52 as competing readings (liturgical, native-generative) require increasing institutional suppression to prevent them from displacing the contact-language reading.
 *
 * PERSPECTIVAL GAP:
 *   From the diaspora community seat, the constraint is genuine coordination — a shared language solving a real communication problem across linguistic boundaries. From the native speaker seat, the same structure operates as enforced labor extraction — constant teaching and accommodation to maintain a diaspora version of Hebrew that diverges from native norms. From the purist seat, it is institutionalized degradation — the corruption of Hebrew through contact with foreign structures. From the Israeli state seat, it is soft power and cultural authority. The engine should compute these divergent classifications from the structural data — institutional power, exit options (identity_locked for native speakers and purists; constrained for diaspora Jews), and the asymmetric beneficiary/victim structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities are beneficiaries with moderate-to-organized power and constrained exit (they depend on Hebrew for institutional access): d in the range 0.25-0.40. Hebrew language institutions are agenda-setters with institutional power and mobile exit (they could reallocate to other languages): d in the range 0.15-0.25 (they benefit but are not trapped). Native speakers are targets with moderate power but identity_locked exit (they cannot stop speaking Hebrew): d in the range 0.70-0.85. Linguistic purists are targets with moderate power and identity_locked exit: d in the range 0.70-0.85. Non-Hebrew diaspora Jews are targets with moderate power and constrained exit: d in the range 0.60-0.75. The state apparatus is an agenda_setter with institutional power and mobile exit, benefiting from soft power without full commitment: d in the range 0.15-0.30.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem — creating a shared linguistic identity across multilingual diaspora — is CONTESTED in status (some argue it is substantially solved by English and translation tech; others argue it remains live for Jews maintaining diaspora identity). The disappearance_verdict is WORLD_REARRANGES (diaspora communities would reorganize around alternatives). The combination should trigger mandatrophy investigation: an arrangement whose founding problem is dead or contested, whose world would rearrange without it, yet which persists and even intensifies institutional investment, is a candidate for zombie extraction. The rising theater_ratio and rising extractiveness measurements support this hypothesis: the constraint increasingly appears to be institutional self-maintenance rather than functional coordination. A snare reclassification (from the tangled_rope claim) would be warranted if the founders and beneficiaries have fully captured the institutional apparatus such that the coordination function is now purely performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pidginization_vs_native_viability,
    'Does the pidginized contact-language reading constitute a genuine viable form of Hebrew, or is it a degraded approximation that ultimately depends on native-speaker authority to maintain legitimacy?',
    'Longitudinal tracking of second-generation diaspora speakers'' Hebrew competence and autonomy: if a stable non-native-dependent Hebrew dialect emerges among diaspora children, the reading is viable; if diaspora Hebrew remains perpetually dependent on native-speaker correction and institutional scaffolding, it is derivative.',
    'If pidginized Hebrew is independently viable, it is a coordination rope with legitimate functional value. If it is perpetually derivative, the constraint is a snare — extraction masked as coordination, with institutional apparatus dependent on perpetual ''correction'' labor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pidginization_vs_native_viability, empirical, 'Whether diaspora Hebrew can stand as an independent linguistic system or remains perpetually subordinate to native norms.').

omega_variable(
    foundational_problem_obsolescence,
    'Has the founding problem of cross-diaspora linguistic coordination been substantially solved by English globalization and digital translation, such that the constraint now persists as institutional maintenance rather than functional necessity?',
    'Comparative case analysis: (a) diaspora communities that have shifted to English as primary lingua franca while maintaining Hebrew in ritual/symbolic contexts; (b) measurement of actual diaspora coordination traffic and where it primarily flows (through Hebrew, English, or multilingual code-switching); (c) institutional budget tracking for Hebrew pedagogy vs. actual diaspora communication needs.',
    'If the founding problem is obsolete (founding_problem_status = dead), and world_rearranges remains true, the constraint''s persistence becomes a case study in mandatrophy — institutional extraction defending a form whose function has transferred elsewhere.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_problem_obsolescence, empirical, 'Whether the contact-language bridge solves a live coordination problem or maintains institutional form after function has migrated.').

omega_variable(
    liturgical_vs_instrumental_boundary,
    'Is the boundary between liturgical Hebrew (the sibling reading''s domain) and instrumental diaspora Hebrew (this reading''s domain) a real structural distinction, or an institutional categorization that suppresses a continuum?',
    'Ethnographic analysis of actual Hebrew use in diaspora: Are liturgical contexts actually separate from instrumental communication, or do liturgical and instrumental registers mix in practice? Does institutional enforcement of the boundary require active suppression of code-switching and register-mixing?',
    'If the boundary is real and natural, this reading and the liturgical_preservation reading are genuinely distinct constraints. If it is institutional creation, both readings are expressions of a single constraint whose enforcement requires dramatic suppression of mixed-mode Hebrew use.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(liturgical_vs_instrumental_boundary, conceptual, 'Whether liturgical and instrumental Hebrew are structurally separable or institutionally divided categories imposed on a continuum.').

omega_variable(
    identity_lock_mechanism,
    'For native speakers carrying the accommodation labor (exit: identity_locked), is the lock structural (linguistic identity is constituted through Hebrew and cannot be severed) or internalized (native speakers have been socialized to accept accommodation as their role)?',
    'Comparative ethnography with diaspora populations where language shift has occurred: What happens to native speaker identity when the diaspora language is displaced? Do native speakers maintain linguistic identity through new channels, or does identity partially dissolve? Post-exit ethnography: Do native Hebrew speakers in non-Hebrew-dominant contexts maintain strong identity lock, or is the lock contingent on institutional context?',
    'If the lock is structural, native speakers are genuinely trapped and the extraction is high-confidence. If the lock is internalized institutional conditioning, it could be loosened by changed institutional framing — the extraction would remain but with different persistence conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether native Hebrew speaker identity-lock is constitutive or institutionally contingent.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the bridge_pidginized reading logically foreclose the native_generative reading, or do they coexist as alternative readings of the same kernel?',
    'Formal logical analysis: Does accepting Hebrew as a viable contact language (this reading) logically require denying that native-generative use is the ''true'' form of Hebrew? Or can both readings be held by different parties without internal contradiction?',
    'If bridge_pidginized forecloses native_generative, the two readings cannot coexist in a unified framework; the constraint should be reclassified as foreclosing. If they coexist, they are alternative readings of the same kernel, and the engine should compute the coexistence relation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between bridge_pidginized and native_generative readings of the Hebrew kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__bridge_pidginized, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t8, hebrew_continuity__bridge_pidginized, theater_ratio, 8, 0.27).
narrative_ontology:measurement_basis(hebr_tr_t8, observed).
narrative_ontology:measurement(hebr_tr_t16, hebrew_continuity__bridge_pidginized, theater_ratio, 16, 0.33).
narrative_ontology:measurement_basis(hebr_tr_t16, observed).
narrative_ontology:measurement(hebr_tr_t24, hebrew_continuity__bridge_pidginized, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(hebr_tr_t24, observed).
narrative_ontology:measurement(hebr_tr_t32, hebrew_continuity__bridge_pidginized, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(hebr_tr_t32, observed).
narrative_ontology:measurement(hebr_tr_t40, hebrew_continuity__bridge_pidginized, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(hebr_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__bridge_pidginized, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t8, hebrew_continuity__bridge_pidginized, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(hebr_be_t8, observed).
narrative_ontology:measurement(hebr_be_t16, hebrew_continuity__bridge_pidginized, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(hebr_be_t16, observed).
narrative_ontology:measurement(hebr_be_t24, hebrew_continuity__bridge_pidginized, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(hebr_be_t24, observed).
narrative_ontology:measurement(hebr_be_t32, hebrew_continuity__bridge_pidginized, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(hebr_be_t32, observed).
narrative_ontology:measurement(hebr_be_t40, hebrew_continuity__bridge_pidginized, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(hebr_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__bridge_pidginized, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(hebr_su_t0, observed).
narrative_ontology:measurement(hebr_su_t8, hebrew_continuity__bridge_pidginized, suppression_requirement, 8, 0.42).
narrative_ontology:measurement_basis(hebr_su_t8, observed).
narrative_ontology:measurement(hebr_su_t16, hebrew_continuity__bridge_pidginized, suppression_requirement, 16, 0.47).
narrative_ontology:measurement_basis(hebr_su_t16, observed).
narrative_ontology:measurement(hebr_su_t24, hebrew_continuity__bridge_pidginized, suppression_requirement, 24, 0.5).
narrative_ontology:measurement_basis(hebr_su_t24, observed).
narrative_ontology:measurement(hebr_su_t32, hebrew_continuity__bridge_pidginized, suppression_requirement, 32, 0.51).
narrative_ontology:measurement_basis(hebr_su_t32, observed).
narrative_ontology:measurement(hebr_su_t40, hebrew_continuity__bridge_pidginized, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(hebr_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__bridge_pidginized, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__bridge_pidginized, 0.12).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, yiddish_displacement_in_diaspora).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, israeli_soft_power_projection).

% DUAL FORMULATION NOTE:
% This story (bridge_pidginized reading) is one of three constraint stories decomposing the contested kernel 'hebrew_continuity'. The three readings are: (1) liturgical_preservation — Hebrew lives through preserved ritual and textual transmission; (2) native_generative — Hebrew lives only through native speaker generative use; (3) bridge_pidginized (THIS STORY) — Hebrew lives as contact language for diaspora coordination. Each reading has distinct ε, beneficiary/victim structure, and type classification. The readings are not simultaneously true of any single commitment framework; they represent live alternative positions in an ongoing institutional and cultural dispute about what counts as 'real Hebrew'. This reading influences and is influenced by both siblings through displacement, suppression, and competing claims to represent legitimate Hebrew continuity. All three stories are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_continuity__bridge_pidginized, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
