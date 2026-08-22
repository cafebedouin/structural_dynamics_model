% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__bridge_pidginized
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
 *   constraint_id: hebrew_continuity__bridge_pidginized
 *   human_readable: Hebrew as Diaspora Contact Language (Bridge Pidginized Reading)
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   Hebrew between 1780–1948 functioned as a contact language — a bridge
 *   pidgin — for Jewish diaspora interaction. It was not a native spoken
 *   language (few native speakers until late 19th century) nor purely
 *   liturgical (maskilic writers produced secular Hebrew literature,
 *   newspapers, scientific texts). The constraint is the structural
 *   arrangement that maintained Hebrew as a written lingua franca across
 *   mutually unintelligible Jewish vernaculars, with an Ashkenazi-normative
 *   standard emerging through maskilic and early Zionist
 *   institutionalization. This reading claims the bridge pidgin as a real,
 *   functional stage — not a 'corrupted' Hebrew (per liturgical_preservation)
 *   nor a 'failed' revival (per native_generative). The extraction comes from
 *   the Ashkenazi standardization that marginalized other Jewish linguistic
 *   traditions. The coordination is real: it solved a genuine intercommunal
 *   communication problem.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, 0.42).
domain_priors:suppression_score(hebrew_continuity__bridge_pidginized, 0.28).
domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, extractiveness, 0.42).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Hebrew as Diaspora Contact Language (Bridge Pidginized Reading)").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__bridge_pidginized).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, 'e773c3ef-7999-4e2f-9c40-ae362d09d616').
narrative_ontology:cs_kernel_codification('e773c3ef-7999-4e2f-9c40-ae362d09d616', distributed).
narrative_ontology:cs_authority_grounding('e773c3ef-7999-4e2f-9c40-ae362d09d616', extraction).
narrative_ontology:cs_interpretation_layer_present('e773c3ef-7999-4e2f-9c40-ae362d09d616').
narrative_ontology:cs_reading_relation('e773c3ef-7999-4e2f-9c40-ae362d09d616', hebrew_continuity__liturgical_preservation, influences).
narrative_ontology:cs_reading_relation('e773c3ef-7999-4e2f-9c40-ae362d09d616', hebrew_continuity__native_generative, forecloses).
narrative_ontology:cs_axiom('e773c3ef-7999-4e2f-9c40-ae362d09d616', foundational, hebrew_as_instrumental_diaspora_medium).
narrative_ontology:cs_axiom_status(hebrew_as_instrumental_diaspora_medium, holdable).
narrative_ontology:cs_axiom_grounding('e773c3ef-7999-4e2f-9c40-ae362d09d616', hebrew_as_instrumental_diaspora_medium, conventional).
narrative_ontology:cs_axiom('e773c3ef-7999-4e2f-9c40-ae362d09d616', foundational, native_speakers_not_required_for_language_life).
narrative_ontology:cs_axiom_status(native_speakers_not_required_for_language_life, holdable).
narrative_ontology:cs_axiom_grounding('e773c3ef-7999-4e2f-9c40-ae362d09d616', native_speakers_not_required_for_language_life, empirically_contingent).
narrative_ontology:cs_reference_frame('e773c3ef-7999-4e2f-9c40-ae362d09d616', pre_maskilic_diaspora_hebrew_interface).
narrative_ontology:cs_drift_state('e773c3ef-7999-4e2f-9c40-ae362d09d616', post_1948_state_hebrew, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('e773c3ef-7999-4e2f-9c40-ae362d09d616', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__bridge_pidginized, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, diaspora_traders).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, maskilic_writers).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, rabbinic_mediators).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, zionist_institution_builders).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, traditional_shtetl_speakers).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, sephardi_ladino_speakers).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, yemenite_arabic_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, mizrahi_communities).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, hebrew_as_portable_jewish_medium).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, linguistic_unity_as_national_prerequisite).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, revival_through_instrumental_utility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Used Hebrew as a lingua franca across Jewish communities from Amsterdam to Aleppo to Baghdad. Hebrew was a practical tool for contracts, correspondence, and credit networks — not a native language but a commercial asset. They benefited from a shared written register that worked across vernaculars. Their exit was mobility: they could shift to Ladino, Judeo-Arabic, or local languages, but Hebrew gave them a pan-diaspora edge.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, diaspora_traders, beneficiary,
    organized, biographical, mobile, global).

% Haskalah intellectuals (Mendelssohn, Wessely, later Smolenskin, Peretz) produced high-register Hebrew literature, periodicals, and scientific texts. They deliberately expanded Hebrew's vocabulary for modern concepts, treating it as a national instrument. They collected cultural capital and institutional positions from this work. Their exit was constrained: professional identity fused to Hebrew revival; abandoning it meant losing their intellectual project.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, maskilic_writers, agenda_setter,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__bridge_pidginized, maskilic_writers, beneficiary).

% Rabbinic authorities across Ashkenaz and Sepharad used Hebrew for responsa, halakhic correspondence, and communal governance. They were gatekeepers of Hebrew's textual authority. They benefited from Hebrew's status as the language of Torah — their interpretive monopoly depended on it. Their exit was identity-locked: rabbinic authority is constituted through Hebrew textual mastery; leaving Hebrew means ceasing to be a rabbinic authority.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, rabbinic_mediators, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__bridge_pidginized, rabbinic_mediators, beneficiary).

% Ben-Yehuda, the Va'ad HaLashon, the Hebrew University, the Histadrut — they hijacked the bridge pidgin and forced it into native generative territory. They collected state-building legitimacy, educational monopoly, and demographic engineering success. Their exit was arbitrage-grade: they had state power, could pivot to other national languages if needed, but chose Hebrew as the flagship.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, zionist_institution_builders, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Yiddish-speaking masses in Eastern Europe. Hebrew was imposed as the sacred tongue they were supposed to revere but not speak; the maskilic modernizers then demanded they adopt a modernized Hebrew they had no native intuition for. They paid in cultural displacement: their living vernacular (Yiddish) was denigrated as 'jargon' while a constructed Hebrew was elevated. They were trapped — no exit to a Jewish modernity that valued their language.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, traditional_shtetl_speakers, payer,
    powerless, biographical, trapped, regional).

% Judeo-Spanish communities with a rich written and oral tradition. The bridge pidgin's Ashkenazi-normative modernization marginalized Ladino's Hebrew component. They paid in linguistic assimilation: their distinct Hebrew pronunciation, syntax, and lexical traditions were erased by the 'standard' Hebrew the bridge produced. Exit was constrained — they could maintain Ladino at home but lost communal institutional support.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, sephardi_ladino_speakers, payer,
    moderate, biographical, constrained, continental).

% Yemeni Jews with a living tradition of Hebrew pronunciation and grammar distinct from both Ashkenazi and Sephardi norms. The bridge pidgin's standardization (driven by European maskilim and later Zionist institutions) treated their tradition as 'backward.' They paid in epistemic erasure: their oral Torah reading tradition was delegitimized. Trapped — no institutional power to resist the standard.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, yemenite_arabic_hebrew_speakers, payer,
    powerless, biographical, trapped, regional).

% Jewish communities from the Islamic world (Iraq, Persia, North Africa) with deep Hebrew textual traditions but distinct pronunciations and religious practices. The bridge pidgin's Ashkenazi hegemony (especially post-1948) forced a single standard. They paid in cultural subordination: their Hebrew was 'corrected' in schools, their liturgy standardized away. Exit constrained — they could preserve family traditions privately but lost public institutional space.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, mizrahi_communities, payer,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__bridge_pidginized, mizrahi_communities, excluded).

% Bundists, YIVO scholars, Yiddishist educators who argued Yiddish, not Hebrew, should be the Jewish national language. They were structurally excluded from the bridge pidgin's trajectory — the constraint's logic (Hebrew as the portable medium) had no seat for a rival vernacular nationalism. Their objection was that the bridge pidgin wasn't a neutral tool but a weapon against Yiddish. Identity-locked: their entire intellectual project was constituted through the Yiddish-Hebrew opposition.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, yiddish_cultural_autonomists, excluded,
    organized, generational, identity_locked, continental).

% Pre-1948 and post-1948 Palestinian intellectuals who engaged with Hebrew as a local Semitic language (e.g., Yahya Hammuda, Tawfiq Canaan). They were excluded from the 'Jewish national language' framing — the bridge pidgin's instrumental utility became a tool of dispossession. Their exit was constrained: they could study Hebrew academically but not claim it as theirs.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, palestinian_arab_intellectuals, excluded,
    organized, generational, constrained, regional).

% Scholars (e.g., Hary, Schwarzwald, Zuckermann, Sucharov) analyzing Hebrew's contact history, pidginization/creolization debates, and the gap between 'revival' mythology and sociolinguistic reality. They see the full structure: the bridge pidgin as a real historical stage, the native_generative reading as a political achievement retrojected as linguistic fact, the liturgical_preservation reading as a persistent but partial truth.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, contemporary_sociolinguists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a portable, textually anchored medium for Jewish communication across mutually unintelligible vernaculars (Yiddish, Ladino, Judeo-Arabic, Judeo-Persian, etc.) — enabling trade, halakhic correspondence, and intellectual exchange without requiring a shared spoken language.
% TRANSFER_FUNCTION: Moves linguistic authority and institutional resources from diverse local Jewish speech communities toward a centralized, Ashkenazi-normative Hebrew standard. The bridge pidgin's 'neutrality' was the mechanism: it presented itself as a tool for all but was built on Ashkenazi phonology, maskilic vocabulary, and European grammatical norms.
% ABSENT_VOICES: The speakers of Judeo-Arabic, Judeo-Persian, Judeo-Greek, and other non-Ashkenazi Jewish languages who had their own Hebrew traditions — they were never consulted in the standardization that the bridge pidgin enabled. Also absent: the mass of Yiddish speakers who were told their language was a 'jargon' while a constructed Hebrew was elevated. Their absence is structural: the bridge pidgin's coordination function required a single written standard, which could only be built by marginalizing competing vernaculars and their Hebrew interfaces.
% DISAPPEARANCE_RATIONALE: If the bridge pidgin constraint vanished overnight, the entire edifice of modern Hebrew — its standardized grammar, its 'revival' narrative, its status as Israel's official language, its pedagogical apparatus — would lose its historical legitimating chain. The Zionist project would have to invent a different linguistic foundation. The diaspora communities that used Hebrew as a contact language would revert to their vernaculars or adopt local languages. The world rearranges because this constraint is the historical bridge between liturgical preservation and native generative use — without it, the two readings have no structural connection.
% FOUNDING_PROBLEM: Jewish communities across the diaspora needed a shared language for high-stakes communication (trade, law, intellectual exchange) that was Jewishly legible but not tied to any single vernacular. Liturgical Hebrew was too rigid and sacred for daily commerce; vernaculars were mutually unintelligible. The bridge pidgin solved this by stripping Hebrew to a functional written core.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary sociolinguists (Zuckermann 2009, Schwarzwald 2001, Hary 2003) attest that the bridge pidgin's founding problem — diaspora intercommunal communication — is dead: the diaspora that needed it has largely assimilated or shifted to majority languages; the State of Israel provides a native Hebrew environment that makes the contact language obsolete. The maskilic and Zionist beneficiaries' own historiography claims the problem was 'solved' by revival, but the corroboration from outside the beneficiary set (sociolinguistics, Mizrahi studies, Yiddish scholarship) confirms the problem itself disappeared with the communities that had it.
narrative_ontology:disappearance_verdict(hebrew_continuity__bridge_pidginized, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__bridge_pidginized, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__bridge_pidginized, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(hebrew_continuity__bridge_pidginized, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__bridge_pidginized, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__bridge_pidginized_tests).
:- end_tests(hebrew_continuity__bridge_pidginized_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the asymmetric imposition of Ashkenazi norms on diverse communities — real but not totalizing; the bridge pidgin genuinely coordinated. Suppression (0.28) is moderate: alternatives (Yiddish, Ladino, Judeo-Arabic) persisted but were institutionally marginalized. Theater ratio (0.55) is high because the 'revival' narrative retroactively frames the bridge pidgin as a deliberate step toward native Hebrew, masking its instrumental, improvised character. Accessibility collapse (0.35) is low: vernaculars and their Hebrew interfaces remained accessible. Resistance (0.48) is significant: Yiddishists, Sephardi traditionalists, and Mizrahi communities actively contested the standard.
 *
 * PERSPECTIVAL GAP:
 *   From the Zionist institution-builder seat: the bridge pidgin was a necessary, heroic stage — a rope becoming native. From the Mizrahi/Yemenite payer seat: the same structure was a snare — Ashkenazi norms imposed as 'standard Hebrew.' From the Yiddish autonomist excluded seat: the constraint was a weapon against their language. The liturgical_preservation reading sees the bridge pidgin as profanation; the native_generative reading sees it as embryonic. The engine will compute these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora traders (beneficiary, mobile exit) gained coordination utility. Maskilic writers and rabbinic mediators (agenda_setters, identity_locked/constrained exit) gained cultural authority. Zionist institution-builders (agenda_setter, arbitrage exit) captured the constraint for state-building. Traditional shtetl speakers, Yemenite and Mizrahi communities (payers, trapped/constrained exit) bore cultural erasure. Yiddish autonomists and Palestinian intellectuals (excluded, identity_locked/constrained exit) were structurally shut out. The engine computes per-seat types from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora intercommunal communication) is dead — the communities that needed the bridge pidgin are gone or assimilated. The constraint persists (mandatrophy_resolved=false) because its institutional descendants (Va'ad HaLashon, Academy of Hebrew Language, Israeli education system) repurposed it for national construction. The mandate has outlived its function but the machinery remains. This is not a piton (which is inertial performance) but a scaffold that never got its sunset clause — it was hijacked for a new function (native generative) without acknowledging the transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bridge_pidgin_vs_creole,
    'Was the pre-1948 Hebrew a stable pidgin (no native speakers, reduced grammar) or an early creole (emerging native speakers, expanding grammar)?',
    'Detailed corpus analysis of Hebrew texts 1880–1948: first-language acquisition evidence, grammatical expansion rates, nativization markers in children''s speech/writing.',
    'If creole, the native_generative reading has a stronger claim to continuity; if pidgin, the bridge_pidginized reading''s claim of ''instrumental utility without native intuition'' is structurally accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bridge_pidgin_vs_creole, empirical, 'Pidgin vs. creole status of pre-state Hebrew').

omega_variable(
    ashkenazi_hegemony_necessity,
    'Was Ashkenazi-normative standardization structurally necessary for Hebrew''s function as a diaspora contact language, or was it a contingent power grab?',
    'Counterfactual modeling: could a multi-dialectal written standard (like Arabic''s diglossia) have served the same coordination function? Compare with other diaspora contact languages (Swahili, Malay, Hausa).',
    'If necessary, the extraction is the price of coordination (tangled_rope holds). If contingent, the extraction is a separable layer of domination (snare component dominates).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ashkenazi_hegemony_necessity, conceptual, 'Whether Ashkenazi standardization was functionally necessary or contingently extractive').

omega_variable(
    kernel_reading_boundary,
    'Where exactly does the bridge_pidginized reading end and the native_generative reading begin? Is there a sharp transition or a gradient?',
    'Identify the first cohort of native Hebrew speakers (Ben-Yehuda''s son Itamar, First Aliyah children) and trace when their speech community reached critical mass for generative grammar.',
    'If sharp transition, the two readings are distinct constraints (ε-invariance holds). If gradient, they may be phases of one constraint — requiring a single story with temporal measurements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, empirical, 'Boundary between bridge pidgin and native generative Hebrew').

omega_variable(
    liturgical_preservation_continuity,
    'Does the liturgical_preservation reading describe a continuous constraint from antiquity, or is ''liturgical Hebrew'' itself a construct of the same period that produced the bridge pidgin?',
    'Compare pre-modern Hebrew liturgical practice across communities: was there a single ''liturgical Hebrew'' or community-specific liturgical Hebrew interfaces?',
    'If liturgical_preservation is itself a modern construct, the kernel ''hebrew_continuity'' has no stable referent — all three readings are modern projections.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(liturgical_preservation_continuity, conceptual, 'Whether liturgical preservation is a continuous tradition or a modern construct').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 1780, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1780, hebrew_continuity__bridge_pidginized, theater_ratio, 1780, 0.15).
narrative_ontology:measurement(hebr_tr_t1820, hebrew_continuity__bridge_pidginized, theater_ratio, 1820, 0.22).
narrative_ontology:measurement(hebr_tr_t1860, hebrew_continuity__bridge_pidginized, theater_ratio, 1860, 0.35).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_continuity__bridge_pidginized, theater_ratio, 1880, 0.42).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_continuity__bridge_pidginized, theater_ratio, 1900, 0.48).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_continuity__bridge_pidginized, theater_ratio, 1920, 0.52).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_continuity__bridge_pidginized, theater_ratio, 1948, 0.55).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1780, hebrew_continuity__bridge_pidginized, base_extractiveness, 1780, 0.18).
narrative_ontology:measurement(hebr_be_t1820, hebrew_continuity__bridge_pidginized, base_extractiveness, 1820, 0.25).
narrative_ontology:measurement(hebr_be_t1860, hebrew_continuity__bridge_pidginized, base_extractiveness, 1860, 0.32).
narrative_ontology:measurement(hebr_be_t1880, hebrew_continuity__bridge_pidginized, base_extractiveness, 1880, 0.38).
narrative_ontology:measurement(hebr_be_t1900, hebrew_continuity__bridge_pidginized, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(hebr_be_t1920, hebrew_continuity__bridge_pidginized, base_extractiveness, 1920, 0.41).
narrative_ontology:measurement(hebr_be_t1948, hebrew_continuity__bridge_pidginized, base_extractiveness, 1948, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1780, hebrew_continuity__bridge_pidginized, suppression_requirement, 1780, 0.1).
narrative_ontology:measurement(hebr_su_t1820, hebrew_continuity__bridge_pidginized, suppression_requirement, 1820, 0.15).
narrative_ontology:measurement(hebr_su_t1860, hebrew_continuity__bridge_pidginized, suppression_requirement, 1860, 0.2).
narrative_ontology:measurement(hebr_su_t1880, hebrew_continuity__bridge_pidginized, suppression_requirement, 1880, 0.25).
narrative_ontology:measurement(hebr_su_t1900, hebrew_continuity__bridge_pidginized, suppression_requirement, 1900, 0.28).
narrative_ontology:measurement(hebr_su_t1920, hebrew_continuity__bridge_pidginized, suppression_requirement, 1920, 0.28).
narrative_ontology:measurement(hebr_su_t1948, hebrew_continuity__bridge_pidginized, suppression_requirement, 1948, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__bridge_pidginized, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__bridge_pidginized, 0.08).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, zionist_national_construction).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, yiddish_nationalism).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, mizrahi_cultural_erasure).

% DUAL FORMULATION NOTE:
% The kernel 'hebrew_continuity' decomposes into three constraint stories: liturgical_preservation (Mountain-claiming, low extraction), bridge_pidginized (Tangled Rope, moderate extraction), native_generative (Snare-claiming, high extraction from non-native communities). This story is the bridge_pidginized reading. The three are linked by network.affects_constraints. The ε values differ structurally: liturgical_preservation ε ≈ 0.05 (near Mountain), bridge_pidginized ε ≈ 0.42 (Tangled Rope), native_generative ε ≈ 0.65 (Snare for non-native communities).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_continuity__bridge_pidginized, institutional, 0.15).
constraint_indexing:directionality_override(hebrew_continuity__bridge_pidginized, powerless, 0.85).
constraint_indexing:directionality_override(hebrew_continuity__bridge_pidginized, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
