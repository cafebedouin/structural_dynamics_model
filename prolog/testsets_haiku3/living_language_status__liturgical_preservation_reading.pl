% ============================================================================
% CONSTRAINT STORY: living_language_status__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__liturgical_preservation_reading, []).

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
 *   constraint_id: living_language_status__liturgical_preservation_reading
 *   human_readable: Living Language Status via Liturgical Preservation (Rabbinical Reading)
 *   domain: sociolinguistics/religious_studies/nationalism
 *
 * SUMMARY:
 *   Hebrew in the modern Jewish diaspora presents a contested linguistic
 *   phenomenon: a language that was not natively transmitted for centuries,
 *   survived through liturgical and scholarly transmission, and then
 *   experienced massive native-speaker revival beginning in the nineteenth
 *   century with the Haskalah and Zionist settlement in Palestine. Three
 *   readings propose fundamentally different measures of linguistic
 *   'aliveness.' The rabbinical-preservation reading defines vitality through
 *   continuous recitation, study, and ritual use of sacred texts according to
 *   rabbinical interpretive authority. This reading benefits the rabbinical
 *   institutional authority (preserves their interpretive monopoly) and
 *   validates orthodox communities' liturgical practice. It simultaneously
 *   delegitimizes secular Hebrew speakers and literary modernizers as
 *   'desecrators' whose generative use is secondary to proper
 *   (rabbinical-certified) preservation. The constraint is CLAIMED as rope
 *   (genuine coordination around a shared definition of linguistic
 *   legitimacy) while the authored metrics describe moderately extractive,
 *   actively enforced operation — the engine measures this divergence.
 *
 * KEY AGENTS:
 *   - rabbinical_authority: institutional beneficiary; controls the definition and its enforcement
 *   - orthodox_jewish_communities: moderate beneficiary; gain validation without need for secular adoption
 *   - secular_hebrew_speakers: powerful payer; delegitimized as auxiliary to the 'true' language preservation
 *   - literary_modernizers: powerful payer; their creative innovation is subordinated to rabbinical preservation norms
 *   - non_orthodox_movements: organized payer; caught between benefiting from liturgical validation and paying through loss of hermeneutic authority
 *   - linguistic_analysts: analytical observer; can measure empirically distinct phenomena (liturgical continuity vs. native generativity)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.38).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.62).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Living Language Status via Liturgical Preservation (Rabbinical Reading)").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism").

domain_priors:requires_active_enforcement(living_language_status__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, 'ade241fb-4163-4e75-8c22-a8f4cbbfcfa6').
narrative_ontology:cs_kernel_codification('ade241fb-4163-4e75-8c22-a8f4cbbfcfa6', fixed_text).
narrative_ontology:cs_authority_grounding('ade241fb-4163-4e75-8c22-a8f4cbbfcfa6', lineage).
narrative_ontology:cs_interpretation_layer_present('ade241fb-4163-4e75-8c22-a8f4cbbfcfa6').
narrative_ontology:cs_reading_relation('ade241fb-4163-4e75-8c22-a8f4cbbfcfa6', living_language_status__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('ade241fb-4163-4e75-8c22-a8f4cbbfcfa6', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('ade241fb-4163-4e75-8c22-a8f4cbbfcfa6', foundational, sacred_text_transmission_sufficient_for_vitality).
narrative_ontology:cs_axiom_status(sacred_text_transmission_sufficient_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('ade241fb-4163-4e75-8c22-a8f4cbbfcfa6', sacred_text_transmission_sufficient_for_vitality, deontological).
narrative_ontology:cs_axiom('ade241fb-4163-4e75-8c22-a8f4cbbfcfa6', foundational, rabbinical_hermeneutic_monopoly_preserves_authenticity).
narrative_ontology:cs_axiom_status(rabbinical_hermeneutic_monopoly_preserves_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('ade241fb-4163-4e75-8c22-a8f4cbbfcfa6', rabbinical_hermeneutic_monopoly_preserves_authenticity, conventional).
narrative_ontology:cs_axiom('ade241fb-4163-4e75-8c22-a8f4cbbfcfa6', secondary, secular_speech_innovation_is_linguistic_desecration).
narrative_ontology:cs_axiom_status(secular_speech_innovation_is_linguistic_desecration, holdable).
narrative_ontology:cs_axiom_grounding('ade241fb-4163-4e75-8c22-a8f4cbbfcfa6', secular_speech_innovation_is_linguistic_desecration, deontological).
narrative_ontology:cs_reference_frame('ade241fb-4163-4e75-8c22-a8f4cbbfcfa6', medieval_diaspora_hebrew_preservation).
narrative_ontology:cs_drift_state('ade241fb-4163-4e75-8c22-a8f4cbbfcfa6', modern_secular_hebrew_vitality_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ade241fb-4163-4e75-8c22-a8f4cbbfcfa6', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, orthodox_jewish_communities).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_hebrew_speakers).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, non_orthodox_jewish_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, non_orthodox_jewish_movements).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, literary_modernizers).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, sacred_text_sanctity).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, interpretive_monopoly_doctrine).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, liturgical_continuity_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and enforces the definition of linguistic vitality through the lens of sacred-text transmission. Controls interpretive authority over canonical texts and their recitation norms. Certifies what counts as authentic Hebrew use and linguistic legitimacy. Their institutional power rests on maintaining the boundary between sacred (their domain) and secular (delegitimized) speech.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, rabbinical_authority, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Gain validation that their liturgical practice IS the preservation of linguistic vitality. Do not need to adopt secular modern usage or literary innovation to claim the language is alive. Their daily recitation of texts, study practices, and ritual use are sufficient. The constraint certifies their linguistic authenticity without requiring generational native transmission or literary modernization.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, orthodox_jewish_communities, beneficiary,
    moderate, civilizational, identity_locked, global).

% Their living, generative use of Hebrew in daily life, journalism, literature, and secular education is rendered structurally secondary to the rabbinical measure. They must either accept that their vitality is contingent on rabbinical validation (delegitimizing their autonomous linguistic community) or contest the definition itself—but the authority structure treats such contestation as linguistic rebellion. Their exit would require abandoning Hebrew or accepting permanent delegitimation.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_hebrew_speakers, payer,
    powerful, biographical, constrained, regional).

% Conservative and Reform movements face a structural bind: they practice sacred-text transmission and liturgical use but do not accept rabbinical exclusivity of interpretive authority. They benefit from the constraint's legitimation of liturgical vitality but pay through the loss of authority to innovate liturgically or reinterpret sacred texts—innovation outside rabbinical parameters is cast as linguistic desecration.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, non_orthodox_jewish_movements, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, non_orthodox_jewish_movements, beneficiary).

% Writers, journalists, and linguistic innovators who use Hebrew as a living creative medium for new expression. The constraint subordinates their work to the category of 'secular innovation' and treats modern literary vitality as secondary to liturgical preservation. They bear the structural delegitimation of their practice and can exit by abandoning Hebrew publication or by moving to venues (literary magazines, publishing houses) that claim independent authority outside the rabbinical frame.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, literary_modernizers, payer,
    powerful, biographical, mobile, regional).

% Academic and cultural learners of Hebrew have no seat in the rabbinical definition. Their study and use cannot contribute to claims of linguistic vitality under this reading, because vitality is defined as continuation of Jewish sacred transmission, not linguistic generativity. They are structurally silent in the contest.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, non_jewish_hebrew_learners, excluded,
    powerless, biographical, trapped, global).

% Sociolinguists and comparative language scholars can measure generation rates, lexical innovation, morphological productivity, and transmission channels. They can observe that liturgical recitation and living generational use are empirically distinct phenomena and that the rabbinical reading conflates preservation with vitality. They lack enforcement power but can challenge the definition through empirical analysis.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, linguistic_analysts, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared metric of linguistic legitimacy centered on sacred-text transmission: Jewish communities can coordinate around the understanding that continuous recitation and study of canonical texts suffice to preserve the language's identity and ensure its vitality, without requiring secular adoption or literary innovation.
% TRANSFER_FUNCTION: Transfers interpretive authority and linguistic prestige from decentralized Hebrew speakers (secular, literary, generational, quotidian users) to the rabbinical institutional authority that controls the definition of what makes the language 'alive.' Moves recognition and legitimacy from modern speakers to those who practice liturgical transmission according to rabbinical norms. Also transfers the burden of defending linguistic authenticity: secular speakers must either accept their use is inauthentic or engage in costly contestation of the rabbinical frame.
% ABSENT_VOICES: Secular Hebrew speakers and literary modernizers are structurally present but delegitimized—their voices are heard but treated as inauthentic or corrupting. Competing-reading advocates (native-generation speakers, literary-vitality proponents) are treated as heretics to the rabbinical frame and are kept out of the authority structure that certifies linguistic legitimacy. Non-Jewish learners of Hebrew are absent entirely; their use cannot contribute to vitality under this reading because vitality is defined as continuation of Jewish sacred transmission.
% DISAPPEARANCE_RATIONALE: If the rabbinical definition of living-language-status-via-liturgical-preservation vanished, secular Hebrew communities would immediately claim linguistic vitality for their native transmission and literary innovation without requiring rabbinical validation. Literary modernization would be framed as enhancement rather than desecration. Competing readings (native-generation vitality, literary-continuity vitality) would surface as live alternatives and would reorganize the institutional landscape around empirical measures of linguistic productivity rather than sacred-text transmission alone. The rabbinical authority's claim to certify linguistic authenticity would lose institutional force.
% FOUNDING_PROBLEM: In the medieval diaspora, Hebrew ceased to be natively transmitted as a first language across most Jewish communities (though it persisted in liturgical and scholarly contexts). The rabbinical reading answers: how can the language remain alive without native transmission? Through continuous recitation and study of the canonical sacred texts, transmitted through rabbinical authority and halakhic obligation, Hebrew's linguistic and cultural continuity can be preserved across centuries of exile, drift, and institutional fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox rabbinical authorities attest the founding problem was historically live and remains important for spiritual continuity (though they may not acknowledge it as 'dead'). Secular Hebrew speakers and linguists attest the founding problem was solved by native-speaker revival beginning in the Haskalah period (eighteenth century onward) and that Hebrew is now demonstrably alive through generative use, literary production, and native acquisition. Historical scholarship on Hebrew's revival and contemporary sociolinguistic data on Hebrew's empirical vitality (native speakers in Israel, continuous neologism, expanding domains of use) come from outside the benefiting parties and corroborate that the founding problem is substantially solved—the constraint persists not because the problem is live but because it serves institutional interests.
narrative_ontology:disappearance_verdict(living_language_status__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__liturgical_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__liturgical_preservation_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__liturgical_preservation_reading_tests).
:- end_tests(living_language_status__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the constraint extracts interpretive authority and prestige from decentralized speech communities to a centralized institutional arbiter. It is not a pure snare because liturgical coordination is real and non-trivial; the rabbinical authority did genuinely preserve Hebrew across diaspora. However, extractiveness accumulates over the interval (0.28→0.38) as secular Hebrew's empirical vitality becomes undeniable, creating pressure to defend the rabbinical definition more explicitly—this drives rising theater_ratio (0.25→0.41) as the constraint increasingly performs definitional enforcement rather than resting on natural coordination. Suppression is substantial (0.62 at interval end) because the constraint requires active enforcement: secular use must be delegitimized, literary innovation must be subordinated, and competing definitions must be suppressed from public authority. Accessibility_collapse is moderate (0.48) because alternatives are not physically closed off—secular speakers CAN use Hebrew and DO so every day—but they operate under delegitimation and institutional pressure. Resistance is high (0.72) because secular Hebrew speakers and literary modernizers actively resist the definition through their generative use and published work; this is not passive acceptance but active contestation. The measurement series tracks extractiveness rising and theater-ratio rising as the constraint adjusts to defend itself against empirical challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinical-authority seat, this is genuine coordination: they have solved the problem of linguistic continuity across diaspora and centuries of dispersion through institutional transmission of sacred texts. Their role is curator and interpreter of a shared resource. From the secular-speaker seat, this is enforced extraction: their autonomous linguistic vitality is rendered structurally secondary, they are told their use is inauthentic without rabbinical permission, and they bear the cost of constant delegitimation. From the literary-modernizer seat, the constraint is a form of institutional capture: their creative work is stolen (its vitality is attributed to rabbi-mediated preservation rather than to their own generativity), and they are forced to either accept permanent illegitimacy or exit the language entirely. The engine computes these divergences from the structural data—different power atoms, different exit costs, different roles—without reconciling them.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for rabbinical_authority is near beneficiary (d ≈ 0.18) because they control the definition, enforce its boundaries, and collect institutional prestige and authority without bearing the costs of delegitimation. Directionality for secular_hebrew_speakers is high-target (d ≈ 0.78) despite their formal power, because their exit from Hebrew requires abandoning linguistic and cultural identity—identity_lock amplifies their extraction exposure. Literary_modernizers have high formal exit (can publish outside the rabbinical frame) but their power is concentrated in cultural/literary production rather than institutional authority; their directionality is moderate-target (d ≈ 0.65). Orthodox_jewish_communities benefit from validation but pay through suppression of hermeneutic innovation; they land near d ≈ 0.48 (near-symmetric). Non-orthodox movements are caught in a bind: they gain some legitimacy but lose authority; their directionality is slightly toward target (d ≈ 0.55).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint presents a classic mandatrophy pattern: the founding problem (how to preserve Hebrew across diaspora without native transmission) was LIVE and the liturgical-preservation solution was FUNCTIONAL in the medieval and early-modern periods when native transmission had ceased. However, from the eighteenth century onward (Haskalah period), the problem was SOLVED by native-speaker revival and literary modernization—secular Hebrew became alive through generative use and literary production. The constraint persists not because the founding problem is live (it is not; Hebrew is empirically alive through multiple channels) but because the rabbinical authority benefits from maintaining its interpretive monopoly and from the institutional structure that rests on the claim that ONLY rabbinical-mediated transmission counts as true preservation. The constraint's persistence depends on suppressing the empirical evidence of native vitality, which is why theater_ratio rises (increasing proportion of enforcement goes to definitional defense rather than functional coordination) and suppression rises (more effort required to delegitimize competing vitality measures). The divergence between founding_problem_status=contested and disappearance_verdict=world_rearranges signals mandatrophy: the arrangement persists not because it is needed to solve a live problem but because powerful seats benefit from its continuation and can enforce continued suppression of competing definitions. This is the seat the engine reads to detect whether a rope has become a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_foreclosure,
    'Does the rabbinical claim that ''living language = liturgical preservation'' logically foreclose the native-generation reading''s claim that ''living language = native transmission''? Or do they coexist as competing empirical definitions of the same linguistic phenomenon?',
    'Examine whether a language can satisfy both conditions (continuous liturgical transmission AND native generational transmission) simultaneously in the same community. If yes, the readings coexist; if the rabbinical claim is that liturgical transmission ALONE suffices without native transmission, then foreclosure is claimed but depends on empirical separation of the conditions.',
    'If coexists: the readings are different measurements of the same phenomenon and can both be true (language is alive via both channels). If forecloses: the rabbinical reading asserts that native transmission is NOT necessary, which directly contradicts the native-generation reading''s core premise; foreclosure would be justified only if no language can be alive through native transmission alone when liturgical transmission occurs (unlikely empirically).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether this reading''s definition of liturgical sufficiency forecloses or coexists with native-generation vitality.').

omega_variable(
    suppression_mechanism_rabbinical_vs_structural,
    'Is the measured suppression (0.62) imposed by rabbinical institutional enforcement (external coercion), or by the internalization of the reading itself such that secular speakers come to believe their use is inauthentic or inferior?',
    'Post-challenge empirical test: if secular Hebrew speakers in communities that reject rabbinical authority show no reduction in confidence or innovation in their speech (despite continued rabbinical denunciation), suppression is primarily structural-external; if they show internalized uncertainty about their legitimacy, suppression has been internalized.',
    'If primarily structural: the constraint requires continuous enforcement investment; if internalized: the suppression persists even after enforcement weakens, making the constraint harder to dislodge. Internalized suppression would increase effective extraction and warrant upward revision of the suppression metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_rabbinical_vs_structural, empirical, 'Whether suppression is externally enforced institutional control or internalized delegitimation.').

omega_variable(
    literary_continuity_coexistence,
    'Can the rabbinical reading (liturgical preservation suffices) and the literary-continuity reading (modern literature demonstrates vitality) both be true of the same language at the same time, or does the rabbinical reading''s claim of sufficiency require that literary innovation NOT be a measure of vitality?',
    'Examine the logical structure: if the rabbinical reading says ''liturgical preservation alone is sufficient,'' it is making a claim about necessity (literary innovation is not necessary). This directly contradicts the literary-continuity reading''s assessment that modern literature is evidence of vitality. Whether they coexist depends on whether ''sufficient'' can mean ''both sufficient AND necessary for validation'' or merely ''sufficient to preserve the language''s existence.''',
    'If sufficient means ''alone necessary'': the readings foreclose each other (the rabbinical claim denies literary vitality''s legitimacy). If sufficient means ''one valid preservation path among others'': they coexist and can both be empirically true. The committer framing determines this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literary_continuity_coexistence, conceptual, 'Whether the rabbinical sufficiency claim entails foreclosure of literary-vitality measures.').

omega_variable(
    boundary_between_preservation_and_vitality,
    'Is preservation (keeping something unchanged) structurally compatible with vitality (adaptive capacity and generative power)? The rabbinical reading treats them as synonymous; empirical sociolinguistics treats them as distinct phenomena.',
    'Test whether communities practicing liturgical preservation without secular adoption show the same metrics of linguistic vitality (neologism generation, domain expansion, native acquisition) as communities with native transmission and literary innovation. A language can be preserved unchangingly and still be dead (Latin in Catholic liturgy); a language can be vital without being preserved (English has lost inflections; it is more vital, not less).',
    'If they are distinct: the constraint is conflating two different linguistic phenomena under one label. If they can be synonymous: preservation through fixed liturgical transmission is a form of vitality. This determines whether the reading is descriptively accurate or definitionally stipulative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_between_preservation_and_vitality, empirical, 'Whether linguistic preservation and vitality are empirically equivalent or conceptually distinct.').

omega_variable(
    kernelness_of_living_language_definition,
    'Is ''what makes a language living'' a kernel commitment of Jewish textual authority, or is it a derivative claim that serves institutional interests? Does the rabbinical authority genuinely stake its legitimacy on this definition, or is it a contingent assertion of control?',
    'Examine whether the rabbinical authority would cede the definition if pressured by other institutional losses (e.g., if accepting native-generation vitality preserved more of their hermeneutic authority). If the definition is truly foundational, it would be defended even at institutional cost; if it is instrumental, it would be abandoned if better served institutional interests.',
    'If foundational: this is a genuine commitment-system reading and the axiom_overriding path applies. If instrumental: the constraint is better classified as extractive (snare-like) with a cover story, not as rope-like coordination. This affects type classification and mandatrophy detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernelness_of_living_language_definition, preference, 'Whether the definition is a genuine authority commitment or an instrumental assertion of control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__liturgical_preservation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(livi_tr_t0, projected).
narrative_ontology:measurement(livi_tr_t8, living_language_status__liturgical_preservation_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(livi_tr_t8, observed).
narrative_ontology:measurement(livi_tr_t16, living_language_status__liturgical_preservation_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement_basis(livi_tr_t16, observed).
narrative_ontology:measurement(livi_tr_t24, living_language_status__liturgical_preservation_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement_basis(livi_tr_t24, observed).
narrative_ontology:measurement(livi_tr_t32, living_language_status__liturgical_preservation_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(livi_tr_t32, observed).
narrative_ontology:measurement(livi_tr_t40, living_language_status__liturgical_preservation_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(livi_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__liturgical_preservation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(livi_be_t0, projected).
narrative_ontology:measurement(livi_be_t8, living_language_status__liturgical_preservation_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement_basis(livi_be_t8, observed).
narrative_ontology:measurement(livi_be_t16, living_language_status__liturgical_preservation_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement_basis(livi_be_t16, observed).
narrative_ontology:measurement(livi_be_t24, living_language_status__liturgical_preservation_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement_basis(livi_be_t24, observed).
narrative_ontology:measurement(livi_be_t32, living_language_status__liturgical_preservation_reading, base_extractiveness, 32, 0.38).
narrative_ontology:measurement_basis(livi_be_t32, observed).
narrative_ontology:measurement(livi_be_t40, living_language_status__liturgical_preservation_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(livi_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__liturgical_preservation_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(livi_su_t0, projected).
narrative_ontology:measurement(livi_su_t8, living_language_status__liturgical_preservation_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement_basis(livi_su_t8, observed).
narrative_ontology:measurement(livi_su_t16, living_language_status__liturgical_preservation_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement_basis(livi_su_t16, observed).
narrative_ontology:measurement(livi_su_t24, living_language_status__liturgical_preservation_reading, suppression_requirement, 24, 0.61).
narrative_ontology:measurement_basis(livi_su_t24, observed).
narrative_ontology:measurement(livi_su_t32, living_language_status__liturgical_preservation_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement_basis(livi_su_t32, observed).
narrative_ontology:measurement(livi_su_t40, living_language_status__liturgical_preservation_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(livi_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__liturgical_preservation_reading, 0.12).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% The 'living_language_status' kernel decomposes into three structurally distinct constraints, each corresponding to a different reading of what makes a language alive. Each reading has distinct beneficiaries, distinct victims, and distinct ε values. The three stories form a constraint family linked by network.affects_constraints: rabbinical-authority benefits from the liturgical reading and has institutional power to suppress competing readings; secular Hebrew speakers benefit from the native-generation and literary-continuity readings and are victimized by the liturgical reading. The readings coexist as live competing commitments across different institutional seats (rabbinical vs. secular, orthodox vs. non-orthodox, traditional vs. modern), not as alternative observations of a single linguistic state.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
