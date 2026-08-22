% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__liturgical_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__liturgical_preservation, []).

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
 *   constraint_id: hebrew_continuity__liturgical_preservation
 *   human_readable: Hebrew Liturgical Continuity Through Textual Preservation
 *   domain: sociolinguistic/cultural/commitment_system
 *
 * SUMMARY:
 *   Hebrew continuity through liturgical preservation is a constraint that
 *   preserves a language across diaspora and centuries by freezing it in
 *   sacred texts and ritualized recitation, decoupled from native speaker
 *   speech. The reading instantiated here treats liturgical preservation as
 *   the operative mechanism: Hebrew lives because communities recite it in
 *   prayer and interpret it in study, not because anyone speaks it natively
 *   as a first language (native speaker generativity is a separate sibling
 *   reading). The constraint benefits religious institutional authority (who
 *   controls the textual canon) and the textual transmission community (whose
 *   expertise depends on preserved textuality). It extracts from secular
 *   speakers (who face pressure to code-switch and conform to canonical
 *   norms) and from linguistic innovation communities (whose natural language
 *   changes are suppressed in sacred domains). The measured extractiveness
 *   has risen over the interval as secularization increased and the
 *   functional necessity of the constraint declined, leaving institutional
 *   extraction more visible.
 *
 * KEY AGENTS:
 *   - religious_institutional_authority: Rabbinical councils, liturgical authorities — sets and enforces the preserved canon (institutional power, arbitrage exit)
 *   - textual_transmission_community: Scholars, cantors, prayer leaders — benefits from textual fidelity, organized power, mobile exit
 *   - secular_jewish_speakers: Modern Hebrew speakers in Israel and diaspora — faces pressure to conform to canonical norms (moderate power, identity-locked exit)
 *   - linguistic_innovation_pressure: Natural language change, slang, neologism — suppressed in sacred domains by the constraint (powerful structural force, constrained by the constraint itself)
 *   - competing_cultural_narratives: Secular Zionism, atheistic Jewish identity, assimilationist frameworks — excluded from authority over the sacred domain (moderate power, trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.68).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.71).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.68).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Hebrew Liturgical Continuity Through Textual Preservation").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistic/cultural/commitment_system").

domain_priors:requires_active_enforcement(hebrew_continuity__liturgical_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, '31e2a8a4-a59f-4b9a-96f1-a09e20e9bdf0').
narrative_ontology:cs_kernel_codification('31e2a8a4-a59f-4b9a-96f1-a09e20e9bdf0', fixed_text).
narrative_ontology:cs_authority_grounding('31e2a8a4-a59f-4b9a-96f1-a09e20e9bdf0', lineage).
narrative_ontology:cs_interpretation_layer_present('31e2a8a4-a59f-4b9a-96f1-a09e20e9bdf0').
narrative_ontology:cs_reading_relation('31e2a8a4-a59f-4b9a-96f1-a09e20e9bdf0', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_reading_relation('31e2a8a4-a59f-4b9a-96f1-a09e20e9bdf0', hebrew_continuity__bridge_pidginized, influences).
narrative_ontology:cs_axiom('31e2a8a4-a59f-4b9a-96f1-a09e20e9bdf0', foundational, language_persists_through_textual_fixity).
narrative_ontology:cs_axiom_status(language_persists_through_textual_fixity, holdable).
narrative_ontology:cs_axiom_grounding('31e2a8a4-a59f-4b9a-96f1-a09e20e9bdf0', language_persists_through_textual_fixity, instrumental).
narrative_ontology:cs_axiom('31e2a8a4-a59f-4b9a-96f1-a09e20e9bdf0', secondary, clerical_authority_preserves_sacred_language).
narrative_ontology:cs_axiom_status(clerical_authority_preserves_sacred_language, holdable).
narrative_ontology:cs_axiom_grounding('31e2a8a4-a59f-4b9a-96f1-a09e20e9bdf0', clerical_authority_preserves_sacred_language, conventional).
narrative_ontology:cs_reference_frame('31e2a8a4-a59f-4b9a-96f1-a09e20e9bdf0', diaspora_liturgical_unity).
narrative_ontology:cs_drift_state('31e2a8a4-a59f-4b9a-96f1-a09e20e9bdf0', modern_israeli_native_speaker_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('31e2a8a4-a59f-4b9a-96f1-a09e20e9bdf0', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, religious_institutional_authority).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, textual_transmission_community).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, secular_jewish_speakers).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, linguistic_innovation_pressure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, practicing_observant_jews).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, practicing_observant_jews).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, linguistic_innovation_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinical councils, liturgical authorities, and textual establishment bodies define the authoritative Hebrew canon: which texts are sacred, how they are pronounced, which innovations are permitted. They enforce textual fidelity through religious sanction and institutional control of transmission (yeshiva curricula, prayer-book production, liturgical standardization). The authority collects symbolic capital and institutional preservation by maintaining the constraint.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, religious_institutional_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Scholars, cantors, prayer leaders, textual interpreters, and religious educators whose professional identity and expertise are constituted by mastery of preserved Hebrew texts. They benefit from the constraint because it secures their authority as keepers of tradition and their role as interpreters of sacred language. Textual fidelity creates demand for their scholarship and interpretive labor.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, textual_transmission_community, beneficiary,
    organized, generational, mobile, global).

% Participate in liturgy and textual study; they benefit from the constraint because it unites them across diaspora (one Hebrew text, one prayer-book language) and anchors their religious identity to an invariant past. They also pay through the labor of memorization, the cognitive load of liturgical Hebrew distinct from vernacular speech, and the suppression of innovations they might introduce.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, practicing_observant_jews, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, practicing_observant_jews, payer).

% Speak Hebrew as a daily language (particularly in Israel) but occupy a structural tension: the liturgical constraint treats secular speech as outside the preserved domain, placing secular speakers in a subordinate relationship to religious textual authority. They pay through continuous pressure to conform their speech to canonical norms for public/formal contexts, through the impossibility of innovating the language in sacred domains, and through the identity-fusion that makes exiting the Hebrew domain feel like exiting Jewishness itself.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, secular_jewish_speakers, payer,
    moderate, biographical, identity_locked, global).

% Speakers who naturally generate linguistic innovations — new vocabulary, phonetic shifts, grammatical changes — to meet contemporary communicative needs. The liturgical preservation constraint suppresses these innovations in the sacred domain (where they are ritually impermissible) and creates cognitive load where sacred and secular domains must be kept distinct. Linguistically, they pay through the constraint's suppression of natural language evolution.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, linguistic_innovation_communities, payer,
    powerful, immediate, constrained, global).

% Secular Zionism, atheistic Jewish identity, diaspora assimilationist movements, and other frameworks that contest whether Hebrew *should* be tied to religious textual preservation or instead allowed to evolve as a fully secular, generative language. These voices are structurally excluded from the authority structure that sets the liturgical canon; they would argue for full linguistic autonomy but cannot override religious institutional authority over the sacred domain.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, competing_cultural_narratives, excluded,
    moderate, generational, trapped, global).

% Academic linguists document the constraint's effects: how liturgical preservation produces diglossia (two registers), code-switching behavior, phonetic maintenance in formal contexts but drift in informal speech, and the cognitive profiles of bilingual Hebrew speakers navigating the preserved/vernacular boundary.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, linguistics_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__liturgical_preservation, religious_institutional_authority).
narrative_ontology:fixing_cost_class(hebrew_continuity__liturgical_preservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Binds diaspora Jewish communities across centuries and geographies through a single, invariant textual language: one prayer-book, one liturgical register, one way to encounter the sacred texts regardless of whether you speak Yiddish, Arabic, Spanish, Russian, or modern Hebrew natively. Preserves continuity with biblical and rabbinic traditions. Solves the coordination problem of diaspora religious identity: how do Jews in Cairo, Baghdad, Amsterdam, and Vilna pray together?
% TRANSFER_FUNCTION: Transfers interpretive authority from individual speakers to religious institutional bodies; transfers linguistic innovation capacity from living communities to textual scholars; moves the cognitive labor of code-switching and memorization onto speakers. Secular speakers transfer deference (in formal/public contexts) to religious authority definitions of proper speech.
% ABSENT_VOICES: Secular Zionists who would argue Hebrew should be a fully generative, evolving language decoupled from religious authority; linguistic communities whose natural innovations are suppressed (slang generators, technical innovators, speakers adapting Hebrew to contemporary needs); women whose exclusion from certain textual roles structures their access to the authority domain.
% DISAPPEARANCE_RATIONALE: If the liturgical preservation constraint vanished overnight, Hebrew would immediately fragmentize into multiple registers with no shared canonical standard — Modern Hebrew (Israeli) would drift faster, diaspora communities would diverge, and the unified textual bridge would splinter into competing local Hebrews. The religious institutional authority would lose its structural role as keeper of the invariant form. Jewish identity would become negotiable across communities rather than linguistically unified.
% FOUNDING_PROBLEM: After the Babylonian exile and the loss of Hebrew as a native language, communities needed a way to preserve access to sacred texts across diaspora and generations without native speakers to maintain the language naturally. Textual preservation through fixed recitation and scribal transmission solved this: the language lives in ritual, not in daily speech.
% FOUNDING_PROBLEM_CORROBORATION: Religious authorities attest the founding problem is perpetually live: the diaspora remains scattered, threats to textual transmission persist, and liturgical preservation is the mechanism that prevents Hebrew's complete loss. Secular scholars and modern Israeli linguists attest the founding problem is substantially solved: Hebrew is now a native language in Israel (since mid-20th century), textual preservation is redundant with living-language transmission, and the constraint now operates primarily as institutional extraction (preserving clerical authority) rather than functional necessity. The Israeli linguistic data supports the shifted-function reading: Hebrew has evolved rapidly in modern Israeli despite the liturgical preservation constraint, which suggests the constraint's function is institutional, not linguistically necessary.
narrative_ontology:disappearance_verdict(hebrew_continuity__liturgical_preservation, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__liturgical_preservation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__liturgical_preservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__liturgical_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__liturgical_preservation, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__liturgical_preservation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__liturgical_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measured at 0.68 at interval end because the constraint's function has shifted: originally it was the ONLY mechanism preserving Hebrew across diaspora (functional necessity, low extraction). Once modern Hebrew became a native language in Israel (mid-20th century), the constraint's functional role diminished while its institutional-authority role persisted unchanged. The rising extractiveness trajectory (0.42 → 0.68) models this shift: at T=0 (diaspora-era baseline) the constraint was mostly functional coordination; by T=30 (contemporary era) its primary effect is institutional preservation and clerical authority, with lingering coordination benefits. Theater ratio rising from 0.25 to 0.42 models the increasing proportion of enforcement activity dedicated to defending textual purity for its own sake (theatrical maintenance) rather than linguistic necessity. Suppression plateaus at 0.71 because the mechanisms of suppression (religious sanction, institutional control, code-switching pressure) are stable; what changes is whether they serve functional necessity or institutional extraction. The secular_jewish_speakers stakeholder carries identity_locked exit because exiting Hebrew entirely would mean exiting Jewish identity itself, even for secular Jews — the language and ethnicity are fusion-locked despite the secular/religious divide.
 *
 * PERSPECTIVAL GAP:
 *   Religious institutional authority and textual transmission communities perceive the constraint as necessary coordination (preserving diaspora unity, maintaining textual authenticity). Secular speakers perceive it as institutional extraction with suppressive enforcement. The engine computes this divergence from power/exit: the agenda-setter (institutional authority) has arbitrage exit (can switch religious frameworks or redefine canon) so derives lower directionality; the secular_jewish_speakers have identity_locked exit (cannot exit without exiting Jewishness) so derive higher directionality toward full target. Same constraint, opposite seats, opposite extraction readings. The claim is tangled_rope (real coordination + asymmetric extraction); the metrics support this because beneficiaries exist (coordination function) alongside victims (extraction enforcement).
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutional authority: agenda_setter role, institutional power, arbitrage exit → low directionality (near 0.1-0.2), full beneficiary position. They set the constraint and can redefine it; they collect institutional authority and interpretive control. Textual transmission community: beneficiary role, organized power, mobile exit → low directionality (near 0.15-0.25), beneficiary position. They depend on the constraint but can theoretically move to secular Hebrew scholarship or other languages; the constraint benefits their professional identity without trapping them. Secular Jewish speakers: payer role, moderate power, identity_locked exit → high directionality (near 0.75-0.85), near full target. They face the constraint's suppressive enforcement (code-switching pressure, canonical conformity demands) and cannot exit without exiting their own identity. Linguistic innovation: payer role (innovations are suppressed), powerful structural force but constrained by the constraint → override d to 0.80 (full target) because the constraint's entire enforcement mechanism is directed at suppressing this agent's natural operation. Competing narratives: excluded role, moderate power, trapped → observer position, directionality irrelevant (not in the active game).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding mandate was diaspora preservation: keep Hebrew alive across exile when no unified political territory or native-speaker base exists. This mandate is DEAD in the contemporary period — Israel has 6+ million native Hebrew speakers, modern Hebrew is fully generative and evolving, and textual preservation is redundant with living-language transmission. Yet the constraint persists with full institutional enforcement. This is the classic mandatrophy signature: the functional problem is solved, the constraint remains because institutional interests (clerical authority, textual establishment, identity-preservation narratives) benefit from its continuation. The measurement trajectory (extractiveness rising as functional necessity falls) is the temporal signature of mandatrophy — as the original mandate dies, the extractive residue becomes more visible. Authorizing the tangled_rope type (not snare) acknowledges that real coordination benefits still exist (diaspora unity through a shared textual language), but the classification prevents misreading this as pure coordination when the mandate has died and institutional extraction is the primary persistence mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_necessity_vs_institutional_extraction,
    'Is the liturgical preservation constraint''s current operation a continuation of its original diaspora-preservation function, or has it become primarily a mechanism for maintaining clerical institutional authority over the Hebrew language?',
    'Compare pre/post-Israeli-statehood enforcement patterns: if suppression intensity remains constant while the functional need (diaspora coherence without native speakers) has been replaced by native-speaker coherence, the shift is toward institutional extraction. Examine whether religious authorities resist or accommodate modern Hebrew innovations — integration signals functional coordination, resistance signals institutional extraction.',
    'If primarily institutional extraction, the classification should possibly shift to snare (with only residual coordination) or confirm tangled_rope with high theater_ratio. If still functional coordination, the rising extractiveness is a measurement error and the constraint should reclassify toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_necessity_vs_institutional_extraction, empirical, 'Whether the constraint serves surviving functional need or primarily maintains institutional authority.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) structural (external enforcement by religious authorities, institutional sanctions, formal rules) or internalized (secular speakers have absorbed the canon as part of their identity, experiencing it as self-imposed restraint)?',
    'Post-exit trajectory: if secular speakers who leave religious observance continue code-switching or deferring to canonical norms, suppression is partially internalized. If they immediately shift to unconstrained modern Hebrew, suppression was primarily structural.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure — the target carries the suppression with them after institutional enforcement is removed. This affects both the directionality computation and the piton/theater diagnosis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is external institutional enforcement or internalized as identity-constituting restraint.').

omega_variable(
    identity_lock_vs_cultural_choice,
    'Is the identity_locked exit_option for secular_jewish_speakers a genuine fusion (exiting the constraint would mean exiting Jewish identity, which is perceived as impossible), or a framing choice (secular Jews could in principle divorce Hebrew language fidelity from Jewish identity, but currently perceive them as inseparable)?',
    'Compare secular Jewish communities that have adopted other languages as primary (e.g., Yiddish-dominant Orthodox communities, English-dominant American Jews, Arabic-dominant Mizrahi Jews): if their identity remains Jewish and robust despite reduced Hebrew literacy, identity-lock is a contemporary contingency, not a structural necessity. If Hebrew abandonment correlates with identity loss, identity-lock is more structurally binding.',
    'If contingent framing, directionality for secular speakers might be lower (constrained rather than identity_locked) and exit possibilities more available. If structurally binding, the current directionality and extraction calculations are accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_cultural_choice, conceptual, 'Whether Hebrew/Jewish identity fusion is structurally necessary or a contingent contemporary framing.').

omega_variable(
    reading_foreclosure_native_vs_liturgical,
    'Does the liturgical_preservation reading logically foreclose the native_generative reading (a language cannot be both a fixed liturgical text and a living native language simultaneously), or do they coexist as different mechanisms in play across different communities?',
    'Examine modern Israel: Hebrew is simultaneously a native language (native speaker intuition drives innovation) and subject to liturgical preservation constraints (formal registers preserve older forms, religious authority maintains textual canon). The two readings coexist — both mechanisms operate. Foreclosure would require that one reading''s core premise makes the other impossible within a single framework; here, they are simply weighted differently across speakers.',
    'If coexists_with (the reading relation declared in cs_structure), both native generativity and liturgical preservation operate simultaneously and compete for authority; the engine computes how each seat experiences the constraint differently depending on which mechanism dominates from their position. If foreclosed, only one reading is coherent and the other must be rejected.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_native_vs_liturgical, conceptual, 'Whether native speaker mechanism and liturgical preservation mechanism are mutually exclusive or coexisting.').

omega_variable(
    kernel_contest_neutrality,
    'Is this constraint story a neutral description of the liturgical_preservation reading, or does authoring extractiveness at 0.68 and theater_ratio at 0.42 implicitly endorse the secular/native-speech reading''s critique that liturgical preservation is institutional extraction?',
    'Author a parallel story from the religious institutional perspective where the same constraint is measured with lower extractiveness and theater_ratio (coordination emphasis over extraction). Compare whether the metrics diverge by seat or by reading-endorsement. If the divergence is purely metrics-per-reading (independent of seat perspective), the framework is neutral; if metrics are tuned to a reading''s critique, the story is value-laden.',
    'If readings can carry different metrics for the same constraint independently, the corpus remains neutral across kernel contests. If metrics are locked to reading-endorsement, committer stories are advocacy and should be labeled as such.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_neutrality, preference, 'Whether this constraint story''s metrics reflect neutral structural observation or implicit endorsement of a particular reading''s critique.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__liturgical_preservation, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t5, hebrew_continuity__liturgical_preservation, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(hebr_tr_t5, observed).
narrative_ontology:measurement(hebr_tr_t10, hebrew_continuity__liturgical_preservation, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(hebr_tr_t10, observed).
narrative_ontology:measurement(hebr_tr_t15, hebrew_continuity__liturgical_preservation, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(hebr_tr_t15, observed).
narrative_ontology:measurement(hebr_tr_t20, hebrew_continuity__liturgical_preservation, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(hebr_tr_t20, observed).
narrative_ontology:measurement(hebr_tr_t25, hebrew_continuity__liturgical_preservation, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(hebr_tr_t25, observed).
narrative_ontology:measurement(hebr_tr_t30, hebrew_continuity__liturgical_preservation, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(hebr_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__liturgical_preservation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t5, hebrew_continuity__liturgical_preservation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(hebr_be_t5, observed).
narrative_ontology:measurement(hebr_be_t10, hebrew_continuity__liturgical_preservation, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(hebr_be_t10, observed).
narrative_ontology:measurement(hebr_be_t15, hebrew_continuity__liturgical_preservation, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(hebr_be_t15, observed).
narrative_ontology:measurement(hebr_be_t20, hebrew_continuity__liturgical_preservation, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(hebr_be_t20, observed).
narrative_ontology:measurement(hebr_be_t25, hebrew_continuity__liturgical_preservation, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(hebr_be_t25, observed).
narrative_ontology:measurement(hebr_be_t30, hebrew_continuity__liturgical_preservation, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(hebr_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__liturgical_preservation, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(hebr_su_t0, observed).
narrative_ontology:measurement(hebr_su_t5, hebrew_continuity__liturgical_preservation, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(hebr_su_t5, observed).
narrative_ontology:measurement(hebr_su_t10, hebrew_continuity__liturgical_preservation, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(hebr_su_t10, observed).
narrative_ontology:measurement(hebr_su_t15, hebrew_continuity__liturgical_preservation, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(hebr_su_t15, observed).
narrative_ontology:measurement(hebr_su_t20, hebrew_continuity__liturgical_preservation, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(hebr_su_t20, observed).
narrative_ontology:measurement(hebr_su_t25, hebrew_continuity__liturgical_preservation, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(hebr_su_t25, observed).
narrative_ontology:measurement(hebr_su_t30, hebrew_continuity__liturgical_preservation, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(hebr_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__liturgical_preservation, 0.12).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way kernel contest over Hebrew continuity. The Hebrew_continuity kernel decomposes into three structurally distinct constraint stories: (1) liturgical_preservation: Hebrew lives through fixed texts and ritual recitation (this file); (2) native_generative: Hebrew lives through native speaker intuition and daily generative use in living speech communities; (3) bridge_pidginized: Hebrew lives as a contact language among diaspora communities, neither purely liturgical nor fully native. Each reading has different ε, different beneficiaries, different victim sets. The readings do not aggregate into one constraint — they compete. A single authority structure (religious institutional bodies) owns the liturgical domain, while secular Israeli society owns the native-speech domain, and diaspora communities operate the contact-lingua-franca domain. The ε values for each reading are independent: liturgical_preservation measures extraction from the religious-authority perspective (ε = 0.68 because mandatrophy is visible, the functional need is dead but institutional extraction persists); native_generative would measure ε differently (lower, because native speaker innovation is the primary mechanism and religious constraint is experienced as friction, not as the core structure); bridge_pidginized would measure ε differently again (measuring what contact-lingua-franca dynamics extract from full linguistic autonomy). The network links acknowledge that the three readings affect each other: mandatrophy in one affects the viability of another, secular dominance of native_generative reduces the functional force of liturgical_preservation, diaspora linguistic needs compete with both native and liturgical authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_continuity__liturgical_preservation, powerful, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
