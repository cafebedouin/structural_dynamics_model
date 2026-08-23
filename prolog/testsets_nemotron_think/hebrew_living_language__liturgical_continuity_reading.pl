% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__liturgical_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__liturgical_continuity_reading, []).

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
 *   constraint_id: hebrew_living_language__liturgical_continuity_reading
 *   human_readable: Hebrew Living Through Liturgical Continuity
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint story represents the liturgical_continuity_reading of the
 *   hebrew_living_language kernel. It asserts that Hebrew remained a living
 *   language through two millennia of diaspora because Jewish communities
 *   maintained unbroken liturgical recitation and textual study — prayer,
 *   Torah reading, Talmud study, legal responsa, poetry — in Hebrew. The
 *   reading claims this practice constitutes genuine language vitality
 *   (coordination function: shared textual medium across space and time) with
 *   negligible extraction (voluntary participation, no material transfer) and
 *   no suppression (exit is identity-costly but not coerced). The
 *   claimed_type is rope. The sibling readings (native_generation_reading,
 *   literary_revival_reading) instantiate different constraints with
 *   different ε and different stakeholder structures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__liturgical_continuity_reading, 0.15).
domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, 0.08).
domain_priors:theater_ratio(hebrew_living_language__liturgical_continuity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__liturgical_continuity_reading, "Hebrew Living Through Liturgical Continuity").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_continuity_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_continuity_reading, '6a4b31f0-cd1c-43dc-b0c5-407d223b24ac').
narrative_ontology:cs_kernel_codification('6a4b31f0-cd1c-43dc-b0c5-407d223b24ac', fixed_text).
narrative_ontology:cs_authority_grounding('6a4b31f0-cd1c-43dc-b0c5-407d223b24ac', lineage).
narrative_ontology:cs_interpretation_layer_present('6a4b31f0-cd1c-43dc-b0c5-407d223b24ac').
narrative_ontology:cs_reading_relation('6a4b31f0-cd1c-43dc-b0c5-407d223b24ac', hebrew_living_language__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('6a4b31f0-cd1c-43dc-b0c5-407d223b24ac', hebrew_living_language__literary_revival_reading, coexists_with).
narrative_ontology:cs_axiom('6a4b31f0-cd1c-43dc-b0c5-407d223b24ac', foundational, liturgical_recitation_preserves_living_status).
narrative_ontology:cs_axiom_status(liturgical_recitation_preserves_living_status, holdable).
narrative_ontology:cs_axiom_grounding('6a4b31f0-cd1c-43dc-b0c5-407d223b24ac', liturgical_recitation_preserves_living_status, conventional).
narrative_ontology:cs_axiom('6a4b31f0-cd1c-43dc-b0c5-407d223b24ac', foundational, textual_study_sustains_vitality).
narrative_ontology:cs_axiom_status(textual_study_sustains_vitality, holdable).
narrative_ontology:cs_axiom_grounding('6a4b31f0-cd1c-43dc-b0c5-407d223b24ac', textual_study_sustains_vitality, conventional).
narrative_ontology:cs_reference_frame('6a4b31f0-cd1c-43dc-b0c5-407d223b24ac', rabinic_transmission_framework).
narrative_ontology:cs_drift_state('6a4b31f0-cd1c-43dc-b0c5-407d223b24ac', modern_secular_revival_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6a4b31f0-cd1c-43dc-b0c5-407d223b24ac', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, jewish_liturgical_communities).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, hebrew_scholars).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, diaspora_congregations).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, hebrew_liturgical_continuity_preserves_living_status).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, textual_study_sustains_language_vitality).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, shared_liturgical_medium_enables_diaspora_cohesion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the liturgical calendar, prayer texts, and study cycles that constitute the living practice of Hebrew. They set the norms of pronunciation, cantillation, and textual interpretation. Their authority derives from chain of transmission. Exit means leaving the community of practice, which carries identity costs but no legal penalties.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, jewish_liturgical_communities, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__liturgical_continuity_reading, jewish_liturgical_communities, beneficiary).

% Study, edit, comment on, and teach the textual corpus (Bible, Mishnah, Talmud, codes, responsa, poetry). Their professional standing depends on mastery of the liturgical-textual tradition. They benefit from the language's vitality as a research medium. Exit is professionally costly but structurally open — they can shift to other languages or fields.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, hebrew_scholars, beneficiary,
    moderate, biographical, mobile, global).

% Participate in weekly and festival liturgy, lifecycle rituals, and communal study. They acquire Hebrew literacy through repetition and schooling. The shared liturgy lets a congregation in Buenos Aires and one in Melbourne pray identically. Exit means losing the primary vehicle for communal cohesion and textual access; identity costs are real but not coercive.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, diaspora_congregations, beneficiary,
    moderate, biographical, constrained, global).

% Built modern Hebrew as a native spoken language in Palestine/Israel (late 19th–20th century). They explicitly rejected liturgical Hebrew as fossilized and sought generative daily speech. Their reading (native_generation_reading) competes for the 'living language' designation. They are excluded from the liturgical continuity framework by ideological choice, not structural barrier.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, secular_hebrew_revivalists, excluded,
    institutional, generational, arbitrage, national).

% Analyze Hebrew's trajectory from biblical through rabbinic, medieval, Haskalah, and modern periods. They evaluate vitality criteria (native speakers, domains of use, intergenerational transmission) without participating in the liturgical practice. Their seat is analytical — they see the full structural field across readings.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, linguistic_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__liturgical_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_living_language__liturgical_continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves Hebrew as a shared liturgical and textual medium across geographically and temporally dispersed Jewish communities, enabling mutual intelligibility in prayer, study, and legal discourse without requiring native daily speech in a common territory.
% TRANSFER_FUNCTION: Moves the cognitive effort of textual mastery, recitation fluency, and interpretive competence from each generation to the next through communal schooling and ritual participation. No material resources are extracted; the transfer is effort and attention invested in the tradition, which returns communal cohesion and textual access.
% ABSENT_VOICES: Native-generation advocates (secular revivalists, modern Israeli linguists) who argue liturgical competence without generative daily speech is 'fossilized' or 'ritualized' rather than living. Also absent: communities that lost Hebrew literacy entirely and shifted to vernaculars (Judeo-Arabic, Yiddish, Ladino) — they would dispute that liturgical continuity alone preserved Hebrew for them.
% DISAPPEARANCE_RATIONALE: If liturgical recitation and textual study ceased overnight, the primary vehicle linking modern Hebrew speakers to the biblical and rabbinic corpus would fracture. Diaspora communities would lose the shared textual medium that enables cross-community prayer and study. The chain of transmission that sustained Hebrew literacy for two millennia would break, and Hebrew would survive only as an academic subject or modern national language severed from its textual past.
% FOUNDING_PROBLEM: How to maintain Hebrew as a living sacred language — capable of sustaining prayer, study, and legal discourse — after the loss of native speech communities and territorial sovereignty following the Roman exile (70 CE and 135 CE).
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic tradition (continuous chain of transmission documented in responsa literature). Historical linguistics scholarship outside the tradition: Geoffrey Khan (Cambridge) on Hebrew's diglossic continuity; Saul Lieberman (JTS) on textual transmission; Bernard Spolsky on language policy. Diaspora community practice: liturgical Hebrew remains the primary literacy vehicle for observant communities worldwide, corroborated by enrollment data in day schools and yeshivas across continents.
narrative_ontology:disappearance_verdict(hebrew_living_language__liturgical_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__liturgical_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__liturgical_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_living_language__liturgical_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__liturgical_continuity_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__liturgical_continuity_reading_tests).
:- end_tests(hebrew_living_language__liturgical_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint extracts no rents — participation is voluntary, the 'cost' is cognitive effort invested in learning texts, and the 'return' is communal cohesion and textual access. Suppression is near-zero (0.08) because no enforcement machinery compels participation; communities that abandoned Hebrew (shifted to Yiddish, Judeo-Arabic, Ladino, or European languages) faced no penalty from the liturgical system itself. Theater_ratio is moderate (0.25) because some liturgical performance is performative (rote recitation without comprehension), but the study component (chevruta, shiur) is genuinely generative of understanding. Accessibility_collapse (0.35) reflects that alternatives existed (vernaculars, other languages) but the liturgical medium remained uniquely authoritative for sacred purposes. Resistance (0.12) is low because the constraint meets no organized opposition — its critics (maskilim, Zionists) built rival systems rather than attacking this one.
 *
 * PERSPECTIVAL GAP:
 *   The native_generation_reading would compute this constraint as a snare (extracting effort to maintain a fossilized ritual language while denying the need for living speech). The literary_revival_reading would compute it as a piton (atrophied coordination maintained theatrically). This reading computes it as a rope. The engine's per-seat classification will show this divergence: from the liturgical community seat, it's coordination; from the secular revivalist seat (if modeled), it's extraction. The divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   All named stakeholders are beneficiaries (d ≈ 0.1–0.3). Jewish_liturgical_communities are agenda_setters who also benefit — they administer the tradition and gain cohesion. Hebrew_scholars and diaspora_congregations are beneficiaries who invest effort and receive textual access and communal belonging. No stakeholder is a payer or victim. Secular_hebrew_revivalists are excluded by ideological choice (they reject this reading's criterion), not by structural barrier. Linguistic_scholars are analytical observers. The engine will compute low χ for all seats because d is low and ε is low.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintain Hebrew as sacred language without native speakers) remains live for diaspora observant communities. The constraint has not outlived its function — it still solves the coordination problem it was built for. Mandatrophy is not resolved; the arrangement persists because the problem persists. The modern secular revival (native_generation_reading) created a parallel vitality track but did not obsolete the liturgical one for the communities that maintain it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the liturgical_continuity_reading of the contested kernel hebrew_living_language. What structural differences distinguish it from the native_generation_reading and literary_revival_reading?',
    'Compare ε, beneficiary/victim structures, and type classifications across the three constraint stories. The kernel contest is resolved by decomposition, not by adjudicating one reading as ''correct.''',
    'If the readings are not structurally distinct, the kernel decomposition fails and the colloquial label ''Hebrew is living'' remains ambiguous. If distinct, each reading gets its own classification and the engine can detect where extraction or suppression appears in one reading but not another.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to the kernel-reading decomposition: one kernel, three constraints, three ε values.').

omega_variable(
    liturgical_vs_native_boundary,
    'Is liturgical competence (recitation, textual study, legal discourse) sufficient for ''living language'' status, or does vitality require generative daily speech by native speakers?',
    'Sociolinguistic criteria: intergenerational transmission in home domains (native_generation) vs. transmission via communal institutions (liturgical). The engine classifies each reading on its own structural merits; the omega records the irreducibility of the criterion choice.',
    'If liturgical competence suffices, this reading''s claimed_type (rope) holds with low ε. If native speech is necessary, this reading''s continuity claim is a cover story and ε rises (the constraint extracts effort to maintain a ''living'' designation that doesn''t meet the stricter criterion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_vs_native_boundary, conceptual, 'The definitional boundary between ''living through liturgy'' and ''living through native speech'' — a criterion choice, not an empirical fact.').

omega_variable(
    textual_preservation_vs_generative_speech,
    'Does textual study without generative daily speech constitute language vitality, or is it preservation of a cultural artifact?',
    'Measure domains of active use: liturgy, study, legal writing, correspondence (historical), vs. home, street, workplace, media (modern). The liturgical reading claims the former domains are sufficient for vitality; the native_generation reading claims they are not.',
    'If textual domains suffice, the constraint is genuine coordination (rope) with near-zero extraction. If they don''t, the ''living'' claim extracts legitimacy from a vitality it doesn''t possess, raising ε and potentially shifting classification toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_preservation_vs_generative_speech, empirical, 'Whether the functional domains of liturgical Hebrew meet a vitality threshold.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the low suppression measured here structural (no barriers to exit) or does internalized identity pressure create de facto coercion for community members?',
    'Post-exit trajectory study: do individuals who leave observant communities retain Hebrew literacy, or does literacy collapse without the liturgical scaffold? If literacy persists, suppression is structural. If it collapses, internalized identity-lock operated.',
    'If internalized, effective suppression is higher than the structural measure — the constraint carries identity-lock dynamics that the scalar suppression metric understates. This would add an omega-modulated directionality correction for community members.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in a voluntary-participation constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_continuity_reading, 0, 1954).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_liturgical_tr_t0, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hebrew_liturgical_tr_t500, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 500, 0.18).
narrative_ontology:measurement(hebrew_liturgical_tr_t1000, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1000, 0.22).
narrative_ontology:measurement(hebrew_liturgical_tr_t1500, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1500, 0.25).
narrative_ontology:measurement(hebrew_liturgical_tr_t1800, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1800, 0.28).
narrative_ontology:measurement(hebrew_liturgical_tr_t1954, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1954, 0.25).

% Extraction over time
narrative_ontology:measurement(hebrew_liturgical_be_t0, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(hebrew_liturgical_be_t500, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 500, 0.1).
narrative_ontology:measurement(hebrew_liturgical_be_t1000, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1000, 0.13).
narrative_ontology:measurement(hebrew_liturgical_be_t1500, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1500, 0.14).
narrative_ontology:measurement(hebrew_liturgical_be_t1800, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(hebrew_liturgical_be_t1954, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1954, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_liturgical_su_t0, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(hebrew_liturgical_su_t500, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 500, 0.06).
narrative_ontology:measurement(hebrew_liturgical_su_t1000, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1000, 0.07).
narrative_ontology:measurement(hebrew_liturgical_su_t1500, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1500, 0.08).
narrative_ontology:measurement(hebrew_liturgical_su_t1800, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1800, 0.1).
narrative_ontology:measurement(hebrew_liturgical_su_t1954, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1954, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__liturgical_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__liturgical_continuity_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__native_generation_reading).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% The hebrew_living_language kernel decomposes into three constraint stories: liturgical_continuity_reading (this story, rope, ε≈0.15), native_generation_reading (snare or tangled_rope from diaspora view, rope from Israeli view, ε higher due to state enforcement of Hebrew), literary_revival_reading (scaffold — transitional coordination for modern Hebrew's emergence, ε moderate). Each reading has different beneficiaries, different criteria for vitality, and different temporal intervals. They are linked because each cites the others' claimed vitality as evidence for its own, and because the modern revival explicitly drew on liturgical and literary Hebrew while rejecting their sufficiency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
