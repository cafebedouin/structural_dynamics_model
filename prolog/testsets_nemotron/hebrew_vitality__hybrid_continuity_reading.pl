% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__hybrid_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__hybrid_continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: hebrew_vitality__hybrid_continuity_reading
 *   human_readable: Hebrew Vitality: Hybrid Continuity Reading
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   The hybrid continuity reading of Hebrew vitality is an analytical
 *   synthesis that resolves the contest between the liturgical_reading
 *   (ritual preservation constitutes vitality) and the native_daily_reading
 *   (only native generation constitutes vitality). It argues that liturgical
 *   preservation was a necessary enabler — maintaining the vocabulary,
 *   morphology, and textual corpus without which reconstruction would have
 *   lacked raw material — but was insufficient for vernacular vitality, which
 *   required active reconstruction of syntax, semantics, and pragmatics for
 *   daily use. This reading carries low extractiveness because it is an
 *   interpretive framework, not an actionable constraint organizing social
 *   arrangements; it has no beneficiaries who collect rents and no victims
 *   who bear costs.
 *
 * KEY AGENTS:
 *   - historical_linguists: analytical observers documenting the trajectory
 *   - revival_practitioners: beneficiaries of the synthetic framework for pedagogy
 *   - diaspora_communities: beneficiaries inheriting both liturgical and vernacular Hebrew
 *   - israeli_native_speakers: observers living the outcome
 *   - traditionalist_authorities: excluded — their liturgical_reading position is synthesized over
 *   - zionist_ideologues: excluded — their native_daily_reading position is synthesized over
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__hybrid_continuity_reading, 0.08).
domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, 0.12).
domain_priors:theater_ratio(hebrew_vitality__hybrid_continuity_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__hybrid_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__hybrid_continuity_reading, "Hebrew Vitality: Hybrid Continuity Reading").
narrative_ontology:topic_domain(hebrew_vitality__hybrid_continuity_reading, "sociolinguistics/language_revitalization/jewish_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__hybrid_continuity_reading, 'eb8b779d-18c6-4baa-90e1-d74d82e120aa').
narrative_ontology:cs_kernel_codification('eb8b779d-18c6-4baa-90e1-d74d82e120aa', distributed).
narrative_ontology:cs_authority_grounding('eb8b779d-18c6-4baa-90e1-d74d82e120aa', distributed).
narrative_ontology:cs_reading_relation('eb8b779d-18c6-4baa-90e1-d74d82e120aa', hebrew_vitality__liturgical_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb8b779d-18c6-4baa-90e1-d74d82e120aa', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_axiom('eb8b779d-18c6-4baa-90e1-d74d82e120aa', foundational, liturgical_preservation_necessary_insufficient).
narrative_ontology:cs_axiom_status(liturgical_preservation_necessary_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('eb8b779d-18c6-4baa-90e1-d74d82e120aa', liturgical_preservation_necessary_insufficient, empirically_contingent).
narrative_ontology:cs_axiom('eb8b779d-18c6-4baa-90e1-d74d82e120aa', foundational, vernacular_reconstruction_necessary).
narrative_ontology:cs_axiom_status(vernacular_reconstruction_necessary, holdable).
narrative_ontology:cs_axiom_grounding('eb8b779d-18c6-4baa-90e1-d74d82e120aa', vernacular_reconstruction_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('eb8b779d-18c6-4baa-90e1-d74d82e120aa', pre_revival_liturgical_continuity).
narrative_ontology:cs_drift_state('eb8b779d-18c6-4baa-90e1-d74d82e120aa', contemporary_sociolinguistic_consensus, gap(stable, minor, true)).
narrative_ontology:cs_created_at('eb8b779d-18c6-4baa-90e1-d74d82e120aa', '2026-06-12T14:30:00Z').
narrative_ontology:cs_kernel_id(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, revival_practitioners).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, diaspora_communities).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, liturgical_substrate_necessary_for_vernacular_revival).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, reconstruction_required_for_daily_vitality).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, synthesis_of_preservation_and_innovation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Analyze the historical trajectory of Hebrew from liturgical preservation through vernacular reconstruction. They document the linguistic evidence for both substrate continuity and innovative reconstruction without advocating for either reading's normative claims.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, historical_linguists, observer,
    analytical, civilizational, analytical, global).

% Language educators, ulpan teachers, and revival activists who draw on both liturgical texts and reconstructed vocabulary. They benefit from the synthetic framework that legitimizes using traditional sources while innovating for modern needs.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, revival_practitioners, beneficiary,
    organized, generational, mobile, global).

% Jewish communities worldwide who use Hebrew for prayer, study, and increasingly for cultural connection. They inherit the liturgical substrate and participate in its adaptation to contemporary life without bearing extraction costs.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, diaspora_communities, beneficiary,
    moderate, biographical, constrained, global).

% First-language Hebrew speakers in Israel who live the vernacular outcome. They are neither constrained by nor beneficiaries of the analytical framework — they simply speak the language that resulted from the historical process.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, israeli_native_speakers, observer,
    institutional, biographical, analytical, national).

% Religious authorities who maintain that liturgical Hebrew alone constitutes authentic vitality. They would object to the hybrid reading's claim that reconstruction was necessary, but their position is not incorporated into the analytical synthesis.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, traditionalist_authorities, excluded,
    organized, generational, identity_locked, global).

% Early revival ideologues who insisted only native daily speech constitutes true vitality. They would reject the hybrid reading's validation of liturgical continuity as a necessary enabler, but their position is not part of the synthetic framework.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, zionist_ideologues, excluded,
    institutional, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an analytical framework that resolves the liturgical-vs-native contest by recognizing both as structurally necessary phases: liturgical preservation maintained the linguistic substrate (vocabulary, morphology, textual corpus) without which reconstruction would have had no raw material; reconstruction supplied the syntactic, semantic, and pragmatic innovations required for daily vernacular use.
% TRANSFER_FUNCTION: Transfers analytical legitimacy from the contested readings to a synthetic account that acknowledges both preservation and innovation as necessary. No material resources flow; the transfer is epistemic — the framework redistributes explanatory credit across the historical phases.
% ABSENT_VOICES: Traditionalist religious authorities who hold that liturgical use alone constitutes vitality (liturgical_reading) and early Zionist ideologues who held that only native daily generation constitutes vitality (native_daily_reading) are structurally excluded — their positions are the ones being synthesized over, not incorporated into the synthesis.
% DISAPPEARANCE_RATIONALE: If this analytical reading vanished, the historical facts of Hebrew's trajectory would remain — liturgical texts preserved, Ben-Yehuda and others reconstructed, modern Hebrew spoken. The reading is an interpretive lens on events, not a constraint organizing social arrangements. The world would not rearrange; the contest between the other two readings would persist without this synthesis.
% FOUNDING_PROBLEM: The contest between 'Hebrew never died' (liturgical continuity) and 'Hebrew was reborn' (native generation) created a polarized scholarly and ideological field where each side treated the other's evidence as irrelevant. The hybrid reading was built to resolve this false opposition by showing both claims capture necessary but insufficient conditions.
% FOUNDING_PROBLEM_CORROBORATION: Sociolinguists outside the Jewish Studies field (e.g., Fishman on reversing language shift, Dorian on obsolescence) corroborate that substrate preservation and active reconstruction are distinct necessary conditions for revitalization — this is not a claim internal to the Hebrew case. Within the field, the hybrid reading remains contested by advocates of the polarized positions.
narrative_ontology:disappearance_verdict(hebrew_vitality__hybrid_continuity_reading, world_unchanged).
narrative_ontology:founding_problem_status(hebrew_vitality__hybrid_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__hybrid_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(hebrew_vitality__hybrid_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__hybrid_continuity_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__hybrid_continuity_reading_tests).
:- end_tests(hebrew_vitality__hybrid_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.08) because the reading itself extracts nothing — it is a scholarly synthesis. Suppression is low (0.12) because the reading does not silence alternatives; the contest between liturgical and native readings persists in parallel. Theater ratio is near zero (0.05) — there is no performative maintenance of this analytical position. Accessibility collapse is low (0.15) because alternative readings remain fully available and defended. Resistance is minimal (0.05) because the reading makes no demands on agents. The claimed type is rope: a genuine coordination function (resolving a false opposition in the scholarly field) with minimal coercive overhead.
 *
 * PERSPECTIVAL GAP:
 *   All stakeholder seats experience this constraint similarly — as a low-extraction analytical framework. The excluded seats (traditionalist_authorities, zionist_ideologues) experience the constraint differently only in the sense that their positions are not incorporated, but they bear no extraction cost from the hybrid reading's existence. The engine will compute near-identical types across seats, which is appropriate for an analytical synthesis.
 *
 * DIRECTIONALITY LOGIC:
 *   No agent is a structural beneficiary in the extraction sense — revival_practitioners and diaspora_communities benefit epistemically from a framework that legitimizes their work, but they do not collect rents from the constraint. No agent is a victim — the excluded readings persist unchanged. Directionality values will cluster near symmetric (d ≈ 0.5) for all seats because the constraint imposes no asymmetric transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resolves mandatrophy by showing that neither polarized reading captures the full historical structure: liturgical preservation alone could not produce vernacular vitality (the liturgical_reading's mandatrophy — its founding problem of maintaining ritual continuity is live, but its claim to sufficiency for vitality is dead); native generation alone had no substrate to work from (the native_daily_reading's founding problem of creating a spoken language is live, but its denial of the substrate's necessity is false). The hybrid reading's founding problem — resolving the false opposition — remains contested because the polarized readings persist as live ideological positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_sufficiency_threshold,
    'How much liturgical substrate (vocabulary, morphology, textual corpus) is minimally necessary for successful vernacular reconstruction? Could a smaller substrate have sufficed, or was the full liturgical corpus required?',
    'Comparative analysis of other revitalization cases (e.g., Māori, Welsh, Hawaiian) measuring correlation between pre-revival textual corpus size/depth and reconstruction success metrics.',
    'If a minimal threshold exists below the actual Hebrew liturgical corpus, the ''necessary enabler'' claim is weakened — preservation beyond the threshold was not structurally necessary. If the full corpus was required, the necessary-enabler claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_sufficiency_threshold, empirical, 'Whether the full liturgical corpus was necessary or a smaller substrate would have sufficed for reconstruction.').

omega_variable(
    reconstruction_autonomy,
    'To what extent was the reconstruction phase autonomous from the liturgical substrate vs. constrained by it? Did the reconstructed vernacular''s syntax and semantics bear the substrate''s imprint necessarily or contingently?',
    'Detailed linguistic comparison of modern Hebrew syntax/semantics against both Biblical/Mishnaic Hebrew and the European languages of the revivalists (Yiddish, Russian, German), quantifying substrate influence vs. innovation.',
    'If reconstruction was heavily constrained by the substrate, the ''reconstruction required'' claim shades toward ''reconstruction was channeled by substrate'' — the two phases are less distinct. If reconstruction was largely autonomous, the two-phase model is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reconstruction_autonomy, conceptual, 'Whether the reconstruction phase was structurally independent of or constrained by the liturgical substrate.').

omega_variable(
    reading_foreclosure_structure,
    'Does the hybrid reading logically foreclose the liturgical_reading and native_daily_reading, or do all three coexist as live positions in the field?',
    'Citation network analysis: do scholars who adopt the hybrid reading cite the polarized readings as refuted, or as partial-truths? Do advocates of the polarized readings engage with the hybrid reading as a serious alternative or dismiss it?',
    'If forecloses: the hybrid reading would be a structural successor that displaces the siblings (rare for analytical syntheses). If coexists_with: the contest persists as a three-way field. If influences: the hybrid reading reshapes the discourse without eliminating the polarized positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Structural relationship between the hybrid reading and its sibling readings in the hebrew_vitality kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__hybrid_continuity_reading, 1880, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_vitality__hybrid_continuity_reading_tr_t1880, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1880, 0.02).
narrative_ontology:measurement(hebrew_vitality__hybrid_continuity_reading_tr_t1900, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1900, 0.03).
narrative_ontology:measurement(hebrew_vitality__hybrid_continuity_reading_tr_t1920, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1920, 0.04).
narrative_ontology:measurement(hebrew_vitality__hybrid_continuity_reading_tr_t1948, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(hebrew_vitality__hybrid_continuity_reading_tr_t1970, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(hebrew_vitality__hybrid_continuity_reading_tr_t2000, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(hebrew_vitality__hybrid_continuity_reading_tr_t2025, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(hebrew_vitality__hybrid_continuity_reading_be_t1880, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1880, 0.05).
narrative_ontology:measurement(hebrew_vitality__hybrid_continuity_reading_be_t1900, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1900, 0.06).
narrative_ontology:measurement(hebrew_vitality__hybrid_continuity_reading_be_t1920, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1920, 0.07).
narrative_ontology:measurement(hebrew_vitality__hybrid_continuity_reading_be_t1948, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1948, 0.08).
narrative_ontology:measurement(hebrew_vitality__hybrid_continuity_reading_be_t1970, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1970, 0.08).
narrative_ontology:measurement(hebrew_vitality__hybrid_continuity_reading_be_t2000, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 2000, 0.08).
narrative_ontology:measurement(hebrew_vitality__hybrid_continuity_reading_be_t2025, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 2025, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_vitality__hybrid_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__hybrid_continuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__hybrid_continuity_reading, 0.02).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__native_daily_reading).

% DUAL FORMULATION NOTE:
% This constraint (hybrid_continuity_reading) and its two siblings (liturgical_reading, native_daily_reading) form the hebrew_vitality constraint family. The kernel 'hebrew_vitality' is the contested commitment: what constitutes Hebrew vitality. The three readings instantiate three distinct constraints with different ε values and beneficiary/victim structures. The hybrid reading has ε ≈ 0.08 (analytical synthesis); the liturgical_reading has higher ε (institutional authority extracting compliance with ritual norms); the native_daily_reading has higher ε (ideological exclusion of non-native speakers). All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
