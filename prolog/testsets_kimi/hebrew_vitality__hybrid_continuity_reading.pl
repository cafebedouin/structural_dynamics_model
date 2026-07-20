% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__hybrid_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Hebrew Vitality â Hybrid Continuity Reading
 *   domain: sociolinguistic/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid_continuity_reading of the
 *   hebrew_vitality kernel. The reading asserts that Hebrew vernacular
 *   revival in the late nineteenth and early twentieth centuries was only
 *   possible because a liturgical substrate (the long tradition of Hebrew
 *   literacy in religious contexts) provided necessary linguistic resources,
 *   yet this substrate alone was insufficient; deliberate sociolinguistic
 *   reconstruction (lexical modernization, institutional planning,
 *   spoken-language engineering) was also required. The reading reframes
 *   vitality as a dual-requirement process, rejecting both the liturgical
 *   reading (which identifies vitality with unbroken ritual use) and the
 *   native_daily_reading (which treats native acquisition as the sole
 *   criterion). As an analytical synthesis, it carries low extractiveness and
 *   no enforcement mechanism; its function is coordinative reframing within
 *   scholarship and language planning.
 *
 * KEY AGENTS:
 *   - hebrew_sociolinguistic_scholars: Primary beneficiary (organized/global/mobile exit) â gains analytical resolution from the synthesis.
 *   - language_revitalization_planners: Secondary beneficiary (institutional/national/constrained exit) â gains a transferable policy roadmap.
 *   - liturgical_essentialists: Excluded voice (organized/national/constrained exit) â sufficiency claim is rejected by the reading.
 *   - native_daily_advocates: Excluded voice (powerful/national/mobile exit) â exclusive nativity claim is partially absorbed but its denial of substrate is rejected.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__hybrid_continuity_reading, 0.12).
domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, 0.08).
domain_priors:theater_ratio(hebrew_vitality__hybrid_continuity_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__hybrid_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__hybrid_continuity_reading, "Hebrew Vitality â Hybrid Continuity Reading").
narrative_ontology:topic_domain(hebrew_vitality__hybrid_continuity_reading, "sociolinguistic/language_revitalization/jewish_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__hybrid_continuity_reading, '908cb6a9-c26d-40d1-9411-dce5a0eca7f6').
narrative_ontology:cs_kernel_codification('908cb6a9-c26d-40d1-9411-dce5a0eca7f6', distributed).
narrative_ontology:cs_authority_grounding('908cb6a9-c26d-40d1-9411-dce5a0eca7f6', expertise).
narrative_ontology:cs_reading_relation('908cb6a9-c26d-40d1-9411-dce5a0eca7f6', hebrew_vitality__liturgical_reading, coexists_with).
narrative_ontology:cs_reading_relation('908cb6a9-c26d-40d1-9411-dce5a0eca7f6', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_axiom('908cb6a9-c26d-40d1-9411-dce5a0eca7f6', foundational, liturgical_substrate_necessary).
narrative_ontology:cs_axiom_status(liturgical_substrate_necessary, holdable).
narrative_ontology:cs_axiom_grounding('908cb6a9-c26d-40d1-9411-dce5a0eca7f6', liturgical_substrate_necessary, empirically_contingent).
narrative_ontology:cs_axiom('908cb6a9-c26d-40d1-9411-dce5a0eca7f6', foundational, vernacular_reconstruction_required).
narrative_ontology:cs_axiom_status(vernacular_reconstruction_required, holdable).
narrative_ontology:cs_axiom_grounding('908cb6a9-c26d-40d1-9411-dce5a0eca7f6', vernacular_reconstruction_required, empirically_contingent).
narrative_ontology:cs_reference_frame('908cb6a9-c26d-40d1-9411-dce5a0eca7f6', substrate_plus_reconstruction).
narrative_ontology:cs_drift_state('908cb6a9-c26d-40d1-9411-dce5a0eca7f6', contemporary_native_hegemony, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('908cb6a9-c26d-40d1-9411-dce5a0eca7f6', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, hebrew_sociolinguistic_scholars).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, language_revitalization_planners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop analytical frameworks for Hebrew vitality. They benefit from a synthesis that resolves the disciplinary deadlock between preservationist and nativist camps by assigning historical necessity to liturgical substrate while affirming the indispensability of vernacular reconstruction.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, hebrew_sociolinguistic_scholars, beneficiary,
    organized, generational, mobile, global).

% Design policy and curricula for endangered and heritage languages. The hybrid reading provides a transferable roadmap: maintain ritual or literary substrate while deliberately reconstructing modern vernaculars, avoiding the policy paralysis of choosing between museum preservation and natural acquisition.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, language_revitalization_planners, beneficiary,
    institutional, generational, constrained, national).

% Assert that unbroken liturgical use is not merely necessary but sufficient for Hebrew vitality. They are excluded from the center of this reading because their sufficiency claim is explicitly rejected, though their necessity is affirmed.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, liturgical_essentialists, excluded,
    organized, generational, constrained, national).

% Treat native spoken Hebrew as the sole authentic benchmark of vitality and regard liturgical layers as archaic heritage. They are excluded from this synthesis to the extent that they deny the substrate necessity, though their emphasis on reconstruction is incorporated.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, native_daily_advocates, excluded,
    powerful, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the disciplinary and communal deadlock between liturgical preservationists and native-daily exclusivists by providing a unified historical narrative in which liturgical substrate and active vernacular reconstruction are both necessary for full vitality.
% TRANSFER_FUNCTION: Moves scholarly and planning attention and legitimacy away from a zero-sum contest between preservation and nativity toward a phased, integrated model where credit and resource allocation are distributed across historical stages.
% ABSENT_VOICES: Liturgical essentialists who claim ritual use alone constitutes vitality, and native-daily advocates who deny the relevance of liturgical substrate, are both backgrounded; they would object that the synthesis dissolves their categorical claims into a historicist compromise.
% DISAPPEARANCE_RATIONALE: Without this analytical frame, language planning and historical scholarship revert to a polarized binary that cannot allocate credit or design policy across the substrate-reconstruction divide; the conceptual space for integrated revitalization collapses.
% FOUNDING_PROBLEM: The false binary in Hebrew vitality discourse that forced an impossible choice between liturgical preservation and native spoken use, preventing coherent language planning and historical analysis.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by comparative language revitalization studies (Irish, Welsh, Maori) where identical substrate-versus-acquisition deadlocks are documented by scholars outside the Hebrew-specific beneficiary set; also attested by educational policymakers who report practical difficulty when forced to choose between heritage and communicative goals.
narrative_ontology:disappearance_verdict(hebrew_vitality__hybrid_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__hybrid_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__hybrid_continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_vitality__hybrid_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__hybrid_continuity_reading, 0.12, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.12) because the reading does not command resources or enforce behavior; it persuades and reframes. Suppression is minimal (0.08) because the reading does not block alternatives â the liturgical and native-daily readings remain fully visible and institutionally powerful. Theater ratio is very low (0.08) as there is little performative maintenance; the reading survives or fails on analytical coherence and empirical fit. Accessibility collapse is moderate (0.30) because the binary readings are prominent and easily accessible alternatives. Resistance is moderate (0.25) because both polar camps resist the dilution of their claims. The measurement series runs on a single shared grid (0, 20, 40) with all three tracked metrics authored at each point.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (scholars, planners), the reading appears as a genuine coordination mechanism that resolves a conceptual deadlock and enables policy. From the excluded seats (liturgical essentialists, native-daily exclusivists), the same reading appears as an unwarranted compromise that weakens their respective categorical claims. The engine computes this divergence: low power and mobile exit for the native-daily camp yields low effective extraction, while constrained exit for the liturgical camp yields slightly higher extraction, though all remain in the rope zone.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary beneficiaries are the scholarly and planning communities that gain coordination value from a non-binary model. There are no structural victims because the reading does not extract from any party; rather, it denies exclusive sufficiency to two competing camps, which experience the constraint as analytical inconvenience rather than material cost. The excluded stakeholders retain their own platforms and power; their exclusion here is intellectual, not institutional. Consequently, directionality for beneficiaries sits near the subsidy end, while excluded stakeholders sit nearer symmetric than target because no transfer is extracted from them.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading avoids mandatrophy because its founding problem â the polarized deadlock in vitality discourse â remains live in language revitalization contexts globally. It does not claim enforcement authority, so it cannot atrophy into a piton. Should the problem be solved (e.g., if the hybrid model became the undisputed default across revitalization fields), the reading would become a historical description rather than an active coordination, but it would not degrade into extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_necessity_universality,
    'Is the liturgical-substrate necessity a general structural feature of language revitalization, or a post-hoc rationalization unique to Hebrew''s exceptional history?',
    'Comparative sociolinguistic analysis of revitalization cases without deep liturgical substrates (e.g., Cornish, Manx) to test whether similar revivals succeeded or failed without such a substrate.',
    'If the substrate is not universally necessary, this reading becomes a historically contingent rope rather than a general model, reducing its coordinative authority in non-Hebrew planning contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_necessity_universality, empirical, 'Whether the hybrid model generalizes beyond Hebrew.').

omega_variable(
    insufficiency_as_diminishment,
    'Does labeling liturgical preservation ''insufficient'' structurally diminish the standing of liturgical communities, even without explicit victimization?',
    'Discourse analysis of how the ''necessary but insufficient'' framing is received in liturgical educational institutions; measuring resource and status shifts.',
    'If the framing is read as diminishment, the reading''s directionality for liturgical stakeholders shifts toward extraction, and the computed type may edge toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(insufficiency_as_diminishment, conceptual, 'Whether the insufficiency claim covertly extracts standing from liturgical communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__hybrid_continuity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(hebr_tr_t20, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(hebr_tr_t40, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 40, 0.08).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(hebr_be_t20, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(hebr_be_t40, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 40, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0, 0.04).
narrative_ontology:measurement(hebr_su_t20, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 20, 0.06).
narrative_ontology:measurement(hebr_su_t40, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 40, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
