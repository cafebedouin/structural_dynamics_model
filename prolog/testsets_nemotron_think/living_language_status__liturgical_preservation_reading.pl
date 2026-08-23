% ============================================================================
% CONSTRAINT STORY: living_language_status__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: living_language_status__liturgical_preservation_reading
 *   human_readable: Living Language Status via Liturgical Preservation
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint story captures the liturgical_preservation_reading of the
 *   living_language_status kernel: the claim that a language is living if its
 *   sacred texts are continuously recited, studied, and used in ritual. The
 *   reading originates in rabbinic Judaism's response to diaspora — where
 *   Hebrew and Aramaic lost native speakers but persisted through liturgy.
 *   The constraint coordinates communal identity and textual preservation
 *   (genuine coordination function) while extracting interpretive authority
 *   for rabbinical gatekeepers and delegitimizing secular speech communities
 *   (asymmetric extraction). The ε is low (0.25) because the coordination
 *   around a fixed liturgical corpus is the dominant structure; extraction
 *   manifests as definitional monopoly and symbolic exclusion rather than
 *   material transfer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.25).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.35).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Living Language Status via Liturgical Preservation").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, '458f748b-e7cd-4f2d-8d10-f89fdc3f774c').
narrative_ontology:cs_kernel_codification('458f748b-e7cd-4f2d-8d10-f89fdc3f774c', formalized).
narrative_ontology:cs_authority_grounding('458f748b-e7cd-4f2d-8d10-f89fdc3f774c', lineage).
narrative_ontology:cs_interpretation_layer_present('458f748b-e7cd-4f2d-8d10-f89fdc3f774c').
narrative_ontology:cs_reading_relation('458f748b-e7cd-4f2d-8d10-f89fdc3f774c', living_language_status__native_generation_reading, forecloses).
narrative_ontology:cs_reading_relation('458f748b-e7cd-4f2d-8d10-f89fdc3f774c', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('458f748b-e7cd-4f2d-8d10-f89fdc3f774c', foundational, liturgical_transmission_suffices_for_vitality).
narrative_ontology:cs_axiom_status(liturgical_transmission_suffices_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('458f748b-e7cd-4f2d-8d10-f89fdc3f774c', liturgical_transmission_suffices_for_vitality, theological).
narrative_ontology:cs_axiom('458f748b-e7cd-4f2d-8d10-f89fdc3f774c', foundational, rabbinical_interpretive_monopoly_legitimate).
narrative_ontology:cs_axiom_status(rabbinical_interpretive_monopoly_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('458f748b-e7cd-4f2d-8d10-f89fdc3f774c', rabbinical_interpretive_monopoly_legitimate, theological).
narrative_ontology:cs_axiom('458f748b-e7cd-4f2d-8d10-f89fdc3f774c', secondary, secular_usage_is_not_vitality).
narrative_ontology:cs_axiom_status(secular_usage_is_not_vitality, holdable).
narrative_ontology:cs_axiom_grounding('458f748b-e7cd-4f2d-8d10-f89fdc3f774c', secular_usage_is_not_vitality, theological).
narrative_ontology:cs_reference_frame('458f748b-e7cd-4f2d-8d10-f89fdc3f774c', halakhic_linguistic_continuity).
narrative_ontology:cs_drift_state('458f748b-e7cd-4f2d-8d10-f89fdc3f774c', post_hebrew_revival_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('458f748b-e7cd-4f2d-8d10-f89fdc3f774c', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_speech_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, liturgical_practitioners).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, liturgical_practitioners).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, liturgical_transmission_suffices_for_vitality).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, sacred_text_continuity_defines_living_language).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, rabbinical_interpretive_monopoly_legitimate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and administers the criterion for living language status through halakhic interpretation; maintains interpretive monopoly over what counts as valid liturgical transmission; collects authority and communal legitimacy from being the gatekeepers of sacred linguistic continuity.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, rabbinical_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Speakers who use the language in daily secular life (modern Hebrew, contemporary Aramaic dialects); delegitimized as desecrators or inauthentic by the liturgical criterion; bear the cost of having their linguistic creativity and native transmission denied as 'vitality'; cannot easily exit the definitional framework because it shapes education policy, state recognition, and funding.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_speech_community, payer,
    organized, biographical, constrained, national).

% Communities who actively recite, study, and ritualize sacred texts (yeshiva students, synagogue congregations, ritual specialists); benefit from the coordination function — communal cohesion, religious meaning, intergenerational transmission structure; also bear costs of rigorous textual mastery and time commitment; exit is identity-locked because religious self-concept is constituted through the practice.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, liturgical_practitioners, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, liturgical_practitioners, payer).

% Proponents of the native_generation_reading (Zionist Hebrew revivalists, language planners, intergenerational transmission theorists); structurally excluded from this reading's framework because their criterion (mother-tongue transmission) directly contradicts the liturgical sufficiency claim; would argue that ritual recitation without daily generational transmission is preservation of a corpse.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, native_generation_advocates, excluded,
    organized, generational, trapped, national).

% Proponents of the literary_continuity_reading (Haskalah maskilim, modern Hebrew literary critics, academic literary historians); excluded because their criterion (productive literary/intellectual medium) competes with liturgical transmission as the marker of vitality; would argue that a language living only in ritual is fossilized, not vital.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, literary_continuity_advocates, excluded,
    organized, generational, constrained, global).

% Descriptive linguists, sociolinguists, and language typologists who analyze vitality metrics (speaker counts, domains of use, intergenerational transmission); they do not set the criterion but their classifications influence policy; analytical seat sees the full structural field of competing readings.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, academic_linguists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves sacred texts and religious continuity across diaspora through communal ritual practice — solves the problem of maintaining a shared linguistic-religious identity without a territorial base or native speaker community.
% TRANSFER_FUNCTION: Moves interpretive authority and definitional power over 'living language' status to rabbinical authorities; moves delegitimization onto secular speech communities whose native transmission is denied as vitality; moves communal cohesion and religious meaning to liturgical practitioners.
% ABSENT_VOICES: Native generation advocates (mother-tongue transmission proponents) and literary continuity advocates (Haskalah/modern literature proponents) are structurally excluded from this reading's framework; they would argue that ritual recitation without daily generational transmission is preservation of a corpse, not vitality.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the definition of 'living language' for Hebrew, Aramaic, Ge'ez, and Classical Arabic would shift to native generation or literary continuity criteria, reorganizing language policy, education curricula, state recognition, funding streams, and identity politics — the Israeli revival's religious legitimacy would be contested, diaspora communities would lose their primary vitality claim.
% FOUNDING_PROBLEM: How to maintain Jewish linguistic and religious continuity across diaspora without a territorial base or native speaker community — the liturgical framework solved this by making sacred text transmission the criterion of vitality.
% FOUNDING_PROBLEM_CORROBORATION: Historical linguistics (Harshav on Hebrew revival, Fishman on reversing language shift) corroborates the diaspora continuity problem; rabbinical authorities self-attest the liturgical solution; secular Hebrew revivalists (Ben-Yehuda circle) attest the problem was real but the liturgical solution was insufficient without native generation — corroboration from outside the beneficiary set exists but is mixed.
narrative_ontology:disappearance_verdict(living_language_status__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__liturgical_preservation_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low but nonzero (0.25) because the constraint's primary operation is coordination — preserving texts through ritual — but the rabbinical authority's interpretive monopoly and the delegitimization of secular speakers constitute asymmetric extraction. Suppression (0.35) is moderate: the constraint doesn't physically prevent secular speech but delegationitimizes it through definitional authority, making alternatives cognitively and institutionally harder to sustain. Theater ratio is low (0.15) because the liturgical practice is genuinely functional for its participants. Accessibility collapse (0.45) is moderate: secular speakers can and do use the language, but the 'living' label is structurally denied them. Resistance (0.55) is significant: secular revival movements (Hebrew, Yiddish, Neo-Aramaic) actively contest the criterion.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinical seat, the constraint is a rope — genuine coordination solving the diaspora continuity problem. From the secular speaker seat, it is a snare — their native transmission is denied legitimacy to preserve rabbinical authority. From the liturgical practitioner seat, it is a tangled rope — real coordination they depend on, but the interpretive monopoly is extractive. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the analytical seat seeing both functions.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinical authority is the structural beneficiary (d ~ 0.15): they set the agenda, control the definition, and collect legitimacy — the constraint subsidizes their interpretive monopoly. Secular speech community is the target (d ~ 0.85): they bear the delegitimization, have constrained exit (cannot easily change the definition that shapes policy), and are identity-locked into the language even as it denies their vitality. Liturgical practitioners sit near symmetric (d ~ 0.5): genuine coordination benefit (community, meaning) balanced by high commitment costs. Native generation and literary continuity advocates are excluded (d not computed): their readings are structurally foreclosed or marginalized within this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora continuity without territory) was historically live and the liturgical solution was effective. For Hebrew, the problem is now contested: native generation revival resolved it differently, but the liturgical definition persists. For Aramaic, Ge'ez, and other liturgical languages, the problem remains live. The constraint has not undergone mandatrophy resolution because the coordination function remains active for current practitioners, even as the extraction (delegitimization of secular vitality) has become more visible. The persistence is not pure inertia — the liturgical coordination is still doing work — but the extraction has accumulated as the secular rival reading gained empirical instantiation (modern Hebrew).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the living_language_status kernel, or does it collapse into the kernel''s general ambiguity?',
    'Compare the ε, beneficiary/victim structure, and classification of this reading against the sibling readings; if each has a stable, distinct structural profile, the kernel decomposition is validated.',
    'If the readings share the same ε and structural profile, the kernel label masks a single constraint; if they diverge, the decomposition into separate constraint stories is analytically necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the living_language_status kernel genuinely decomposes into structurally distinct constraints.').

omega_variable(
    sibling_reading_delta_native_generation,
    'How does the native_generation_reading''s beneficiary/victim structure differ from this reading''s?',
    'Author the native_generation_reading as a separate constraint story and compare: its beneficiary is the native speaker community / language planners, its victim is the liturgical practitioner (denied vitality), its ε may be higher (exclusionary).',
    'If the native_generation_reading extracts from liturgical practitioners (foreclosing their vitality claim), the two readings are in forecloses relation; if both can coexist as live positions, they are coexists_with.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delta_native_generation, conceptual, 'Structural delta between liturgical_preservation_reading and native_generation_reading.').

omega_variable(
    sibling_reading_delta_literary_continuity,
    'How does the literary_continuity_reading''s beneficiary/victim structure differ from this reading''s?',
    'Author the literary_continuity_reading separately: its beneficiary is the literary/intellectual class, its victim may be the purely ritual community (denied vitality without literary production), its ε reflects coordination around creative production.',
    'Determines whether the relation is coexists_with (both non-native criteria can be live) or forecloses (one criterion excludes the other).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_delta_literary_continuity, conceptual, 'Structural delta between liturgical_preservation_reading and literary_continuity_reading.').

omega_variable(
    suppression_mechanism_delegitimation,
    'Is the measured suppression (delegitimization of secular speech) structural (institutional policy, funding, education) or internalized (secular speakers accepting they are ''not truly living'' the language)?',
    'Post-exclusion trajectory: if secular speakers internalize the delegitimization and reduce transmission efforts even when policy permits, suppression is partially internalized; if they resist and build alternative vitality metrics, suppression is primarily structural.',
    'If internalized, effective suppression is higher than the structural measure suggests — the target carries the suppression with them. This would increase χ for the secular_speech_community seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_delegitimation, empirical, 'Structural vs. internalized suppression mechanism for delegitimization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__liturgical_preservation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(livi_tr_t30, living_language_status__liturgical_preservation_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(livi_tr_t60, living_language_status__liturgical_preservation_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement(livi_tr_t90, living_language_status__liturgical_preservation_reading, theater_ratio, 90, 0.14).
narrative_ontology:measurement(livi_tr_t120, living_language_status__liturgical_preservation_reading, theater_ratio, 120, 0.15).
narrative_ontology:measurement(livi_tr_t150, living_language_status__liturgical_preservation_reading, theater_ratio, 150, 0.15).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__liturgical_preservation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(livi_be_t30, living_language_status__liturgical_preservation_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement(livi_be_t60, living_language_status__liturgical_preservation_reading, base_extractiveness, 60, 0.22).
narrative_ontology:measurement(livi_be_t90, living_language_status__liturgical_preservation_reading, base_extractiveness, 90, 0.25).
narrative_ontology:measurement(livi_be_t120, living_language_status__liturgical_preservation_reading, base_extractiveness, 120, 0.25).
narrative_ontology:measurement(livi_be_t150, living_language_status__liturgical_preservation_reading, base_extractiveness, 150, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__liturgical_preservation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(livi_su_t30, living_language_status__liturgical_preservation_reading, suppression_requirement, 30, 0.25).
narrative_ontology:measurement(livi_su_t60, living_language_status__liturgical_preservation_reading, suppression_requirement, 60, 0.3).
narrative_ontology:measurement(livi_su_t90, living_language_status__liturgical_preservation_reading, suppression_requirement, 90, 0.33).
narrative_ontology:measurement(livi_su_t120, living_language_status__liturgical_preservation_reading, suppression_requirement, 120, 0.35).
narrative_ontology:measurement(livi_su_t150, living_language_status__liturgical_preservation_reading, suppression_requirement, 150, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__liturgical_preservation_reading, 0.08).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the living_language_status constraint family. The kernel decomposes into three readings with distinct ε values and beneficiary/victim structures: liturgical_preservation (this story, ε=0.25, tangled_rope), native_generation (ε≈0.4, snare/tangled_rope), literary_continuity (ε≈0.2, rope). The upstream liturgical reading historically influenced the downstream revival readings (Hebrew revivalists had to contend with the liturgical definition); the contemporary native generation reading now exerts authority_erosion pressure on the liturgical reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(living_language_status__liturgical_preservation_reading, institutional, 0.15).
constraint_indexing:directionality_override(living_language_status__liturgical_preservation_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
