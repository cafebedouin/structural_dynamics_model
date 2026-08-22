% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__symbolic_archive_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: sacrifice_obligation_kernel__symbolic_archive_reading
 *   human_readable: Sacrificial Law as Cultural-Historical Archive (Symbolic Archive Reading)
 *   domain: religious_law/halakhic_authority/commitment_system
 *
 * SUMMARY:
 *   This constraint instantiates the symbolic_archive_reading of the
 *   sacrifice_obligation_kernel. It treats the extensive halakhic corpus
 *   concerning Temple sacrifices as a cultural-historical archive rather than
 *   a currently operative legal obligation. Study of these texts is
 *   understood as a voluntary practice preserving Jewish collective identity
 *   and legal continuity across diaspora. The reading makes no halakhic
 *   claim: it does not assert that study fulfills a mitzvah, nor that the
 *   obligation is suspended, nor that performance is required. The structural
 *   result is a constraint with zero extractiveness and zero suppression: no
 *   agent is coerced, no victim set exists, and the sole function is
 *   non-coercive coordination of memory. The claim/metric independence
 *   principle is observed: the claimed type is rope (coordination of identity
 *   through voluntary archive maintenance), while the metrics independently
 *   register the near-total absence of coercion or extraction.
 *
 * KEY AGENTS:
 *   - Jewish communities (beneficiary): Engage voluntarily with the archive for identity preservation.
 *   - Torah scholars (agenda_setter): Custodians of the textual tradition; transmit without enforcing.
 *   - Temple movement activists (excluded): Hold operative-obligation readings; backgrounded in this frame.
 *   - Secular Jewish studies scholars (observer): Corroborate the archive function from outside halakhah.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0).
domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0).
domain_priors:theater_ratio(sacrifice_obligation_kernel__symbolic_archive_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__symbolic_archive_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrificial Law as Cultural-Historical Archive (Symbolic Archive Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious_law/halakhic_authority/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__symbolic_archive_reading, '7c905581-ea6c-45b1-af17-ee4a0bfb3128').
narrative_ontology:cs_kernel_codification('7c905581-ea6c-45b1-af17-ee4a0bfb3128', fixed_text).
narrative_ontology:cs_authority_grounding('7c905581-ea6c-45b1-af17-ee4a0bfb3128', lineage).
narrative_ontology:cs_interpretation_layer_present('7c905581-ea6c-45b1-af17-ee4a0bfb3128').
narrative_ontology:cs_reading_relation('7c905581-ea6c-45b1-af17-ee4a0bfb3128', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c905581-ea6c-45b1-af17-ee4a0bfb3128', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c905581-ea6c-45b1-af17-ee4a0bfb3128', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_axiom('7c905581-ea6c-45b1-af17-ee4a0bfb3128', foundational, sacrificial_law_non_operative_archive).
narrative_ontology:cs_axiom_status(sacrificial_law_non_operative_archive, holdable).
narrative_ontology:cs_axiom_grounding('7c905581-ea6c-45b1-af17-ee4a0bfb3128', sacrificial_law_non_operative_archive, empirically_contingent).
narrative_ontology:cs_axiom('7c905581-ea6c-45b1-af17-ee4a0bfb3128', foundational, study_preserves_identity_not_fulfills_mitzvah).
narrative_ontology:cs_axiom_status(study_preserves_identity_not_fulfills_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('7c905581-ea6c-45b1-af17-ee4a0bfb3128', study_preserves_identity_not_fulfills_mitzvah, conventional).
narrative_ontology:cs_reference_frame('7c905581-ea6c-45b1-af17-ee4a0bfb3128', cultural_memory_repository).
narrative_ontology:cs_drift_state('7c905581-ea6c-45b1-af17-ee4a0bfb3128', contemporary_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('7c905581-ea6c-45b1-af17-ee4a0bfb3128', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_communities).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, cultural_continuity_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, non_halakhic_archive_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage with sacrificial law texts as part of cultural heritage and collective memory. Study is voluntary and serves identity preservation across diaspora. No penalty for non-engagement; the archive is available as a resource rather than a demand.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_communities, beneficiary,
    organized, generational, mobile, global).

% Curate and transmit the sacrificial law corpus as historical and legal heritage. They do not claim current binding obligation but maintain the textual archive for communal access. Their authority is scholarly and custodial, not coercive.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, torah_scholars, agenda_setter,
    organized, generational, mobile, global).

% Hold that sacrificial obligation is currently operative and should be physically performed. Their position is recognized as a distinct reading of the kernel but is backgrounded within the archive-only framework.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, temple_movement_activists, excluded,
    organized, biographical, constrained, national).

% Analyze the sacrificial law corpus as a historical and anthropological artifact. They corroborate the archive reading from outside the halakhic system, describing its function in identity preservation without affirming obligation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, secular_jewish_studies_scholars, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves Jewish collective identity and intergenerational continuity across diaspora by maintaining engagement with a central but currently impossible historical practice, converting it into a shared textual and educational focal point.
% TRANSFER_FUNCTION: Moves cultural memory, historical legal knowledge, and identity-relevant narrative from the archived textual tradition to the community through voluntary study and ritual commemoration.
% ABSENT_VOICES: Advocates for current physical performance and those who treat study as actual halakhic exercise of the mitzvah are present in the broader kernel discourse but are backgrounded in this reading; their exclusion is epistemic rather than institutional.
% DISAPPEARANCE_RATIONALE: If the archive and its study vanished, a significant component of Jewish collective memory and Talmudic legal continuity would be lost; educational curricula would rearrange, and a distinctive marker of diasporic adaptation would disappear.
% FOUNDING_PROBLEM: The destruction of the Second Temple eliminated the institutional and physical possibility of sacrificial practice, creating the problem of how to maintain continuity with a central cultic institution without claiming falsely that the obligation remains operable in the same form.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of Jewish law and anthropologists of religion attest that archive-like preservation of defunct temple practices is a known cross-cultural adaptation mechanism; their analysis from outside the benefiting community supports the continuity-without-obligation framing.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__symbolic_archive_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__symbolic_archive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__symbolic_archive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).
:- end_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set to 0.0 because the reading explicitly denies any binding obligation; there is nothing to extract. Suppression is 0.0 because engagement is voluntary and alternatives (ignoring the archive, adopting other readings) are not structurally blocked. Theater ratio is near-zero because the archive function is genuine and not performative maintenance of a defunct obligation. Accessibility collapse is low (0.2) because alternatives to the archive are readily available within and outside Jewish practice. Resistance is near-zero because the arrangement makes no demands. The measurement series shows flat zero extraction over two millennia, consistent with a stable rope function.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (scholars) and beneficiary seat (communities) should compute similarly: both experience low directionality because neither extracts from the other. The excluded seat (performance advocates) would compute a radically different type if modeled under its own reading, but under this reading they are simply not parties to the constraint. The engine will see no structural targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish communities are beneficiaries (low d) because the archive subsidizes their identity continuity. Torah scholars are near-symmetric (d approximately 0.5) because they invest labor in custodianship and receive scholarly continuity in return. No agent is positioned as a target because the reading constructs no obligation that could be violated.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this archive as a piton or snare because there is no enforcement decaying into theater, no identifiable victim set, and no coercive residue of a defunct obligation. The founding problem (Temple destruction) is still live in the sense that the condition persists, so the archive is not a mandate that has outlived its function; it is a continuously adaptive coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the symbolic_archive reading logically foreclose operative-obligation readings, or merely coexist as a parallel hermeneutic?',
    'Sociological mapping of how single individuals and institutions relate to the kernel: whether they treat the readings as mutually exclusive truth-claims or as context-dependent framings.',
    'If foreclosing, the archive reading competes for authority within the same normative space; if coexisting, it occupies a non-overlapping magisterium and extractiveness remains zero.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the archive reading logically excludes sibling readings').

omega_variable(
    archive_beneficiary_ambiguity,
    'Is the benefit of the archive genuinely distributed across Jewish communities, or concentrated among scholarly sub-communities that derive status from custodianship?',
    'Demographic analysis of who engages in sacrificial law study and whether engagement correlates with communal status or is distributed broadly.',
    'If concentrated among scholars, the archive may carry latent extractive status dynamics despite zero halakhic coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(archive_beneficiary_ambiguity, empirical, 'Whether archive benefit is diffuse or concentrated').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacrifice_sym_tr_t0, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacrifice_sym_tr_t500, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 500, 0.05).
narrative_ontology:measurement(sacrifice_sym_tr_t1000, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(sacrifice_sym_tr_t1500, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(sacrifice_sym_tr_t2000, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 2000, 0.05).

% Extraction over time
narrative_ontology:measurement(sacrifice_sym_be_t0, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(sacrifice_sym_be_t500, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 500, 0.0).
narrative_ontology:measurement(sacrifice_sym_be_t1000, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement(sacrifice_sym_be_t1500, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement(sacrifice_sym_be_t2000, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 2000, 0.0).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__symbolic_archive_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__symbolic_archive_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, messianic_suspension_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the sacrifice_obligation_kernel. It is structurally paired with three sibling readings that share the same textual kernel but assign different normative statuses to sacrificial law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
