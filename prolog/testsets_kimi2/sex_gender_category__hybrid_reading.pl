% ============================================================================
% CONSTRAINT STORY: sex_gender_category__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__hybrid_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: sex_gender_category__hybrid_reading
 *   human_readable: Medical Gatekeeping Hybrid Model for Sex/Gender Category Membership
 *   domain: social/legal
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid reading of the
 *   sex_gender_category kernel: legal and social category membership is
 *   determined by a combination of biological indicators and certified
 *   medical transition. The model delegates authority to medical institutions
 *   to adjudicate who qualifies for recognition, creating a gatekeeping
 *   apparatus that extracts costs from trans individuals while providing a
 *   coordination mechanism for post-transition legal status. The constraint
 *   is actively enforced through diagnostic protocols (ICD, DSM, WPATH) and
 *   state registries that require medical certification. It excludes
 *   non-transitioning trans individuals entirely. As a kernel reading, it is
 *   one of three structurally distinct constraints (biology_reading,
 *   hybrid_reading, identity_reading) that share a natural-language label but
 *   differ in epsilon, beneficiary structure, and victim sets.
 *
 * KEY AGENTS:
 *   - medical_gatekeeping_institutions: Primary agenda-setter and beneficiary (institutional/constrained) â administers gatekeeping protocols and collects authority/revenue.
 *   - trans_individuals_seeking_recognition: Primary payer (powerless/trapped) â bears medical, financial, and temporal costs of gatekeeping.
 *   - non_transitioning_trans_individuals: Secondary payer (powerless/trapped) â excluded from recognition and bears costs of document mismatch.
 *   - trans_rights_advocates: Observer/secondary resistance (organized/constrained) â contests the medical prerequisite and advocates for self-identification.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, 0.72).
domain_priors:suppression_score(sex_gender_category__hybrid_reading, 0.68).
domain_priors:theater_ratio(sex_gender_category__hybrid_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Medical Gatekeeping Hybrid Model for Sex/Gender Category Membership").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social/legal").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, '6cfbc019-c597-4c1e-8156-95eff812114c').
narrative_ontology:cs_kernel_codification('6cfbc019-c597-4c1e-8156-95eff812114c', formalized).
narrative_ontology:cs_authority_grounding('6cfbc019-c597-4c1e-8156-95eff812114c', expertise).
narrative_ontology:cs_interpretation_layer_present('6cfbc019-c597-4c1e-8156-95eff812114c').
narrative_ontology:cs_reading_relation('6cfbc019-c597-4c1e-8156-95eff812114c', sex_gender_category__biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('6cfbc019-c597-4c1e-8156-95eff812114c', sex_gender_category__identity_reading, coexists_with).
narrative_ontology:cs_axiom('6cfbc019-c597-4c1e-8156-95eff812114c', foundational, medical_transition_prerequisite_for_recognition).
narrative_ontology:cs_axiom_status(medical_transition_prerequisite_for_recognition, holdable).
narrative_ontology:cs_axiom_grounding('6cfbc019-c597-4c1e-8156-95eff812114c', medical_transition_prerequisite_for_recognition, conventional).
narrative_ontology:cs_axiom('6cfbc019-c597-4c1e-8156-95eff812114c', foundational, binary_sex_category_persistence).
narrative_ontology:cs_axiom_status(binary_sex_category_persistence, holdable).
narrative_ontology:cs_axiom_grounding('6cfbc019-c597-4c1e-8156-95eff812114c', binary_sex_category_persistence, empirically_contingent).
narrative_ontology:cs_reference_frame('6cfbc019-c597-4c1e-8156-95eff812114c', medical_authority_over_sex_classification).
narrative_ontology:cs_drift_state('6cfbc019-c597-4c1e-8156-95eff812114c', self_identification_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6cfbc019-c597-4c1e-8156-95eff812114c', '').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, trans_individuals_seeking_recognition).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer diagnostic protocols (psychiatric evaluation, hormone therapy, surgery) that certify trans individuals for legal gender recognition. Collect fees, professional authority, and institutional budgets from being the necessary intermediary between trans people and the state. Their gatekeeping role is delegated by legal frameworks but concentrated in specialized clinics and professional boards. Could lose this role if self-identification laws replace medical certification, but currently monopolize the recognition pathway.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions, beneficiary).

% Must undergo months or years of psychiatric evaluation, hormone therapy, and often genital surgery to obtain legal gender recognition. Bear direct medical costs, travel costs, waiting-list delays, invasive questioning, and mandatory sterilization in some jurisdictions. Exit from the constraint means forgoing legal recognition, which exposes them to discrimination, violence, and document mismatch in employment, housing, and travel.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_individuals_seeking_recognition, payer,
    powerless, biographical, trapped, national).

% Are categorically excluded from legal gender recognition because they cannot or will not undergo medical transition. Bear the costs of persistent document mismatch, social misrecognition, lack of legal protection, and vulnerability in gender-segregated spaces. The constraint offers no pathway to recognition for this group; they are structurally invisible under the hybrid model.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals, payer,
    powerless, biographical, trapped, national).

% Are erased by the binary biological component of the hybrid model, which presumes a clean dimorphic biological baseline for category assignment. Medical protocols generally do not account for intersex variation in gatekeeping criteria, and intersex voices are absent from the design of diagnostic standards.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, intersex_community, excluded,
    powerless, generational, trapped, national).

% Contest the medical prerequisite for legal recognition, arguing that self-determination should replace gatekeeping. Produce research, litigation, and public campaigns documenting the harms of the hybrid model. Do not collect from the constraint but are structurally positioned to alter its enforcement environment through political and legal pressure.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_rights_advocates, observer,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions).
narrative_ontology:fixing_cost_class(sex_gender_category__hybrid_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally administrable, seemingly objective criterion for sex/gender category membership that bridges biological indicators and social transition, intended to resolve classification disputes by deferring to medical expertise and bodily modification.
% TRANSFER_FUNCTION: Moves authority over gender recognition from individuals and democratic deliberation to medical institutions; moves money, time, bodily autonomy, fertility, and dignity from trans individuals seeking recognition to the medical gatekeeping apparatus and the state registry.
% ABSENT_VOICES: Non-transitioning trans individuals are excluded from recognition entirely and largely absent from policy design; trans individuals who reject medicalization as a prerequisite for identity are not at the table; intersex people whose biology does not fit the binary are erased by the biological component.
% DISAPPEARANCE_RATIONALE: If the hybrid medical gatekeeping model vanished overnight, jurisdictions relying on it would lose their mechanism for adjudicating legal sex/gender status; trans individuals would immediately gain or lose recognition depending on replacement rules; medical institutions would lose delegated authority and a revenue stream; the absence of an alternative criterion would force rapid legal and social reorganization.
% FOUNDING_PROBLEM: Legal systems in the mid-to-late twentieth century needed a way to classify individuals who had undergone sex reassignment procedures into the 'opposite' legal category, bridging the gap between biological birth characteristics and social/presented gender, while reassuring courts and legislatures that the change was 'real' and irreversible.
% FOUNDING_PROBLEM_CORROBORATION: Medical historians and trans studies scholars outside the benefiting institutions attest that the model emerged from a combination of clinical paternalism, psychiatric pathologization, and state demand for administrable boundaries. Some former gatekeeping practitioners have corroborated that screening was designed as much to protect cisgender social order and exclude 'false positives' as to serve trans patients' interests.
narrative_ontology:disappearance_verdict(sex_gender_category__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sex_gender_category__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__hybrid_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because legal recognition is conditioned on costly, invasive medical procedures and evaluations that are decoupled from the individual's self-understanding. Suppression (0.68) is high because the constraint actively excludes self-identification alternatives and enforces compliance through medical certification. Theater (0.45) reflects the performative objectivity of medical diagnosis masking socially contingent gatekeeping. Accessibility collapse (0.60) captures the closure of non-medical pathways to recognition. Resistance (0.55) reflects sustained activism against gatekeeping. The measurement series shows extraction accumulating as protocols formalized and waiting lists grew.
 *
 * PERSPECTIVAL GAP:
 *   The medical institution seat experiences the constraint as legitimate expertise and necessary safeguarding; the trans individual seats experience it as a coercive barrier to self-determination. The engine computes this divergence from structural data: medical institutions have institutional power and constrained exit (they could lose the role but currently monopolize it), while trans seekers are powerless with trapped exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical gatekeeping institutions sit near the beneficiary end (d low): they are subsidized by the constraint via fees, budgets, and professional authority. Trans individuals seeking recognition sit near the target end (d high): they pay extraction directly through compliance costs. Non-transitioning trans individuals sit at the extreme target end (d near 1.0): they are fully excluded and receive no coordination benefit. The divergence is structural: same constraint, opposite directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid model prevents mislabeling by preserving the genuine coordination it provides (a legal path to recognition for medically transitioning individuals) while naming the asymmetric extraction (costs borne by all trans individuals, especially the excluded). Without the victim declaration, the constraint might read as a scaffold or rope; without the beneficiary declaration, it might read as a snare. Both are required for tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_vs_construction_of_gatekeeping,
    'Is the medical gatekeeping requirement a natural outgrowth of biological sex dimorphism, or a constructed bureaucratic mechanism?',
    'Comparative jurisdictional analysis: jurisdictions with self-identification achieve similar social coordination without medical gatekeeping.',
    'If purely constructed, the constraint''s extraction is fully discretionary and reformable; if partially natural, it carries mountain-like immunity to reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_vs_construction_of_gatekeeping, conceptual, 'Whether the hybrid criterion is natural or constructed').

omega_variable(
    cost_inflation_by_gatekeeping,
    'Are the medical costs and waiting times intrinsic to transition care, or inflated by the gatekeeping function itself?',
    'Compare cost and delay in informed-consent jurisdictions versus gatekeeping jurisdictions.',
    'If inflated, effective extraction is higher than the medical necessity baseline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_inflation_by_gatekeeping, empirical, 'Whether gatekeeping inflates transition costs beyond medical necessity').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of self-identification alternatives structural (legal barriers) or internalized (trans individuals believing they must be medically certified to be ''truly'' recognized)?',
    'Post-reform trajectory in jurisdictions that shifted to self-identification: does demand for medical gatekeeping persist among trans individuals?',
    'If internalized, suppression persists even after legal reform, raising effective extraction beyond the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sgc_hybrid_tr_t0, sex_gender_category__hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(sgc_hybrid_tr_t10, sex_gender_category__hybrid_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(sgc_hybrid_tr_t20, sex_gender_category__hybrid_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(sgc_hybrid_tr_t30, sex_gender_category__hybrid_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(sgc_hybrid_tr_t40, sex_gender_category__hybrid_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(sgc_hybrid_tr_t50, sex_gender_category__hybrid_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(sgc_hybrid_be_t0, sex_gender_category__hybrid_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sgc_hybrid_be_t10, sex_gender_category__hybrid_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(sgc_hybrid_be_t20, sex_gender_category__hybrid_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(sgc_hybrid_be_t30, sex_gender_category__hybrid_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(sgc_hybrid_be_t40, sex_gender_category__hybrid_reading, base_extractiveness, 40, 0.69).
narrative_ontology:measurement(sgc_hybrid_be_t50, sex_gender_category__hybrid_reading, base_extractiveness, 50, 0.72).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sex_gender_category__hybrid_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, identity_reading).

% DUAL FORMULATION NOTE:
% The sex_gender_category kernel decomposes into three constraints because the natural-language label conflates structurally distinct claims with different epsilon values, beneficiary structures, and victim sets. The hybrid reading influences the political feasibility of the biology reading (by showing transition can override biology) and the identity reading (by staking a middle position that delays self-identification reform).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
