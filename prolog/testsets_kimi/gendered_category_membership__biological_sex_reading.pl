% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__biological_sex_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__biological_sex_reading, []).

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
 *   constraint_id: gendered_category_membership__biological_sex_reading
 *   human_readable: Biological Sex Category Membership Reading
 *   domain: social ontology / political philosophy / bioethics
 *
 * SUMMARY:
 *   This constraint instantiates the biological_sex_reading of the
 *   gendered_category_membership kernel. It grounds membership in the
 *   categories 'man' and 'woman' in immutable biological markersâtypically
 *   chromosomes and reproductive anatomy observed at birthâand uses these
 *   markers to administer sex-segregated spaces, legal identity documents,
 *   and sporting competitions. The constraint is actively enforced by medical
 *   and state institutions. Trans women are excluded from the 'woman'
 *   category, intersex people are erased or forced into binary boxes, and cis
 *   women are positioned as the protected beneficiary class. The constraint
 *   claims to be a neutral description of biological reality, but its
 *   persistence requires active suppression of gender-identity-based
 *   alternatives.
 *
 * KEY AGENTS:
 *   - Sex binary administrators (institutional/agenda_setter): enforce classification rules
 *   - Cis women (organized/beneficiary): receive protected access to sex-segregated spaces
 *   - Trans women (powerless/payer): excluded and identity-suppressed
 *   - Intersex people (powerless/payer): erased by binary markers
 *   - Gender identity advocates (organized/excluded): excluded from definitional authority
 *   - Bioethicists (analytical/observer): analyze the description-to-norm slide
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.82).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.85).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Biological Sex Category Membership Reading").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social ontology / political philosophy / bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, '95750be6-f588-4110-89e1-19c2d16a1714').
narrative_ontology:cs_kernel_codification('95750be6-f588-4110-89e1-19c2d16a1714', fixed_text).
narrative_ontology:cs_authority_grounding('95750be6-f588-4110-89e1-19c2d16a1714', expertise).
narrative_ontology:cs_interpretation_layer_present('95750be6-f588-4110-89e1-19c2d16a1714').
narrative_ontology:cs_reading_relation('95750be6-f588-4110-89e1-19c2d16a1714', gendered_category_membership__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('95750be6-f588-4110-89e1-19c2d16a1714', gendered_category_membership__social_role_reading, forecloses).
narrative_ontology:cs_axiom('95750be6-f588-4110-89e1-19c2d16a1714', foundational, biological_sex_determines_category_membership).
narrative_ontology:cs_axiom_status(biological_sex_determines_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('95750be6-f588-4110-89e1-19c2d16a1714', biological_sex_determines_category_membership, empirically_contingent).
narrative_ontology:cs_axiom('95750be6-f588-4110-89e1-19c2d16a1714', foundational, sexual_dimorphism_is_binary_exhaustive).
narrative_ontology:cs_axiom_status(sexual_dimorphism_is_binary_exhaustive, holdable).
narrative_ontology:cs_axiom_grounding('95750be6-f588-4110-89e1-19c2d16a1714', sexual_dimorphism_is_binary_exhaustive, empirically_contingent).
narrative_ontology:cs_reference_frame('95750be6-f588-4110-89e1-19c2d16a1714', binary_biological_dimorphism).
narrative_ontology:cs_drift_state('95750be6-f588-4110-89e1-19c2d16a1714', contemporary_gender_identity_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('95750be6-f588-4110-89e1-19c2d16a1714', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cis_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, intersex_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer legal, medical, and sporting sex classification systems by inspecting birth records, chromosomal tests, or reproductive anatomy. Enforce single-sex space admission rules and maintain the administrative infrastructure that treats biological sex as exhaustive and immutable.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, sex_binary_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive access to sex-segregated spaces, services, and legal protections that are reserved for the biologically female category. Their category membership is treated as protected by the binary boundary, though they do not administer it.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cis_women, beneficiary,
    organized, biographical, constrained, global).

% Excluded from the 'woman' category regardless of identity, presentation, or social transition. Denied access to sex-segregated spaces and legal recognition because immutable biological markers are treated as dispositive. Cannot exit the enforcement framework without leaving the jurisdiction or detransitioning, which the constraint treats as returning to their 'true' sex.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, trans_women, payer,
    powerless, biographical, trapped, global).

% Possess chromosomal or anatomical variations that do not fit the binary biological markers used for classification. Are typically surgically or administratively forced into one binary category at birth, erasing their actual biology within a system that claims to be grounded in biological reality.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, intersex_people, payer,
    powerless, generational, trapped, global).

% Advance frameworks in which category membership follows self-identification rather than biology. Are excluded from policy-making tables where biological definitions are adopted; their arguments are treated as ontologically illegitimate within this reading.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_identity_advocates, excluded,
    organized, generational, mobile, global).

% Analyze the boundary between biological description and normative category assignment. They observe that the reading collapses sex-as-biology into sex-as-category-membership without justifying why biological markers should govern social, legal, and ethical status.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, bioethicists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes social, legal, medical, and sporting institutions around a binary sex classification grounded in immutable biological markers so that single-sex spaces, reproductive rights, and sex-based statistics can be administered without ambiguity.
% TRANSFER_FUNCTION: Moves category membership, legal recognition, and access to sex-segregated protections away from trans women and intersex people toward the cis female category, preserving the binary boundary.
% ABSENT_VOICES: Trans women, intersex people, and gender identity advocates are structurally excluded from the category-definition process; their testimony about their own bodies and identities is ruled inadmissible by the biological-marker test.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, sex-segregated spaces would lose their biological admission criteria, legal sex classification would require re-administration around different principles, and the binary infrastructure of medicine, sport, and statistics would reorganize.
% FOUNDING_PROBLEM: The need to classify humans into male and female categories for medical, reproductive, legal, and protective purposes in a sexually dimorphic species.
% FOUNDING_PROBLEM_CORROBORATION: Medical and biological authorities attest to sexual dimorphism as a statistical pattern, but intersex scholars, trans activists, and critical bioethicists from outside the benefiting parties contest that immutable binary markers are sufficient or appropriate grounds for contemporary social category membership.
narrative_ontology:disappearance_verdict(gendered_category_membership__biological_sex_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__biological_sex_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__biological_sex_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gendered_category_membership__biological_sex_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__biological_sex_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__biological_sex_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__biological_sex_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint denies category membership, legal recognition, and space access to trans women regardless of social transition, extracting identity and status. Suppression is higher (0.85) because the constraint must actively exclude gender-identity frameworks and medical transition histories to preserve the binary boundary. Theater is moderate (0.45): much enforcement is functional (medical testing, record-keeping), but a growing share is performative (genital inspections in sport, rhetorical appeals to chromosomes by non-medical actors). Resistance is high (0.75) because trans communities and allies actively contest the boundary. Accessibility collapse is moderate (0.60): gender identity alternatives are widely understood but institutionally blocked.
 *
 * PERSPECTIVAL GAP:
 *   From the administrator seat, the constraint is a necessary coordination mechanism for medicine, sport, and safety; from the trans woman seat, the same structure is an enforced extraction of identity and exclusion from social membership. The engine computes this divergence from the structural data: same constraint, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Cis women sit near the beneficiary end: the constraint is justified as protecting their spaces and rights, and they bear low direct cost. Sex binary administrators sit near beneficiary but with moderate d because they pay institutional maintenance costs while gaining definitional power. Trans women and intersex people sit at the full-target end: they bear the direct extraction of exclusion and erasure. Gender identity advocates are excluded rather than coordinated; their exclusion is the enforcement object.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both beneficiaries and victims. A pure coordination story (rope) would have no victims, yet trans women and intersex people bear clear costs. A pure extraction story (snare) would lack the genuine coordination function that sex-segregated spaces provide for some contexts (shelter, certain medical care). The tangled rope classification captures the hybrid: real coordination for cis women layered with asymmetric extraction from trans and intersex people.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_naturalness,
    'Do immutable biological markers constitute an irreducible natural boundary or a selectively enforced construct that naturalizes power?',
    'Cross-cultural and historical analysis of how binary categories handle intersex conditions, combined with analysis of whether the constraint persists independently of enforcement.',
    'If genuinely natural, the constraint approaches mountain-like immunity; if constructed, the high extraction and suppression indicate a snare or tangled rope maintaining itself through coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_naturalness, conceptual, 'Whether the biological binary is natural law or enforced construct.').

omega_variable(
    identity_suppression_mechanism,
    'Is the suppression of trans women''s category membership primarily structural (legal and administrative exclusion) or internalized (self-invalidating cognitive patterns)?',
    'Post-policy-change trajectory analysis measuring persistent harm after structural barriers are removed.',
    'If internalized, effective extraction exceeds the structural measure because targets carry the suppression after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_suppression_mechanism, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    kernel_reading_ambiguity,
    'Does the biological sex reading of category membership foreclose its siblings on logical grounds alone, or does it rely on institutional power to maintain foreclosure?',
    'Comparative analysis across jurisdictions where different readings are institutionalized to see if logical contradiction alone sustains the exclusion.',
    'If logical foreclosure is genuine, the reading behaves like a commitment system with non-negotiable axioms; if power-dependent, its foreclosure is enforced extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Nature of foreclosure between kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bio_sex_reading_tr_t0, gendered_category_membership__biological_sex_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bio_sex_reading_tr_t10, gendered_category_membership__biological_sex_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(bio_sex_reading_tr_t20, gendered_category_membership__biological_sex_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(bio_sex_reading_tr_t30, gendered_category_membership__biological_sex_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(bio_sex_reading_tr_t40, gendered_category_membership__biological_sex_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(bio_sex_reading_tr_t50, gendered_category_membership__biological_sex_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(bio_sex_reading_be_t0, gendered_category_membership__biological_sex_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(bio_sex_reading_be_t10, gendered_category_membership__biological_sex_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(bio_sex_reading_be_t20, gendered_category_membership__biological_sex_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(bio_sex_reading_be_t30, gendered_category_membership__biological_sex_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement(bio_sex_reading_be_t40, gendered_category_membership__biological_sex_reading, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(bio_sex_reading_be_t50, gendered_category_membership__biological_sex_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(bio_sex_reading_su_t0, gendered_category_membership__biological_sex_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(bio_sex_reading_su_t10, gendered_category_membership__biological_sex_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(bio_sex_reading_su_t20, gendered_category_membership__biological_sex_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(bio_sex_reading_su_t30, gendered_category_membership__biological_sex_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(bio_sex_reading_su_t40, gendered_category_membership__biological_sex_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement(bio_sex_reading_su_t50, gendered_category_membership__biological_sex_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__biological_sex_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gender_identity_reading).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, social_role_reading).

% DUAL FORMULATION NOTE:
% This constraint and its siblings form the gendered_category_membership family. Each reading emits a different constraint from the same kernel; they are linked because the biological reading's enforcement directly suppresses the institutional space in which the other readings could operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
