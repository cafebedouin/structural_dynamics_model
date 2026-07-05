% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__biological_sex_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Biological Sex Reading of Gendered Category Membership
 *   domain: social ontology / political philosophy / bioethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   gendered_category_membership kernel: the claim that category membership
 *   in 'man'/'woman' is properly grounded in immutable biological markers
 *   fixed at birth (chromosomal karyotype, reproductive anatomy), independent
 *   of subsequent identity, transition, hormonal status, or social role.
 *   Under this reading, sex-segregated spaces, sports categories, and legal
 *   sex markers correctly track the birth marker, and trans women are
 *   correctly excluded from the 'woman' category regardless of legal or
 *   medical transition. This is NOT a story about the kernel contest itself —
 *   it is a clean, ε-invariant account of what this one reading structurally
 *   does: who it benefits, who it costs, and how it is enforced. The sibling
 *   readings (gender_identity_reading, social_role_reading) are separate
 *   constraints with their own ε values, beneficiary/victim structures, and
 *   classifications; they are not described here except as named siblings in
 *   the omega variables and cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - trans_women: primary target (powerless/identity_locked) — categorically excluded regardless of transition status
 *   - intersex_individuals: secondary target (powerless/trapped) — the binary premise erases their existence structurally
 *   - cis_women_advocacy_organizations: primary beneficiary (organized/mobile) — gains protected-category boundary they organized to defend
 *   - sex_segregated_space_administrators: agenda_setter (institutional/arbitrage) — enforces bright-line rule for administrative legibility
 *   - medical_and_legal_professionals: analytical observer — evidence on chromosomal/anatomical variability complicates the 'immutable' premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.68).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.71).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Biological Sex Reading of Gendered Category Membership").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social ontology / political philosophy / bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, 'a0996c91-1dc9-487c-a63f-e0d57bb38f44').
narrative_ontology:cs_kernel_codification('a0996c91-1dc9-487c-a63f-e0d57bb38f44', distributed).
narrative_ontology:cs_authority_grounding('a0996c91-1dc9-487c-a63f-e0d57bb38f44', distributed).
narrative_ontology:cs_reading_relation('a0996c91-1dc9-487c-a63f-e0d57bb38f44', gendered_category_membership__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('a0996c91-1dc9-487c-a63f-e0d57bb38f44', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('a0996c91-1dc9-487c-a63f-e0d57bb38f44', foundational, birth_marker_is_the_sole_determinant_of_sex_category).
narrative_ontology:cs_axiom_status(birth_marker_is_the_sole_determinant_of_sex_category, holdable).
narrative_ontology:cs_axiom_grounding('a0996c91-1dc9-487c-a63f-e0d57bb38f44', birth_marker_is_the_sole_determinant_of_sex_category, empirically_contingent).
narrative_ontology:cs_axiom('a0996c91-1dc9-487c-a63f-e0d57bb38f44', foundational, self_declared_identity_cannot_alter_category_membership).
narrative_ontology:cs_axiom_status(self_declared_identity_cannot_alter_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('a0996c91-1dc9-487c-a63f-e0d57bb38f44', self_declared_identity_cannot_alter_category_membership, deontological).
narrative_ontology:cs_reference_frame('a0996c91-1dc9-487c-a63f-e0d57bb38f44', chromosomal_anatomical_birth_marker_standard).
narrative_ontology:cs_drift_state('a0996c91-1dc9-487c-a63f-e0d57bb38f44', contemporary_medical_and_legal_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a0996c91-1dc9-487c-a63f-e0d57bb38f44', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, sex_segregated_space_administrators).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cis_women_advocacy_organizations).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, gender_critical_policy_coalitions).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, intersex_individuals).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, gender_nonconforming_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are categorically excluded from the 'woman' classification under this reading regardless of transition status, legal recognition, or duration of lived social role. This determines access to sex-segregated facilities, sports categories, prisons, shelters, and legal documentation. There is no exit from the birth-assigned category under this reading's terms — the marker (chromosomal/anatomical at birth) is treated as permanently fixed, so the only 'exit' would be abandoning the identity itself.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, trans_women, payer,
    powerless, biographical, identity_locked, national).

% Fall outside the binary the reading requires; classification decisions made about them at birth or in infancy (often surgically enforced) are treated as the immutable marker even when the biological reality was itself ambiguous or medically assigned. They have no voice in the marker that determines their lifelong categorical assignment.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, intersex_individuals, payer,
    powerless, biographical, trapped, national).

% Are sorted by birth marker regardless of presentation or role, which can create friction and scrutiny in single-sex spaces and documentation checks even for those who do not identify as trans.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_nonconforming_people, payer,
    powerless, biographical, constrained, national).

% Advocate for the biological-marker boundary as protection of sex-based rights (domestic violence shelters, sports categories, prisons) won through decades of feminist organizing. They frame trans-inclusive readings as diluting hard-won sex-based protections and organize legally and politically to maintain the boundary. They are not without genuine grievance: some report specific safety and fairness harms attributable to mixed-category admission, which this reading treats as decisive.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cis_women_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__biological_sex_reading, cis_women_advocacy_organizations, agenda_setter).

% Prisons, shelters, sports federations, and school administrators who must operationalize a category boundary somewhere and choose the birth-marker line as administratively legible and legally defensible. They enforce documentation checks, medical verification requirements, and exclusion criteria, and benefit from a clear bright-line rule that reduces case-by-case litigation exposure.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, sex_segregated_space_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Legislators and lobbying groups that codify the biological-marker standard into law (bathroom bills, sports bans, prison policy). They gain political capital, donor support, and electoral mobilization from maintaining and litigating the boundary.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_critical_policy_coalitions, beneficiary,
    organized, generational, mobile, national).

% Endocrinologists, geneticists, and legal scholars who testify on the actual variability of chromosomal and anatomical presentation (intersex variation, mosaicism, hormonal profiles) and on how administrative sex-marking has historically been revised. Their evidence complicates the 'immutable, binary' premise the reading depends on.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, medical_and_legal_professionals, observer,
    institutional, generational, analytical, national).

% Argue the biological-marker reading is not a neutral description of nature but a policy choice that could be drawn differently, and that it functions to exclude and stigmatize. Present in adjacent public debate but structurally excluded from the categorical determination itself — the marker is treated as pre-political, so their objection is framed as a category error rather than a policy disagreement to be weighed.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, trans_rights_organizations, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__biological_sex_reading, diffuse).
narrative_ontology:fixing_cost_class(gendered_category_membership__biological_sex_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administratively legible rule for allocating access to sex-segregated resources (shelters, prisons, sports, some medical and legal contexts) without requiring case-by-case adjudication of contested identity claims.
% TRANSFER_FUNCTION: Moves categorical membership, legal recognition, and access to segregated spaces away from self-identified and socially-recognized trans women and toward the birth-assigned category, while transferring a sense of protected boundary integrity to cis women's advocacy organizations and administrative certainty to space administrators.
% ABSENT_VOICES: Trans women and intersex individuals are the parties whose categorization is being decided, but the reading treats their input as inadmissible in principle — the marker is defined as pre-social and thus not something their testimony, identity, or lived history can speak to. They are present in public debate but structurally locked out of the determination itself.
% DISAPPEARANCE_RATIONALE: If the biological-marker reading were abandoned as the operative rule, access criteria for sex-segregated facilities, sports categories, prison assignment, and legal sex markers would shift substantially — administrators would need new criteria (self-identification, medical transition status, or social role), trans women would gain categorical access currently denied, and the coalitions currently organized around defending the biological boundary would lose their primary organizing object.
% FOUNDING_PROBLEM: Historically, sex-segregated spaces and sex-based legal protections (maternity provisions, domestic violence shelters, sports categories) were built to address material vulnerabilities specific to people categorized female at birth — reproductive vulnerability, physical strength disparities in athletics, and documented patterns of male violence.
% FOUNDING_PROBLEM_CORROBORATION: Some medical and legal professionals outside both advocacy camps corroborate that certain original concerns (average physical strength differentials in sport, documented patterns of sex-based violence) remain empirically live; other medical researchers and legal historians outside the benefiting coalitions note that the birth-marker line was not the only administratively workable solution even at founding, and that many jurisdictions have already revised sex-marker rules without the harms the reading predicts, suggesting the 'immutable marker' framing is a policy choice retrofitted with naturalized justification rather than a discovered necessity.
narrative_ontology:disappearance_verdict(gendered_category_membership__biological_sex_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__biological_sex_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__biological_sex_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gendered_category_membership__biological_sex_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__biological_sex_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.68 by interval end: the reading does not merely coordinate access to segregated spaces, it also imposes identity-suppression costs on trans women and intersex people that are structurally decoupled from any service they receive in return — they bear the categorical exclusion without compensating benefit. Suppression is high (0.71) because the boundary depends on active enforcement: documentation checks, medical verification requirements, legislative codification, and litigation to prevent boundary erosion. Theater ratio is comparatively low (0.28) because the enforcement mechanisms (facility access rules, sports eligibility checks, legal sex-marker requirements) are functionally real, not merely symbolic — though it is rising as the biological premise faces increasing empirical challenge and enforcement shifts toward defending the line rather than serving the originally-claimed function. Accessibility collapse is moderate-high (0.62): once the birth-marker rule is adopted administratively, alternative criteria (self-identification, medical transition, social role) become difficult to introduce without wholesale policy reversal. Resistance is high (0.74) — trans rights organizations, some medical bodies, and international human rights frameworks actively contest the boundary.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats, this reading functions as coordination: a stable, litigation-resistant rule for allocating scarce segregated resources, grounded in what those seats experience as biological fact. From the trans women and intersex seats, the identical rule operates as enforced exclusion from a category that materially and psychologically constitutes their claimed selfhood — the same administrative bright-line is lived as categorical erasure. The engine's per-seat computation should reflect this divergence: a tangled_rope classification requires exactly this — genuine coordination benefit for the beneficiary/agenda-setter seats and asymmetric extraction for the payer seats, held together by active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women and intersex individuals are declared victims and sit near the full-target end of directionality: the constraint's central operation is denying them the category, and their exit options are identity_locked or trapped — there is no meaningful way to exit the birth-marker classification within this reading's own terms. Cis women's advocacy organizations and gender-critical policy coalitions are declared beneficiaries: they receive a bounded, legally defensible category and organizing object, and their exit options are mobile (they are not trapped by the constraint; they help construct it). Sex-segregated space administrators are agenda-setters who benefit from administrative simplicity without necessarily sharing the advocacy coalitions' stakes — an override was considered but not applied since the derived directionality already captures their institutional beneficiary-adjacent position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting reproductive-vulnerability-specific and violence-pattern-specific spaces) is authored as contested rather than resolved: some of the original material concerns (average strength differentials, documented violence patterns) may remain partially live, which prevents this from being a clean mandatrophy case where the mandate has purely outlived its function. But the specific mechanism chosen to solve that problem — an immutable birth-marker line rather than alternative administrable criteria — is not required by the founding problem itself, and evidence from professionals outside the benefiting coalitions suggests the birth-marker solution was a policy choice, not a discovered necessity. This is precisely the ambiguity the tangled_rope classification is built to hold: real coordination function, real extraction, and a contested founding problem that neither wholly vindicates nor wholly discredits the arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_marker_naturalness_vs_construction,
    'Is the chromosomal/anatomical-at-birth marker a discovered natural boundary that pre-exists policy, or is it one of several administratively possible criteria that has been retrofitted with naturalized justification?',
    'Historical and comparative-jurisdictional analysis: examine whether legal sex-marker rules have in fact been revised across jurisdictions without the harms this reading predicts, and whether the chromosomal/anatomical variability documented in intersex populations is compatible with treating the marker as a clean binary.',
    'If the marker is substantially constructed/policy-chosen rather than a clean natural fact, the reading''s claim to be tracking biological reality rather than making a contestable political choice weakens substantially, which would push the classification further toward snare (extraction dressed as natural necessity) rather than tangled_rope (genuine coordination plus extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_marker_naturalness_vs_construction, conceptual, 'Whether the biological marker is discovered nature or constructed policy criterion.').

omega_variable(
    sibling_reading_structural_delta,
    'How would the beneficiary/victim structure invert or redistribute under the gender_identity_reading or social_role_reading of the same kernel?',
    'Author and compare the sibling constraint stories (gender_identity_reading, social_role_reading) with their own ε, beneficiary, and victim declarations; examine where the victim sets overlap, diverge, or invert (e.g., cis women''s advocacy organizations may become payers under gender_identity_reading if they experience category access as diluted).',
    'This does not change THIS story''s classification — ε-invariance requires this reading to stand on its own — but it clarifies that the kernel-level contest is precisely about which reading''s beneficiary/victim structure the governing institution adopts, and that switching readings is a switch of constraints, not a re-measurement of one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'The kernel-level contest is a choice between structurally distinct constraints, not a single constraint measured differently.').

omega_variable(
    safety_and_fairness_grievance_validity,
    'To what extent are the specific safety and fairness harms reported by cis women''s advocacy organizations (in shelters, prisons, sports) attributable to the categorical boundary itself versus to independent, addressable factors (facility design, screening procedures, sport-specific eligibility criteria)?',
    'Empirical review of documented incidents and comparative outcomes in jurisdictions with different categorical rules, disaggregating harms attributable to the presence of trans women per se from harms attributable to inadequate procedural safeguards.',
    'If harms are substantially attributable to procedural gaps rather than categorical inclusion itself, the coordination function claimed by the beneficiary coalitions weakens, pushing the classification toward snare; if harms are substantially attributable to the categorical question itself, the coordination function is more genuine, supporting the tangled_rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safety_and_fairness_grievance_validity, empirical, 'Whether cited safety/fairness harms are caused by categorical inclusion or by separable procedural factors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__biological_sex_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gend_tr_t4, gendered_category_membership__biological_sex_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(gend_tr_t8, gendered_category_membership__biological_sex_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(gend_tr_t12, gendered_category_membership__biological_sex_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(gend_tr_t16, gendered_category_membership__biological_sex_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__biological_sex_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__biological_sex_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gend_be_t4, gendered_category_membership__biological_sex_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(gend_be_t8, gendered_category_membership__biological_sex_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(gend_be_t12, gendered_category_membership__biological_sex_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(gend_be_t16, gendered_category_membership__biological_sex_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__biological_sex_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__biological_sex_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gend_su_t4, gendered_category_membership__biological_sex_reading, suppression_requirement, 4, 0.53).
narrative_ontology:measurement(gend_su_t8, gendered_category_membership__biological_sex_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(gend_su_t12, gendered_category_membership__biological_sex_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(gend_su_t16, gendered_category_membership__biological_sex_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__biological_sex_reading, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__biological_sex_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__biological_sex_reading, 0.08).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__gender_identity_reading).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__social_role_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'gendered category membership' per the ε-invariance principle: biological_sex_reading (this story), gender_identity_reading, and social_role_reading. Each reading grounds category membership in a structurally distinct criterion, produces a different beneficiary/victim structure, and carries its own ε. They are linked here rather than merged because measuring 'gendered category membership' by the biological criterion versus the identity criterion versus the social-role criterion yields substantially different extraction and suppression profiles — exactly the signal that indicates decomposition rather than a single observer-relative constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
