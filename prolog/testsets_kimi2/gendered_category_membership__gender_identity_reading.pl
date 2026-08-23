% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__gender_identity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: gendered_category_membership__gender_identity_reading
 *   human_readable: Gender Category Membership via Subjective Self-Identification
 *   domain: social/political/bioethical
 *
 * SUMMARY:
 *   This constraint story instantiates the gender_identity_reading of the
 *   gendered_category_membership kernel. It models category membership
 *   grounded in subjective identity and self-declaration, where trans women
 *   are included in the 'woman' category via self-identification,
 *   sex-segregated spaces become gender-segregated, and cis women who resist
 *   are positioned as perpetrators of exclusion. Sibling readings â
 *   biological_sex_reading and social_role_reading â instantiate mutually
 *   exclusive grounds for the same kernel and are documented via network
 *   links and cs_structure.
 *
 * KEY AGENTS:
 *   - trans_women: Primary beneficiary (moderate/identity_locked) â gain inclusion via self-declaration and reduced gatekeeping
 *   - cis_women: Primary payer (organized/constrained) â bear costs of reclassified spaces and social sanction for asserting sex-based boundaries
 *   - institutional_adopters: Agenda setter (institutional/mobile) â administer and enforce self-identification policies
 *   - sex_based_advocates: Excluded voice (moderate/constrained) â contest the framework from outside institutional tables
 *   - bioethicist_observers: Analytical observer (analytical/analytical) â examine the ontological shift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, 0.55).
domain_priors:suppression_score(gendered_category_membership__gender_identity_reading, 0.6).
domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Gender Category Membership via Subjective Self-Identification").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social/political/bioethical").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, 'fdf9d6e7-9431-4b95-8680-3572ae724ec9').
narrative_ontology:cs_kernel_codification('fdf9d6e7-9431-4b95-8680-3572ae724ec9', distributed).
narrative_ontology:cs_authority_grounding('fdf9d6e7-9431-4b95-8680-3572ae724ec9', distributed).
narrative_ontology:cs_reading_relation('fdf9d6e7-9431-4b95-8680-3572ae724ec9', gendered_category_membership__biological_sex_reading, forecloses).
narrative_ontology:cs_reading_relation('fdf9d6e7-9431-4b95-8680-3572ae724ec9', gendered_category_membership__social_role_reading, influences).
narrative_ontology:cs_axiom('fdf9d6e7-9431-4b95-8680-3572ae724ec9', foundational, self_determination_of_gender_category).
narrative_ontology:cs_axiom_status(self_determination_of_gender_category, holdable).
narrative_ontology:cs_axiom_grounding('fdf9d6e7-9431-4b95-8680-3572ae724ec9', self_determination_of_gender_category, deontological).
narrative_ontology:cs_axiom('fdf9d6e7-9431-4b95-8680-3572ae724ec9', secondary, institutional_recognition_tracks_identity).
narrative_ontology:cs_axiom_status(institutional_recognition_tracks_identity, holdable).
narrative_ontology:cs_axiom_grounding('fdf9d6e7-9431-4b95-8680-3572ae724ec9', institutional_recognition_tracks_identity, conventional).
narrative_ontology:cs_reference_frame('fdf9d6e7-9431-4b95-8680-3572ae724ec9', identity_autonomy_framework).
narrative_ontology:cs_drift_state('fdf9d6e7-9431-4b95-8680-3572ae724ec9', contemporary_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fdf9d6e7-9431-4b95-8680-3572ae724ec9', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cis_women).
narrative_ontology:constraint_vindicates(gendered_category_membership__gender_identity_reading, gender_identity_autonomy).
narrative_ontology:constraint_vindicates(gendered_category_membership__gender_identity_reading, self_id_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Included in the 'woman' category via self-declaration without medical gatekeeping requirements. Benefit from legal recognition and access to gender-segregated spaces based on subjective identity. Their structural position is bound to the identity category they declare; exit would mean returning to medicalized or bureaucratic gatekeeping for membership.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, trans_women, beneficiary,
    moderate, biographical, identity_locked, national).

% Bear the costs of sex-segregated spaces being reclassified as gender-segregated. Positioned as perpetrators of exclusion if they assert sex-based boundaries. Must accept self-identified women into previously sex-exclusive contexts or face social and institutional sanction. Cannot exit the category system itself.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cis_women, payer,
    organized, biographical, constrained, national).

% State agencies, employers, and organizations that implement self-identification policies for documentation, facilities, and programs. They define administrative criteria for category membership and enforce compliance through anti-discrimination frameworks and policy guidelines.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, institutional_adopters, agenda_setter,
    institutional, generational, mobile, national).

% Advocate for sex-based category boundaries and the retention of sex-segregated provisions. Structurally excluded from institutional policy formulation and publicly characterized as exclusionary or discriminatory when contesting self-identification frameworks.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, sex_based_advocates, excluded,
    moderate, biographical, constrained, national).

% Academic analysts examining the ontological shift from biological-sex to identity-based category membership. Document the boundary renegotiation and its distributional effects without being directly governed by the constraint.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, bioethicist_observers, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(gendered_category_membership__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves who belongs in gendered social and legal categories without requiring medical intervention or external validation, enabling self-determination of identity and reducing pathologizing gatekeeping.
% TRANSFER_FUNCTION: Moves the authority to assign category membership from biological or medical adjudication to individual self-declaration, and moves the costs of boundary maintenance â loss of sex-segregated spaces and social sanction for resistance â to cis women.
% ABSENT_VOICES: Sex-based advocates and some cis women's groups are structurally excluded from policy formulation; their objections are preemptively framed as exclusionary. Medical gatekeepers who previously controlled access to categories lose standing under self-identification frameworks.
% DISAPPEARANCE_RATIONALE: If self-identification based category membership vanished overnight, sex-segregated spaces would revert to sex-based boundaries, legal documentation would require biological or medical evidence again, and trans women would lose current pathways to recognition â the social and institutional landscape would reorganize around biological or performative criteria.
% FOUNDING_PROBLEM: Medicalized and bureaucratic gatekeeping for gender category membership was burdensome, pathologizing, and excluded many trans people from legal and social recognition.
% FOUNDING_PROBLEM_CORROBORATION: Trans advocacy organizations attest the founding problem remains live due to ongoing barriers. Gender-critical feminists and some medical professionals attest the self-identification solution has created new harms for cis women; bioethics literature from outside both advocacy camps documents the trade-off between autonomy and sex-based protections.
narrative_ontology:disappearance_verdict(gendered_category_membership__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gendered_category_membership__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__gender_identity_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.55) because the constraint shifts tangible costs â loss of sex-segregated spaces, social sanction for boundary assertion â onto cis women while delivering coordination benefits (inclusion, reduced medical gatekeeping) to trans women. Suppression (0.60) reflects active social and institutional enforcement against sex-based categorization dissent. Theater ratio (0.42) captures the performative dimension of institutional inclusivity signaling that sometimes exceeds functional policy change. Accessibility collapse (0.65) indicates that once self-identification is adopted, sex-based alternatives become socially and institutionally inaccessible in governed contexts. Resistance (0.75) is high because the constraint faces sustained organized opposition from sex-based advocates and some cis women's groups. The claim/metric independence is maintained: the reading is claimed as tangled_rope while metrics are authored descriptively.
 *
 * PERSPECTIVAL GAP:
 *   The trans women seat experiences the constraint as affirming coordination that reduces harmful gatekeeping. The cis women seat experiences the same structural arrangement as extraction that dissolves sex-based boundaries and imposes social costs for resistance. The institutional adopter seat experiences it as a manageable policy framework with political benefits. The engine computes this divergence from identical structural data via directionality and power/exit modulation.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women are declared beneficiaries (low directionalities, low effective extraction): the constraint subsidizes their category membership by removing medical gatekeeping. Cis women are declared payers (high directionalities, high effective extraction): the constraint extracts through reclassified spaces and imposed perpetrator framing. Institutional adopters sit near the agenda-setter middle: they administer the constraint but do not personally collect the extraction. Sex-based advocates are excluded from the derivation chain by their excluded role. The directionality derivation follows from these structural declarations without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents misreading the constraint as pure coordination (rope) by insisting on the victim declaration (cis women bear asymmetric costs) and active enforcement (the framework requires ongoing institutional maintenance to hold against sex-based alternatives). It prevents misreading as pure extraction (snare) by preserving the genuine coordination function for trans women. If the founding problem (medicalized gatekeeping) were dead and only the extraction remained, the constraint would drift toward snare or piton; the measurements show extraction accumulation but not yet severance from the coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_id_boundary_stability,
    'Can a category boundary grounded solely in subjective self-declaration remain stable against strategic entry and internal disagreement, or does it require escalating enforcement to maintain coherence?',
    'Comparative analysis of jurisdictions with varying durations of self-identification policy to measure boundary dispute rates and policy amendment frequency.',
    'If boundaries destabilize, extraction rises as enforcement intensifies; if stable, the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_id_boundary_stability, conceptual, 'Whether self-declared category boundaries are structurally stable').

omega_variable(
    cis_woman_extraction_or_reciprocity,
    'Do cis women experience net extraction from the shift to gender-segregated spaces, or is the rearrangement reciprocally cost-neutral with only redistributed boundaries?',
    'Empirical tracking of sex-based service utilization, safety reporting, and social sanctioning rates pre- and post-policy adoption.',
    'Confirmed net extraction would strengthen tangled_rope classification; cost-neutrality would push toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cis_woman_extraction_or_reciprocity, empirical, 'Whether cis women bear net costs or reciprocal redistribution').

omega_variable(
    kernel_committer_position,
    'This constraint is one reading of a contested kernel. Which structural feature determines whether gender_identity_reading or biological_sex_reading governs a given institutional context?',
    'Mapping institutional domains (sports, prisons, healthcare, shelters) to which reading''s axioms are operative in each.',
    'Determines whether the kernel produces fragmented governance (different readings in different contexts) or convergence on one reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_position, conceptual, 'What determines which kernel reading governs which institutional domain').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of sex-based advocacy structural (institutional exclusion, policy barriers) or internalized (self-censorship due to social stigma)?',
    'Post-exit suppression trajectory: if advocates continue to self-censor after institutional barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__gender_identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gender_id_tr_t0, gendered_category_membership__gender_identity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gender_id_tr_t4, gendered_category_membership__gender_identity_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(gender_id_tr_t8, gendered_category_membership__gender_identity_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(gender_id_tr_t12, gendered_category_membership__gender_identity_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(gender_id_tr_t16, gendered_category_membership__gender_identity_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(gender_id_tr_t20, gendered_category_membership__gender_identity_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(gender_id_be_t0, gendered_category_membership__gender_identity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gender_id_be_t4, gendered_category_membership__gender_identity_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(gender_id_be_t8, gendered_category_membership__gender_identity_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(gender_id_be_t12, gendered_category_membership__gender_identity_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(gender_id_be_t16, gendered_category_membership__gender_identity_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(gender_id_be_t20, gendered_category_membership__gender_identity_reading, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(gender_id_su_t0, gendered_category_membership__gender_identity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(gender_id_su_t4, gendered_category_membership__gender_identity_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(gender_id_su_t8, gendered_category_membership__gender_identity_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(gender_id_su_t12, gendered_category_membership__gender_identity_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(gender_id_su_t16, gendered_category_membership__gender_identity_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(gender_id_su_t20, gendered_category_membership__gender_identity_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, social_role_reading).

% DUAL FORMULATION NOTE:
% The gendered_category_membership kernel decomposes into three structurally distinct readings. biological_sex_reading treats membership as grounded in immutable biological markers (low epsilon, mountain-like from biological realist seats). gender_identity_reading (this file) treats membership as grounded in subjective self-declaration (moderate epsilon, tangled rope). social_role_reading treats membership as grounded in sustained performance and recognition (moderate epsilon, distinct extraction profile). They form a constraint family linked by mutual exclusivity as category grounds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
