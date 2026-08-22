% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__developmental_potentiality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__developmental_potentiality_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: legal_personhood_boundary__developmental_potentiality_reading
 *   human_readable: Developmental Potentiality Personhood Reading
 *   domain: legal/philosophical/rights
 *
 * SUMMARY:
 *   This constraint story instantiates the developmental_potentiality_reading
 *   of the contested legal_personhood_boundary kernel. It treats personhood
 *   and full rights-bearing status as commencing at biological conception,
 *   thereby inserting the fetus into the rights-holder set and subordinating
 *   the pregnant person's autonomy to fetal protection claims. The state
 *   acquires sweeping enforcement authority over pregnancy outcomes, medical
 *   practice, and intimate bodily decisions. The constraint is claimed as a
 *   natural-law or constitutional necessity by its proponents; the authored
 *   metrics describe an actively enforced extraction of autonomy from
 *   pregnant persons and medical providers. This is a kernel reading: sibling
 *   readings (restrictive_anthropocentric_reading,
 *   functional_capacity_reading) would remove the fetus from the
 *   rights-bearer set at conception and reallocate the beneficiary/victim
 *   structure entirely.
 *
 * KEY AGENTS:
 *   - pregnant_persons (powerless/trapped): primary targets â bear extraction through subordinated autonomy and forced pregnancy continuation.
 *   - state_enforcement_apparatus (institutional/analytical): agenda setter â administers criminal statutes and surveillance over reproduction.
 *   - fetal_rights_advocacy_institutions (organized/mobile): beneficiaries â collect institutional influence from the legal enshrinement of fetal personhood.
 *   - medical_providers (moderate/constrained): secondary targets â face criminal liability and ethical coercion to serve as enforcement deputies.
 *   - constitutional_interpreters_judiciary (institutional/analytical): observers â adjudicate the boundary's permissibility without direct enforcement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, 0.82).
domain_priors:suppression_score(legal_personhood_boundary__developmental_potentiality_reading, 0.85).
domain_priors:theater_ratio(legal_personhood_boundary__developmental_potentiality_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__developmental_potentiality_reading, snare).
narrative_ontology:human_readable(legal_personhood_boundary__developmental_potentiality_reading, "Developmental Potentiality Personhood Reading").
narrative_ontology:topic_domain(legal_personhood_boundary__developmental_potentiality_reading, "legal/philosophical/rights").

domain_priors:requires_active_enforcement(legal_personhood_boundary__developmental_potentiality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__developmental_potentiality_reading, '02cb340c-b8d1-4c79-945c-9c249535e778').
narrative_ontology:cs_kernel_codification('02cb340c-b8d1-4c79-945c-9c249535e778', fixed_text).
narrative_ontology:cs_authority_grounding('02cb340c-b8d1-4c79-945c-9c249535e778', lineage).
narrative_ontology:cs_interpretation_layer_present('02cb340c-b8d1-4c79-945c-9c249535e778').
narrative_ontology:cs_reading_relation('02cb340c-b8d1-4c79-945c-9c249535e778', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('02cb340c-b8d1-4c79-945c-9c249535e778', legal_personhood_boundary__functional_capacity_reading, forecloses).
narrative_ontology:cs_axiom('02cb340c-b8d1-4c79-945c-9c249535e778', foundational, conception_confers_full_personhood).
narrative_ontology:cs_axiom_status(conception_confers_full_personhood, holdable).
narrative_ontology:cs_axiom_grounding('02cb340c-b8d1-4c79-945c-9c249535e778', conception_confers_full_personhood, deontological).
narrative_ontology:cs_axiom('02cb340c-b8d1-4c79-945c-9c249535e778', foundational, developmental_continuity_mandates_equal_protection).
narrative_ontology:cs_axiom_status(developmental_continuity_mandates_equal_protection, holdable).
narrative_ontology:cs_axiom_grounding('02cb340c-b8d1-4c79-945c-9c249535e778', developmental_continuity_mandates_equal_protection, deontological).
narrative_ontology:cs_reference_frame('02cb340c-b8d1-4c79-945c-9c249535e778', conception_as_rights_origin).
narrative_ontology:cs_drift_state('02cb340c-b8d1-4c79-945c-9c249535e778', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('02cb340c-b8d1-4c79-945c-9c249535e778', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, fetal_rights_advocacy_institutions).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_apparatus).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, medical_providers).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, conception_personhood_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the full cost of the constraint: autonomy over reproduction and bodily integrity is subordinated to fetal rights claims. Subject to state surveillance of pregnancy outcomes, criminal and civil penalties for seeking prohibited procedures, and geographic barriers to legal termination. Exit requires leaving the jurisdiction or accepting forced pregnancy continuation.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons, payer,
    powerless, immediate, trapped, national).

% Administers and enforces the personhood boundary through criminal statutes, medical licensing boards, and surveillance of reproductive healthcare. Acquires expanded authority over intimate medical decisions and the power to deputize medical providers as enforcement agents.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Collect institutional influence, funding, and policy access from the legal enshrinement of fetal personhood. Their organizational mission and donor base depend on maintaining the conception threshold as a non-negotiable rights boundary.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, fetal_rights_advocacy_institutions, beneficiary,
    organized, generational, mobile, national).

% Face criminal liability, loss of license, and professional sanctions for providing prohibited procedures. Medical ethics around patient autonomy are subordinated to legal personhood claims. Must choose between compliance, civil disobedience, or relocating practice.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, medical_providers, payer,
    moderate, biographical, constrained, regional).

% Adjudicate disputes over the personhood boundary and the scope of state authority to regulate reproduction. Their interpretive framework determines whether the constraint is constitutionally permissible, but they do not directly enforce pregnancy outcomes.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, constitutional_interpreters_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_apparatus).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__developmental_potentiality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to resolve legal and moral uncertainty about the onset of rights-bearing status by establishing conception as a bright-line threshold, providing a uniform standard for the protection of human life from its earliest stage.
% TRANSFER_FUNCTION: Moves autonomy, medical decision-making authority, and bodily integrity from pregnant persons to state enforcement apparatus and judicial oversight, under the premise of protecting fetal rights from conception.
% ABSENT_VOICES: Pregnant persons seeking termination are routinely excluded from legislative hearings; medical ethicists emphasizing bodily autonomy are sidelined in favor of moral philosophers and religious authorities; the functional_capacity_reading and its cross-species cognitive threshold are structurally excluded from the policy framework.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared, criminal abortion statutes would lose their constitutional and moral grounding, state surveillance of pregnancy outcomes would contract, medical providers would no longer face criminal liability for standard care, and the legal apparatus governing reproduction would reorganize around maternal autonomy or alternative capacity-based thresholds.
% FOUNDING_PROBLEM: Uncertainty and dispute over the moral and legal beginning of human personhood, and the perceived failure of existing frameworks to protect prenatal human life from elective termination.
% FOUNDING_PROBLEM_CORROBORATION: Anti-abortion advocacy movements and some religious authorities attest the problem is live and unsolved. Medical associations, reproductive rights organizations, and constitutional scholars adhering to the functional_capacity_reading attest the problem is either solved by autonomy-based frameworks or is a moral construction rather than a genuine coordination failure; no neutral corroboration exists outside the polarized dispute.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__developmental_potentiality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__developmental_potentiality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__developmental_potentiality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legal_personhood_boundary__developmental_potentiality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__developmental_potentiality_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__developmental_potentiality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__developmental_potentiality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint transfers core autonomy and bodily integrity from pregnant persons to state control. Suppression is very high (0.85) because persistence depends on criminalizing abortion, suppressing alternative personhood frameworks, and surveilling medical outcomes. Accessibility collapse is high (0.80) because legal prohibition and geographic barriers collapse alternatives once the constraint is understood. Resistance is substantial (0.75) because the constraint meets sustained legal challenge, civil disobedience, and electoral opposition. Theater ratio is moderate (0.45): the 'protection of innocent life' narrative is sincerely held by many advocates, but a significant share of enforcement activity functions to maintain state authority and political coalitions rather than to protect recognized persons. The measurement series share a single time grid so temporal analysis samples all metrics at the same points.
 *
 * PERSPECTIVAL GAP:
 *   The pregnant person experiences the constraint as pure extraction and coercion; the fetal rights institution experiences it as moral vindication and organizational mission fulfillment; the state experiences it as legitimate authority expansion. The engine will compute snare or near-snare for the payer seats and a markedly different profile for the beneficiary/agenda-setter seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Pregnant persons are full targets (d near 1.0) because the constraint extracts autonomy and imposes physical costs directly on them. Medical providers are near-target (d ~0.75) because they bear professional and criminal liability. State enforcement sits near the beneficiary end (d near 0.0) because the constraint expands its authority and budget. Fetal rights advocacy institutions are beneficiaries (d near 0.0) because the constraint is the source of their institutional influence.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this arrangement as coordination because there is no mutual benefit or collective-action problem being solved. The 'coordination' is the uniform imposition of a contested moral view. The costs are borne entirely by pregnant persons and medical providers, while the benefits accrue as authority and influence to state and advocacy institutions. The R5 genealogy (contested founding problem) combined with world_rearranges disappearance indicates the constraint persists through active enforcement and beneficiary investment, not through problem-solving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the developmental_potentiality_reading of the legal_personhood_boundary kernel. How would classification change if the functional_capacity_reading (cognitive capacity threshold) or restrictive_anthropocentric_reading (born humans only) were adopted instead?',
    'Comparative analysis of sibling constraint stories in the same kernel family; empirical assessment of which reading is instantiated in positive law.',
    'Adopting a sibling reading would remove pregnant persons from the victim set and eliminate state enforcement authority over pregnancy outcomes, likely reclassifying the constraint as a mountain or rope depending on the replacement framework''s extraction profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading contest and structural delta across sibling personhood boundaries.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression against pregnant persons primarily structural (criminal penalties, geographic barriers, provider shortages) or internalized (shame, moral guilt, community ostracism)?',
    'Post-exit trajectory analysis: if demand for prohibited procedures persists at high rates in jurisdictions where legal barriers are removed, suppression was primarily structural; if demand remains suppressed after legalization, internalized factors dominate.',
    'If internalized, effective suppression exceeds the structural measure and the constraint operates partly as a cognitive snare; if structural, the constraint is a pure institutional snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in reproductive autonomy constraints.').

omega_variable(
    natural_law_vs_constructed_status,
    'Is the conception threshold a discoverable natural-law fact or a normative construction projected onto biological events?',
    'Cross-cultural and cross-temporal legal anthropology: if personhood boundaries vary systematically with social structure rather than converging on conception, the threshold is constructed.',
    'If constructed, the constraint is a false-summit mountain or snare rather than a natural-law mountain; this shifts the directionality derivation and triggers false-summit detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_status, conceptual, 'Whether the personhood boundary is natural law or moral construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__developmental_potentiality_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lega_tr_t10, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(lega_tr_t20, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(lega_tr_t30, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(lega_tr_t50, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(lega_be_t10, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(lega_be_t20, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(lega_be_t30, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(lega_be_t50, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(lega_su_t10, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(lega_su_t20, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(lega_su_t30, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement(lega_su_t50, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
