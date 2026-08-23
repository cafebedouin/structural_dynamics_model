% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__user_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__user_centric_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__user_centric_reading
 *   human_readable: Fair Use Four-Factor Test (User-Centric Reading)
 *   domain: legal/intellectual_property
 *
 * SUMMARY:
 *   This constraint instantiates the user-centric reading of the fair use
 *   four-factor test kernel: fair use is an affirmative user right, and the
 *   four factors are weighed primarily to preserve public access and cultural
 *   production. It is one of three live readings of the same statutory text
 *   (17 U.S.C. Â§ 107), alongside a creator-centric reading that treats fair
 *   use as a narrow exception to property rights, and a transformative-use
 *   reading that elevates transformativeness above market harm. The
 *   user-centric reading sees the doctrine as a low-extraction coordination
 *   mechanism that protects education, criticism, and follow-on creativity at
 *   the expense of reduced licensing revenue for rights holders.
 *
 * KEY AGENTS:
 *   - public_educational_users: Primary beneficiary (organized/constrained) â gains access to culture and knowledge without permission
 *   - rights_holders: Primary target (powerful/constrained) â bears the cost of uncompensated fair uses
 *   - federal_judiciary: Agenda setter (institutional/analytical) â interprets and enforces the four-factor framework
 *   - independent_creators: Excluded voice (moderate/constrained) â hybrid creator-user interests absent from the doctrinal binary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, 0.25).
domain_priors:suppression_score(fair_use_four_factor_test__user_centric_reading, 0.45).
domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__user_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__user_centric_reading, "Fair Use Four-Factor Test (User-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__user_centric_reading, "legal/intellectual_property").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__user_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, '8b85a0bc-2406-4594-91fd-7a46b6b074b0').
narrative_ontology:cs_kernel_codification('8b85a0bc-2406-4594-91fd-7a46b6b074b0', fixed_text).
narrative_ontology:cs_authority_grounding('8b85a0bc-2406-4594-91fd-7a46b6b074b0', lineage).
narrative_ontology:cs_interpretation_layer_present('8b85a0bc-2406-4594-91fd-7a46b6b074b0').
narrative_ontology:cs_reading_relation('8b85a0bc-2406-4594-91fd-7a46b6b074b0', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b85a0bc-2406-4594-91fd-7a46b6b074b0', fair_use_four_factor_test__transformative_use_reading, influences).
narrative_ontology:cs_axiom('8b85a0bc-2406-4594-91fd-7a46b6b074b0', foundational, fair_use_as_affirmative_user_right).
narrative_ontology:cs_axiom_status(fair_use_as_affirmative_user_right, holdable).
narrative_ontology:cs_axiom_grounding('8b85a0bc-2406-4594-91fd-7a46b6b074b0', fair_use_as_affirmative_user_right, deontological).
narrative_ontology:cs_axiom('8b85a0bc-2406-4594-91fd-7a46b6b074b0', foundational, copyright_limited_grant_public_benefit).
narrative_ontology:cs_axiom_status(copyright_limited_grant_public_benefit, holdable).
narrative_ontology:cs_axiom_grounding('8b85a0bc-2406-4594-91fd-7a46b6b074b0', copyright_limited_grant_public_benefit, conventional).
narrative_ontology:cs_reference_frame('8b85a0bc-2406-4594-91fd-7a46b6b074b0', limited_grant_public_benefit).
narrative_ontology:cs_drift_state('8b85a0bc-2406-4594-91fd-7a46b6b074b0', digital_copyright_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8b85a0bc-2406-4594-91fd-7a46b6b074b0', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, public_educational_users).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, rights_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rely on the fair use doctrine to access, teach, critique, and transform copyrighted material without negotiating individual licenses or paying permissions fees. Their capacity to participate in culture and education depends on judicial willingness to weigh the four statutory factors toward public access.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, public_educational_users, beneficiary,
    organized, generational, constrained, national).

% Hold statutorily granted exclusive rights that are limited by judicial application of the fair use test. They bear the economic cost of uncompensated uses that courts deem fair, and they resist the doctrine through litigation, legislative lobbying, and rights-management technology designed to shrink the space of unauthorized use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, rights_holders, payer,
    powerful, biographical, constrained, national).

% Interprets and applies the four statutory factors in individual cases, setting precedents that expand or contract the scope of fair use. Maintains the doctrine through active adjudication and appellate review, absorbing social and technological change into the interpretive framework without statutory amendment.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Are simultaneously creators and users of copyrighted material, but lack the resources to litigate fair use claims or to participate in the policy conversations dominated by institutional rights holders and large educational entities. Their hybrid interests are poorly captured by the binary beneficiary-victim framing of the doctrine.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, independent_creators, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances copyright exclusivity against the need for public access to culture, solving the collective-action problem of clearing permissions for education, criticism, and follow-on creativity by establishing a default permission-free zone for certain socially valuable uses.
% TRANSFER_FUNCTION: Moves limited use privileges from the exclusive control of rights holders to the public and educational sectors, transferring the economic value of those uses from potential licensing revenue to uncompensated public access.
% ABSENT_VOICES: Independent creators who both make and reuse content, and advocates for open licensing regimes such as Creative Commons, are structurally underrepresented in the judicial four-factor framework, which presumes an all-rights-reserved baseline and litigates around it.
% DISAPPEARANCE_RATIONALE: If the fair use safety valve vanished overnight, educational institutions would face prohibitive licensing costs, documentary and critical practices would require extensive clearances, and a permissions culture would chill spontaneous cultural production, political speech, and scientific discourse.
% FOUNDING_PROBLEM: Overly broad copyright grants in the mid-twentieth century threatened to suppress education, criticism, and follow-on creativity by requiring permission and payment for every use of a copyrighted work.
% FOUNDING_PROBLEM_CORROBORATION: Librarians, educators, and documentary filmmakers attest to the ongoing need for fair use from outside the rights-holder set; rights holders and their trade associations contest that the problem is overstated and that market licensing can substitute for the doctrine.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__user_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__user_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__user_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_four_factor_test__user_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__user_centric_reading, 0.25, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__user_centric_reading_tests).
:- end_tests(fair_use_four_factor_test__user_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.25) because the user-centric reading frames the doctrine as primarily permissive and protective of access, not as a rent-extraction mechanism. Suppression is moderate (0.45): rights holders actively litigate to chill uncompensated uses, and the indeterminacy of the four-factor test itself suppresses some socially beneficial uses that cannot risk liability. Theater ratio is moderate (0.40): factor balancing involves substantial judicial performance and outcome uncertainty that can obscure the doctrine's protective function. Resistance is high (0.60) because content industries have sustained legislative and litigation campaigns to narrow the doctrine. Accessibility collapse is moderate (0.35): alternatives such as licensing and permission-based clearance exist but are costly and friction-laden; understanding the doctrine opens options rather than collapsing them, though legal uncertainty keeps many alternatives latent.
 *
 * PERSPECTIVAL GAP:
 *   From the rights-holder seat, the constraint operates as a forced subsidy: they lose exclusivity and licensing revenue to benefit users who do not pay. From the public-educational seat, the constraint is a necessary bulwark against overreaching copyright that would otherwise commodify every cultural interaction. The federal judiciary experiences the constraint as a doctrinal balancing act with no direct financial stake. The engine will compute different per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Public_educational_users are declared beneficiaries with constrained exit, yielding a low directionality value: the constraint subsidizes their access. Rights_holders are declared victims (payers) with constrained exit, yielding a high directionality value: the constraint extracts from their exclusivity. The federal_judiciary sits near symmetric because it administers the framework without being a direct financial beneficiary or cost-bearer. Independent_creators are excluded from the conversation, so their structural relationship is not factored into the core directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both beneficiary and victim declarations: the public access function is real coordination, while the rights-holder cost is real asymmetric extraction. A pure coordination reading (rope) would ignore the uncompensated transfer; a pure extraction reading (snare) would ignore the genuine social value of permission-free education and criticism. The temporal measurements show theater rising over the interval, which flags the risk of Goodhart drift: as factor balancing becomes more performative, the doctrine could atrophy toward a piton where judicial theater substitutes for actual public access protection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_ambiguity,
    'Is the fair use doctrine structurally a user protection, a creator incentive preservation mechanism, or a transformativeness gate â and does the same four-factor test produce different epsilon values under each framing?',
    'Comparative doctrinal analysis of judicial outcomes across circuits and time periods to determine which reading has greater predictive power over case dispositions.',
    'If the user-centric reading has distinct predictive power, the kernel should be decomposed into separate constraints; if the readings are merely rhetorical dressing on identical outcomes, the epsilon divergence is illusory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the three readings of the fair use kernel represent structurally distinct constraints or rhetorical variants.').

omega_variable(
    market_harm_measurement,
    'Does fair use measurably reduce creator compensation, or do market segmentation, licensing friction, and non-payment by users explain observed revenue loss?',
    'Empirical economic studies comparing creator revenue trajectories in jurisdictions with broad fair use doctrines versus jurisdictions with narrow exceptions, controlling for market size and enforcement.',
    'If market harm is negligible, the victim classification of rights holders weakens and the constraint trends toward rope; if substantial, the tangled_rope classification strengthens and the user-centric reading''s low-epsilon claim becomes harder to sustain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_measurement, empirical, 'Whether uncompensated fair uses cause measurable economic harm to rights holders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__user_centric_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fair_tr_t9, fair_use_four_factor_test__user_centric_reading, theater_ratio, 9, 0.3).
narrative_ontology:measurement(fair_tr_t18, fair_use_four_factor_test__user_centric_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(fair_tr_t27, fair_use_four_factor_test__user_centric_reading, theater_ratio, 27, 0.42).
narrative_ontology:measurement(fair_tr_t36, fair_use_four_factor_test__user_centric_reading, theater_ratio, 36, 0.45).
narrative_ontology:measurement(fair_tr_t45, fair_use_four_factor_test__user_centric_reading, theater_ratio, 45, 0.4).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(fair_be_t9, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 9, 0.24).
narrative_ontology:measurement(fair_be_t18, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 18, 0.22).
narrative_ontology:measurement(fair_be_t27, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 27, 0.26).
narrative_ontology:measurement(fair_be_t36, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 36, 0.3).
narrative_ontology:measurement(fair_be_t45, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 45, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(fair_su_t9, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 9, 0.38).
narrative_ontology:measurement(fair_su_t18, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 18, 0.35).
narrative_ontology:measurement(fair_su_t27, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 27, 0.48).
narrative_ontology:measurement(fair_su_t36, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 36, 0.52).
narrative_ontology:measurement(fair_su_t45, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 45, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
