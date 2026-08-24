% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__guided_nationalism_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__guided_nationalism_reading
 *   human_readable: July Charter Islamic-Nationalist Sovereign Legitimacy (Guided Nationalism Reading)
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   The July Charter, ratified after a popular revolution, establishes an
 *   Islamic-nationalist framework where religious identity constitutes the
 *   ground of sovereign legitimacy. The charter's preamble declares that
 *   'sovereignty belongs to God alone' and that the state's legitimacy
 *   derives from implementing divine law as interpreted by the religious
 *   establishment. This reading — the guided_nationalism_reading — treats the
 *   charter as a genuine coordination mechanism that resolves the
 *   post-revolutionary legitimacy vacuum through culturally authentic means.
 *   The secular_democratic_reading and military_custodian_reading are sibling
 *   constraints from the same kernel (the charter text), each instantiating a
 *   different constraint with different beneficiary/victim structures. This
 *   story models ONLY the guided_nationalism_reading as a clean ε-invariant
 *   constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.72).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.78).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "July Charter Islamic-Nationalist Sovereign Legitimacy (Guided Nationalism Reading)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, '14de0fac-ca29-42ff-935b-403bf5a96bde').
narrative_ontology:cs_kernel_codification('14de0fac-ca29-42ff-935b-403bf5a96bde', formalized).
narrative_ontology:cs_authority_grounding('14de0fac-ca29-42ff-935b-403bf5a96bde', lineage).
narrative_ontology:cs_interpretation_layer_present('14de0fac-ca29-42ff-935b-403bf5a96bde').
narrative_ontology:cs_reading_relation('14de0fac-ca29-42ff-935b-403bf5a96bde', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('14de0fac-ca29-42ff-935b-403bf5a96bde', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('14de0fac-ca29-42ff-935b-403bf5a96bde', foundational, religious_identity_as_sovereign_ground).
narrative_ontology:cs_axiom_status(religious_identity_as_sovereign_ground, holdable).
narrative_ontology:cs_axiom_grounding('14de0fac-ca29-42ff-935b-403bf5a96bde', religious_identity_as_sovereign_ground, deontological).
narrative_ontology:cs_axiom('14de0fac-ca29-42ff-935b-403bf5a96bde', foundational, religious_law_supremacy_over_secular_legislation).
narrative_ontology:cs_axiom_status(religious_law_supremacy_over_secular_legislation, holdable).
narrative_ontology:cs_axiom_grounding('14de0fac-ca29-42ff-935b-403bf5a96bde', religious_law_supremacy_over_secular_legislation, conventional).
narrative_ontology:cs_axiom('14de0fac-ca29-42ff-935b-403bf5a96bde', secondary, religious_establishment_as_constitutional_interpreter).
narrative_ontology:cs_axiom_status(religious_establishment_as_constitutional_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('14de0fac-ca29-42ff-935b-403bf5a96bde', religious_establishment_as_constitutional_interpreter, conventional).
narrative_ontology:cs_reference_frame('14de0fac-ca29-42ff-935b-403bf5a96bde', classical_islamic_political_order).
narrative_ontology:cs_drift_state('14de0fac-ca29-42ff-935b-403bf5a96bde', post_revolutionary_constitutional_moment, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('14de0fac-ca29-42ff-935b-403bf5a96bde', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamic_nationalist_elites).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_establishment).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_institution).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_institution).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_identity_as_sovereign_ground).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__guided_nationalism_reading, divine_law_supremacy_over_human_legislation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and ratified the charter; control the constitutional court and legislative agenda. Use religious-nationalist framing to consolidate political authority and distribute patronage. Can shape the constraint's interpretation through appointed judicial and religious bodies.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamic_nationalist_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains constitutional recognition as the authoritative interpreter of religious norms guiding legislation. Receives state funding for religious institutions, control over family law, education curriculum, and public morality enforcement. Their institutional identity is fused with the charter's legitimacy claim.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_establishment, beneficiary,
    organized, generational, identity_locked, national).

% Political parties, NGOs, unions, and professional associations that organized around secular democratic demands during the revolution. Now face legal restrictions on advocacy, assembly, and candidacy. Must operate within religious-normative boundaries or risk dissolution. Exit requires exile or quietism.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society, payer,
    organized, biographical, constrained, national).

% Non-Muslim communities and heterodox Muslim sects. Lose equal citizenship guarantees; face restrictions on worship, building permits, personal status law, and political representation. The charter's religious identity ground renders them permanent second-class subjects. Emigration is the only full exit but requires abandoning ancestral homes and community.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities, payer,
    powerless, biographical, trapped, national).

% Retains autonomy over defense budget and internal affairs in exchange for endorsing the charter's legitimacy. Gains constitutional protection from civilian oversight but loses the 'custodian of the revolution' narrative to religious-nationalist elites. Constrained by needing to suppress secular dissent that threatens the charter.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_institution, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_institution, payer).

% UN human rights mechanisms, foreign ministries, democracy indices. Document the constraint's impact on minority rights and democratic freedoms. Can impose diplomatic costs but lack enforcement leverage. Their analytical seat sees the full structural asymmetry.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, international_observers, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified sovereign legitimacy framework for post-revolutionary state-building by anchoring constitutional authority in a shared religious-nationalist identity, resolving the legitimacy vacuum left by the fallen regime.
% TRANSFER_FUNCTION: Moves constitutional interpretation authority and legislative supremacy from secular-democratic institutions to a religious-normative framework; extracts political autonomy, legal equality, and civic space from secular civil society and religious minorities, transferring status and resource control to the religious establishment and islamic_nationalist_elites.
% ABSENT_VOICES: Secular democratic forces who led the revolution's early phase; religious minorities excluded from the drafting committee; women's rights advocates whose demands for equal citizenship were ruled 'contrary to divine law'; trade unionists and leftist parties banned from the constitutional process. They are structurally excluded — the charter's preamble declares the revolution's 'true meaning' as religious-nationalist, retroactively delegitimizing their participation.
% DISAPPEARANCE_RATIONALE: If the charter's religious legitimacy ground vanished overnight, the entire constitutional order would require a new foundation: the legislature, judiciary, and executive all derive authority from the charter's Islamic-nationalist preamble. A constituent assembly would be necessary; the religious establishment would lose its constitutional role; secular civil society would demand immediate reopening of the constitutional process. The state would face a legitimacy crisis equivalent to the post-revolutionary vacuum.
% FOUNDING_PROBLEM: Post-revolutionary legitimacy vacuum: the fallen regime's secular-nationalist framework had collapsed, leaving no accepted source of sovereign authority. The charter was built to provide an immediate, culturally resonant legitimacy ground that could unify the polity and legitimize a new constitutional order.
% FOUNDING_PROBLEM_CORROBORATION: Islamic-nationalist elites and the religious establishment attest the founding problem remains live — citing ongoing threats of secularism, foreign intervention, and moral decay. Secular civil society leaders and religious minority representatives (from outside the beneficiary set) attest the legitimacy vacuum was substantially resolved by the revolution itself and the charter's religious ground was a capture mechanism, not a solution. Independent historians of the revolutionary period corroborate the manufactured-vacuum reading.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__guided_nationalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__guided_nationalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.72) because the constraint transfers constitutional authority from pluralistic democratic processes to a single religious-normative framework, extracting political autonomy from secular actors and legal equality from minorities. Suppression is high (0.78) because the charter actively bans secular parties, restricts minority worship, and criminalizes 'insult to religious constants' — alternatives are not merely discouraged but legally foreclosed. Theater is moderate (0.35): the religious establishment performs genuine interpretive work, but a growing share of enforcement targets political dissent rather than theological deviation. Accessibility collapse is high (0.75) because once the religious legitimacy ground is accepted, secular constitutional alternatives become logically incoherent within the framework. Resistance is moderate (0.55): secular civil society organizes but faces severe repression; religious minorities resist through quiet endurance and emigration.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the constraint is a rope — genuine coordination solving the legitimacy vacuum. From the secular civil society payer seat, it is a snare — pure extraction suppressing democratic alternatives. From the religious minority payer seat, it is a snare with identity-locked entrapment. The engine computes this divergence from the structural data; the authored claim (tangled_rope) captures the hybrid reality: real coordination function (state unity, legitimate authority) coexisting with asymmetric extraction (secular exclusion, minority subordination).
 *
 * DIRECTIONALITY LOGIC:
 *   Islamic-nationalist elites are structural beneficiaries (d ≈ 0.1): they collect constitutional authority, patronage distribution, and agenda control. Religious establishment is a beneficiary with identity_locked exit (d ≈ 0.15): they gain institutional privilege but their identity is fused to the constraint. Secular civil society are payers with constrained exit (d ≈ 0.85): they bear the cost of exclusion but retain some organizational capacity. Religious minorities are trapped payers (d ≈ 0.95): they bear the highest extraction with no political exit. Military institution sits near symmetric (d ≈ 0.5): gains autonomy but loses narrative primacy and must enforce the charter. International observers are analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-revolutionary legitimacy vacuum) is contested as live vs. manufactured. The charter's religious ground has acquired extraction that exceeds its coordination function — the religious establishment's expanding jurisdiction over family law, education, and morality policing serves institutional interests, not the original unity function. The constraint shows mandatrophy indicators: theater rising, suppression intensifying, and the founding problem's status contested by non-beneficiary witnesses. Yet it persists because the coordination function (preventing fragmentation) remains real enough that no coalition can agree on an alternative legitimacy ground.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_natural_vs_constructed,
    'Is the charter''s religious legitimacy ground a genuine natural coordination solution for this polity, or a constructed constraint that benefits identifiable agents (islamic_nationalist_elites, religious_establishment)?',
    'Counterfactual historical analysis: if the revolution had produced a secular constituent assembly with the same popular mandate, would the polity have fragmented? Comparative analysis of post-revolutionary transitions with different legitimacy grounds.',
    'If constructed, the constraint is a false summit candidate (mountain claim with beneficiaries) and the FSM signature would reclassify toward tangled_rope/snare. If natural, the high extraction/suppression metrics represent genuine coordination costs of a culturally necessary framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_structure_natural_vs_constructed, conceptual, 'Whether the religious legitimacy ground is structurally necessary or a beneficiary-serving construction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.78) primarily structural (legal bans, criminalization, state enforcement) or partially internalized (secular actors self-censoring, minorities internalizing second-class status)?',
    'Post-exit suppression trajectory: track secular activists who emigrate — do they continue advocacy abroad at same intensity? Survey religious minorities on internalized stigma vs. external barriers. Compare suppression metrics before/after crackdown waves.',
    'If substantially internalized, effective suppression is higher than structural measure — targets carry the constraint with them. This would increase χ for payer seats and strengthen snare classification. If purely structural, suppression is reversible with regime change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for secular civil society and religious minorities.').

omega_variable(
    coordination_extraction_separability,
    'Can the charter''s coordination function (unified legitimacy, state unity) be separated from its extraction function (religious establishment privilege, minority subordination)?',
    'Institutional design experiment: would a charter with the same unity preamble but secular democratic institutions (secular_democratic_reading) achieve comparable stability? Natural experiment from the military_custodian_reading''s partial implementation period.',
    'If separable, the extraction is avoidable overhead — the constraint is tangled_rope with removable extractive components. If inseparable, the religious ground IS the coordination mechanism — extraction is the price of unity, pushing toward mountain-like structural necessity (but with beneficiaries, triggering FSM).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction are structurally separable in this constraint.').

omega_variable(
    military_accommodation_stability,
    'Is the military_institution''s dual role (beneficiary of autonomy / payer of enforcement burden) stable, or does the charter create structural pressure for military intervention?',
    'Track military public statements, budget negotiations, and coup-proofing measures. Compare with military_custodian_reading''s predicted trajectory.',
    'If unstable, the constraint''s enforcement capacity degrades — suppression_requirement may fall as military refuses to suppress secular dissent, or a military_custodian_reading coup displaces the charter entirely. This would register as a drift_state shift in the kernel''s constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_accommodation_stability, empirical, 'Stability of the military''s accommodation within the guided_nationalism_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_charter_guided_nationalism_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(july_charter_guided_nationalism_tr_t2, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 2, 0.25).
narrative_ontology:measurement(july_charter_guided_nationalism_tr_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(july_charter_guided_nationalism_tr_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(july_charter_guided_nationalism_tr_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(july_charter_guided_nationalism_tr_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(july_charter_guided_nationalism_tr_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 12, 0.35).

% Extraction over time
narrative_ontology:measurement(july_charter_guided_nationalism_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(july_charter_guided_nationalism_be_t2, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 2, 0.6).
narrative_ontology:measurement(july_charter_guided_nationalism_be_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 4, 0.65).
narrative_ontology:measurement(july_charter_guided_nationalism_be_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(july_charter_guided_nationalism_be_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 8, 0.7).
narrative_ontology:measurement(july_charter_guided_nationalism_be_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement(july_charter_guided_nationalism_be_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 12, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(july_charter_guided_nationalism_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(july_charter_guided_nationalism_su_t2, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 2, 0.65).
narrative_ontology:measurement(july_charter_guided_nationalism_su_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 4, 0.7).
narrative_ontology:measurement(july_charter_guided_nationalism_su_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 6, 0.73).
narrative_ontology:measurement(july_charter_guided_nationalism_su_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 8, 0.75).
narrative_ontology:measurement(july_charter_guided_nationalism_su_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 10, 0.77).
narrative_ontology:measurement(july_charter_guided_nationalism_su_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 12, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.08).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__military_custodian_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, family_law_religious_jurisdiction).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, education_curriculum_islamic_norms).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, blasphemy_law_enforcement).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, political_party_ban_secular).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the july_charter_sovereign_legitimacy kernel. The guided_nationalism_reading (this story) instantiates religious identity as sovereign ground with ε≈0.72. The secular_democratic_reading instantiates popular sovereignty with ε≈0.15 (claimed mountain/rope). The military_custodian_reading instantiates institutional guardianship with ε≈0.45 (claimed scaffold). The three constraints form a family linked by affects_constraints; they share the same charter text but diverge in beneficiary/victim structure, enforcement logic, and drift trajectory. This decomposition follows the ε-invariance principle: the label 'July Charter' covers structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, institutional, 0.1).
constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, organized, 0.85).
constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
