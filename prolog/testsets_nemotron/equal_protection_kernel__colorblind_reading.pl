% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__colorblind_reading, []).

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
 *   constraint_id: equal_protection_kernel__colorblind_reading
 *   human_readable: Colorblind Equal Protection Reading (Constitutional Constraint on Race-Conscious Remediation)
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the colorblind_reading of the
 *   equal_protection_kernel — the interpretation that the Fourteenth
 *   Amendment categorically forbids all state use of racial classifications,
 *   regardless of purpose. The reading operates as a structural snare: it
 *   presents itself as a neutral coordination rule (formal equality,
 *   administrable categorical ban) but functions as an extraction mechanism
 *   that forecloses remedial pathways for historically excluded groups while
 *   benefiting institutions and actors who avoid remedial obligations. The
 *   beneficiary/victim structure is asymmetric: white applicants in
 *   competitive admissions and institutions avoiding remediation gain
 *   concrete advantages; historically excluded groups lose the only
 *   structural offset to cumulative disadvantage. The constraint requires
 *   active enforcement (litigation, judicial review, DOJ/OCR guidance
 *   reversals) to maintain the categorical ban against institutional and
 *   political pressure for race-conscious remediation. The ε-invariance
 *   principle applies: this reading's ε (0.72) is assessed against the
 *   standing arrangement it contests (the remedial/antisubordination
 *   framework), not against the colorblind alternative it endorses. The
 *   sibling readings (remedial_reading, antisubordination_reading) are
 *   separate constraints with their own ε values, linked via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, 0.72).
domain_priors:suppression_score(equal_protection_kernel__colorblind_reading, 0.85).
domain_priors:theater_ratio(equal_protection_kernel__colorblind_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__colorblind_reading, snare).
narrative_ontology:human_readable(equal_protection_kernel__colorblind_reading, "Colorblind Equal Protection Reading (Constitutional Constraint on Race-Conscious Remediation)").
narrative_ontology:topic_domain(equal_protection_kernel__colorblind_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__colorblind_reading, '4884a75d-2c9f-4ad5-b33b-6bcb25261f49').
narrative_ontology:cs_kernel_codification('4884a75d-2c9f-4ad5-b33b-6bcb25261f49', fixed_text).
narrative_ontology:cs_authority_grounding('4884a75d-2c9f-4ad5-b33b-6bcb25261f49', lineage).
narrative_ontology:cs_interpretation_layer_present('4884a75d-2c9f-4ad5-b33b-6bcb25261f49').
narrative_ontology:cs_reading_relation('4884a75d-2c9f-4ad5-b33b-6bcb25261f49', equal_protection_kernel__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('4884a75d-2c9f-4ad5-b33b-6bcb25261f49', equal_protection_kernel__antisubordination_reading, forecloses).
narrative_ontology:cs_axiom('4884a75d-2c9f-4ad5-b33b-6bcb25261f49', foundational, state_race_classification_categorically_forbidden).
narrative_ontology:cs_axiom_status(state_race_classification_categorically_forbidden, holdable).
narrative_ontology:cs_axiom_grounding('4884a75d-2c9f-4ad5-b33b-6bcb25261f49', state_race_classification_categorically_forbidden, deontological).
narrative_ontology:cs_axiom('4884a75d-2c9f-4ad5-b33b-6bcb25261f49', foundational, formal_equality_requires_identical_treatment).
narrative_ontology:cs_axiom_status(formal_equality_requires_identical_treatment, holdable).
narrative_ontology:cs_axiom_grounding('4884a75d-2c9f-4ad5-b33b-6bcb25261f49', formal_equality_requires_identical_treatment, deontological).
narrative_ontology:cs_axiom('4884a75d-2c9f-4ad5-b33b-6bcb25261f49', secondary, remedial_purpose_does_not_justify_classification).
narrative_ontology:cs_axiom_status(remedial_purpose_does_not_justify_classification, holdable).
narrative_ontology:cs_axiom_grounding('4884a75d-2c9f-4ad5-b33b-6bcb25261f49', remedial_purpose_does_not_justify_classification, deontological).
narrative_ontology:cs_reference_frame('4884a75d-2c9f-4ad5-b33b-6bcb25261f49', post_brown_anti_classification_principle).
narrative_ontology:cs_drift_state('4884a75d-2c9f-4ad5-b33b-6bcb25261f49', contemporary_anti_remediation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4884a75d-2c9f-4ad5-b33b-6bcb25261f49', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__colorblind_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, white_applicants_in_competitive_admissions).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, institutions_avoiding_remedial_obligations).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, colorblind_constitutional_doctrine_advocates).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, historically_excluded_racial_groups).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, institutions_seeking_diversity_remediation).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, future_generations_in_segregated_systems).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, formal_equality_do_categorical).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, colorblind_constitution_thesis).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, anti_classification_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain competitive advantage when race-conscious admissions are barred; their relative position improves as remedial pathways close. Exit to alternative institutions or private markets is feasible. Their litigation capacity is amplified by institutional support from advocacy organizations.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, white_applicants_in_competitive_admissions, beneficiary,
    organized, biographical, mobile, national).

% Lose the only structural pathway that partially offset cumulative exclusion in competitive admissions. Cannot exit the constraint — it governs the public sphere where opportunity is allocated. Bear the cost of formal equality applied to unequal starting positions. Coalition-building across groups is the primary resistance mechanism but faces collective-action barriers.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, historically_excluded_racial_groups, payer,
    moderate, generational, constrained, national).

% Universities and employers that would face pressure to implement remedial programs are relieved of that obligation by the categorical ban. They control the narrative of 'merit' and 'neutrality' that the reading enshrines. Can shift to race-neutral proxies that preserve demographic outcomes without legal vulnerability.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, institutions_avoiding_remedial_obligations, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__colorblind_reading, institutions_avoiding_remedial_obligations, agenda_setter).

% Institutions that genuinely seek to remediate exclusion or achieve diversity are blocked from the most effective tools. Must invest in costly, less-effective race-neutral alternatives (percent plans, socioeconomic proxies) that do not achieve equivalent results. Face litigation risk from both directions.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, institutions_seeking_diversity_remediation, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__colorblind_reading, institutions_seeking_diversity_remediation, agenda_setter).

% Ideological and legal movement that has built careers, organizations, and jurisprudence around the colorblind reading. Professional identity is fused to the categorical anti-classification principle. Exit would mean abandoning the framework that constitutes their institutional relevance and self-conception.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, colorblind_constitutional_doctrine_advocates, agenda_setter,
    organized, generational, identity_locked, national).

% Children who will inherit the educational and economic stratification that the constraint locks in. Have no voice in the constitutional debate that determines their starting conditions. The constraint operates on them before they can participate in politics.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, future_generations_in_segregated_systems, excluded,
    powerless, generational, trapped, national).

% Analyze the reading's doctrinal coherence, historical fidelity, and empirical consequences. Their role is diagnostic — they map the structural relationships the constraint creates but do not bear its costs or collect its benefits directly.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administrable rule for courts: no racial classification by the state, ever. Eliminates the need for courts to evaluate purpose, tailoring, or remedial justification — replaces multi-factor balancing with a categorical prohibition.
% TRANSFER_FUNCTION: Moves the burden of historical exclusion from institutions (which would bear remedial costs) onto historically excluded groups (who bear the continuing effects without structural offset). Transfers institutional risk and resource allocation authority from diversity-seeking institutions to colorblind-doctrine advocates and litigation organizations.
% ABSENT_VOICES: Future generations who will live in the stratified systems this reading entrenches; historically excluded communities whose remedial claims are categorically foreclosed without their consent; institutions that would pursue race-conscious remediation but are legally barred.
% DISAPPEARANCE_RATIONALE: If the colorblind reading vanished overnight, race-conscious admissions and remedial programs would immediately resume at universities and in public employment; institutional resources would shift toward targeted remediation; the litigation architecture built around the categorical ban would collapse; the constitutional conversation would shift from 'whether' to 'how' to structure race-conscious remedies.
% FOUNDING_PROBLEM: Post-Brown resistance to desegregation: Southern states used 'race-conscious' classifications to maintain segregation while claiming compliance. The colorblind principle emerged as a tool to strike down de jure segregation by forbidding any state use of race — a symmetric weapon against the architects of Jim Crow.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (state-mandated segregation using racial classifications) was resolved by the Civil Rights Act of 1964 and subsequent desegregation enforcement. The colorblind reading's advocates (colorblind_constitutional_doctrine_advocates) assert the problem persists in 'new forms'; this is contested by historians of the civil rights movement (e.g., Tomiko Brown-Nagin, Risa Goluboff) and by the remedial_reading's institutional proponents who document that the reading now operates against the groups it was originally wielded to protect.
narrative_ontology:disappearance_verdict(equal_protection_kernel__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__colorblind_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__colorblind_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(equal_protection_kernel__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__colorblind_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__colorblind_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   High extractiveness (0.72) because the constraint transfers the entire remedial burden from institutions to excluded groups while providing no substitute mechanism. High suppression (0.85) because the categorical ban is maintained through active judicial enforcement that strikes down any race-conscious program — alternatives are not merely discouraged but legally forbidden. Moderate theater (0.28) because the coordination function (administrable rule) is real but shrinking as a proportion of the constraint's operation; the doctrinal edifice increasingly serves to legitimize the extraction. High accessibility_collapse (0.82) because once the categorical principle is accepted, any race-conscious alternative is logically excluded — the constraint's internal logic closes the space of permissible remedies. Moderate resistance (0.55) because institutional and political pushback exists but is channeled into increasingly narrow race-neutral proxies rather than direct challenge to the categorical principle.
 *
 * DIRECTIONALITY LOGIC:
 *   White applicants (organized, mobile) are structural beneficiaries — d near 0.1 (subsidized). Historically excluded groups (moderate power, constrained exit, generational horizon) are targets — d near 0.9. Institutions avoiding remediation (institutional, arbitrage) are beneficiaries and agenda-setters — d near 0.15. Institutions seeking remediation (institutional, constrained) are payers despite formal power — d near 0.7. Colorblind advocates (organized, identity_locked) are agenda-setters with high d despite beneficiary alignment because their professional identity fuses to the constraint — they cannot exit without self-dissolution. Future generations (powerless, trapped) are excluded with maximal d — they bear the intergenerational extraction with zero voice. Scholars (analytical) sit at d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jim Crow's use of racial classifications to maintain caste) is dead — resolved by the very civil rights enforcement the colorblind principle helped enable. Yet the constraint persists and has expanded its extraction target from segregationist states to institutions attempting remediation. This is classic mandatrophy: the mandate (forbid state racial classification) has outlived its function (strike down de jure segregation) and now serves a new function (block remediation). The colorblind reading's advocates deny the founding problem is dead — they claim 'new segregation' justifies the same categorical tool — but the structural evidence (who benefits, who pays, what the constraint now prevents) shows the mandate has been repurposed. The engine's mandatrophy detection should flag this divergence between founding_problem_status=dead and disappearance_verdict=world_rearranges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblind_naturalness_vs_construction,
    'Is the categorical anti-classification principle a genuine natural-law constraint on state power (mountain) or a constructed doctrinal choice that benefits identifiable agents (snare/tangled_rope)?',
    'Historical-genealogical analysis: trace the principle''s emergence from post-Brown resistance strategy to supreme court doctrine. If the principle''s doctrinal trajectory maps onto the political interests of its beneficiaries at each stage, the natural-law claim is undermined.',
    'If constructed, the constraint is a false summit candidate (mountain claim with beneficiaries) and FSM reclassification to tangled_rope or snare is warranted. If natural, the high extraction is a measurement error — the constraint would be a mountain with measurement artifacts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_naturalness_vs_construction, conceptual, 'Whether the colorblind principle''s categorical form reflects natural law or political construction.').

omega_variable(
    remedial_alternative_effectiveness,
    'Do race-neutral alternatives (percent plans, socioeconomic proxies, targeted outreach) achieve equivalent remedial outcomes to race-conscious policies?',
    'Empirical comparison of demographic outcomes, graduation rates, and career trajectories under race-conscious vs. race-neutral regimes in jurisdictions that have banned affirmative action (CA, MI, WA, FL).',
    'If race-neutral alternatives are substantially less effective, the colorblind reading''s coordination claim (''we can achieve the same goals without racial classification'') is empirically false — the constraint extracts the difference. If equally effective, the extraction is lower and the reading''s coordination function is genuine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedial_alternative_effectiveness, empirical, 'Whether the constraint''s coordination function (race-neutral alternatives work) is empirically sustained.').

omega_variable(
    identity_lock_mechanism_colorblind_advocates,
    'What specific identity-fusion mechanism binds colorblind_constitutional_doctrine_advocates to the constraint such that exit_options = identity_locked rather than mobile or constrained?',
    'Sociology of the legal movement: interview advocates, trace career paths, analyze organizational funding structures. Determine whether professional identity, ideological commitment, institutional position, or reputational investment is the primary lock.',
    'If identity_locked is ideological (worldview makes exit unthinkable), the constraint''s persistence is more robust than if it is institutional (funding-dependent). The engine''s directionality computation treats identity_locked as near-target (high d) — confirming the mechanism validates the extraction amplification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_colorblind_advocates, empirical, 'Mechanism of identity lock for the advocacy movement that administers the constraint.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the equal_protection_kernel admit only the three declared readings (colorblind, remedial, antisubordination), or are there structurally distinct framings that would produce different constraint families?',
    'Constitutional theory survey: map all live positions in the academic literature and judicial opinions. Test whether each maps cleanly to one of the three declared readings or requires a fourth/fifth constraint story.',
    'If additional framings exist, the current three-story family is incomplete — missing constraints mean missing extraction pathways. The network.affects_constraints links would need expansion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the declared kernel reading set is exhaustive or omits structurally distinct positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__colorblind_reading, 1954, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epcbr_tr_t1954, equal_protection_kernel__colorblind_reading, theater_ratio, 1954, 0.12).
narrative_ontology:measurement(epcbr_tr_t1978, equal_protection_kernel__colorblind_reading, theater_ratio, 1978, 0.18).
narrative_ontology:measurement(epcbr_tr_t1996, equal_protection_kernel__colorblind_reading, theater_ratio, 1996, 0.22).
narrative_ontology:measurement(epcbr_tr_t2003, equal_protection_kernel__colorblind_reading, theater_ratio, 2003, 0.25).
narrative_ontology:measurement(epcbr_tr_t2014, equal_protection_kernel__colorblind_reading, theater_ratio, 2014, 0.26).
narrative_ontology:measurement(epcbr_tr_t2023, equal_protection_kernel__colorblind_reading, theater_ratio, 2023, 0.28).

% Extraction over time
narrative_ontology:measurement(epcbr_be_t1954, equal_protection_kernel__colorblind_reading, base_extractiveness, 1954, 0.25).
narrative_ontology:measurement(epcbr_be_t1978, equal_protection_kernel__colorblind_reading, base_extractiveness, 1978, 0.42).
narrative_ontology:measurement(epcbr_be_t1996, equal_protection_kernel__colorblind_reading, base_extractiveness, 1996, 0.58).
narrative_ontology:measurement(epcbr_be_t2003, equal_protection_kernel__colorblind_reading, base_extractiveness, 2003, 0.63).
narrative_ontology:measurement(epcbr_be_t2014, equal_protection_kernel__colorblind_reading, base_extractiveness, 2014, 0.68).
narrative_ontology:measurement(epcbr_be_t2023, equal_protection_kernel__colorblind_reading, base_extractiveness, 2023, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(epcbr_su_t1954, equal_protection_kernel__colorblind_reading, suppression_requirement, 1954, 0.35).
narrative_ontology:measurement(epcbr_su_t1978, equal_protection_kernel__colorblind_reading, suppression_requirement, 1978, 0.55).
narrative_ontology:measurement(epcbr_su_t1996, equal_protection_kernel__colorblind_reading, suppression_requirement, 1996, 0.72).
narrative_ontology:measurement(epcbr_su_t2003, equal_protection_kernel__colorblind_reading, suppression_requirement, 2003, 0.78).
narrative_ontology:measurement(epcbr_su_t2014, equal_protection_kernel__colorblind_reading, suppression_requirement, 2014, 0.82).
narrative_ontology:measurement(epcbr_su_t2023, equal_protection_kernel__colorblind_reading, suppression_requirement, 2023, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% This constraint and its siblings decompose the 'equal protection' label into structurally distinct claims. The colorblind_reading (this story) has high ε (0.72) because it blocks remedial pathways. The remedial_reading would have lower ε (coordination dominant) but faces suppression from this reading's enforcement. The antisubordination_reading has a different victim set (hierarchy-maintainers) and different coordination function (dismantling caste). All three share the kernel_id but are separate constraints per ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_kernel__colorblind_reading, organized, 0.15).
constraint_indexing:directionality_override(equal_protection_kernel__colorblind_reading, moderate, 0.85).
constraint_indexing:directionality_override(equal_protection_kernel__colorblind_reading, institutional, 0.7).
constraint_indexing:directionality_override(equal_protection_kernel__colorblind_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
