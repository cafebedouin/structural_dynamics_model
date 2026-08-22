% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__constitutional_hybrid_reading, []).

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
 *   constraint_id: sovereign_legitimacy__constitutional_hybrid_reading
 *   human_readable: Constitutional Hybrid Legitimacy: Dual-Source Authority with Constitutional Mediation
 *   domain: political_philosophy/constitutional_theory/legitimacy_studies
 *
 * SUMMARY:
 *   This constraint story models the constitutional hybrid reading of
 *   sovereign legitimacy: legitimate authority derives from two distinct
 *   sources — ceremonial/symbolic authority inherited through the monarchy,
 *   and political authority delegated through democratic elections — with
 *   constitutional law (written and unwritten) mediating the boundary between
 *   them. This is ONE READING of the contested kernel 'sovereign_legitimacy',
 *   instantiated as constitutional_hybrid_reading. The sibling readings are
 *   monarchical_reading (pure downward authority from sovereign) and
 *   republican_reading (pure upward authority from people). The hybrid
 *   reading claims structural stability through institutional separation but
 *   admits vulnerability to boundary disputes resolved through constitutional
 *   interpretation and precedent.
 *
 * KEY AGENTS:
 *   - hereditary_monarch: Primary beneficiary (institutional/identity_locked) — retains status/income
 *   - elected_officials: Primary beneficiary + agenda setter (institutional/constrained) — exercises policy power
 *   - absolutist_proponents: Victim + excluded (organized/constrained) — seeks pure hereditary authority
 *   - republican_proponents: Victim + excluded (organized/constrained) — seeks pure popular sovereignty
 *   - constitutional_courts: Agenda setter (institutional/arbitrage) — mediates boundary through interpretation
 *   - citizen_subjects: Payer + beneficiary (moderate/constrained) — bears costs of dual structure, gains stability
 *   - political_theorists: Observer (analytical/analytical) — analyzes from outside the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.35).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.45).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Constitutional Hybrid Legitimacy: Dual-Source Authority with Constitutional Mediation").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political_philosophy/constitutional_theory/legitimacy_studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, '64a9b98e-544e-4a7c-9416-acc59bd6bc20').
narrative_ontology:cs_kernel_codification('64a9b98e-544e-4a7c-9416-acc59bd6bc20', formalized).
narrative_ontology:cs_authority_grounding('64a9b98e-544e-4a7c-9416-acc59bd6bc20', lineage).
narrative_ontology:cs_interpretation_layer_present('64a9b98e-544e-4a7c-9416-acc59bd6bc20').
narrative_ontology:cs_reading_relation('64a9b98e-544e-4a7c-9416-acc59bd6bc20', sovereign_legitimacy__monarchical_reading, coexists_with).
narrative_ontology:cs_reading_relation('64a9b98e-544e-4a7c-9416-acc59bd6bc20', sovereign_legitimacy__republican_reading, coexists_with).
narrative_ontology:cs_axiom('64a9b98e-544e-4a7c-9416-acc59bd6bc20', foundational, dual_source_legitimacy).
narrative_ontology:cs_axiom_status(dual_source_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('64a9b98e-544e-4a7c-9416-acc59bd6bc20', dual_source_legitimacy, conventional).
narrative_ontology:cs_axiom('64a9b98e-544e-4a7c-9416-acc59bd6bc20', foundational, constitutional_mediation_supremacy).
narrative_ontology:cs_axiom_status(constitutional_mediation_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('64a9b98e-544e-4a7c-9416-acc59bd6bc20', constitutional_mediation_supremacy, conventional).
narrative_ontology:cs_reference_frame('64a9b98e-544e-4a7c-9416-acc59bd6bc20', constitutional_monarchy_settlement).
narrative_ontology:cs_drift_state('64a9b98e-544e-4a7c-9416-acc59bd6bc20', contemporary_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('64a9b98e-544e-4a7c-9416-acc59bd6bc20', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_proponents).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, republican_proponents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, citizen_subjects).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, citizen_subjects).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__constitutional_hybrid_reading, institutional_separation_of_powers).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__constitutional_hybrid_reading, legitimacy_through_legal_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ceremonial status, symbolic authority, and state-funded income in exchange for renouncing political power. The role is fused with personal and dynastic identity — abdication is structurally possible but existentially destructive to the office and the individual's self-conception as a constitutional figurehead.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, beneficiary,
    institutional, generational, identity_locked, national).

% Exercise political authority through democratic mandate while operating within constitutional boundaries mediated by courts and conventions. They benefit from the legitimacy the ceremonial head provides to the state apparatus, but are constrained by the requirement to maintain the fiction of royal assent and constitutional convention.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, agenda_setter).

% Advocate for restoration of undivided sovereign authority in the monarch. They bear the cost of political marginalization — their position is structurally excluded from legitimate discourse by the hybrid settlement, forced into extra-constitutional activism or academic marginalization.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_proponents, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_proponents, excluded).

% Advocate for complete popular sovereignty and abolition of hereditary office. They bear the cost of perpetuating an institution they consider illegitimate — their preferred constitutional form is blocked by the hybrid settlement's entrenchment of the monarchy, forcing them into incremental reformism or revolutionary rhetoric that the system treats as illegitimate.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, republican_proponents, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, republican_proponents, excluded).

% Mediate boundary disputes between ceremonial and political authority through interpretation and precedent. They hold the decisive interpretive power over what the constitutional mediation means in practice, but their own legitimacy depends on maintaining the hybrid settlement's coherence.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_courts, agenda_setter,
    institutional, generational, arbitrage, national).

% Subject to dual authority structure — they fund the monarchy through taxes and obey laws made by elected officials. They receive stability and symbolic continuity from the ceremonial source, and policy responsiveness from the political source. Exit is constrained by nationality and the lack of pure-alternative regimes.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, citizen_subjects, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, citizen_subjects, beneficiary).

% Analyze the hybrid settlement's coherence, stability, and legitimacy from outside the operational constraint. They map the boundary disputes, track the interpretive drift, and assess whether the arrangement solves or displaces the founding problem of legitimate authority.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, political_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, continuous source of legitimate authority that avoids both the arbitrariness of pure hereditary rule and the volatility of pure popular sovereignty. The ceremonial source supplies symbolic continuity and non-partisan legitimacy; the political source supplies democratic responsiveness and policy legitimacy. Constitutional law mediates the boundary so neither source can fully capture the other.
% TRANSFER_FUNCTION: Moves symbolic authority and state-funded status to the hereditary monarch; moves political decision-making power and democratic legitimacy to elected officials. The constitutional mediation transfers interpretive authority to courts. Ambiguity costs are distributed across all parties — no one gets pure authority, all must negotiate the boundary.
% ABSENT_VOICES: Those who would reject the very concept of a mediated boundary — radical monarchists who see any constraint on the sovereign as illegitimate usurpation, and radical republicans who see any hereditary office as a structural injustice. They are excluded because the hybrid settlement defines the legitimate discursive space as the space between these extremes.
% DISAPPEARANCE_RATIONALE: If the constitutional hybrid settlement vanished overnight, the authority vacuum would force an immediate and consequential choice: either the monarchy reclaims political power (absolutist restoration), the monarchy is abolished (republican transition), or a new ad hoc mediation emerges. The specific constitutional conventions, judicial precedents, and informal norms that structure the boundary would be the first casualties, and the rearrangement would be contested, potentially violent, and path-dependent.
% FOUNDING_PROBLEM: How to secure legitimate authority after the collapse of divine-right absolutism without succumbing to the instability of pure popular sovereignty or the injustice of hereditary rule. The hybrid settlement was built to solve the transition crisis of the 17th-19th centuries: providing continuity while conceding democratic legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians (e.g., Bogdanor, Hennessy) attest the founding transition problem was substantially solved by the late 19th century. Republican theorists (e.g., Pettit, Skinner) and monarchist traditionalists both attest the problem persists in new forms — the former because hereditary office remains, the latter because the monarchy's political neutrality is a fiction. No consensus exists outside the benefiting institutional parties.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__constitutional_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__constitutional_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(sovereign_legitimacy__constitutional_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).
:- end_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.35) because the compromise reduces the extraction of both pure forms: the monarch extracts less than an absolutist sovereign would, elected officials extract less than an unconstrained majority could. But ambiguity costs exist — the boundary is never fully settled, requiring continuous interpretation. Suppression is moderate (0.45): the constraint actively excludes pure-form advocates from legitimate power, but does not violently repress them in stable periods. Theater ratio is low-moderate (0.25): the ceremonial function is genuinely performative but the political function is substantive; the gap between ceremonial theater and political reality is the constraint's central tension. Accessibility collapse is low (0.35): alternatives (republicanism, absolutism) remain intellectually and politically live. Resistance is moderate-high (0.55): both excluded groups maintain organized opposition, and boundary disputes are constant.
 *
 * PERSPECTIVAL GAP:
 *   The hereditary monarch experiences the constraint as a protective shell — identity-locked into a role that provides status without accountability. Elected officials experience it as an enabling framework — constrained but legitimated by the ceremonial source. Absolutists and republicans experience it as a snare — their preferred form is structurally excluded. Constitutional courts experience it as a rope — they coordinate the boundary through interpretation. The engine will compute these as different types from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (hereditary_monarch, elected_officials) collect status, income, and power from the hybrid settlement — directionality near 0.0. Victims (absolutist_proponents, republican_proponents) bear exclusion costs and ambiguity costs — directionality near 1.0. Citizen_subjects sit near symmetric (0.5) — they pay taxes and obey laws but receive stability and representation. Constitutional courts are agenda_setters with analytical exit — they administer the mediation. The directionality derivation from beneficiary/victim declarations plus exit options captures this structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-absolutist transition) is contested as to whether it persists. The hybrid settlement prevents mislabeling: it is not pure coordination (both pure forms are excluded) nor pure extraction (both beneficiaries gain but both are also constrained). The mandatrophy question — whether the arrangement has outlived its transition function — is exactly the contested status. The constraint persists because neither pure form can capture the center, not because the hybrid is universally loved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the constitutional_hybrid_reading a genuine structural compromise between monarchical and republican principles, or a temporary truce that masks an unresolved contest over the true source of legitimacy?',
    'Longitudinal analysis of boundary disputes: if disputes consistently resolve toward one pole (monarchy or republic), the hybrid is a truce; if they oscillate or stabilize at the boundary, it is a structural compromise.',
    'If a truce, the constraint is a scaffold with a hidden sunset; if a compromise, it is a tangled_rope with genuine coordination function. The classification hinges on whether the founding problem is dead (truce) or live (compromise).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the hybrid reading is a stable structural form or a contingent pause in a binary contest').

omega_variable(
    boundary_ambiguity_as_extraction,
    'Does the constitutional mediation''s ambiguity function as a feature (flexibility) or a bug (extraction by the interpretive authority)?',
    'Track whether constitutional court rulings on the boundary systematically expand the power of courts/elected officials at the monarchy''s expense, or vice versa, or maintain equilibrium.',
    'If ambiguity systematically benefits the interpretive authority (courts/parliament), the constraint has a hidden extraction vector toward that authority. If equilibrium holds, ambiguity is genuine coordination flexibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_ambiguity_as_extraction, empirical, 'Whether interpretive ambiguity in the constitutional mediation is a coordination resource or an extraction mechanism').

omega_variable(
    ceremonial_extraction_reality,
    'Does the hereditary monarch''s retained status and income constitute genuine extraction from citizen_subjects, or is the cost trivial relative to the symbolic stability provided?',
    'Cost-benefit analysis of monarchy funding vs. measurable stability/continuity benefits (tourism, diplomatic soft power, constitutional crisis avoidance) compared to republican alternatives.',
    'If costs exceed measurable benefits, the monarchy is a net extractive beneficiary and the hybrid tilts toward snare for citizen_subjects. If benefits exceed costs, the ceremonial source is a genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremonial_extraction_reality, empirical, 'Whether the ceremonial authority''s material costs are justified by its coordination benefits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sove_tr_t50, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(sove_tr_t100, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 100, 0.25).
narrative_ontology:measurement(sove_tr_t150, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 150, 0.28).
narrative_ontology:measurement(sove_tr_t200, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 200, 0.25).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sove_be_t50, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 50, 0.4).
narrative_ontology:measurement(sove_be_t100, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 100, 0.35).
narrative_ontology:measurement(sove_be_t150, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 150, 0.38).
narrative_ontology:measurement(sove_be_t200, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 200, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(sove_su_t50, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(sove_su_t100, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 100, 0.45).
narrative_ontology:measurement(sove_su_t150, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 150, 0.48).
narrative_ontology:measurement(sove_su_t200, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 200, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__constitutional_hybrid_reading, 0.1).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, republican_reading).

% DUAL FORMULATION NOTE:
% This constraint (constitutional_hybrid_reading) is one member of the sovereign_legitimacy constraint family. The monarchical_reading and republican_reading are sibling constraints with different ε values and beneficiary/victim structures. The hybrid reading's ε (0.35) is lower than both pure forms would be (monarchical ~0.6-0.7 extraction from subjects; republican ~0.4-0.5 extraction from dissenting minorities) because compromise reduces maximal extraction but introduces ambiguity costs. All three stories share the kernel_id 'sovereign_legitimacy' and are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereign_legitimacy__constitutional_hybrid_reading, institutional, 0.15).
constraint_indexing:directionality_override(sovereign_legitimacy__constitutional_hybrid_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
