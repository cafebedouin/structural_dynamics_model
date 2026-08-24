% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__dependency_trap_reading, []).

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
 *   constraint_id: income_support_conditionality__dependency_trap_reading
 *   human_readable: Unconditional Income Support Dependency Trap
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story captures the 'dependency trap' reading of
 *   unconditional income support: a policy framed as a freedom floor that, in
 *   this reading, functions as a snare. The arrangement extracts from two
 *   victim groups simultaneously — recipients trapped by benefit cliffs and
 *   skill atrophy, and taxpayers funding transfers with no productive return
 *   — while welfare bureaucracies and political incumbents capture the gains.
 *   The coordination story (risk pooling for the unable-to-work) is the
 *   cover; the active enforcement machinery (means-testing bureaucracies,
 *   sanction regimes, benefit cliffs) maintains the extraction. The
 *   ε-invariance principle applies: this reading instantiates a specific
 *   constraint with high extractiveness, high suppression, and a trapped
 *   victim set — distinct from the freedom_floor_reading and
 *   wage_subsidy_reading which share the kernel label but posit different
 *   victim/beneficiary structures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.75).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.82).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support Dependency Trap").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, 'c5b06526-9e49-4247-9596-a6a3531a2793').
narrative_ontology:cs_kernel_codification('c5b06526-9e49-4247-9596-a6a3531a2793', formalized).
narrative_ontology:cs_authority_grounding('c5b06526-9e49-4247-9596-a6a3531a2793', extraction).
narrative_ontology:cs_interpretation_layer_present('c5b06526-9e49-4247-9596-a6a3531a2793').
narrative_ontology:cs_reading_relation('c5b06526-9e49-4247-9596-a6a3531a2793', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5b06526-9e49-4247-9596-a6a3531a2793', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('c5b06526-9e49-4247-9596-a6a3531a2793', foundational, unconditional_transfers_erode_labor_attachment).
narrative_ontology:cs_axiom_status(unconditional_transfers_erode_labor_attachment, holdable).
narrative_ontology:cs_axiom_grounding('c5b06526-9e49-4247-9596-a6a3531a2793', unconditional_transfers_erode_labor_attachment, empirically_contingent).
narrative_ontology:cs_axiom('c5b06526-9e49-4247-9596-a6a3531a2793', secondary, work_requirement_legitimizes_transfer).
narrative_ontology:cs_axiom_status(work_requirement_legitimizes_transfer, holdable).
narrative_ontology:cs_axiom_grounding('c5b06526-9e49-4247-9596-a6a3531a2793', work_requirement_legitimizes_transfer, conventional).
narrative_ontology:cs_reference_frame('c5b06526-9e49-4247-9596-a6a3531a2793', postwar_social_insurance_settlement).
narrative_ontology:cs_drift_state('c5b06526-9e49-4247-9596-a6a3531a2793', contemporary_ubi_debate, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c5b06526-9e49-4247-9596-a6a3531a2793', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, welfare_bureaucracy).
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, political_incumbents).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, ubi_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, taxpayers).
narrative_ontology:constraint_vindicates(income_support_conditionality__dependency_trap_reading, incentive_theory).
narrative_ontology:constraint_vindicates(income_support_conditionality__dependency_trap_reading, work_ethic_doctrine).
narrative_ontology:constraint_vindicates(income_support_conditionality__dependency_trap_reading, moral_hazard_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income but face steep benefit cliffs if they take low-wage work; skills atrophy during prolonged idleness; social networks erode; exiting the trap requires accepting wages below replacement level while losing benefits, creating a rational non-participation equilibrium.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, ubi_recipients, payer,
    powerless, biographical, trapped, national).

% Fund transfers that the reading claims produce no productive return; tax burden rises as recipient pool expands; political voice diluted by concentrated beneficiary lobbying; exit requires emigration or tax avoidance, both costly.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Administers the transfer system; budgets and headcount grow with recipient rolls; designs eligibility rules and compliance machinery; captures administrative rents; can transition to adjacent regulatory roles if system reforms.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, welfare_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain loyal electoral constituencies from dependent recipients; use transfer expansion as campaign platform; capture fiscal discretion; can pivot to private sector or international roles if voted out.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, political_incumbents, beneficiary,
    powerful, biographical, mobile, national).

% Never qualify for support but bear inflated entry-level wages and reduced job creation from the tax wedge; would object to both the transfer level and the labor market distortion but have no organized representation.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, labor_market_excluded, excluded,
    powerless, biographical, trapped, national).

% Study labor supply elasticities, benefit cliff effects, and fiscal sustainability; produce evidence cited by all sides; career incentives reward measurable 'activation' outcomes over structural critique.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, policy_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The claimed coordination is pooling risk for those unable to work — disability, caregiving, structural unemployment — providing a floor below which no one falls. The reading asserts this function has been captured: the floor has become a ceiling.
% TRANSFER_FUNCTION: Moves resources from current taxpayers (and future taxpayers via debt) to non-working recipients, with a bureaucratic extraction layer. The transfer is framed as insurance but operates as a permanent income stream with no contribution requirement.
% ABSENT_VOICES: The never-attached — young people who never enter the labor market because the trap makes entry irrational — and future taxpayers who inherit the fiscal structure. Both are structurally excluded from the policy conversation.
% DISAPPEARANCE_RATIONALE: If the unconditional transfer and its enforcement vanished overnight, labor force participation would rise sharply at the margin as benefit cliffs disappear; fiscal pressure would drop; but a cohort of long-term detached workers would face acute destitution without transitional support — the world rearranges violently, not smoothly.
% FOUNDING_PROBLEM: Post-war reconstruction required income security for workers displaced by technological change and industrial decline, when contributory insurance left gaps for those with interrupted work histories.
% FOUNDING_PROBLEM_CORROBORATION: Original architects (Beveridge, 1942; US Social Security framers) explicitly designed against permanent dependency — corroborated by legislative records. Contemporary labor economists (e.g., Autor, 2019; Marinescu, 2018) find mixed evidence on work disincentives; the reading's claim that the founding problem is dead is contested by anti-poverty advocates who cite persistent structural unemployment.
narrative_ontology:disappearance_verdict(income_support_conditionality__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_conditionality__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__dependency_trap_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_conditionality__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is high because the transfer is decoupled from any reciprocal obligation or time limit; suppression (0.82) is very high because exit requires navigating benefit cliffs that impose effective marginal tax rates exceeding 100%, plus bureaucratic barriers to re-entry; theater_ratio (0.42) reflects the growing share of administrative activity devoted to compliance enforcement rather than service delivery. The measurement series shows extraction and suppression rising together over four decades as the system expanded from targeted relief to near-universal coverage, while theater increased as 'activation' programs proliferated without reducing dependency.
 *
 * PERSPECTIVAL GAP:
 *   From the welfare bureaucracy's seat, the constraint is a rope (coordinating risk pooling with manageable overhead). From the recipient's seat, it is a snare (trapped by rational non-participation). From the taxpayer's seat, it is a snare (funding non-productive transfers). The engine computes this divergence from the structural data — the authored claim (snare) reflects the analytical observer's reading, not a consensus.
 *
 * DIRECTIONALITY LOGIC:
 *   Recipients are full targets (d → 1.0): they bear the skill atrophy, social isolation, and benefit-cliff traps. Taxpayers are moderate targets (d → 0.7): they fund the system but have political voice and exit options. Welfare bureaucracy sits near beneficiary end (d → 0.15): they control the rules and capture administrative rents. Political incumbents are beneficiaries (d → 0.1): they gain electoral capital. The excluded (never-attached youth) would be targets if included but are outside the constraint's formal scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war displacement gaps) is substantially solved by modern labor markets and contributory systems, yet the unconditional transfer persists and expands. The mandate has atrophied into a self-justifying apparatus: the bureaucracy that administers dependency becomes the constituency for its expansion. This is not a piton — the extraction is active and concentrated, not diffuse and inertial — hence snare, not piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the dependency_trap_reading a structurally distinct constraint from the freedom_floor_reading and wage_subsidy_reading, or do they describe the same constraint from different observer positions?',
    'Apply the ε-invariance test: if measuring the same policy arrangement yields different ε values depending on which reading''s beneficiary/victim structure is used, they are different constraints. The dependency_trap_reading assigns victims to both recipients and taxpayers (high ε); freedom_floor_reading assigns beneficiaries to recipients (low ε); wage_subsidy_reading assigns beneficiaries to employers (moderate ε). The ε values differ — they are distinct constraints sharing a kernel label.',
    'If they are one constraint, the engine must reconcile the divergent classifications. If three constraints, each gets its own ε, stakeholders, and classification — linked via network.affects_constraints. The latter is correct per DP-001.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel''s three readings instantiate three constraints or one constraint with observer-relative classification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (benefit cliffs, sanction regimes, bureaucratic barriers) or internalized (learned helplessness, identity fusion with ''unemployed'' status, eroded work-seeking habitus)?',
    'Post-exit suppression trajectory: track recipients who exit via policy reform (e.g., time limits, work requirements). If suppression persists after structural barriers are removed — continued non-participation, psychological withdrawal — reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression after exit. This would increase the snare classification confidence and imply that removal alone is insufficient; remediation requires active reintegration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the dependency trap.').

omega_variable(
    causality_vs_selection,
    'Does unconditional support cause skill atrophy and labor detachment, or does it select for individuals already prone to non-participation (adverse selection)?',
    'Natural experiments from policy discontinuities (e.g., negative income tax experiments, Alaska Permanent Fund, Finnish basic income trial) with pre-post labor supply measurement and control for selection.',
    'If causal, the constraint actively produces its victims — strengthening the snare claim. If selection, the constraint merely concentrates pre-existing non-participation — weakening the extraction claim but not eliminating the transfer from taxpayers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causality_vs_selection, empirical, 'Causal direction of the dependency effect.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(income_support_conditionality__dependency_trap_reading_tr_t0, income_support_conditionality__dependency_trap_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(income_support_conditionality__dependency_trap_reading_tr_t8, income_support_conditionality__dependency_trap_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(income_support_conditionality__dependency_trap_reading_tr_t16, income_support_conditionality__dependency_trap_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(income_support_conditionality__dependency_trap_reading_tr_t24, income_support_conditionality__dependency_trap_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(income_support_conditionality__dependency_trap_reading_tr_t32, income_support_conditionality__dependency_trap_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(income_support_conditionality__dependency_trap_reading_tr_t40, income_support_conditionality__dependency_trap_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(income_support_conditionality__dependency_trap_reading_be_t0, income_support_conditionality__dependency_trap_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(income_support_conditionality__dependency_trap_reading_be_t8, income_support_conditionality__dependency_trap_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(income_support_conditionality__dependency_trap_reading_be_t16, income_support_conditionality__dependency_trap_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(income_support_conditionality__dependency_trap_reading_be_t24, income_support_conditionality__dependency_trap_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(income_support_conditionality__dependency_trap_reading_be_t32, income_support_conditionality__dependency_trap_reading, base_extractiveness, 32, 0.72).
narrative_ontology:measurement(income_support_conditionality__dependency_trap_reading_be_t40, income_support_conditionality__dependency_trap_reading, base_extractiveness, 40, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(income_support_conditionality__dependency_trap_reading_su_t0, income_support_conditionality__dependency_trap_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(income_support_conditionality__dependency_trap_reading_su_t8, income_support_conditionality__dependency_trap_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(income_support_conditionality__dependency_trap_reading_su_t16, income_support_conditionality__dependency_trap_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(income_support_conditionality__dependency_trap_reading_su_t24, income_support_conditionality__dependency_trap_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(income_support_conditionality__dependency_trap_reading_su_t32, income_support_conditionality__dependency_trap_reading, suppression_requirement, 32, 0.8).
narrative_ontology:measurement(income_support_conditionality__dependency_trap_reading_su_t40, income_support_conditionality__dependency_trap_reading, suppression_requirement, 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__dependency_trap_reading, 0.18).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, labor_market_activation_requirements).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, benefit_sanctions_regime).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, tax_wedge_on_low_wage_labor).

% DUAL FORMULATION NOTE:
% This constraint (dependency_trap_reading) and its siblings (freedom_floor_reading, wage_subsidy_reading) form a constraint family decomposing the 'unconditional income support' label. The dependency_trap_reading has ε≈0.75 (snare); freedom_floor_reading has ε≈0.15 (rope); wage_subsidy_reading has ε≈0.45 (tangled_rope). The ε values differ because the beneficiary/victim structures differ — each reading posits a different transfer function. They are linked via affects_constraints because the dependency_trap_reading's enforcement machinery (sanctions, activation) structurally enables the wage_subsidy_reading's employer subsidy effect, and the freedom_floor_reading's political advocacy sustains the unconditionality that the other two readings critique.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_conditionality__dependency_trap_reading, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
