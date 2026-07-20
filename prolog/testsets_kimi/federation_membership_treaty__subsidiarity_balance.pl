% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__subsidiarity_balance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__subsidiarity_balance, []).

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
 *   constraint_id: federation_membership_treaty__subsidiarity_balance
 *   human_readable: Federation Membership Treaty: Subsidiarity-Bounded Free Movement
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint instantiates the subsidiarity_balance reading of the
 *   federation_membership_treaty kernel. It governs free movement of persons
 *   within a federal or quasi-federal polity by treating mobility as a
 *   fundamental freedom that is structurally bounded by proportionality tests
 *   and legitimate national interests. The reading stands between
 *   integration_primary (which treats restrictions as presumptively
 *   illegitimate) and sovereignty_primary (which treats mobility as
 *   conditional on state consent). Under this reading, host states retain a
 *   graduated right to restrict mobility for public policy, public health,
 *   and public security reasons, subject to supranational judicial review.
 *   The constraint coordinates labor markets and federation citizenship but
 *   asymmetrically extracts from mobile workersâwhose rights remain
 *   incompleteâand from static low-wage workersâwho face competitive
 *   pressureâwhile benefiting host state governments (regulatory autonomy),
 *   receiving employers (labor supply), and federation institutions
 *   (adjudicative authority).
 *
 * KEY AGENTS:
 *   - federation_institutions (ECJ, Commission): Institutional agenda-setter that enforces proportionality and adjudicates disputes.
 *   - host_state_governments: Institutional agenda-setter and beneficiary of retained sovereignty to restrict mobility.
 *   - mobile_citizens: Organized payer/beneficiaryâgain mobility rights but bear proportionality restrictions.
 *   - static_workers_host_states: Powerless payer facing labor market competition.
 *   - receiving_employers: Powerful beneficiary of expanded labor pool.
 *   - excluded_tcn_labor: Excluded voice denied free movement rights entirely.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.53).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.55).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.53).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "Federation Membership Treaty: Subsidiarity-Bounded Free Movement").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, '86d81812-f71a-4d78-8c45-649039e15778').
narrative_ontology:cs_kernel_codification('86d81812-f71a-4d78-8c45-649039e15778', formalized).
narrative_ontology:cs_authority_grounding('86d81812-f71a-4d78-8c45-649039e15778', lineage).
narrative_ontology:cs_interpretation_layer_present('86d81812-f71a-4d78-8c45-649039e15778').
narrative_ontology:cs_reading_relation('86d81812-f71a-4d78-8c45-649039e15778', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('86d81812-f71a-4d78-8c45-649039e15778', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('86d81812-f71a-4d78-8c45-649039e15778', foundational, proportionality_as_genuine_limit).
narrative_ontology:cs_axiom_status(proportionality_as_genuine_limit, holdable).
narrative_ontology:cs_axiom_grounding('86d81812-f71a-4d78-8c45-649039e15778', proportionality_as_genuine_limit, conventional).
narrative_ontology:cs_axiom('86d81812-f71a-4d78-8c45-649039e15778', foundational, mobility_as_non_absolute).
narrative_ontology:cs_axiom_status(mobility_as_non_absolute, holdable).
narrative_ontology:cs_axiom_grounding('86d81812-f71a-4d78-8c45-649039e15778', mobility_as_non_absolute, conventional).
narrative_ontology:cs_reference_frame('86d81812-f71a-4d78-8c45-649039e15778', treaty_proportionality_equilibrium).
narrative_ontology:cs_drift_state('86d81812-f71a-4d78-8c45-649039e15778', contemporary_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('86d81812-f71a-4d78-8c45-649039e15778', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, host_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, receiving_employers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, mobile_citizens).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, static_workers_host_states).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, mobile_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret treaty provisions on free movement and proportionality. Issue infringement decisions against member states. Develop doctrinal tests for what counts as a legitimate national interest. Their authority depends on maintaining the treaty as a living legal order.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federation_institutions, agenda_setter,
    institutional, generational, analytical, continental).

% Defend national welfare systems and labor markets by invoking proportionality to restrict incoming mobile citizens' access to benefits or jobs. Must justify restrictions before supranational review. Retain more sovereignty than under a pure integration model but less than under pure intergovernmentalism.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, host_state_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, host_state_governments, beneficiary).

% Exercise the right to move and work across member states. Must navigate varying national rules on residence, welfare eligibility, and professional recognition. Face legal uncertainty about whether their presence will be judged proportionate by host authorities.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, mobile_citizens, payer,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, mobile_citizens, beneficiary).

% Compete for jobs and housing in sectors where mobile citizens are willing to work for lower wages or worse conditions. They do not choose mobility policy but experience its labor market effects directly. Their exit options are constrained by localized skills and family ties.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, static_workers_host_states, payer,
    powerless, biographical, constrained, national).

% Recruit workers from across the federation without navigating visa systems. Benefit from wage differentials and expanded applicant pools. Their business models depend on continued legal access to cross-border labor.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, receiving_employers, beneficiary,
    powerful, biographical, mobile, continental).

% Third-country nationals who perform similar work to mobile citizens but are entirely outside the free movement framework. They are excluded from the treaty bargain and would contest the preferential treatment of intra-federation mobility if granted voice.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, excluded_tcn_labor, excluded,
    powerless, biographical, trapped, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables labor and service mobility across member states while preserving a legal mechanism for host states to defend legitimate national interests against market externalities, avoiding both market fragmentation and regulatory race-to-the-bottom.
% TRANSFER_FUNCTION: Transfers labor supply from sending to receiving regions; transfers regulatory authority from member states to supranational judiciary on proportionality review; transfers fiscal and social costs of mobility to host state welfare systems and static low-wage workers.
% ABSENT_VOICES: Third-country nationals are structurally excluded from free movement rights and would demand equal treatment if present. Subnational regions most affected by labor inflows have limited voice relative to central governments that negotiate treaty terms.
% DISAPPEARANCE_RATIONALE: If the proportionality-bounded free movement framework vanished, member states would either reimpose full border controls or be forced into complete labor market integration; the current graduated balance would collapse into one of the polar alternatives, reorganizing labor markets, welfare systems, and federation legitimacy.
% FOUNDING_PROBLEM: How to build an economic federation with free movement of persons without dissolving member state welfare systems and labor market protections, and without triggering a race-to-the-bottom in social standards.
% FOUNDING_PROBLEM_CORROBORATION: Supranational institutions and federalist scholars attest the problem remains live. Sovereigntist parties and some national constitutional courts attest the founding problem was solved by allowing too much restriction. Independent comparative federalism scholars from outside both camps document the persistent tension between market integration and social protection as a structural feature of incomplete federalism.
narrative_ontology:disappearance_verdict(federation_membership_treaty__subsidiarity_balance, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__subsidiarity_balance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_treaty__subsidiarity_balance, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__subsidiarity_balance, 0.53, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__subsidiarity_balance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__subsidiarity_balance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.53) is moderate-to-high because the proportionality framework, while enabling coordination, systematically leaves mobile citizens with incomplete welfare and labor market access and imposes wage competition on static workers. Suppression (0.55) is moderate because the constraint structurally suppresses both the integrationist alternative (open borders) and the sovereigntist alternative (national closure), channeling politics into proportionality litigation. Theater_ratio (0.40) reflects the significant performative dimension of proportionality tests, where host states must construct legal justifications that often mask protectionist intent. Accessibility_collapse (0.50) captures the fact that genuine alternatives (full federal labor law harmonization or full renationalization) are institutionally available in discourse but blocked by treaty lock-in. Resistance (0.60) is elevated because both sovereignty and integration advocates actively contest the middle ground. The measurement series show extraction and theater rising as the federation deepened and the Court expanded proportionality jurisprudence, with a slight moderation at the interval end as member state pushback intensified.
 *
 * PERSPECTIVAL GAP:
 *   The federation judiciary experiences this constraint as genuine coordination through legal doctrine; from this seat, proportionality is a refined balancing act that sustains the federation's legitimacy. Mobile citizens experience a hybrid: the right to move is real (benefit), but the contingency of welfare access and the risk of deportation for public policy reasons make the right conditional (cost). Static workers experience near-pure extraction: they did not choose mobility but bear its labor market externalities. Host state governments experience net benefit relative to a deeper integration scenario, but still pay sovereignty costs relative to a looser confederation. The engine will compute these seats differently: the agenda-setter seats derive low directionality from their control over the proportionality test; the static worker seat derives high directionality from its lack of exit and concentrated cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to host_state_governments (they collect regulatory autonomy and the power to define 'legitimate national interest'), receiving_employers (they collect a wider labor pool without bearing full social integration costs), and mobile_citizens (they collect the coordination benefit of a cross-border labor market). Victim declarations map to static_workers_host_states (they pay through wage and condition competition) and mobile_citizens (they pay through restricted access to social assistance and persistent legal uncertainty). The dual positioning of mobile_citizens and host_state_governments is structurally accurate for a tangled rope: both are coordinated and both pay, though asymmetrically. No directionality overrides are needed because the structural derivation correctly places mobile_citizens near the symmetric midpoint (listed in both beneficiary and victim) and static workers near the full-target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring both coordination and asymmetric extraction. A pure rope reading (integration_primary) would fail to account for the host state restriction rights and the incomplete rights of mobile workers. A pure snare reading (sovereignty_primary) would fail to account for the genuine legal certainty and market access that the framework provides. The tangled_rope classification captures that the same legal mechanismâproportionality reviewâsimultaneously coordinates mobility and extracts from the mobile and static populations that bear its costs. The mandate is not yet atrophied: the founding problem (how to combine mobility with welfare state preservation) remains live, and the constraint's function has not degraded into pure theater, though the theater_ratio indicates significant performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_asymmetric_impact,
    'Does the proportionality test structurally favor host state public policy claims over mobile citizen mobility rights in practice, despite doctrinal neutrality?',
    'Quantitative analysis of supranational court proportionality rulings: win rates for host states versus mobile citizens across welfare, public policy, and public security domains.',
    'If host states win disproportionately, the effective extraction from mobile citizens is higher than the base metric suggests and the constraint leans toward sovereignty_primary in operation despite subsidiarity_balance in doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_asymmetric_impact, empirical, 'Whether proportionality tests are asymmetric in practice').

omega_variable(
    equilibrium_drift_under_crisis,
    'Does the subsidiarity balance collapse into sovereignty_primary under economic or security stress, or does the enforcement mechanism hold?',
    'Comparative case study of member state behavior during financial, public health, and energy crises: frequency and success rate of mobility restrictions and derogations.',
    'If the balance collapses repeatedly, the constraint functions as a scaffold in crisis times and a rope in normal times, suggesting a cyclical rather than stable classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equilibrium_drift_under_crisis, empirical, 'Cyclical stability of the subsidiarity balance').

omega_variable(
    kernel_reading_contest,
    'This constraint is the subsidiarity_balance reading of a contested kernel. How would classification change if the authoritative interpreter adopted the integration_primary or sovereignty_primary reading?',
    'Jurisprudential tracking: identify sub-intervals when the court leaned integrationist or sovereigntist and recompute directionality for those periods.',
    'Integration_primary would reclassify host states toward payer and mobile citizens toward beneficiary; sovereignty_primary would invert mobile citizens to full target. The current classification is only stable under the subsidiarity_balance doctrinal regime.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Classification sensitivity to kernel reading adoption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmt_subsidiarity_tr_t0, federation_membership_treaty__subsidiarity_balance, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fmt_subsidiarity_tr_t8, federation_membership_treaty__subsidiarity_balance, theater_ratio, 8, 0.28).
narrative_ontology:measurement(fmt_subsidiarity_tr_t16, federation_membership_treaty__subsidiarity_balance, theater_ratio, 16, 0.35).
narrative_ontology:measurement(fmt_subsidiarity_tr_t24, federation_membership_treaty__subsidiarity_balance, theater_ratio, 24, 0.42).
narrative_ontology:measurement(fmt_subsidiarity_tr_t32, federation_membership_treaty__subsidiarity_balance, theater_ratio, 32, 0.45).
narrative_ontology:measurement(fmt_subsidiarity_tr_t40, federation_membership_treaty__subsidiarity_balance, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(fmt_subsidiarity_be_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(fmt_subsidiarity_be_t8, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(fmt_subsidiarity_be_t16, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(fmt_subsidiarity_be_t24, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(fmt_subsidiarity_be_t32, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(fmt_subsidiarity_be_t40, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 40, 0.53).

% Suppression requirement over time
narrative_ontology:measurement(fmt_subsidiarity_su_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fmt_subsidiarity_su_t8, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(fmt_subsidiarity_su_t16, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(fmt_subsidiarity_su_t24, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(fmt_subsidiarity_su_t32, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(fmt_subsidiarity_su_t40, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__sovereignty_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the federation_membership_treaty kernel. The integration_primary and sovereignty_primary readings instantiate structurally distinct constraints from the same treaty text. This decomposition follows the epsilon-invariance principle: the three readings have different epsilon values, different beneficiary/victim structures, and different empirical statuses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
