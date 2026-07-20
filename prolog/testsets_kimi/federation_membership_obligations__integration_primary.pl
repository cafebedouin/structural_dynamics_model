% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__integration_primary, []).

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
 *   constraint_id: federation_membership_obligations__integration_primary
 *   human_readable: EU Free Movement Welfare Integration (Integration-Primary Reading)
 *   domain: political_economy/federalism/migration
 *
 * SUMMARY:
 *   This constraint story captures the integration_primary reading of the
 *   federation_membership_obligations kernel: the claim that free movement of
 *   workers and EU citizenship are constitutive of the European project, and
 *   that member state welfare boundaries must yield to mobility rights. The
 *   constraint is enforced through ECJ jurisprudence and Commission
 *   infringement proceedings, progressively opening national welfare systems
 *   to mobile EU citizens. It coordinates a transnational labor market and
 *   citizenship space while extracting fiscal costs and political sovereignty
 *   from receiving member states and imposing labor-market adjustment costs
 *   on static local workers. The claim is tangled_rope because the
 *   coordination function (single market integration, anti-discrimination) is
 *   inseparable from the asymmetric extraction (welfare fiscal burden on
 *   receiving states, wage pressure on local labor).
 *
 * KEY AGENTS:
 *   - mobile_workers: Primary beneficiary (moderate/mobile) â gain cross-border welfare portability regardless of contribution history
 *   - displaced_local_labor: Primary target (powerless/constrained) â bears wage compression and employment adjustment costs
 *   - receiving_member_states: Institutional payer/target (institutional/constrained) â loses welfare closure autonomy and bears fiscal costs
 *   - ecj: Agenda-setter and authority beneficiary (institutional/analytical) â expands jurisdiction via teleological case law
 *   - eu_commission: Agenda-setter and integration beneficiary (institutional/arbitrage) â enforces mobility rights against national closure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, 0.68).
domain_priors:suppression_score(federation_membership_obligations__integration_primary, 0.72).
domain_priors:theater_ratio(federation_membership_obligations__integration_primary, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "EU Free Movement Welfare Integration (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political_economy/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, '45764a7d-77b7-4612-89d5-4a7a5a33dc83').
narrative_ontology:cs_kernel_codification('45764a7d-77b7-4612-89d5-4a7a5a33dc83', formalized).
narrative_ontology:cs_authority_grounding('45764a7d-77b7-4612-89d5-4a7a5a33dc83', lineage).
narrative_ontology:cs_interpretation_layer_present('45764a7d-77b7-4612-89d5-4a7a5a33dc83').
narrative_ontology:cs_reading_relation('45764a7d-77b7-4612-89d5-4a7a5a33dc83', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('45764a7d-77b7-4612-89d5-4a7a5a33dc83', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('45764a7d-77b7-4612-89d5-4a7a5a33dc83', foundational, free_movement_constitutive_of_citizenship).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('45764a7d-77b7-4612-89d5-4a7a5a33dc83', free_movement_constitutive_of_citizenship, conventional).
narrative_ontology:cs_axiom('45764a7d-77b7-4612-89d5-4a7a5a33dc83', foundational, single_market_requires_welfare_portability).
narrative_ontology:cs_axiom_status(single_market_requires_welfare_portability, holdable).
narrative_ontology:cs_axiom_grounding('45764a7d-77b7-4612-89d5-4a7a5a33dc83', single_market_requires_welfare_portability, instrumental).
narrative_ontology:cs_reference_frame('45764a7d-77b7-4612-89d5-4a7a5a33dc83', eu_citizenship_as_constitutive).
narrative_ontology:cs_drift_state('45764a7d-77b7-4612-89d5-4a7a5a33dc83', post_enlargement_resistance_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('45764a7d-77b7-4612-89d5-4a7a5a33dc83', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__integration_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, ecj).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, eu_commission).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, receiving_member_states).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, eu_citizenship_supremacy_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, single_market_preeminence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Move across member states for employment and gain equal access to welfare benefits in receiving states regardless of prior contribution history in that state. Their mobility is protected by EU citizenship rights and actively enforced against member state resistance through ECJ rulings and Commission infringement procedures.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, mobile_workers, beneficiary,
    moderate, biographical, mobile, national).

% Competes with incoming mobile workers in local labor markets and bears concentrated adjustment costs including wage compression, employment insecurity, and reduced bargaining power. Does not exercise mobility and faces welfare systems strained by new claimants. Politically marginal in EU-level policy formulation.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, displaced_local_labor, payer,
    powerless, biographical, constrained, regional).

% Must open welfare systems to mobile EU citizens upon residence, bearing direct fiscal costs and administrative burden. National policy autonomy in welfare allocation and migration management is overridden by ECJ interpretations and Commission enforcement actions. Exit from the obligation requires treaty revision or EU departure, both structurally prohibitive.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, receiving_member_states, payer,
    institutional, generational, constrained, national).

% Expands EU citizenship and free movement rights through teleological case law interpretation, progressively lowering residence and contribution requirements for welfare access. Authority grows as member state objections are legally overridden; the court functions as the primary interpretive engine of the integration-primary reading.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, ecj, agenda_setter,
    institutional, generational, analytical, continental).

% Monitors member state compliance with free movement rules, launches infringement proceedings against welfare restrictions, and proposes legislation to lock in mobility rights. Represents the integration imperative against national closure and collects institutional leverage from enforcement.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, eu_commission, agenda_setter,
    institutional, generational, arbitrage, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__integration_primary, mobile_workers).
narrative_ontology:fixing_cost_class(federation_membership_obligations__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a single labor market and citizenship space across sovereign states by guaranteeing that workers can move without losing social protections, preventing labor-market fragmentation and solving the hold-up problem where mobility is deterred by fear of welfare exclusion.
% TRANSFER_FUNCTION: Transfers welfare entitlement and fiscal burden from receiving member states to mobile workers who have not necessarily contributed to the receiving system, while transferring regulatory authority over welfare boundaries from national administrations to the ECJ and EU Commission.
% ABSENT_VOICES: Displaced local workers in receiving states whose wages and employment are affected by labor mobility are largely excluded from EU-level policy formulation; national electorates seeking welfare closure are structurally sidelined by supremacy doctrines and infringement procedures.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, mobile workers would lose cross-border welfare portability, member states would reassert welfare closure and residence requirements, the ECJ would lose a primary vector of authority expansion, and the single labor market would fragment into national silos with divergent social rights.
% FOUNDING_PROBLEM: Post-war Europe needed to prevent national labor market fragmentation and social dumping while enabling economic integration; the founding problem was how to coordinate welfare systems across borders so that mobility was not penalized by loss of social protection.
% FOUNDING_PROBLEM_CORROBORATION: The Commission and ECJ attest the problem remains live, citing ongoing mobility barriers. Member state governments and displaced labor unions attest the founding problem has shifted: the current challenge is welfare system sustainability and labor market protection, not mobility facilitation. Academic federalism scholars outside both camps note the original coordination function has been partially superseded by authority expansion dynamics.
narrative_ontology:disappearance_verdict(federation_membership_obligations__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__integration_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_obligations__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__integration_primary, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint obliges receiving states to extend welfare benefits to mobile workers without equivalent contribution history, transferring fiscal resources and political authority. Suppression (0.72) is higher because the constraint persists only through active ECJ enforcement and Commission infringement actions against resisting member states; national democratic majorities cannot unilaterally modify it. Theater ratio (0.30) is moderate: much of the activity is functional case law and enforcement, but a growing share involves member states performing compliance while seeking political opt-outs and negotiating de facto exceptions. Accessibility collapse (0.55) reflects that while exit from the EU exists, it is politically and economically catastrophic for most member states, making alternatives largely theoretical. Resistance (0.58) captures sustained member state litigation, political pushback, and the Brexit referendum as a radical resistance act.
 *
 * PERSPECTIVAL GAP:
 *   The ECJ seat computes the constraint as genuine coordination advancing European citizenship and market unity; the receiving member state seat computes it as sovereignty extraction; the displaced local labor seat computes it as labor-market snare. The engine derives this divergence from the same structural data: the agenda-setters see a rope, the institutional payers see a tangled rope, and the powerless payers see a snare. The authored claim (tangled_rope) does not adjudicate the divergence but names the structural reality that coordination and extraction are fused in the same arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers are structural beneficiaries: they receive welfare access and anti-discrimination protection (d near the beneficiary end). The ECJ and Commission are institutional beneficiaries with analytical and arbitrage exit; they collect authority and integration leverage (d low). Displaced local labor is a trapped target: low power, constrained exit, no mobility option, and bears concentrated adjustment costs (d near the full-target end). Receiving member states are institutional targets: despite high global power, they are specifically targeted by this constraint's enforcement machinery, with exit options constrained by treaty lock-in (d around 0.75).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling because it names both the coordination function (single market labor mobility, anti-discrimination) and the extraction function (fiscal transfer to non-contributors, sovereignty loss, local labor displacement). A purely coordinative reading would ignore the asymmetric burden on receiving states and local workers; a purely extractive reading (snare) would ignore the genuine market-integration and citizenship function that workers and employers rely on. The temporal measurements show extraction rising with enlargement and ECJ activism, confirming the hybrid character rather than a pure coordination decaying into extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federalization_by_judiciary,
    'Has the integration of welfare and mobility rights proceeded beyond what treaty drafters intended, constituting judicial federalization via case law rather than negotiated political agreement?',
    'Historical treaty intent analysis through travaux prÃ©paratoires and comparative constitutional law review of ECJ jurisprudence against explicit treaty text.',
    'If yes, the constraint''s authority rests on interpretive expansion rather than enacted consensus, affecting its legitimacy classification and potentially shifting the reading toward axiom-overriding drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalization_by_judiciary, conceptual, 'Whether ECJ authority expansion exceeds original treaty intent').

omega_variable(
    asymmetric_adjustment_distribution,
    'Do the economic gains from labor mobility outweigh the concentrated adjustment costs borne by displaced local labor in receiving states?',
    'Distributional economic analysis of mobility impacts across skill levels, regions, and sectors, separating aggregate GDP effects from localized wage and employment effects.',
    'If costs are concentrated and gains diffuse, the constraint operates as strongly asymmetric extraction on the displaced population; if gains are widely shared even locally, the extraction severity is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_adjustment_distribution, empirical, 'Distributional incidence of mobility gains and losses').

omega_variable(
    integration_vs_sovereignty_resolvability,
    'Is the tension between integration_primary and member_sovereignty_primary readings resolvable within the current EU legal framework, or does it require a constitutional settlement?',
    'Comparative analysis of federal systems and tracking of treaty revision or exit dynamics.',
    'If unresolvable, the constraint will face continued resistance and potential terminal drift; if resolvable, the kernel may stabilize into a new synthesized reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(integration_vs_sovereignty_resolvability, conceptual, 'Whether the kernel''s competing readings can coexist indefinitely').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(federation_integ_tr_t0, federation_membership_obligations__integration_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(federation_integ_tr_t6, federation_membership_obligations__integration_primary, theater_ratio, 6, 0.18).
narrative_ontology:measurement(federation_integ_tr_t12, federation_membership_obligations__integration_primary, theater_ratio, 12, 0.22).
narrative_ontology:measurement(federation_integ_tr_t18, federation_membership_obligations__integration_primary, theater_ratio, 18, 0.25).
narrative_ontology:measurement(federation_integ_tr_t24, federation_membership_obligations__integration_primary, theater_ratio, 24, 0.28).
narrative_ontology:measurement(federation_integ_tr_t30, federation_membership_obligations__integration_primary, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(federation_integ_be_t0, federation_membership_obligations__integration_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(federation_integ_be_t6, federation_membership_obligations__integration_primary, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(federation_integ_be_t12, federation_membership_obligations__integration_primary, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(federation_integ_be_t18, federation_membership_obligations__integration_primary, base_extractiveness, 18, 0.65).
narrative_ontology:measurement(federation_integ_be_t24, federation_membership_obligations__integration_primary, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(federation_integ_be_t30, federation_membership_obligations__integration_primary, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(federation_integ_su_t0, federation_membership_obligations__integration_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(federation_integ_su_t6, federation_membership_obligations__integration_primary, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(federation_integ_su_t12, federation_membership_obligations__integration_primary, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(federation_integ_su_t18, federation_membership_obligations__integration_primary, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(federation_integ_su_t24, federation_membership_obligations__integration_primary, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(federation_integ_su_t30, federation_membership_obligations__integration_primary, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__member_sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__selective_solidarity).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the federation_membership_obligations kernel. Each reading has a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family because they compete to interpret the same legal-political kernel (EU citizenship and free movement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
