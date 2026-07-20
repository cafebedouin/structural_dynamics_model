% ============================================================================
% CONSTRAINT STORY: liability_attribution__deployer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__deployer_liability, []).

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
 *   constraint_id: liability_attribution__deployer_liability
 *   human_readable: Deployer Primary Liability for AI Harm
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This constraint story models the deployer_liability reading of the
 *   contested liability_attribution kernel. Under this reading,
 *   deployersâparties that control deployment context and make operational
 *   decisionsâbear primary legal liability for AI system harms. The rule
 *   coordinates victim redress by providing a clear defendant and
 *   incentivizes deployment-care, but it also extracts disproportionately
 *   from deployers who must police opaque models they did not create, while
 *   developers and foundation model providers externalize risk. The
 *   claim/metric independence is maintained: the constraint is claimed as
 *   tangled_rope because it combines genuine coordination with asymmetric
 *   extraction, while the metrics describe a moderately high extraction
 *   profile with rising enforcement.
 *
 * KEY AGENTS:
 *   - deployers (payer/victim): Organizations deploying AI, bearing primary liability and due diligence burdens despite upstream opacity (organized/national/constrained).
 *   - ai_system_developers (beneficiary): Upstream creators externalizing deployment risk while retaining licensing revenue (institutional/global/arbitrage).
 *   - foundation_model_providers (beneficiary): General-purpose model developers shielded from downstream harm claims (institutional/global/arbitrage).
 *   - liability_regulators (agenda_setter): Legislators and courts setting the liability standard and adjudicating its boundaries (institutional/national/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__deployer_liability, 0.68).
domain_priors:suppression_score(liability_attribution__deployer_liability, 0.72).
domain_priors:theater_ratio(liability_attribution__deployer_liability, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, extractiveness, 0.68).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__deployer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__deployer_liability, "Deployer Primary Liability for AI Harm").
narrative_ontology:topic_domain(liability_attribution__deployer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__deployer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, '18e15543-43f2-4007-b601-8e47ee76f401').
narrative_ontology:cs_kernel_codification('18e15543-43f2-4007-b601-8e47ee76f401', formalized).
narrative_ontology:cs_authority_grounding('18e15543-43f2-4007-b601-8e47ee76f401', lineage).
narrative_ontology:cs_interpretation_layer_present('18e15543-43f2-4007-b601-8e47ee76f401').
narrative_ontology:cs_reading_relation('18e15543-43f2-4007-b601-8e47ee76f401', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('18e15543-43f2-4007-b601-8e47ee76f401', liability_attribution__shared_liability, coexists_with).
narrative_ontology:cs_axiom('18e15543-43f2-4007-b601-8e47ee76f401', foundational, deployment_control_creates_accountability).
narrative_ontology:cs_axiom_status(deployment_control_creates_accountability, holdable).
narrative_ontology:cs_axiom_grounding('18e15543-43f2-4007-b601-8e47ee76f401', deployment_control_creates_accountability, deontological).
narrative_ontology:cs_axiom('18e15543-43f2-4007-b601-8e47ee76f401', foundational, upstream_opacity_deployer_burden).
narrative_ontology:cs_axiom_status(upstream_opacity_deployer_burden, holdable).
narrative_ontology:cs_axiom_grounding('18e15543-43f2-4007-b601-8e47ee76f401', upstream_opacity_deployer_burden, instrumental).
narrative_ontology:cs_reference_frame('18e15543-43f2-4007-b601-8e47ee76f401', deployer_control_accountability).
narrative_ontology:cs_drift_state('18e15543-43f2-4007-b601-8e47ee76f401', post_foundation_model_scaling, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('18e15543-43f2-4007-b601-8e47ee76f401', '').
narrative_ontology:cs_kernel_id(liability_attribution__deployer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, ai_system_developers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, deployers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations that integrate AI systems into products or services and place them on the market. Under this liability rule, they bear primary legal and financial responsibility for harms arising from deployment, including due diligence burdens to investigate opaque model behavior they did not create. Their exit options are constrained by market participation requirements and the cost of liability insurance.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, deployers, payer,
    organized, biographical, constrained, national).

% Organizations that design and train AI systems but do not deploy them directly to end users. They externalize deployment risk downstream under this rule, retaining revenue from licensing while avoiding direct exposure to harm caused by contextual application of their models.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_system_developers, beneficiary,
    institutional, generational, arbitrage, global).

% Entities that develop general-purpose foundation models and provide them via API or weights to downstream deployers. They are shielded from downstream harm liability, insulating them from claims arising from specific deployment contexts they do not control.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, foundation_model_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Legislators, courts, and regulatory agencies that establish and enforce the primary liability rule. They set the standard of care and due diligence expected of deployers, adjudicate claims, and determine the boundary between deployment harm and upstream design defect.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, liability_regulators, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, administrable standard for AI harm liability by anchoring accountability to the party that makes deployment decisions and controls the operational context, reducing legal uncertainty for plaintiffs and creating a direct incentive for pre-deployment risk assessment.
% TRANSFER_FUNCTION: Moves liability risk, compliance cost, and due diligence burden from upstream developers and foundation model providers to downstream deployers, while transferring to plaintiffs the right to sue the deployer as the most accessible defendant.
% ABSENT_VOICES: Deployers who lack technical visibility into foundation model internals but are held liable for their behavior; shared-liability advocates who argue causal contribution should be distributed across the value chain; affected end users who might prefer upstream deep-pocket defendants but are not consulted in the liability design.
% DISAPPEARANCE_RATIONALE: If deployer primary liability disappeared, upstream developers and foundation model providers would face direct exposure to harm claims, insurance and contractual markets would reallocate risk along the value chain, deployers would reduce due diligence spending, and plaintiffs would lose the clarity of a single accessible defendant.
% FOUNDING_PROBLEM: Legal uncertainty over who pays for AI harm, coupled with under-deterrence of risky deployments and barriers to victim redress when upstream creators are unreachable or judgment-proof.
% FOUNDING_PROBLEM_CORROBORATION: Consumer protection agencies and tort scholars attest the redress gap from outside the beneficiary set. Developer and foundation model industry associations contest the framing, arguing the problem is misuse rather than design, corroborating the contested status from outside the deployer-payer seat.
narrative_ontology:disappearance_verdict(liability_attribution__deployer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__deployer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__deployer_liability, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(liability_attribution__deployer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__deployer_liability, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__deployer_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__deployer_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__deployer_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because deployers assume liability for harms arising from model behavior they cannot fully inspect or control, while upstream parties retain benefits without corresponding exposure. Suppression (0.72) reflects the active legal and institutional enforcement required to maintain this liability boundary against competing theories (shared liability, developer liability). Theater_ratio (0.25) is moderate-low: the coordination function (clear defendant, victim redress) is real, but a growing share of deployer due diligence activity is performative compliance with opacity they cannot resolve. Accessibility_collapse (0.65) captures the narrowing of judicial and legislative pathways for shared or upstream liability as the deployer-primary frame becomes default. Resistance (0.40) tracks organized deployer pushback through litigation and lobbying.
 *
 * PERSPECTIVAL GAP:
 *   The deployer seat experiences the constraint as extraction (bearing costs for opaque upstream systems), while the developer and foundation model provider seats experience it as coordination (clear rules, risk externalization). The regulator seat sees a solvable administrative framework. The engine computes this divergence from structural data: identical scope but opposite beneficiary/victim declarations, with deployers constrained and upstream actors mobile.
 *
 * DIRECTIONALITY LOGIC:
 *   Deployers are declared victims (high d, near target): they pay the liability transfer and lack exit from the legal jurisdiction. Developers and foundation model providers are declared beneficiaries (low d, near beneficiary): they collect risk externalization and enjoy global arbitrage exit. Regulators are agenda_setters with analytical exit, sitting near neutral. No overrides are needed because the structural derivation matches the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a rope would ignore the asymmetric extraction: deployers pay for upstream opacity they did not create. Classifying as a snare would ignore the genuine coordination in victim redress and deployment incentives. Tangled_rope is the only category that admits both the coordination function (clear liability standard) and the extraction function (risk offloading to the party with least informational control). Mandatrophyâpersistence after the founding problem is solvedâis flagged as contested because the redress gap is partially addressed by this rule, but the rule may persist even if opacity renders deployer control a fiction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint instantiates the deployer_liability reading of the liability_attribution kernel; the disagreement with sibling readings developer_liability and shared_liability is located on whether deployment context control, creation of underlying capability, or distributed causal contribution should determine primary liability. Is this reading stable or will empirical pressure from upstream opacity collapse it into shared liability?',
    'Comparative regulatory impact assessment across jurisdictions adopting different readings; litigation outcomes testing deployer capacity to exercise due diligence over opaque models.',
    'If deployers cannot meaningfully control opaque models, this reading either shifts to scaffold status (transitional until distributed liability is enacted) or drifts toward piton (theatrical maintenance of a liability fiction); if control is demonstrable, the reading stabilizes as tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Stability of the deployer_liability reading against empirical opacity challenges and sibling reading pressure.').

omega_variable(
    opacity_as_extraction_vector,
    'Does the opacity of foundation models function as an irreducible information asymmetry that makes deployer liability extractive, or is opacity a solvable due diligence cost that deployers can contract around?',
    'Industry cost accounting of deployer due diligence; contractual analysis of whether developers disclose sufficient technical documentation to render liability non-extractive.',
    'If opacity is irreducible and developers withhold information, the liability rule extracts from deployers without giving them means to comply, strengthening the snare/tangled_rope classification; if opacity is contractually solvable, the extraction is the legitimate price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_as_extraction_vector, empirical, 'Whether model opacity is a solvable cost or an extraction mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__deployer_liability, theater_ratio, 0, 0.1).
narrative_ontology:measurement(liab_tr_t4, liability_attribution__deployer_liability, theater_ratio, 4, 0.12).
narrative_ontology:measurement(liab_tr_t8, liability_attribution__deployer_liability, theater_ratio, 8, 0.15).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__deployer_liability, theater_ratio, 12, 0.2).
narrative_ontology:measurement(liab_tr_t16, liability_attribution__deployer_liability, theater_ratio, 16, 0.23).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__deployer_liability, theater_ratio, 20, 0.25).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__deployer_liability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(liab_be_t4, liability_attribution__deployer_liability, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(liab_be_t8, liability_attribution__deployer_liability, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(liab_be_t12, liability_attribution__deployer_liability, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(liab_be_t16, liability_attribution__deployer_liability, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(liab_be_t20, liability_attribution__deployer_liability, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__deployer_liability, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(liab_su_t4, liability_attribution__deployer_liability, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(liab_su_t8, liability_attribution__deployer_liability, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(liab_su_t12, liability_attribution__deployer_liability, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(liab_su_t16, liability_attribution__deployer_liability, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(liab_su_t20, liability_attribution__deployer_liability, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__deployer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, developer_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, shared_liability).

% DUAL FORMULATION NOTE:
% The liability_attribution kernel decomposes into three structurally distinct readings because the epsilon values and victim/beneficiary structures differ materially across deployer, developer, and shared liability frames. Each reading has its own epsilon, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
