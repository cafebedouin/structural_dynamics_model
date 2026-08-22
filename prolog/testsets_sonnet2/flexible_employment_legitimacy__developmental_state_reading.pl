% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__developmental_state_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__developmental_state_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: flexible_employment_legitimacy__developmental_state_reading
 *   human_readable: Flexible Employment as State-Managed Transition to Formalization
 *   domain: labor economics/platform economy/social policy
 *
 * SUMMARY:
 *   This story instantiates the developmental-state reading of the
 *   flexible-employment-legitimacy kernel: flexible employment is authored
 *   here as a transitional labor-market form that the state is actively
 *   steering toward formalization by 2027, using a 12-point plan as the
 *   instrument of sequenced, managed conversion rather than either an
 *   efficient market-clearing mechanism (the market_efficiency_reading) or a
 *   structural extraction device (the precarity_extraction_reading). Under
 *   this reading, ε is assessed for the standing flexible-employment
 *   arrangement as the developmental-state narrative itself understands it:
 *   real coordination cost paid by workers now, in exchange for a credible,
 *   milestone-bearing state commitment to end the arrangement's precarity by
 *   a fixed date. The rising theater_ratio and suppression_requirement track
 *   a specific developmental-state risk this reading itself must confront:
 *   milestone slippage across the plan's already-observed history, which
 *   shifts the constraint's actual operation toward performative deferral
 *   even while its declared logic remains transitional.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, 0.52).
domain_priors:suppression_score(flexible_employment_legitimacy__developmental_state_reading, 0.44).
domain_priors:theater_ratio(flexible_employment_legitimacy__developmental_state_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__developmental_state_reading, scaffold).
narrative_ontology:human_readable(flexible_employment_legitimacy__developmental_state_reading, "Flexible Employment as State-Managed Transition to Formalization").
narrative_ontology:topic_domain(flexible_employment_legitimacy__developmental_state_reading, "labor economics/platform economy/social policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__developmental_state_reading).
narrative_ontology:has_sunset_clause(flexible_employment_legitimacy__developmental_state_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__developmental_state_reading, 'c51e5e84-0da5-45f4-9ea0-3df4026d88b7').
narrative_ontology:cs_kernel_codification('c51e5e84-0da5-45f4-9ea0-3df4026d88b7', formalized).
narrative_ontology:cs_authority_grounding('c51e5e84-0da5-45f4-9ea0-3df4026d88b7', extraction).
narrative_ontology:cs_interpretation_layer_present('c51e5e84-0da5-45f4-9ea0-3df4026d88b7').
narrative_ontology:cs_reading_relation('c51e5e84-0da5-45f4-9ea0-3df4026d88b7', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('c51e5e84-0da5-45f4-9ea0-3df4026d88b7', flexible_employment_legitimacy__precarity_extraction_reading, influences).
narrative_ontology:cs_axiom('c51e5e84-0da5-45f4-9ea0-3df4026d88b7', foundational, state_capacity_can_sequence_formalization).
narrative_ontology:cs_axiom_status(state_capacity_can_sequence_formalization, holdable).
narrative_ontology:cs_axiom_grounding('c51e5e84-0da5-45f4-9ea0-3df4026d88b7', state_capacity_can_sequence_formalization, instrumental).
narrative_ontology:cs_axiom('c51e5e84-0da5-45f4-9ea0-3df4026d88b7', secondary, transitional_precarity_is_justified_by_credible_endpoint).
narrative_ontology:cs_axiom_status(transitional_precarity_is_justified_by_credible_endpoint, holdable).
narrative_ontology:cs_axiom_grounding('c51e5e84-0da5-45f4-9ea0-3df4026d88b7', transitional_precarity_is_justified_by_credible_endpoint, empirically_contingent).
narrative_ontology:cs_reference_frame('c51e5e84-0da5-45f4-9ea0-3df4026d88b7', sequenced_developmental_formalization).
narrative_ontology:cs_drift_state('c51e5e84-0da5-45f4-9ea0-3df4026d88b7', post_2027_target_slippage_observation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c51e5e84-0da5-45f4-9ea0-3df4026d88b7', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, platform_workers_pending_formalization).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, state_labor_ministries).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, ruling_party_industrial_policy).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, platform_gig_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, informal_sector_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, platform_gig_workers).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__developmental_state_reading, developmental_state_capacity_doctrine).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__developmental_state_reading, sequenced_formalization_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the 12-point plan and the 2027 standardization target, treating flexible employment as an interim category to be actively steered toward contract formalization, benefits portability, and registered employment status. Sets milestones, publishes compliance benchmarks, and periodically revises the timeline. Its authority rests on the claim that the transition is real and being managed, not merely declared.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, state_labor_ministries, agenda_setter,
    institutional, generational, analytical, national).

% Work under flexible/gig arrangements now, without the protections the 2027 target promises later. Bear the current absence of benefits, job security, and bargaining leverage as the cost of a transition whose timeline is set and revised by others. If the plan delivers, they become the direct beneficiaries of formalization; if it slips, they remain in the same precarious position indefinitely.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, platform_gig_workers, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__developmental_state_reading, platform_gig_workers, beneficiary).

% Operate entirely outside the platform economy's visibility, in informal labor arrangements the formalization plan is meant to eventually absorb. Bear the cost of being defined as a 'pre-transition' population whose labor conditions are treated as an administrative pipeline problem rather than an immediate one, while receiving none of the interim income flexibility gig work at least offers.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, informal_sector_workers, payer,
    powerless, biographical, trapped, national).

% The subset of gig workers closest to formalization criteria (tenure, platform, sector) stand to gain registered status, portable benefits, and contract protections if the 2027 target is met. Their interests are represented in ministry consultations and pilot programs, giving them a seat the broader gig workforce lacks.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, platform_workers_pending_formalization, beneficiary,
    moderate, generational, constrained, national).

% Uses the developmental-state narrative of managed transition as evidence of state capacity and industrial planning competence, both domestically and to international investors and lenders. The 12-point plan functions as a legitimacy artifact for the broader governing project, independent of whether formalization is actually completed on schedule.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, ruling_party_industrial_policy, beneficiary,
    institutional, civilizational, analytical, national).

% Structure their labor supply around the flexible-employment classification and are the entities whose classification of workers the 2027 target would formally override. They are consulted in some technical working groups but are not treated as co-authors of the formalization roadmap; they would prefer the transition target remain indefinitely deferred and quietly lobby against binding milestones.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, gig_platform_operators, excluded,
    powerful, biographical, mobile, global).

% Track whether the ministry's formalization milestones are being met, publish independent labor-market surveys, and assess whether wage growth attributed to 'managed transition' is distinguishable from ordinary market wage dynamics. Their assessments are the primary outside check on whether the state's narrative matches outcomes.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, labor_economists_independent, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__developmental_state_reading, ruling_party_industrial_policy).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__developmental_state_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sequences a labor market transition: rather than mandating immediate universal formalization (which platforms and much of the informal sector could not absorb), the state stages a multi-year plan with milestones, pilot programs, and a fixed target date, coordinating platforms, workers, and regulators around a shared timeline for converting flexible arrangements into formal employment.
% TRANSFER_FUNCTION: In the interim period, income flexibility and reduced compliance cost flow to platforms and to workers who value flexibility; the deferred cost of the missing protections (benefits, security, bargaining power) is borne by gig and informal workers now, in exchange for a state commitment to formalize by 2027. Ruling-party industrial policy also draws legitimacy capital from the plan's existence regardless of delivery.
% ABSENT_VOICES: Informal sector workers outside platform visibility are the population the plan is nominally most ambitious about reaching, but they have no consultation seat comparable to registered gig workers or platform operators — the formalization roadmap is largely designed around the platform-visible population, leaving the harder-to-formalize informal sector as an afterthought in the plan's own text.
% DISAPPEARANCE_RATIONALE: If the state withdrew the formalization framework overnight, platform operators would face no near-term change (their classification practices would continue under whatever prior labor code existed), but workers and observers dispute whether anything of substance would be lost: platform-pending workers say a real, if incomplete, path to protection would vanish; skeptical economists argue the plan's absence would only remove the state's legitimacy cover, since the underlying market dynamics driving flexible employment predate and do not depend on the plan.
% FOUNDING_PROBLEM: Rapid platform-economy growth outpaced existing labor law, leaving a large and growing workforce without employment classification, benefits, or bargaining protections, while an abrupt reclassification mandate risked mass platform exit or worker income loss.
% FOUNDING_PROBLEM_CORROBORATION: The labor ministry attests the problem remains live and cites interim milestone progress. Independent labor economists corroborate that classification gaps remain real and unresolved, but their published tracking also shows repeated milestone slippage since the plan's initial 2023 announcement, raising doubt — from a source outside the benefiting ministry and ruling party — about whether the 2027 target functions as binding commitment or renewable deferral.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__developmental_state_reading, contested).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__developmental_state_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__developmental_state_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__developmental_state_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__developmental_state_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__developmental_state_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__developmental_state_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.40) reflecting a coordination-first read of the arrangement and rises modestly to 0.52 as milestone slippage accumulates and the interim costs to gig and informal workers compound without corresponding delivery. Theater ratio rises more sharply (0.22 to 0.40) because a growing share of the ministry's plan-related activity — renewed announcements, revised timelines, consultation cycles — substitutes for delivered formalization, which is the diagnostic signature a scaffold reading must take seriously: if theater keeps climbing past the sunset date, the developmental-state reading's own criteria for success are failing by its own lights. Suppression is authored moderate rather than low because enforcement of the interim classification (denying gig workers formal status pending the plan) is an active administrative act, not a passive default.
 *
 * DIRECTIONALITY LOGIC:
 *   State labor ministries and ruling-party industrial policy sit at the beneficiary end: they administer the plan and draw legitimacy capital from its existence. Platform-pending workers occupy a genuine intermediate position — real beneficiaries if the plan delivers, meaningfully worse off than a completed formalization if it does not. Gig and informal workers broadly are targets: trapped exit options, bearing the interim cost with no individual leverage over the milestone timeline. Platform operators are treated as excluded rather than beneficiary or victim under this reading, since the developmental-state narrative does not credit them with authorship of the transition, even though they benefit from its deferral in practice.
 *
 * MANDATROPHY ANALYSIS:
 *   The developmental-state reading is precisely the reading most exposed to mandatrophy: its legitimacy rests entirely on the transition remaining live and on schedule. If the founding problem (classification gap, absent worker protections) is real but the 2027 target repeatedly slips without consequence, the scaffold's sunset clause becomes decorative — the arrangement persists in exactly its current extractive form while its formal justification (transition, not steady state) erodes. The rising theater_ratio series is authored specifically to make this erosion visible: a scaffold whose theater keeps climbing toward its own sunset date is a scaffold converting into something closer to a tangled rope, and this story's temporal data is designed to let that computation surface rather than asserting it in the claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    milestone_credibility_ambiguity,
    'Does the 2027 standardization target represent a binding state commitment with real consequences for non-delivery, or a renewable deferral mechanism that resets legitimacy without ever converging on formalization?',
    'Track whether the 2027 date is met, extended, or quietly abandoned; compare against the plan''s own stated interim milestones and whether missed milestones trigger any institutional consequence (budget reallocation, ministerial accountability, legal exposure) versus simply being restated.',
    'If the target is repeatedly extended without consequence, the developmental-state reading''s scaffold classification becomes increasingly untenable on its own terms and the constraint''s actual operation converges toward the precarity_extraction_reading''s characterization, even though the two remain separately authored constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(milestone_credibility_ambiguity, empirical, 'Whether the 2027 target is a credible commitment or a renewable deferral device.').

omega_variable(
    reading_choice_grounding,
    'What specific evidence licenses treating this arrangement primarily through the developmental-state frame (managed transition) rather than through the market-efficiency frame (voluntary market clearing) or the precarity-extraction frame (structural surplus capture) — given that all three readings describe the same underlying platform labor arrangement?',
    'Compare the developmental-state reading''s predictions (milestone-linked improvement in worker protections, convergence of interim costs toward zero as formalization approaches) against the actual trajectory of wage growth, benefits coverage, and classification litigation; a trajectory that tracks platform bargaining power better than plan milestones would favor the extraction reading; a trajectory tracking labor supply/demand shocks better than plan announcements would favor the market-efficiency reading.',
    'This is a conceptual framing question, not resolvable by a single metric: it determines which of the three sibling constraints most accurately describes the arrangement''s actual operative logic, and different observers with access to the same data can reasonably select different readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_choice_grounding, conceptual, 'Under-determination between the three kernel readings given shared underlying data.').

omega_variable(
    developmental_state_capacity_authenticity,
    'Is the ministry''s demonstrated administrative capacity (pilot programs, consultation cycles, published benchmarks) sufficient evidence of genuine state capacity to complete formalization, or is capacity-signaling itself a substitute for capacity, serving the ruling party''s legitimacy interest independent of delivery?',
    'Compare resourcing and staffing trends in the implementing ministry against comparable completed formalization programs in peer economies; assess whether administrative capacity is being built (durable institutional infrastructure) or performed (announcement cycles without underlying staffing/budget growth).',
    'If capacity is largely performed rather than built, the vindicated proposition ''developmental_state_capacity_doctrine'' is itself contestable, and the beneficiary status of ruling_party_industrial_policy shifts from incidental to primary — closer to the extraction reading''s characterization of the arrangement as legitimacy capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developmental_state_capacity_authenticity, empirical, 'Whether administrative activity reflects genuine or performed state capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__developmental_state_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(flex_tr_t8, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(flex_tr_t16, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(flex_tr_t24, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(flex_tr_t36, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 36, 0.38).
narrative_ontology:measurement(flex_tr_t48, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 48, 0.4).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(flex_be_t8, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(flex_be_t16, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(flex_be_t24, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(flex_be_t36, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 36, 0.51).
narrative_ontology:measurement(flex_be_t48, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 48, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(flex_su_t8, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(flex_su_t16, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(flex_su_t24, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(flex_su_t36, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 36, 0.42).
narrative_ontology:measurement(flex_su_t48, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 48, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__developmental_state_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__precarity_extraction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'flexible employment legitimacy,' per the ε-invariance principle: measuring the arrangement through the developmental-state lens (managed transition, milestone-bound) yields a different ε, beneficiary/victim structure, and claimed type than measuring it through the market-efficiency lens (voluntary clearing, near-zero extraction) or the precarity-extraction lens (structural surplus capture, high extraction, victim-heavy). Each reading is authored as its own file with its own stable ε; they are linked here rather than merged because no single ε value could honestly represent all three framings simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
