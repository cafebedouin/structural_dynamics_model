% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__civic_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__civic_right_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Second Amendment Civic-Rights Reading: Individual Arms Right Conditioned on Militia Participation
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel. The colloquial
 *   label 'the Second Amendment' covers three structurally distinct claims —
 *   an unconditioned individual right, a state-collective authority, and an
 *   individual right conditioned on civic militia participation — and per the
 *   epsilon-invariance principle they are authored as separate constraint
 *   stories linked through network.affects_constraints, because each carries
 *   its own beneficiary set, its own enforcement surface, and its own stable
 *   epsilon. This file authors only the third: the arrangement in which
 *   individuals hold a constitutional arms right whose protection is
 *   conditioned on participation in a well-regulated militia. Under this
 *   reading the militia is the gate: service creates the protected right,
 *   non-service leaves arms-bearing to ordinary statute, and the state both
 *   administers the gate and collects the defense capacity it yields.
 *   Assumptions stated: (1) the arrangement is modeled as it operates where
 *   enforced, blending the historical record of the militia-conditioned
 *   regime from 1791 onward with the reading's contemporary proposed
 *   instantiations; (2) epsilon's referent is the standing conditioned-rights
 *   arrangement as this reading holds it, assessed by this reading's own
 *   lights — not the unconditional regime the individual_right_reading would
 *   install; (3) the T=235 measurements are marked projected because the
 *   contemporary instantiation is proposed rather than fully operative. KEY
 *   AGENTS (by structural relationship): - militia_service_members:
 *   dual-positioned participant (organized/constrained) — bears the service
 *   burden, holds the conditioned right - non_participating_citizens:
 *   gate-excluded class (moderate/constrained) — bears exclusion from the
 *   protected right - state_militia_authority: agenda setter
 *   (institutional/arbitrage) — defines eligibility, administers service,
 *   receives defense capacity - federal_militia_regulator: organizing
 *   authority (institutional/mobile) — prescribes discipline and can rewrite
 *   service conditions - historically_excluded_populations: structurally
 *   excluded (powerless/trapped) — outside eligibility itself -
 *   judicial_interpreters: analytical observer (institutional/analytical) —
 *   adjudicates the gate's scope
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.55).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.55).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Second Amendment Civic-Rights Reading: Individual Arms Right Conditioned on Militia Participation").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "constitutional/political").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, '4ff32d9b-8fe3-4dd0-8117-c226832459bd').
narrative_ontology:cs_kernel_codification('4ff32d9b-8fe3-4dd0-8117-c226832459bd', fixed_text).
narrative_ontology:cs_authority_grounding('4ff32d9b-8fe3-4dd0-8117-c226832459bd', lineage).
narrative_ontology:cs_interpretation_layer_present('4ff32d9b-8fe3-4dd0-8117-c226832459bd').
narrative_ontology:cs_reading_relation('4ff32d9b-8fe3-4dd0-8117-c226832459bd', second_amendment_scope__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ff32d9b-8fe3-4dd0-8117-c226832459bd', second_amendment_scope__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('4ff32d9b-8fe3-4dd0-8117-c226832459bd', foundational, arms_right_conditioned_on_militia_participation).
narrative_ontology:cs_axiom_status(arms_right_conditioned_on_militia_participation, holdable).
narrative_ontology:cs_axiom_grounding('4ff32d9b-8fe3-4dd0-8117-c226832459bd', arms_right_conditioned_on_militia_participation, instrumental).
narrative_ontology:cs_axiom('4ff32d9b-8fe3-4dd0-8117-c226832459bd', secondary, militia_service_constitutes_civic_standing).
narrative_ontology:cs_axiom_status(militia_service_constitutes_civic_standing, holdable).
narrative_ontology:cs_axiom_grounding('4ff32d9b-8fe3-4dd0-8117-c226832459bd', militia_service_constitutes_civic_standing, deontological).
narrative_ontology:cs_reference_frame('4ff32d9b-8fe3-4dd0-8117-c226832459bd', universal_civic_militia_order).
narrative_ontology:cs_drift_state('4ff32d9b-8fe3-4dd0-8117-c226832459bd', contemporary_post_heller_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('4ff32d9b-8fe3-4dd0-8117-c226832459bd', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_service_members).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, state_militia_authority).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, non_participating_citizens).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, militia_service_members).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, historically_excluded_populations).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, civic_republican_citizenship_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, anti_standing_army_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enroll in their state's militia, attend scheduled musters and training days, keep and maintain their own arms and equipment, and place themselves under militia discipline when called. In return they hold a constitutionally protected individual right to keep and bear arms that non-members do not hold. Stepping out of service ends the protection; continuing it costs recurring time, equipment expense, and exposure to call-up. Historically they supplied their own muskets and received little or no pay.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_service_members, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, militia_service_members, payer).

% Do not join, or are dropped from, the militia rolls — by choice, occupation, conscience, age, or failure to meet standards. They may still own arms under general statutes but sit outside the constitutional protection the conditioned right confers, exposed to regulation and confiscation that members are shielded from. Their path back inside the protection runs through the service they declined or cannot perform.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, non_participating_citizens, payer,
    moderate, biographical, constrained, national).

% Defines who counts as militia, sets muster schedules and equipment standards, maintains the rolls, and disciplines absence — the office that decides what participation means and therefore who clears the gate. It receives a trained, partially self-funded defense force, and it can restructure eligibility, shift to volunteer formations, or lean on federal organizing acts when the local system sags.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, state_militia_authority, agenda_setter,
    institutional, generational, arbitrage, regional).

% Prescribes the organization, armament, and discipline of the militia under its organizing power, funding and standardizing what the states administer. It can rewrite the service conditions nationally — converting the citizen muster into organized reserves, as it did in the early twentieth century — and absorbs the defense capacity the arrangement yields.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, federal_militia_regulator, agenda_setter,
    institutional, generational, mobile, national).

% Enslaved people, most free Black men, women, and in many places the propertyless were barred from militia enrollment by statute and custom during the arrangement's formative centuries. Barred from the militia, they were barred from the conditioned right and from the civic standing that service confers; they had no seat where eligibility was defined and no service path that admission rules would recognize.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, historically_excluded_populations, excluded,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, historically_excluded_populations, payer).

% Adjudicate what the arms right protects and whether service conditions fall inside or outside its scope, hearing the competing readings in court. Their doctrinal choices determine whether the gate binds, dissolves, or migrates into permitting and training statutes.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, judicial_interpreters, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__civic_right_reading, state_militia_authority).
narrative_ontology:fixing_cost_class(second_amendment_scope__civic_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the founding-era defense dilemma — secure the polity without a standing army — by tying individual armament to organized, trained, accountable collective service: the militia coordinates who may hold protected arms with who contributes trained defense capacity, so armament and accountability are produced by the same act.
% TRANSFER_FUNCTION: Moves defense labor and equipment costs from militia-service members to the state (members historically mustered at their own expense, unpaid or minimally paid), and moves legal standing differentially: protected arms-bearing to those who serve, exposure to ordinary regulation for those who do not.
% ABSENT_VOICES: Those the eligibility line kept out of the militia — enslaved people, most free Black men, women, and in many jurisdictions the propertyless — were never seated where participation was defined, and would contest the gate as administered caste; conscientious objectors who cannot take up arms on principle sit in the gate's unpriced blind spot. Their objection survives in dissenting records, anti-muster resistance, and later equality jurisprudence rather than in the arrangement's own proceedings.
% DISAPPEARANCE_RATIONALE: If the conditioned arrangement vanished overnight, the defense-labor transfer ends (the state must fund defense professionally or accept unorganized armament), the rights differential dissolves (members and non-members merge into one legal class under whichever successor reading governs), and militia institutions lose the recruitment rationale the gate provides — the load-bearing moves are precisely the transfer and the differential.
% FOUNDING_PROBLEM: A free state needs defense but distrusts standing armies as instruments of tyranny: arm and organize the citizen body itself, with service as the condition that keeps widespread armament trained, accountable, and attached to civic obligation.
% FOUNDING_PROBLEM_CORROBORATION: Divided along the kernel's own fault line, with attesters outside the beneficiary set on each side: military historians and the legislative record of the militia system's replacement (Volunteer Act, National Defense Act, the death of compulsory musters) corroborate that the founding solution is obsolete; civic-republican political theory and the recurring civilian-control literature corroborate that the underlying problem — accountable republican defense — remains live. No neutral single attester exists; the arrangement's own beneficiaries (militia authorities) assert liveness and are discounted accordingly.
narrative_ontology:disappearance_verdict(second_amendment_scope__civic_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__civic_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__civic_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_scope__civic_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__civic_right_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__civic_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type and metrics are authored independently. The claim is tangled_rope because the arrangement possesses both halves the category requires: a genuine coordination function (linking armament to trained, accountable collective defense addresses the founding-era standing-army dilemma) and asymmetric extraction through the same structure (non-participants are excluded from a protection members hold, and members fund the arrangement with undercompensated labor and self-equipped materiel), held together by active enforcement (muster discipline and fines historically; administrative denial and permit gating in modern proposals). Metrics are descriptive: extractiveness 0.55 — moderate, because the service-for-right exchange retains real reciprocity while the gate's exclusion and the labor transfer run one way; suppression 0.55 — enforcement machinery is real (fines, courts-martial, roll purges, and in modern form administrative denial) but the compliance path — serving — remains open, so alternatives are narrowed rather than closed; theater_ratio 0.38 at endpoint — the historical record shows heavy performative phases (ceremonial musters, nominal rolls, fines in lieu of service) with modern proposals re-substantivizing service; accessibility_collapse 0.60 — once the conditioning is understood the free-ride alternative collapses, but the serve path keeps a genuine route into compliance open, well short of natural-law closure; resistance 0.60 — muster evasion helped kill the compulsory system, individual-right advocates reject the conditioning outright, and equality movements attacked the eligibility line. The three measurement series share one six-point grid (T=0, 47, 94, 141, 188, 235). The suppression_requirement series is authored because this story specifically traces enforcement-capacity change: buildup in the early republic, decay through the nineteenth and twentieth centuries, partial revival in contemporary conditional-regime proposals. The theater trajectory documents Goodhart drift (service replaced by fines, then ceremony), and the extractiveness hump tracks reciprocity thinning faster than exclusion lifted. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From state_militia_authority the arrangement is a defense-provisioning system it designed and can restructure: the gate recruits, trains, and part-funds the force it is owed. From non_participating_citizens the identical structure is a rights denial administered by someone else's institution. From militia_service_members it is a reciprocal bargain whose fairness turns entirely on how the service burden is priced — the same seat is beneficiary and payer, which is why its directionality sits near the middle. Same-level lateral dynamics: militia members and non-participants are the same nominal power class (ordinary citizens), differentiated solely by the gate — the member's exit forfeits the protection, and the non-participant's only path back in is the service they declined, so two structurally similar citizens face opposite sides of the same wall.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation. state_militia_authority declares as beneficiary with arbitrage-grade rule control — nearest the beneficiary pole. militia_service_members declares as beneficiary and payer with constrained exit — pulled toward symmetry: they pay recurring service and equipment costs and receive the protected right. non_participating_citizens declares as payer with constrained exit — near the target pole: they bear exclusion and receive nothing from the structure. historically_excluded_populations declares as payer and excluded with trapped exit and no power — the full-target end, since the eligibility line itself placed them outside the bargain. federal_militia_regulator administers and absorbs defense capacity at national scale; judicial_interpreters hold the analytical seat. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce the structural relationships directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both mislabelings. Reading the arrangement as pure coordination ignores that the same gate that trains the willing strips protection from the unwilling and historically drew the line along race and sex; reading it as pure extraction ignores that the gate solves a real defense problem and that a compliance path remains open. The hybrid holds both. On genealogy: the founding problem (defend a free state without a standing army) is contested rather than dead — civilian-control literature keeps the underlying worry alive while military historians attest the universal-militia solution is obsolete — so the contested-status-by-world-rearranges cell fires no dead-mandate mismatch, but the contested genealogy is recorded. The theater trajectory documents genuine proxy substitution across the nineteenth century without resolving into mandatrophy, because the modern reading proposes re-substantiation rather than inheriting the shell; the endpoint theater of 0.38 prices the residual performance in any near-term instantiation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the civic_right_reading of the second_amendment_scope kernel; what structural changes would adoption of a sibling reading produce?',
    'Compare the compiled sibling files: individual_right_reading deletes the service gate (beneficiary set expands to all individuals, regulatory authority contracts, epsilon falls toward coordination-cost levels); collective_right_reading deletes individual right-holders entirely (states become the right''s holders, and the victim set restructures around federal-state relations).',
    'Classification is reading-relative: the same text supports a near-rope arrangement under the individual reading and a different extraction topology under the collective reading; cross-reading comparison must join on kernel_id, not constraint_id.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Which reading of the Second Amendment kernel governs determines the beneficiary set, the existence of the service gate, and epsilon.').

omega_variable(
    militia_substrate_viability,
    'Does any existing or reconstructable institution satisfy this reading''s demand for genuine militia participation, or does the conditioning gate attach only to nominal rolls?',
    'Institutional audit of organized reserves, state defense forces, and select-service infrastructure against civic-republican criteria (broad availability, real training, accountability to civil authority); legislative tracking of service-conditioned permit proposals.',
    'If no viable substrate exists, the gate operates on a performance and the arrangement drifts toward inertial persistence; if one exists or is built, the conditioning prices a real service and the hybrid structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_substrate_viability, empirical, 'Whether a functional militia substrate exists for the service gate to bind to.').

omega_variable(
    eligibility_boundary_incidence,
    'Who counts as militia-eligible — and therefore who is offered the bargain and who is simply shut out — under a contemporary instantiation?',
    'Statutory comparison of eligibility definitions (age, sex, residency, fitness, discharge status) across service-conditioned proposals and historical militia acts; demographic incidence modeling of each candidate definition.',
    'Narrow eligibility concentrates exclusion on identifiable classes (reproducing the historical caste pattern and raising effective extraction on the shut-out); broad eligibility diffuses the gate but strains the training capacity that justifies it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_boundary_incidence, conceptual, 'The eligibility line determines the gate''s incidence and thus the extraction topology.').

omega_variable(
    service_burden_pricing,
    'Is the service burden fairly priced against the protected right — does the exchange approximate fair terms, or does the authority capture defense labor below its cost?',
    'Cost accounting of militia service (time, equipment, opportunity cost, call-up risk) against the market price of equivalent professional defense provision and against the measurable value of the protected right.',
    'If the burden systematically exceeds the right''s value, the arrangement trends toward pure extraction dressed as civic bargain; if priced fairly, the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(service_burden_pricing, empirical, 'Fairness of the service-for-right exchange.').

omega_variable(
    enforcement_revival_trajectory,
    'Will service-conditioned enforcement revive through permitting and training statutes, or continue decaying until the reading survives only as interpretation?',
    'Track state legislation tying carry permits or purchase eligibility to militia-style service or structured training; track judicial treatment of such conditions.',
    'Revival sustains the actively enforced hybrid; continued decay leaves a nominally conditioned right whose gate no longer binds, shifting the operational profile toward inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_revival_trajectory, empirical, 'Trajectory of the gate''s enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 0, 235).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__civic_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t47, second_amendment_scope__civic_right_reading, theater_ratio, 47, 0.28).
narrative_ontology:measurement_basis(seco_tr_t47, observed).
narrative_ontology:measurement(seco_tr_t94, second_amendment_scope__civic_right_reading, theater_ratio, 94, 0.55).
narrative_ontology:measurement_basis(seco_tr_t94, observed).
narrative_ontology:measurement(seco_tr_t141, second_amendment_scope__civic_right_reading, theater_ratio, 141, 0.6).
narrative_ontology:measurement_basis(seco_tr_t141, observed).
narrative_ontology:measurement(seco_tr_t188, second_amendment_scope__civic_right_reading, theater_ratio, 188, 0.52).
narrative_ontology:measurement_basis(seco_tr_t188, observed).
narrative_ontology:measurement(seco_tr_t235, second_amendment_scope__civic_right_reading, theater_ratio, 235, 0.38).
narrative_ontology:measurement_basis(seco_tr_t235, projected).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__civic_right_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t47, second_amendment_scope__civic_right_reading, base_extractiveness, 47, 0.54).
narrative_ontology:measurement_basis(seco_be_t47, observed).
narrative_ontology:measurement(seco_be_t94, second_amendment_scope__civic_right_reading, base_extractiveness, 94, 0.61).
narrative_ontology:measurement_basis(seco_be_t94, observed).
narrative_ontology:measurement(seco_be_t141, second_amendment_scope__civic_right_reading, base_extractiveness, 141, 0.56).
narrative_ontology:measurement_basis(seco_be_t141, observed).
narrative_ontology:measurement(seco_be_t188, second_amendment_scope__civic_right_reading, base_extractiveness, 188, 0.5).
narrative_ontology:measurement_basis(seco_be_t188, observed).
narrative_ontology:measurement(seco_be_t235, second_amendment_scope__civic_right_reading, base_extractiveness, 235, 0.55).
narrative_ontology:measurement_basis(seco_be_t235, projected).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__civic_right_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(seco_su_t0, observed).
narrative_ontology:measurement(seco_su_t47, second_amendment_scope__civic_right_reading, suppression_requirement, 47, 0.55).
narrative_ontology:measurement_basis(seco_su_t47, observed).
narrative_ontology:measurement(seco_su_t94, second_amendment_scope__civic_right_reading, suppression_requirement, 94, 0.42).
narrative_ontology:measurement_basis(seco_su_t94, observed).
narrative_ontology:measurement(seco_su_t141, second_amendment_scope__civic_right_reading, suppression_requirement, 141, 0.26).
narrative_ontology:measurement_basis(seco_su_t141, observed).
narrative_ontology:measurement(seco_su_t188, second_amendment_scope__civic_right_reading, suppression_requirement, 188, 0.34).
narrative_ontology:measurement_basis(seco_su_t188, observed).
narrative_ontology:measurement(seco_su_t235, second_amendment_scope__civic_right_reading, suppression_requirement, 235, 0.55).
narrative_ontology:measurement_basis(seco_su_t235, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__civic_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, collective_right_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the 'Second Amendment' label into three constraint stories per the epsilon-invariance principle: the readings assign different right-holders and different gates, so no single epsilon can span them. Upstream/downstream structure: individual_right_reading currently anchors controlling doctrine and exerts the strongest structural pressure on this reading's viability; this reading influences both siblings at the margin — conditioning proposals shape scope doctrine (toward individual_right_reading) and keep state military authority salient (toward collective_right_reading). Cross-reading comparison must join on kernel_id.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
