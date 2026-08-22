% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__hybrid_decay_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__hybrid_decay_reading, []).

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
 *   constraint_id: exercise_as_competence_maintenance__hybrid_decay_reading
 *   human_readable: Simulation-Centric Crisis Preparedness Maintenance (Hybrid Decay Reading)
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   Across high-hazard sectors — hospitals, aviation, nuclear operations,
 *   emergency management — crisis preparedness is maintained chiefly through
 *   scheduled simulation exercises, and certification, licensing, and
 *   insurance regimes reward documented drill completion. This story authors
 *   ONE reading of the contested kernel exercise_as_competence_maintenance:
 *   the hybrid_decay_reading, on which simulation genuinely exercises the
 *   procedural component of crisis competence (role execution,
 *   communications, equipment sequences) while the judgment-under-stakes
 *   component (improvisation, prioritization under ambiguity, recognizing
 *   when the script has broken) goes un-exercised and decays between rare
 *   real activations. On this reading the exercise regime solves a real
 *   coordination problem and simultaneously mints a preparedness credential
 *   that outruns delivered readiness; the difference is carried by those the
 *   organization serves. Claim and metrics are authored independently:
 *   claimed_type tangled_rope states the structure as this reading sees it
 *   (real coordination function, asymmetric yield, active enforcement); the
 *   metric values state the arrangement's observed operation. Sibling
 *   readings are separate constraint files linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   certifying_regulators: Agenda-setting enforcer
 *   (institutional/constrained) — mandates exercise counts and accepts drill
 *   records as evidence of preparedness - exercise_industry_vendors:
 *   Fee-collecting beneficiary (powerful/arbitrage) — sells the simulation
 *   apparatus the regime requires - organizational_executives: Beneficiary
 *   and internal commissioner (institutional/arbitrage) — collects the
 *   liability shield and confidence dividend drill records purchase -
 *   crisis_insurers: Secondary beneficiary (institutional/arbitrage) — prices
 *   premiums off documented drill compliance - frontline_responders: Payer
 *   with real partial gains (organized/identity_locked) — retains procedural
 *   fluency while their improvisation edge erodes unmeasured -
 *   protected_public: Principal payer (powerless/trapped) — bears the
 *   residual risk of judgment failures in real events -
 *   safety_science_researchers: Analytical observer (moderate/analytical) —
 *   documents the transfer gap from outside the benefiting parties
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, 0.62).
domain_priors:suppression_score(exercise_as_competence_maintenance__hybrid_decay_reading, 0.58).
domain_priors:theater_ratio(exercise_as_competence_maintenance__hybrid_decay_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__hybrid_decay_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__hybrid_decay_reading, "Simulation-Centric Crisis Preparedness Maintenance (Hybrid Decay Reading)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__hybrid_decay_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__hybrid_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__hybrid_decay_reading, 'c3839fa5-3d6c-4918-a7e9-ef15d241957f').
narrative_ontology:cs_kernel_codification('c3839fa5-3d6c-4918-a7e9-ef15d241957f', formalized).
narrative_ontology:cs_authority_grounding('c3839fa5-3d6c-4918-a7e9-ef15d241957f', extraction).
narrative_ontology:cs_interpretation_layer_present('c3839fa5-3d6c-4918-a7e9-ef15d241957f').
narrative_ontology:cs_reading_relation('c3839fa5-3d6c-4918-a7e9-ef15d241957f', exercise_as_competence_maintenance__simulation_sufficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('c3839fa5-3d6c-4918-a7e9-ef15d241957f', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('c3839fa5-3d6c-4918-a7e9-ef15d241957f', foundational, competence_kernel_bifurcates_under_exercise).
narrative_ontology:cs_axiom_status(competence_kernel_bifurcates_under_exercise, holdable).
narrative_ontology:cs_axiom_grounding('c3839fa5-3d6c-4918-a7e9-ef15d241957f', competence_kernel_bifurcates_under_exercise, empirically_contingent).
narrative_ontology:cs_axiom('c3839fa5-3d6c-4918-a7e9-ef15d241957f', secondary, simulation_transfer_is_component_specific).
narrative_ontology:cs_axiom_status(simulation_transfer_is_component_specific, holdable).
narrative_ontology:cs_axiom_grounding('c3839fa5-3d6c-4918-a7e9-ef15d241957f', simulation_transfer_is_component_specific, empirically_contingent).
narrative_ontology:cs_reference_frame('c3839fa5-3d6c-4918-a7e9-ef15d241957f', component_matched_exercise_regime).
narrative_ontology:cs_drift_state('c3839fa5-3d6c-4918-a7e9-ef15d241957f', contemporary_simulation_heavy_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c3839fa5-3d6c-4918-a7e9-ef15d241957f', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, certifying_regulators).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_industry_vendors).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_executives).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, crisis_insurers).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, protected_public).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_responders).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__hybrid_decay_reading, drill_completion_equals_readiness).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__hybrid_decay_reading, auditable_preparedness_metric).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce the exercise requirements high-hazard organizations must meet for accreditation and operating licenses: minimum drill frequencies, scenario categories, documentation standards. Accept completed-exercise records as primary evidence of preparedness and collect administrative legibility from that role. Their exit is limited: the certification framework they administer is their mandate, and abandoning drill-count evidence would mean rebuilding evidentiary standards across every regulated sector.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, certifying_regulators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__hybrid_decay_reading, certifying_regulators, beneficiary).

% Design, stage, and score the simulations organizations purchase to satisfy exercise mandates. Revenue scales with mandated drill volume and with the audit documentation the regime demands. They operate across jurisdictions and client sectors, so demand shocks in any one market are absorbable.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_industry_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Commission and approve the annual exercise program, sign the attestation packets, and present drill outcomes to boards, regulators, insurers, and the press. Completed-exercise records reduce perceived operational risk, support liability defenses, and lower insurance premiums. Executive tenure is shorter than the interval between major real events, so the confidence dividend is collected long before any judgment-gap failure arrives.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_executives, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_executives, agenda_setter).

% Price operational and catastrophe coverage using documented training compliance as a proxy for preparedness; drill-complete organizations earn premium discounts. Their exposure sits in the tail: a judgment-failure disaster voids the proxy's promise, but premiums were collected throughout.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, crisis_insurers, beneficiary,
    institutional, biographical, arbitrage, global).

% Staff the drills and would staff the real event. Rehearsal builds genuine fluency: role execution, radio discipline, and equipment sequences run without deliberation after enough repetitions. What rehearsal does not build is the capacity the real event demands when the scenario stops matching the script — triage under ambiguity, improvising with degraded resources, overriding the plan. That capacity erodes between rare real activations, and when improvised calls go wrong afterward, blame lands on the responder who made them. Leaving the profession would mean leaving the identity built on being the trained one.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_responders, payer,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_responders, beneficiary).

% Patients, passengers, and residents served by high-hazard organizations. They cannot opt out of hospital care, transit systems, or municipal emergency response, and they do not see exercise records. They receive whatever readiness the organization actually has: full procedural execution, and whatever judgment remains after years without real-stakes activation.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, protected_public, payer,
    powerless, generational, trapped, regional).

% Study transfer from simulated to real performance, publish on the gap between drill scores and incident-command outcomes, and testify after disasters. They hold no operational authority; their influence runs through occasional regulatory attention and post-event investigation reports.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, safety_science_researchers, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_executives).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__hybrid_decay_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Exercises synchronize distributed teams on shared procedures: they fix role assignments, communication protocols, and equipment sequences so the mechanical layer of crisis response executes without renegotiation under time pressure, and they give certifiers, insurers, and executives a common legible artifact for preparedness claims.
% TRANSFER_FUNCTION: Moves operating budget and staff hours from service delivery into the exercise apparatus; moves documented-compliance confidence upward from drill records to executives, regulators, and insurers; moves the unpriced residual — readiness of the judgment layer — downward onto the public the organization serves.
% ABSENT_VOICES: Survivors and bereaved families of events where drilled procedure ran correctly but improvised judgment failed are absent from scenario design; responders themselves rarely author the scenarios that test them, since scenarios are written by the same hierarchy whose decisions the exercise will not stress. Both groups would object that the exercises certify the easy half.
% DISAPPEARANCE_RATIONALE: Certification regimes would lose their primary evidence artifact overnight; insurers would reprice or withdraw coverage pending new preparedness evidence; procedural fluency would decay on its own timeline without rehearsal; organizations would need to reconstruct preparation around live incident rotations or accept unhedged risk. The arrangements of every named seat depend on the regime existing.
% FOUNDING_PROBLEM: Real disasters repeatedly showed response teams executing procedures wrongly under pressure — unclear roles, broken communications, fumbled equipment sequences — consuming the scarce cognition the event demanded. Scheduled simulation was built to make the mechanical layer automatic so it would not compete with thinking during real events.
% FOUNDING_PROBLEM_CORROBORATION: Independent accident investigation boards and the peer-reviewed safety-science literature — neither inside the benefiting parties — attest both halves: post-event analyses credit rehearsal with materially better mechanical execution in real incidents, and the same analyses document judgment failures in exactly the script-breaking situations this reading predicts. No benefiting party's self-assessment is needed for either finding.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__hybrid_decay_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__hybrid_decay_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__hybrid_decay_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.62 is authored for the standing arrangement under contest — simulation-centric competence maintenance as this reading assesses it: a real procedural subsidy (drills do build executable fluency) wrapped in a confidence yield that exceeds delivered readiness, because the certified artifact vouches for a judgment layer the arrangement never touches. Suppression 0.58 is a raw structural property, unscaled by power or scope: nothing bans judgment-bearing training, but mandated drill counts, certification dependence, and liability-driven aversion to generating failure records crowd alternatives out of budgets and calendars. Theater 0.48 reflects heavily scripted casts, pre-briefed responders, and success-oriented debriefs alongside a genuinely functional procedural core. Accessibility_collapse 0.38: alternatives persist (no-notice exercises, decision-forcing cases, secondment to live incidents) — costly, poorly legible to auditors, but not collapsed. Resistance 0.42: accident-board findings, safety-science critique, and veteran skepticism constitute steady, unresolved pushback. All three tracked series share one seven-point grid (t=0..24, step 4). Coalition note: the principal payer seat is powerless individually but holds coalition levers — tort litigation after judgment-failure disasters and electoral pressure on certifying regulators — which is why measured resistance likely understates latent opposition. Scope note: national and regional scopes make verifying actual (versus documented) readiness hard; the engine folds that into effective extraction, while the authored suppression scalar remains untouched by any such scaling.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the regulator and vendor seats the arrangement is a functioning training economy with clean compliance artifacts; from the executive seat it is a risk instrument that pays out steadily; from the responder seat it delivers real procedural skill while quietly taxing an unmeasured capacity they personally depend on; from the public seat it is an invisible wager they never agreed to. The engine computes per-seat classifications from the structural data; this divergence — one structure, different computed types by seat — is the expected output, not noise.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (regulators, vendors, executives, insurers) derive low d at those seats; victim declarations (responders, public) derive high d. One correction is warranted: the derivation reads frontline_responders' presence in victims[] toward near-full-target d, but their procedural gains are material and daily, netting them modestly target-side rather than fully targeted — hence the organized-atom override at d=0.6. The public seat needs no override: trapped exit and generational exposure place it at the full-target end on the derivation alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — procedural chaos under pressure — is still live, and the arrangement still addresses it, so this is not a zombie mandate; mandatrophy is not resolved. What has changed is mandate scope: rehearsal of procedures has quietly expanded into certification of overall readiness, a job the exercise format cannot do for the judgment component. Holding tangled_rope prevents both mislabels: a rope reading would launder the confidence yield as pure coordination cost, and a snare reading would erase the genuine procedural benefit that makes the arrangement defensible at all. The extraction lives precisely in the gap between what the artifact certifies and what the arrangement delivers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_component_boundary_question,
    'Is crisis competence one unitary capacity, or two dissociable components — procedural execution and judgment-under-stakes — with different exercise requirements?',
    'Transfer-of-training designs that score real-event and high-fidelity-test performance by component, correlated with each organization''s exercise mix over time.',
    'If components dissociate, this reading''s victim set stands; if the sufficiency sibling is right, victims shrink to fidelity shortfalls; if the necessity sibling is right, victims expand to everyone relying on simulation for any component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_component_boundary_question, conceptual, 'Whether the kernel bifurcates — the load-bearing premise distinguishing this reading from both siblings.').

omega_variable(
    judgment_decay_rate,
    'How fast does un-exercised judgment-under-stakes capacity decay, and over what real-event base rate does the decay become casualty-relevant?',
    'Longitudinal linkage of incident-command outcomes to elapsed time since last real-stakes activation, controlling for procedural drill volume.',
    'Sets the size and urgency of the victim class; near-zero decay collapses this reading toward the sufficiency sibling, rapid decay pushes it toward the necessity sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judgment_decay_rate, empirical, 'Decay kinetics of the non-simulated competence component.').

omega_variable(
    high_fidelity_threshold,
    'Can sufficiently high-fidelity simulation — no-notice activation, unknown injects, genuine consequence pressure — cross the threshold into exercising judgment-under-stakes?',
    'Comparative outcome studies of organizations running no-notice, consequence-bearing exercises versus conventional scheduled drills.',
    'If a fidelity threshold exists, the decay claim narrows to low-fidelity regimes, the victim set contracts, and the enforcement picture shifts from masking to underinvestment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(high_fidelity_threshold, empirical, 'Whether the procedural/judgment exercise boundary tracks simulation fidelity.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the crowding-out of judgment-bearing training structural (measurement regimes, certification economics, liability exposure to recorded failure) or internalized (professionals fusing drill fluency with readiness identity)?',
    'Post-deregulation trajectory: if mandates loosened and judgment-bearing formats returned, suppression was structural; if drill-fluency preferences persisted without mandates, it is internalized.',
    'Internalized suppression travels with the agent after any reform, raising effective suppression above the structural measure and deepening the responder seat''s lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, conceptual, 'Structural versus internalized mechanism behind the suppression scalar.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__hybrid_decay_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(exer_tr_t4, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 4, 0.33).
narrative_ontology:measurement(exer_tr_t8, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement(exer_tr_t12, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement(exer_tr_t16, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(exer_tr_t24, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 24, 0.48).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(exer_be_t4, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(exer_be_t8, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(exer_be_t12, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(exer_be_t16, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(exer_be_t24, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(exer_su_t4, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 4, 0.43).
narrative_ontology:measurement(exer_su_t8, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(exer_su_t12, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 12, 0.49).
narrative_ontology:measurement(exer_su_t16, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(exer_su_t24, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__hybrid_decay_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'simulation maintains crisis competence' decomposes into three structurally distinct claims per the epsilon-invariance principle. This file authors the hybrid_decay claim (partial retention: procedural component exercised, judgment component decaying; victim set includes those harmed by failures in the non-simulated component). The simulation_sufficiency sibling authors full-retention-with-fidelity-gradients (smaller victim set: fidelity shortfalls only); the lived_catastrophe_necessity sibling authors zero-retention-without-real-stakes (larger victim set: all simulation-reliant preparedness). Epsilon differs across the family because the victim sets differ; the upstream general claim ('exercises maintain competence') is cited as evidence by each downstream contest. All three files link one another via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance__hybrid_decay_reading, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
