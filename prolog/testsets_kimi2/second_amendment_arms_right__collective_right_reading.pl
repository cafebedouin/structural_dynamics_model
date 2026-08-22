% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment Collective Right Reading (State Militia Authority)
 *   domain: constitutional law / political philosophy / legal interpretation
 *
 * SUMMARY:
 *   This constraint story instantiates the collective right reading of the
 *   Second Amendment kernel: the Amendment protects state authority over
 *   militias, leaving individual firearms possession subject to plenary state
 *   and federal regulation outside organized militia service. It was the
 *   dominant professional consensus from the New Deal era until District of
 *   Columbia v. Heller (2008). The story treats the constitutional
 *   arrangement as a federalism coordination mechanism with low
 *   extractiveness, though it structurally displaces individual ownership
 *   claims from constitutional protection.
 *
 * KEY AGENTS:
 *   - State governments: Primary beneficiaries (institutional/constrained) â retain militia authority against federal interference
 *   - Individual firearms owners: Primary payers (organized/constrained) â denied constitutional shield for private possession, subject to legislative prohibition
 *   - Federal judiciary: Agenda-setter (institutional/analytical) â interprets and enforces the collective right framework through precedent
 *   - Gun rights advocates: Excluded voices (organized/constrained) â advance the individual right reading structurally marginalized by this framework
 *   - Legal academia (collective right tradition): Observer (institutional/analytical) â supplies historical and doctrinal arguments maintaining the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.2).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.55).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment Collective Right Reading (State Militia Authority)").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "constitutional law / political philosophy / legal interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, '44128084-6814-46f5-996d-b5b025e7d319').
narrative_ontology:cs_kernel_codification('44128084-6814-46f5-996d-b5b025e7d319', fixed_text).
narrative_ontology:cs_authority_grounding('44128084-6814-46f5-996d-b5b025e7d319', lineage).
narrative_ontology:cs_interpretation_layer_present('44128084-6814-46f5-996d-b5b025e7d319').
narrative_ontology:cs_reading_relation('44128084-6814-46f5-996d-b5b025e7d319', second_amendment_arms_right__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('44128084-6814-46f5-996d-b5b025e7d319', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('44128084-6814-46f5-996d-b5b025e7d319', foundational, state_militia_as_right_bearer).
narrative_ontology:cs_axiom_status(state_militia_as_right_bearer, holdable).
narrative_ontology:cs_axiom_grounding('44128084-6814-46f5-996d-b5b025e7d319', state_militia_as_right_bearer, conventional).
narrative_ontology:cs_axiom('44128084-6814-46f5-996d-b5b025e7d319', foundational, individual_arms_possession_unprotected).
narrative_ontology:cs_axiom_status(individual_arms_possession_unprotected, holdable).
narrative_ontology:cs_axiom_grounding('44128084-6814-46f5-996d-b5b025e7d319', individual_arms_possession_unprotected, conventional).
narrative_ontology:cs_reference_frame('44128084-6814-46f5-996d-b5b025e7d319', state_militia_authority_framework).
narrative_ontology:cs_drift_state('44128084-6814-46f5-996d-b5b025e7d319', post_heller_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('44128084-6814-46f5-996d-b5b025e7d319', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, individual_firearms_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain constitutional authority to organize, arm, and discipline state militias without federal disarmament or preemption. Individual firearms regulation remains within ordinary state police power, exercised through legislatures and subject to local political control.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Subject to state and federal firearms prohibitions and regulations without a constitutional shield for private possession unrelated to organized militia service. Must rely on legislative politics, not judicial rights claims, to resist or reform gun laws.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_firearms_owners, payer,
    organized, biographical, constrained, national).

% Interprets the Second Amendment as a federalism provision that limits federal power over state militias but does not constrain state regulation of individual arms. Maintains the doctrinal framework through precedent, law school training, and appellate review.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Advance an individual right reading that is treated as a category error within the collective right framework. Their historical evidence and normative arguments are structurally sidelined in federal appellate briefing and law school pedagogy.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, gun_rights_advocates, excluded,
    organized, biographical, constrained, national).

% Produces historical and doctrinal scholarship supporting the state militia reading. Observes the constitutional text as a federalism guarantee rather than a liberty provision, and trains successive generations of lawyers in that interpretive tradition.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, legal_academia_collective_right, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates authority between the federal government and the states by guaranteeing that the federal government may not disarm state militias, preserving a clear federalism boundary in military organization.
% TRANSFER_FUNCTION: Transfers discretionary authority over individual firearms regulation from the federal judiciary as a rights-enforcer to state legislatures and ordinary political processes; does not transfer material goods.
% ABSENT_VOICES: Individual right advocates and civic republican theorists who view armed possession as a personal or political liberty are structurally absent from the dominant interpretive framework; their arguments are rendered doctrinally inadmissible.
% DISAPPEARANCE_RATIONALE: If the collective right reading disappeared, federal courts would no longer treat the Second Amendment as a federalism provision shielding state militias. State regulatory authority over firearms would lose its constitutional cover, and the doctrinal framework permitting plenary regulation of individual possession would collapse, forcing gun policy to be renegotiated through different constitutional or statutory channels.
% FOUNDING_PROBLEM: The Founding generation sought to prevent the federal government from disarming state militias as a step toward centralized military tyranny, preserving state military capacity and a federal balance of armed power.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship from outside the benefiting parties attests the founding problem was real but time-bound; state governments and collective-right jurists assert the principle remains structurally necessary, but no external corroboration confirms that federal disarmament of state militias is a live threat in modern US governance.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__collective_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__collective_right_reading, 0.2, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).
:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.20 at interval end) because the constraint does not mandate prohibition or compel transfers; it merely allocates regulatory authority to states, leaving gun policy to ordinary political processes. Suppression is moderate (0.55) because the reading maintained dominance for decades through professional consensus and stare decisis, rendering the individual right reading institutionally disfavored. Theater ratio rises toward the end (0.40) as the reading becomes a dissenting position after Heller, increasingly maintained as interpretive tradition rather than binding doctrine. Accessibility collapse is moderate (0.45): the individual right alternative was always linguistically and historically available but doctrinally inaccessible until Heller. Resistance is substantial (0.60) due to persistent gun rights mobilization against the regulatory permissiveness the reading enables.
 *
 * PERSPECTIVAL GAP:
 *   The state government seat experiences this constraint as a protective federalism guarantee; the individual firearms owner seat experiences it as a constitutional silence that exposes them to regulation. The federal judiciary experiences it as a settled interpretive framework until Heller introduces seat divergence. The engine will compute these directionalities from beneficiary/victim status and exit modulation: states as beneficiaries with constrained exit (they cannot unilaterally change constitutional meaning), individuals as payers with constrained exit (they cannot exit the regulatory jurisdiction), and the judiciary as agenda-setter with analytical exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments are declared beneficiaries (low d, subsidized by the constraint's protection of their authority). Individual firearms owners are declared victims/payers (high d, the constraint extracts a constitutional shield from them). The federal judiciary is not a beneficiary in the rent-collecting sense but an agenda-setter administering the constraint; its directionality is analytically near-neutral. Gun rights advocates are excluded from the interpretive framework, receiving high effective suppression through structural exclusion rather than direct extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â federal disarmament of state militias â is historically attested but no longer live. The constraint persisted for decades after the problem became anachronistic, propping up state regulatory authority under a militia rubric. The low theater ratio for most of the interval suggests genuine coordination function (clear federalism boundary), while the late-interval rise in theater signals degradation toward performance after Heller overruled the framework. The mandatrophy is partially resolved by Heller but the reading persists in academic and political discourse as a zombie framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_right_kernel_contest,
    'Is the Second Amendment''s reference to ''the people'' a collective or individual right-bearing entity?',
    'Historical-linguistic analysis of 18th-century usage of ''the people'' in constitutional text; comparative analysis of state ratifying convention records.',
    'If ''the people'' is irreducibly individual, the collective right reading collapses into a false summit or tangled rope; if irreducibly collective, the individual right reading is the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_right_kernel_contest, conceptual, 'Core linguistic ambiguity driving the kernel contest').

omega_variable(
    professional_consensus_suppression,
    'Was the dominance of the collective right reading from the 1930s to 2000 maintained by evidentiary superiority or by institutional gatekeeping in legal education and appellate appointment?',
    'Sociological study of law school curriculum and citation networks; analysis of briefs and oral arguments in Second Amendment cases.',
    'If gatekeeping, suppression is higher than the doctrinal record suggests and the reading operates with more extraction; if evidentiary, the low epsilon reading is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(professional_consensus_suppression, empirical, 'Institutional vs evidentiary basis of interpretive dominance').

omega_variable(
    heller_override_residue,
    'Does the post-Heller persistence of the collective right reading in legal academia represent a genuine alternative interpretive tradition or inertial resistance to axiom overriding?',
    'Track citation rates and judicial adoption of collective right frameworks post-2008; measure whether the reading produces novel doctrinal applications or only repeats pre-Heller formulations.',
    'If inertial only, the constraint is degrading toward piton; if generative, it remains a live rope or tangled rope competing for restoration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(heller_override_residue, empirical, 'Post-overrule vitality of the collective right tradition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__collective_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(seco_tr_t20, second_amendment_arms_right__collective_right_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(seco_tr_t40, second_amendment_arms_right__collective_right_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(seco_tr_t60, second_amendment_arms_right__collective_right_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(seco_tr_t80, second_amendment_arms_right__collective_right_reading, theater_ratio, 80, 0.4).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__collective_right_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(seco_be_t20, second_amendment_arms_right__collective_right_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(seco_be_t40, second_amendment_arms_right__collective_right_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(seco_be_t60, second_amendment_arms_right__collective_right_reading, base_extractiveness, 60, 0.18).
narrative_ontology:measurement(seco_be_t80, second_amendment_arms_right__collective_right_reading, base_extractiveness, 80, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__collective_right_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(seco_su_t20, second_amendment_arms_right__collective_right_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(seco_su_t40, second_amendment_arms_right__collective_right_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(seco_su_t60, second_amendment_arms_right__collective_right_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(seco_su_t80, second_amendment_arms_right__collective_right_reading, suppression_requirement, 80, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
