% ============================================================================
% CONSTRAINT STORY: competence_occupation__simulation_sufficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__simulation_sufficiency, []).

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
 *   constraint_id: competence_occupation__simulation_sufficiency
 *   human_readable: Simulation Sufficiency as Competence Occupation
 *   domain: organizational/safety/competence
 *
 * SUMMARY:
 *   In high-reliability organizations, the claim that simulation-based drills
 *   are sufficient to occupy the competence kernel and prevent skill decay
 *   has become an institutionalized training standard. This reading treats
 *   simulation compliance as the observable endpoint of competence
 *   maintenance, framing skill decay as a frequency-and-fidelity optimization
 *   problem. The simulation industry becomes the primary structural
 *   beneficiary, while frontline operators and host organizations bear the
 *   latent risk of decayed live-performance skills. This constraint is one
 *   reading of the contested competence_occupation kernel; sibling readings
 *   hold that only real catastrophic incidents (real_incident_necessity) or
 *   continuous hybrid mechanisms (hybrid_occupation) can truly maintain
 *   competence.
 *
 * KEY AGENTS:
 *   - simulation_vendors: Primary beneficiary (organized/global) â captures training budgets via sufficiency claim
 *   - accreditation_bodies: Agenda-setter (institutional/national) â sets and audits the standard, benefits from auditable metric
 *   - frontline_operators: Primary payer/target (moderate/constrained) â bears operational risk of masked skill decay
 *   - host_organizations: Secondary payer/target (institutional/constrained) â bears latent liability and procurement cost
 *   - safety_science_critics: Excluded voice (moderate/mobile) â documents transfer limits, marginalized from standard-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, 0.58).
domain_priors:suppression_score(competence_occupation__simulation_sufficiency, 0.52).
domain_priors:theater_ratio(competence_occupation__simulation_sufficiency, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, tangled_rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Simulation Sufficiency as Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "organizational/safety/competence").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, 'bfd00c8a-80b8-4924-9b46-6957981bd100').
narrative_ontology:cs_kernel_codification('bfd00c8a-80b8-4924-9b46-6957981bd100', formalized).
narrative_ontology:cs_authority_grounding('bfd00c8a-80b8-4924-9b46-6957981bd100', expertise).
narrative_ontology:cs_interpretation_layer_present('bfd00c8a-80b8-4924-9b46-6957981bd100').
narrative_ontology:cs_reading_relation('bfd00c8a-80b8-4924-9b46-6957981bd100', competence_occupation__real_incident_necessity, forecloses).
narrative_ontology:cs_reading_relation('bfd00c8a-80b8-4924-9b46-6957981bd100', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('bfd00c8a-80b8-4924-9b46-6957981bd100', foundational, simulation_constitutes_sufficient_competence_occupation).
narrative_ontology:cs_axiom_status(simulation_constitutes_sufficient_competence_occupation, holdable).
narrative_ontology:cs_axiom_grounding('bfd00c8a-80b8-4924-9b46-6957981bd100', simulation_constitutes_sufficient_competence_occupation, empirically_contingent).
narrative_ontology:cs_axiom('bfd00c8a-80b8-4924-9b46-6957981bd100', foundational, skill_decay_is_frequency_fidelity_solvable).
narrative_ontology:cs_axiom_status(skill_decay_is_frequency_fidelity_solvable, holdable).
narrative_ontology:cs_axiom_grounding('bfd00c8a-80b8-4924-9b46-6957981bd100', skill_decay_is_frequency_fidelity_solvable, empirically_contingent).
narrative_ontology:cs_reference_frame('bfd00c8a-80b8-4924-9b46-6957981bd100', simulation_based_competence_maintenance).
narrative_ontology:cs_drift_state('bfd00c8a-80b8-4924-9b46-6957981bd100', accreditation_compliance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bfd00c8a-80b8-4924-9b46-6957981bd100', '').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_vendors).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, accreditation_bodies).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, host_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, host_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and sell high-fidelity simulation systems to high-reliability organizations; revenue scales with mandated simulation hours and fidelity upgrades; derive structural benefit when regulation treats simulation completion as sufficient evidence of competence maintenance.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_vendors, beneficiary,
    organized, biographical, mobile, global).

% Set training-sufficiency standards and audit compliance; derive institutional authority from defining measurable benchmarks; simulation-hour metrics simplify oversight and create an auditable compliance trail that extends their governance role.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, accreditation_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Must complete mandated simulation hours to maintain licensure and employment; operate under the institutional assumption that simulation-certified competence transfers to live high-stakes scenarios; bear the direct operational and moral risk when rare real-world failures exceed simulation parameters.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, frontline_operators, payer,
    moderate, biographical, constrained, national).

% Procure simulation systems to satisfy accreditation mandates; benefit from schedulable, repeatable training and reduced operational downtime; simultaneously bear latent accident liability, insurance costs, and reputational risk when simulation-sufficiency claims mask decay in live-performance skills.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, host_organizations, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, host_organizations, beneficiary).

% Publish empirical findings on simulation-to-live transfer decay and the limits of frequency-fidelity optimization; systematically marginalized from standard-setting committees where vendor and compliance interests dominate; their research contradicts the sufficiency claim but does not alter accredited metrics.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, safety_science_critics, excluded,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__simulation_sufficiency, simulation_vendors).
narrative_ontology:fixing_cost_class(competence_occupation__simulation_sufficiency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides repeatable, scalable, and safe rehearsal environments for high-risk operational skills where live practice is dangerous, expensive, or statistically rare; solves the collective-action problem of distributing standardized training across dispersed organizations.
% TRANSFER_FUNCTION: Moves training budgets and regulatory approval toward simulation vendors and accreditation bodies; transfers latent operational risk to frontline operators and host organizations by substituting simulation compliance for verified live competence.
% ABSENT_VOICES: Safety scientists documenting transfer-decay limits and context-bound skill loss; frontline operators reporting simulator-to-live performance gaps; hybrid-occupation advocates arguing that competence requires continuous multi-mechanism reinforcement including line audits and live procedural rehearsal.
% DISAPPEARANCE_RATIONALE: If the sufficiency claim vanished, training procurement would shift away from pure-simulation vendors, accreditation metrics would require redesign around hybrid or outcome-based benchmarks, and host organizations would face immediate pressure to diversify training investment; the safety-training field would reorganize around a different competence-occupation mechanism.
% FOUNDING_PROBLEM: Live catastrophic failures are too rare and dangerous to serve as the primary training ground for high-stakes operational skills; early simulation technology provided a safe, repeatable rehearsal alternative that reduced reliance on catastrophe for learning.
% FOUNDING_PROBLEM_CORROBORATION: Aviation and nuclear safety historians corroborate the original danger and rarity of live rehearsal. Corroboration for the sufficiency extension, however, comes primarily from simulation-industry research and vendor-affiliated studies; independent transfer studies from outside the beneficiary set dispute that simulation alone is sufficient, and no disinterested party attests the founding problem remains unsolved.
narrative_ontology:disappearance_verdict(competence_occupation__simulation_sufficiency, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__simulation_sufficiency, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__simulation_sufficiency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__simulation_sufficiency, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__simulation_sufficiency_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__simulation_sufficiency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the capture of training budgets and standard-setting authority by simulation vendors and accreditation regimes. Suppression (0.52) captures the marginalization of hybrid-occupation and real-incident alternatives as regulatory standards solidify around simulation metrics. Theater_ratio (0.55) is elevated because simulation-hour completion has become a compliance proxy that is increasingly decoupled from verified live competence. Accessibility_collapse (0.48) indicates that alternatives are still theoretically available but professionally costly to advocate. Resistance (0.42) reflects persistent but structurally marginalized criticism from safety scientists and some operators. The temporal series show gradual drift: as simulation became the compliance observable, base extraction and theater rose together while enforcement requirements hardened to protect the standard.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (accreditation bodies) experiences the constraint as a rational, auditable standard that simplifies oversight; the beneficiary seat (simulation vendors) experiences it as a revenue stream; the payer seats (frontline operators and host organizations) experience it as a compliance cost that may not map to live-skill retention. The engine should compute divergent classifications from these structural positions: low directionality for vendors and standard-setters, high directionality for constrained operators and risk-bearing institutions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (simulation_vendors, accreditation_bodies) have low directionality because they capture value and authority from the constraint's operation. Victims/payers (frontline_operators, host_organizations) have high directionality because they bear the cost, risk, and compliance burden. Frontline operators are constrained by professional certification requirements; host_organizations are constrained by accreditation standards tied to operating authority. No override is needed because the structural derivation chain captures these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate was safe, repeatable rehearsal â a genuine coordination problem that simulation solved. The sufficiency claim extends that solved mandate into an extraction mechanism by blocking hybrid alternatives and converting compliance into a market. Because the founding problem (safe rehearsal) is dead but the arrangement persists and deepens, the constraint exhibits unresolved mandatrophy: it continues to occupy the competence kernel on the basis of a coordination rationale that no longer matches its actual function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_transfer_validity,
    'Does high-fidelity simulation produce durable, transferable competence for rare catastrophic scenarios, or does it produce localized, context-bound fluency that decays under live operational stress?',
    'Longitudinal operational-outcome studies comparing crews trained exclusively on simulation versus hybrid or live-experience regimes; incident-investigation data isolating skill-decay contributions in rare failures.',
    'If transfer is weak, the sufficiency claim is extractive cover for a coordination shortcut and the constraint tilts toward snare; if transfer is strong, the claim approaches genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_validity, empirical, 'Empirical validity of simulation-to-live skill transfer').

omega_variable(
    kernel_reading_contest,
    'Is the competence-occupation kernel inherently multi-mechanism (hybrid_occupation), or can a single mechanism (simulation) be sufficient?',
    'Comparative analysis of accident rates and near-miss data across organizations adopting each reading; ethnographic study of competence decay in retiring cohorts and transition-to-live performance.',
    'Resolving in favor of hybrid_occupation would reclassify this constraint as having higher extraction and stronger theater; resolving in favor of simulation_sufficiency with robust evidence would shift it toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural contest between kernel readings and its classification consequences').

omega_variable(
    compliance_observable_lock_in,
    'Has the simulation-sufficiency reading become locked in because it is the only compliance-observable standard, or because it genuinely outperforms alternatives?',
    'Regulatory experiments waiving simulation mandates in favor of outcome-based competence verification; observing whether organizations voluntarily maintain simulation or shift to hybrid models when compliance is decoupled from simulation hours.',
    'If organizations exit simulation when not compelled, the constraint is enforcement-dependent extraction; if they retain it, the coordination function is genuine and the reading is closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_observable_lock_in, empirical, 'Whether lock-in is observability-driven or performance-driven').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__simulation_sufficiency, theater_ratio, 0, 0.2).
narrative_ontology:measurement(comp_tr_t6, competence_occupation__simulation_sufficiency, theater_ratio, 6, 0.35).
narrative_ontology:measurement(comp_tr_t12, competence_occupation__simulation_sufficiency, theater_ratio, 12, 0.45).
narrative_ontology:measurement(comp_tr_t18, competence_occupation__simulation_sufficiency, theater_ratio, 18, 0.52).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__simulation_sufficiency, theater_ratio, 24, 0.55).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__simulation_sufficiency, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(comp_be_t6, competence_occupation__simulation_sufficiency, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(comp_be_t12, competence_occupation__simulation_sufficiency, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(comp_be_t18, competence_occupation__simulation_sufficiency, base_extractiveness, 18, 0.54).
narrative_ontology:measurement(comp_be_t24, competence_occupation__simulation_sufficiency, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__simulation_sufficiency, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(comp_su_t6, competence_occupation__simulation_sufficiency, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(comp_su_t12, competence_occupation__simulation_sufficiency, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(comp_su_t18, competence_occupation__simulation_sufficiency, suppression_requirement, 18, 0.48).
narrative_ontology:measurement(comp_su_t24, competence_occupation__simulation_sufficiency, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, identity_coordination).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__hybrid_occupation).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__real_incident_necessity).

% DUAL FORMULATION NOTE:
% This constraint is the simulation_sufficiency reading of the competence_occupation kernel. Sibling readings (hybrid_occupation, real_incident_necessity) instantiate structurally distinct constraints from the same kernel, linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
