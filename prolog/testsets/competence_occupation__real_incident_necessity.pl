% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__real_incident_necessity, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Real Incident Necessity for Competence Occupation
 *   domain: organizational/safety
 *
 * SUMMARY:
 *   High-reliability organizations (aviation, nuclear power, healthcare)
 *   maintain competence through continuous simulation, procedure training,
 *   and refresher regimens. The real_incident_necessity reading asserts that
 *   despite all this infrastructure, true competence can only be demonstrated
 *   through actual catastrophic incident — operators' training is adequate
 *   only retroactively, after they have survived a real emergency. This
 *   reading treats actual harm as the Oracle: accidents are reframed as
 *   validation rather than failure. The constraint extracts catastrophe as
 *   the price of proof, making every incident simultaneously a competence
 *   failure and a competence success. No beneficiary structure is viable —
 *   accidents are unacceptable outcomes that cannot be voluntarily pursued.
 *   The constraint persists through regulatory doctrine,
 *   accident-investigation practice, and safety-culture narrative, not
 *   through any party's desire to maintain it.
 *
 * KEY AGENTS:
 *   - Safety personnel: payers bearing cognitive dissonance from an implicit standard they cannot meet without harm
 *   - Organizational leadership: caught between affirming simulation adequacy and accepting the real_incident_necessity doctrine
 *   - Accident victims: the powerless Oracle whose harm is treated as competence validation
 *   - Simulation industry: beneficiaries of the constraint positioning simulation as necessary-but-insufficient
 *   - Regulatory authorities: agenda-setters locked into the doctrine by precedent and unable to reverse without admitting incoherence
 *   - Safety-culture narrative: non-agent beneficiary vindicated by every incident treated as learning opportunity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.89).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.91).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.89).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, snare).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real Incident Necessity for Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "organizational/safety").

domain_priors:requires_active_enforcement(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, 'c69c27b6-87c8-4d78-85f0-54ccdfb92a4e').
narrative_ontology:cs_kernel_codification('c69c27b6-87c8-4d78-85f0-54ccdfb92a4e', formalized).
narrative_ontology:cs_authority_grounding('c69c27b6-87c8-4d78-85f0-54ccdfb92a4e', extraction).
narrative_ontology:cs_reading_relation('c69c27b6-87c8-4d78-85f0-54ccdfb92a4e', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('c69c27b6-87c8-4d78-85f0-54ccdfb92a4e', competence_occupation__hybrid_occupation, coexists_with).
narrative_ontology:cs_axiom('c69c27b6-87c8-4d78-85f0-54ccdfb92a4e', foundational, authenticity_irreducible_to_simulation).
narrative_ontology:cs_axiom_status(authenticity_irreducible_to_simulation, overridden).
narrative_ontology:cs_axiom_grounding('c69c27b6-87c8-4d78-85f0-54ccdfb92a4e', authenticity_irreducible_to_simulation, empirically_contingent).
narrative_ontology:cs_axiom('c69c27b6-87c8-4d78-85f0-54ccdfb92a4e', foundational, catastrophe_as_oracle).
narrative_ontology:cs_axiom_status(catastrophe_as_oracle, holdable).
narrative_ontology:cs_axiom_grounding('c69c27b6-87c8-4d78-85f0-54ccdfb92a4e', catastrophe_as_oracle, deontological).
narrative_ontology:cs_reference_frame('c69c27b6-87c8-4d78-85f0-54ccdfb92a4e', authenticity_requirement_doctrine).
narrative_ontology:cs_drift_state('c69c27b6-87c8-4d78-85f0-54ccdfb92a4e', contemporary_simulation_capabilities_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c69c27b6-87c8-4d78-85f0-54ccdfb92a4e', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, safety_personnel).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, organizational_leadership).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, accident_victims).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__real_incident_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.89) because the constraint requires catastrophe as proof, making operational failure structurally necessary for validation. Suppression is even higher (0.91) because the constraint is defended by multiple interlocking mechanisms: regulatory doctrine, professional identity (regulators and safety officers cannot question it without appearing to abandon safety), narrative capture (every incident becomes evidence supporting the doctrine), and the absence of direct beneficiaries who could be held accountable. Theater ratio is high (0.68) because the actual function of training — maintaining competence — is divorced from the stated validation mechanism (incidents). Accessibility_collapse is very high (0.92) because organizations cannot exit the doctrine: regulators control licensing, incident investigation is mandatory, and challenging the doctrine appears to invite regulatory retaliation. Resistance is low (0.34) because most parties internalize the narrative as humble realism; only victims (who have no voice) and some simulation engineers (who are financially captured) mount real resistance. The measurement series shows gradual intensification over the interval: extractiveness and suppression both climb as the doctrine ages and becomes more deeply embedded in regulatory procedure, while theater rises as more training infrastructure is built and simultaneously treated as insufficient.
 *
 * PERSPECTIVAL GAP:
 *   Regulatory seat vs. payer seats: The regulator (agenda-setter) experiences the constraint as rigorous doctrine that prevents complacency and treats incidents as learning opportunities. Safety personnel and organization leadership experience it as an incoherent standard they cannot meet without catastrophe, coupled with professional/identity inability to question it. Accident victims experience it as their instrumentalization — their harm is framed as validation rather than failure.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety personnel have moderate power but identity-locked exit (their careers and professional identity are constituted through the safety apparatus that maintains the doctrine; leaving the industry is psychologically costly). They are victims of the constraint because it places them in an unresolvable position: they must internalize that their best efforts are inadequate. Organizational leadership has institutional power but is also identity-locked (they cannot reverse the doctrine without admitting their organization accepted incoherence). Accident victims are powerless and trapped by definition (once the incident occurs, they cannot exit). Regulatory authorities have institutional power and constrained-but-negotiable exit (they could change the doctrine but face significant reputational and administrative cost). The simulation industry is a secondary beneficiary with powerful resources and mobile exit (they can exit to other industries). Directionality_overrides are not needed: the structural derivation from victim declarations and identity-lock mechanisms produces accurate d values without adjustment.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem was live and real: early high-reliability operations revealed failure modes that simulation did not expose. The real_incident_necessity reading was originally justified. However, modern high-fidelity full-scale simulation, procedural refinement, and accumulated operational knowledge have substantially resolved the founding problem. Organizations that invest heavily in simulation and refresher training achieve incident rates an order of magnitude lower than historical baselines. The doctrine persists not because it solves a live problem but because: (1) reversing it requires regulatory authorities to admit the standard was incoherent, (2) safety culture has absorbed the doctrine as non-negotiable humility, (3) incident investigation practices are oriented around extraction of lessons rather than examination of whether the doctrine is false, and (4) the simulation industry has a financial interest in the doctrine's persistence. This is a clear case of mandatrophy: the founding problem is dead but the constraint persists through inertia, narrative capture, and institutional lock-in. No party benefits enough from maintaining it to overcome the reputational cost of reversal. The constraint is a zombie doctrine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Has high-fidelity full-scale simulation with comprehensive procedural training become sufficient to occupy the competence kernel, making the real_incident_necessity reading a false continuation of an obsolete standard?',
    'Comparative incident-rate analysis: organizations with maximum simulation + refresher investment vs. baseline; neuropsychological studies of skill transfer from simulation to novel real conditions; controlled accident investigation examining whether incident-free operations in high-investment organizations violate the real_incident_necessity claim.',
    'If the founding problem is obsolete, the constraint shifts from snare-with-unmet-standard to snare-with-fabricated-standard — worse classification; the entire regulatory doctrine requires reversal; no beneficiary structure remains (catastrophes cannot become acceptable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding problem that justified the constraint has been substantially resolved by modern simulation and training systems.').

omega_variable(
    inversion_of_causation,
    'Does the constraint create the very failure mode it claims to detect — i.e., do organizations that internalize the inadequacy narrative and treat simulation as theater invest less in competence maintenance, thereby making real incidents more likely and then treating them as validation?',
    'Longitudinal comparison: organizations that adopt the real_incident_necessity reading vs. those adopting simulation_sufficiency; measure investment levels in simulation, training rigor, and incident rates over 10+ years; examine causal ordering via testimony from safety leadership about their resource allocation decisions.',
    'If true, the constraint is a self-fulfilling doom prophecy creating the conditions it claims to detect. This would make the classification even more clearly extractive: the suppression mechanism would include epistemic capture (organizations cannot question the narrative without appearing to abandon safety culture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inversion_of_causation, empirical, 'Whether internalizing the constraint produces the incompetence it claims to measure.').

omega_variable(
    authentic_vs_reconstructed_authenticity,
    'When the real_incident_necessity reading claims ''only authentic catastrophic conditions provide authentic occupation,'' is it measuring genuine authenticity or is it measuring only the visible-cost-to-the-observer (the shock of real harm)?',
    'Neurocognitive research on skill maintenance under risk conditions; comparison of operator performance in maximum-fidelity simulation (where true consequences are at stake — reputational, career) vs. training simulation (lower-consequence); examination of whether the distinction is psychological authenticity vs. actual authenticity.',
    'If the distinction is mere visibility-of-harm rather than structural authenticity, the constraint is rationalized theater: organizations witness harm, update procedures, and call it validation. The constraint would be classified as pure extraction via enforced humiliation and victim instrumentalization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authentic_vs_reconstructed_authenticity, conceptual, 'Whether the real_incident_necessity reading measures authentic competence requirements or only the visibility of harm as a psychological mechanism.').

omega_variable(
    committer_frame_reading_contest,
    'Does this reading (real_incident_necessity) coexist with the simulation_sufficiency reading as genuinely live policy options held by different regulatory jurisdictions, or does one reading actually foreclose the other within the global safety governance framework?',
    'Examination of actual regulatory adoption: do high-reliability jurisdictions adopt one reading consistently, or do they mix practices? Can an organization genuinely operate under both readings simultaneously, or do their enforcement mechanisms conflict? Are there precedents where a jurisdiction shifted from one reading to another and what costs did it bear?',
    'If readings coexist without foreclosure, the kernel contains genuine indeterminacy; if one forecloses the other, the committer frame miscategorized the relationship. This affects which sibling reading relation should be declared (coexists_with vs. forecloses).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_reading_contest, empirical, 'Whether the real_incident_necessity and simulation_sufficiency readings are genuinely coexistent or one logically forecloses the other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.52).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__real_incident_necessity, theater_ratio, 5, 0.55).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__real_incident_necessity, theater_ratio, 10, 0.58).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__real_incident_necessity, theater_ratio, 15, 0.61).
narrative_ontology:measurement(comp_tr_t25, competence_occupation__real_incident_necessity, theater_ratio, 25, 0.66).
narrative_ontology:measurement(comp_tr_t35, competence_occupation__real_incident_necessity, theater_ratio, 35, 0.67).
narrative_ontology:measurement(comp_tr_t40, competence_occupation__real_incident_necessity, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(comp_be_t5, competence_occupation__real_incident_necessity, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(comp_be_t10, competence_occupation__real_incident_necessity, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(comp_be_t15, competence_occupation__real_incident_necessity, base_extractiveness, 15, 0.82).
narrative_ontology:measurement(comp_be_t25, competence_occupation__real_incident_necessity, base_extractiveness, 25, 0.86).
narrative_ontology:measurement(comp_be_t35, competence_occupation__real_incident_necessity, base_extractiveness, 35, 0.89).
narrative_ontology:measurement(comp_be_t40, competence_occupation__real_incident_necessity, base_extractiveness, 40, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__real_incident_necessity, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(comp_su_t5, competence_occupation__real_incident_necessity, suppression_requirement, 5, 0.84).
narrative_ontology:measurement(comp_su_t10, competence_occupation__real_incident_necessity, suppression_requirement, 10, 0.86).
narrative_ontology:measurement(comp_su_t15, competence_occupation__real_incident_necessity, suppression_requirement, 15, 0.88).
narrative_ontology:measurement(comp_su_t25, competence_occupation__real_incident_necessity, suppression_requirement, 25, 0.9).
narrative_ontology:measurement(comp_su_t35, competence_occupation__real_incident_necessity, suppression_requirement, 35, 0.91).
narrative_ontology:measurement(comp_su_t40, competence_occupation__real_incident_necessity, suppression_requirement, 40, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__real_incident_necessity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__real_incident_necessity, 0.05).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, incident_investigation_doctrine).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, safety_culture_narrative__authenticity_requirement).

% DUAL FORMULATION NOTE:
% The competence_occupation kernel decomposes into three structurally distinct constraints based on their ε values and beneficiary/victim structure. The real_incident_necessity reading (THIS story) treats catastrophe as the validation mechanism and therefore has no beneficiary and very high extraction cost. The simulation_sufficiency reading (sibling) posits that high-fidelity simulation can occupy the competence kernel, has clear beneficiaries (safety personnel, organizations), and lower extraction. The hybrid_occupation reading (sibling) holds that multiple mechanisms are necessary without consensus on optimal mix, positioning it as a middle ground. These are not alternate framings of one constraint — they have different ε values and different victim sets. The real_incident_necessity reading forecloses simulation_sufficiency if both readings attempt to govern the same regulatory domain, but they coexist as live policy options in different jurisdictions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__real_incident_necessity, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
