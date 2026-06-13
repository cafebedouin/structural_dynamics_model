% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__slippery_slope_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__slippery_slope_mechanism, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: end_of_life_authority__slippery_slope_mechanism
 *   human_readable: End-of-Life Authority: Slippery Slope Mechanism
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint models the 'slippery slope' mechanism in end-of-life
 *   policy, where initial autonomy-based frameworks for competent, terminal
 *   patients empirically expand to include incompetent and non-terminal
 *   populations. The constraint is claimed as a Snare because its
 *   coordination function (respecting autonomy) becomes a cover for
 *   extraction (life-ending decisions for vulnerable populations) driven by
 *   systemic pressures and the erosion of strict eligibility criteria. The
 *   expansion is not a logical necessity but an observed empirical drift.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.65).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.7).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.65).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, snare).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "End-of-Life Authority: Slippery Slope Mechanism").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, 'a4118a53-c2ff-48e6-adae-f49bc873801b').
narrative_ontology:cs_kernel_codification('a4118a53-c2ff-48e6-adae-f49bc873801b', formalized).
narrative_ontology:cs_authority_grounding('a4118a53-c2ff-48e6-adae-f49bc873801b', practice).
narrative_ontology:cs_interpretation_layer_present('a4118a53-c2ff-48e6-adae-f49bc873801b').
narrative_ontology:cs_reading_relation('a4118a53-c2ff-48e6-adae-f49bc873801b', end_of_life_authority__autonomy_reading, influences).
narrative_ontology:cs_reading_relation('a4118a53-c2ff-48e6-adae-f49bc873801b', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_axiom('a4118a53-c2ff-48e6-adae-f49bc873801b', foundational, autonomy_expands_to_vulnerable_populations).
narrative_ontology:cs_axiom_status(autonomy_expands_to_vulnerable_populations, holdable).
narrative_ontology:cs_axiom_grounding('a4118a53-c2ff-48e6-adae-f49bc873801b', autonomy_expands_to_vulnerable_populations, empirically_contingent).
narrative_ontology:cs_reference_frame('a4118a53-c2ff-48e6-adae-f49bc873801b', initial_autonomy_framework).
narrative_ontology:cs_drift_state('a4118a53-c2ff-48e6-adae-f49bc873801b', contemporary_policy_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a4118a53-c2ff-48e6-adae-f49bc873801b', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, healthcare_systems).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, some_family_members).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, non_terminal_patients_with_chronic_suffering).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the policies and protocols for end-of-life decisions. Benefits from reduced long-term care costs and resource allocation efficiencies, even as it navigates ethical complexities. Sets eligibility criteria and manages the process, often under pressure to expand access.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, healthcare_systems, agenda_setter,
    institutional, generational, constrained, national).

% Patients who lack the capacity to make their own end-of-life decisions. They become targets of expanded eligibility criteria, potentially having their lives ended based on proxy decisions or interpretations of 'best interest' that may not align with their unexpressed past wishes or current non-verbalized state. Their autonomy is effectively overridden.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, incompetent_patients, payer,
    powerless, immediate, trapped, local).

% Individuals experiencing chronic, unbearable suffering but who are not imminently terminal. They become eligible for life-ending interventions under expanded frameworks, often feeling subtle pressure or having their suffering framed as a 'burden' that justifies intervention, even if they might otherwise choose to live.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, non_terminal_patients_with_chronic_suffering, payer,
    moderate, biographical, constrained, local).

% Family members who experience relief from the emotional, physical, and financial burdens of caring for a loved one with prolonged illness or disability. They may advocate for expanded end-of-life options, sometimes aligning with systemic pressures.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, some_family_members, beneficiary,
    moderate, biographical, mobile, local).

% Initially championed autonomy-based frameworks but now observe the expansion of eligibility criteria with concern, fearing that the original intent is being subverted and that vulnerable populations are being targeted. They analyze the drift and its ethical implications.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, advocates_for_patient_autonomy, observer,
    organized, generational, analytical, national).

% Oppose any intentional life-ending interventions, viewing them as a violation of intrinsic human value. They are often marginalized in policy discussions that prioritize autonomy, but their concerns about the 'slippery slope' are empirically validated by this constraint's operation.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, sanctity_of_life_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for managing end-of-life decisions, aiming to coordinate patient wishes, medical capabilities, and family needs in complex situations, particularly when patients cannot express their will.
% TRANSFER_FUNCTION: Transfers the authority to make life-ending decisions from the individual (in cases of incompetence) or from a strict terminal prognosis (in cases of chronic suffering) to a broader medical-ethical framework, often influenced by systemic pressures and family preferences. This transfers the 'burden' of prolonged care from systems and families onto the patient's life itself.
% ABSENT_VOICES: The voices of incompetent patients are inherently absent, represented only by proxies whose interpretations may be influenced by the systemic pressures of the slippery slope. Sanctity-of-life advocates are often excluded from the core policy-making process, despite their predictions about expansion proving accurate.
% DISAPPEARANCE_RATIONALE: If this mechanism vanished, end-of-life policies would revert to stricter autonomy-based or terminal-only criteria. Healthcare systems would face increased long-term care costs, and families would bear greater burdens. The ethical landscape would shift dramatically, forcing a re-evaluation of who holds authority over life-ending decisions.
% FOUNDING_PROBLEM: The initial problem was how to respect the autonomy of competent, terminally ill patients to choose a dignified death, avoiding prolonged suffering against their will.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for patient autonomy attest the founding problem is still live for competent, terminal patients. However, sanctity-of-life advocates and some bioethicists, from outside the direct beneficiaries, corroborate that the problem's scope has expanded far beyond its original intent, indicating a drift in the constraint's function.
narrative_ontology:disappearance_verdict(end_of_life_authority__slippery_slope_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__slippery_slope_mechanism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(end_of_life_authority__slippery_slope_mechanism, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__slippery_slope_mechanism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__slippery_slope_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the constraint, while framed as respecting autonomy, leads to life-ending decisions for individuals who cannot consent or who are not imminently dying, effectively extracting their remaining life. Suppression is also high (0.7) due to the vulnerability of the target populations (incompetent patients, those with chronic suffering) and the institutional pressures that make 'exit' (i.e., choosing to live) difficult once eligibility expands. The theater ratio is low (0.2) because the 'autonomy' justification, while still present, increasingly masks the actual expansion of who is eligible for life-ending interventions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of healthcare systems and some family members, the constraint might appear as a compassionate extension of autonomy. However, from the perspective of the vulnerable patients and sanctity-of-life advocates, it operates as a snare, leveraging an initial coordination principle to justify extraction from those least able to resist. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Healthcare systems and some family members are beneficiaries, gaining from reduced care burdens and emotional relief. Incompetent patients and non-terminal patients with chronic suffering are the primary victims, as their lives are ended under expanded criteria. Advocates for patient autonomy act as observers, while sanctity-of-life advocates are excluded, their warnings about the slippery slope having been borne out by the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_drift_causality,
    'Is the observed expansion of end-of-life eligibility criteria an inevitable ''slippery slope'' inherent to autonomy frameworks, or is it driven by specific, remediable systemic pressures (e.g., resource scarcity, caregiver burden)?',
    'Comparative policy analysis across jurisdictions with different healthcare funding models and social support systems for chronic illness. If expansion correlates with specific systemic pressures, it suggests remediable causes; if it occurs universally, it suggests an inherent dynamic.',
    'If inevitable, the constraint is a more fundamental snare, requiring a re-evaluation of the initial autonomy premise. If remediable, the constraint is a tangled rope, where the extraction can be mitigated by addressing the underlying systemic pressures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_drift_causality, empirical, 'Determining the root cause of the expansion of end-of-life eligibility.').

omega_variable(
    autonomy_vs_sanctity_boundary,
    'At what point does the expansion of autonomy-based end-of-life frameworks begin to violate the principle of sanctity of life, and is this boundary conceptually fixed or socially constructed?',
    'Philosophical and legal analysis of ''personhood'' and ''right to life'' definitions across different ethical traditions. If a consistent boundary emerges, it suggests a conceptual limit; if it varies widely, it suggests social construction.',
    'If the boundary is fixed, the constraint''s expansion beyond that point is a clear violation. If constructed, the ''slippery slope'' is a contest over shifting social norms, not a violation of an immutable principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_vs_sanctity_boundary, conceptual, 'The conceptual boundary between autonomy and sanctity in end-of-life decisions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t1980, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 1980, 0.1).
narrative_ontology:measurement_basis(end__tr_t1980, observed).
narrative_ontology:measurement(end__tr_t1990, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 1990, 0.15).
narrative_ontology:measurement_basis(end__tr_t1990, observed).
narrative_ontology:measurement(end__tr_t2000, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 2000, 0.18).
narrative_ontology:measurement_basis(end__tr_t2000, observed).
narrative_ontology:measurement(end__tr_t2010, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 2010, 0.2).
narrative_ontology:measurement_basis(end__tr_t2010, observed).
narrative_ontology:measurement(end__tr_t2020, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 2020, 0.2).
narrative_ontology:measurement_basis(end__tr_t2020, observed).
narrative_ontology:measurement(end__tr_t2024, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 2024, 0.2).
narrative_ontology:measurement_basis(end__tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(end__be_t1980, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement_basis(end__be_t1980, observed).
narrative_ontology:measurement(end__be_t1990, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement_basis(end__be_t1990, observed).
narrative_ontology:measurement(end__be_t2000, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement_basis(end__be_t2000, observed).
narrative_ontology:measurement(end__be_t2010, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement_basis(end__be_t2010, observed).
narrative_ontology:measurement(end__be_t2020, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement_basis(end__be_t2020, observed).
narrative_ontology:measurement(end__be_t2024, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 2024, 0.65).
narrative_ontology:measurement_basis(end__be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1980, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement_basis(end__su_t1980, observed).
narrative_ontology:measurement(end__su_t1990, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement_basis(end__su_t1990, observed).
narrative_ontology:measurement(end__su_t2000, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement_basis(end__su_t2000, observed).
narrative_ontology:measurement(end__su_t2010, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement_basis(end__su_t2010, observed).
narrative_ontology:measurement(end__su_t2020, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement_basis(end__su_t2020, observed).
narrative_ontology:measurement(end__su_t2024, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 2024, 0.7).
narrative_ontology:measurement_basis(end__su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__slippery_slope_mechanism, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__sanctity_reading).

% DUAL FORMULATION NOTE:
% This constraint models the empirical 'slippery slope' mechanism observed in end-of-life policy, where autonomy-based frameworks expand beyond their initial scope. It is a distinct reading from the 'autonomy_reading' (which focuses on the ideal of individual choice) and the 'sanctity_reading' (which opposes intentional life-ending).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
