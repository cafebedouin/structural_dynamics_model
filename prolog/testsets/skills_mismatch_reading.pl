% ============================================================================
% CONSTRAINT STORY: skills_mismatch_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_skills_mismatch_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: skills_mismatch_reading
 *   human_readable: Skills-Mismatch Reading of Technological Displacement
 *   domain: political_economy/labor_economics/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the skills-mismatch reading of the technological
 *   displacement axiom: displacement from automation and trade is real and
 *   costly to individuals but is a solvable friction correctable through
 *   education and retraining allocation, not a permanent caste-forming
 *   collapse of the labor mobility model. Under this reading, the population
 *   of displaced workers is remediable rather than fixed — success or failure
 *   is a matter of program design and funding adequacy. The rising
 *   theater_ratio and suppression_requirement series trace a reading under
 *   increasing strain: as placement and wage-recovery outcomes accumulate and
 *   disappoint, administrators and employers must do more work to maintain
 *   the mismatch framing against the alternative diagnosis that the gap is
 *   structural rather than a training deficit. This is a distinct constraint
 *   from the sibling readings (temporal_equivalence_reading,
 *   clock_incompatibility_reading) of the same kernel — those readings claim
 *   different beneficiary structures, different victim permanence, and
 *   correspondingly different epsilon values. This story does not describe or
 *   average over those siblings; it stands alone as one clean reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(skills_mismatch_reading, 0.42).
domain_priors:suppression_score(skills_mismatch_reading, 0.38).
domain_priors:theater_ratio(skills_mismatch_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(skills_mismatch_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(skills_mismatch_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(skills_mismatch_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(skills_mismatch_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(skills_mismatch_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(skills_mismatch_reading, tangled_rope).
narrative_ontology:human_readable(skills_mismatch_reading, "Skills-Mismatch Reading of Technological Displacement").
narrative_ontology:topic_domain(skills_mismatch_reading, "political_economy/labor_economics/technology_governance").

domain_priors:requires_active_enforcement(skills_mismatch_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(skills_mismatch_reading, 'c1f8da27-4b87-407e-8bb9-2a11475484a5').
narrative_ontology:cs_kernel_codification('c1f8da27-4b87-407e-8bb9-2a11475484a5', distributed).
narrative_ontology:cs_authority_grounding('c1f8da27-4b87-407e-8bb9-2a11475484a5', distributed).
narrative_ontology:cs_reading_relation('c1f8da27-4b87-407e-8bb9-2a11475484a5', technological_displacement_axiom__temporal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1f8da27-4b87-407e-8bb9-2a11475484a5', technological_displacement_axiom__clock_incompatibility_reading, influences).
narrative_ontology:cs_axiom('c1f8da27-4b87-407e-8bb9-2a11475484a5', foundational, displacement_population_is_remediable).
narrative_ontology:cs_axiom_status(displacement_population_is_remediable, holdable).
narrative_ontology:cs_axiom_grounding('c1f8da27-4b87-407e-8bb9-2a11475484a5', displacement_population_is_remediable, empirically_contingent).
narrative_ontology:cs_axiom('c1f8da27-4b87-407e-8bb9-2a11475484a5', secondary, allocation_fix_sufficient_no_structural_redesign_needed).
narrative_ontology:cs_axiom_status(allocation_fix_sufficient_no_structural_redesign_needed, holdable).
narrative_ontology:cs_axiom_grounding('c1f8da27-4b87-407e-8bb9-2a11475484a5', allocation_fix_sufficient_no_structural_redesign_needed, instrumental).
narrative_ontology:cs_reference_frame('c1f8da27-4b87-407e-8bb9-2a11475484a5', post_industrial_mobility_consensus).
narrative_ontology:cs_drift_state('c1f8da27-4b87-407e-8bb9-2a11475484a5', post_automation_wage_stagnation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c1f8da27-4b87-407e-8bb9-2a11475484a5', '').
narrative_ontology:cs_kernel_id(skills_mismatch_reading, technological_displacement_axiom).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(skills_mismatch_reading, retraining_program_administrators).
narrative_ontology:constraint_beneficiary(skills_mismatch_reading, technology_sector_employers).
narrative_ontology:constraint_beneficiary(skills_mismatch_reading, credentialing_bodies).
narrative_ontology:constraint_victim(skills_mismatch_reading, mid_career_displaced_workers).
narrative_ontology:constraint_victim(skills_mismatch_reading, retraining_program_dropouts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers government-funded retraining programs, sets curricula and eligibility criteria, and controls the allocation of retraining budgets. Collects program funding regardless of downstream placement outcomes, and frames the persistence of displacement as evidence that more retraining funding is needed rather than as evidence the model has failed.
narrative_ontology:constraint_stakeholder(skills_mismatch_reading, retraining_program_administrators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(skills_mismatch_reading, retraining_program_administrators, beneficiary).

% Benefit from a subsidized, government-funded pipeline of retrained labor that shifts training costs from firm payrolls to public budgets. Can relocate hiring to jurisdictions with the best-subsidized talent pools, and have no structural stake in whether individual retrained workers actually land comparable jobs.
narrative_ontology:constraint_stakeholder(skills_mismatch_reading, technology_sector_employers, beneficiary,
    powerful, biographical, mobile, global).

% Certify completion of retraining programs and set the credentials employers treat as gatekeeping signals. Revenue and institutional relevance depend on displacement continuing to generate demand for new certifications; a genuinely closed skills gap would shrink their function.
narrative_ontology:constraint_stakeholder(skills_mismatch_reading, credentialing_bodies, beneficiary,
    organized, generational, arbitrage, national).

% Lose stable employment to automation or offshoring and are directed into retraining programs whose completion rates, placement rates, and wage-recovery outcomes are frequently worse than advertised. Bear the opportunity cost of unpaid or underpaid retraining time, often at an age or in a location where the promised new-sector jobs do not materialize locally. Exit from the retraining track usually means exit from the labor force entirely, not a return to the prior occupation.
narrative_ontology:constraint_stakeholder(skills_mismatch_reading, mid_career_displaced_workers, payer,
    moderate, biographical, constrained, regional).

% Begin retraining but cannot complete it due to income loss during the program, caregiving obligations, transportation barriers, or program mismatch with local labor demand. Are counted by administrators as programmatic failures attributable to individual effort rather than as evidence the allocation model itself misjudged their situation. Have the fewest resources to seek an alternative path once dropped.
narrative_ontology:constraint_stakeholder(skills_mismatch_reading, retraining_program_dropouts, payer,
    powerless, biographical, trapped, local).

% Study placement and wage-recovery data from retraining programs across regions and sectors. Positioned to see whether the skills-mismatch framing holds empirically or whether displacement outcomes look more like permanent downward mobility for specific cohorts, informing but not controlling policy design.
narrative_ontology:constraint_stakeholder(skills_mismatch_reading, regional_policy_analysts, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a public response to labor displacement by pooling retraining funding, standardizing curricula against employer-stated skill demand, and channeling workers toward growth sectors rather than leaving displacement to unmanaged individual adjustment.
% TRANSFER_FUNCTION: Moves public retraining budgets and displaced workers' unpaid or underpaid training time toward credentialing bodies and technology-sector employers, who receive a subsidized labor pipeline without bearing the training cost or the risk of program failure.
% ABSENT_VOICES: Displaced workers in regions with no matching local demand for the trained-for sector are rarely represented in program design; their objection — that retraining assumes a job exists to be trained into — is structurally excluded from a framework built around allocation efficiency rather than placement guarantee.
% DISAPPEARANCE_RATIONALE: Administrators and employers would say the world rearranges badly — displaced workers would have no structured path back into employment. Displaced workers with poor placement outcomes and analysts studying wage-recovery data would say the world barely changes for them either way, since the current program already fails to deliver the promised transition for a substantial share of participants.
% FOUNDING_PROBLEM: Automation and trade-driven job loss were producing a structurally idle population with obsolete skills; the arrangement was built to convert that idle population into a re-employable one through subsidized retraining, treating displacement as a temporary friction correctable by education investment.
% FOUNDING_PROBLEM_CORROBORATION: Retraining administrators and technology employers attest the founding problem remains live and solvable with sufficient funding. Independent labor economists and regional policy analysts publishing wage-recovery studies attest that a substantial share of displaced mid-career workers never recover pre-displacement earnings even after program completion, suggesting the founding problem may have shifted from a training gap to a structural demand gap the retraining model cannot address.
narrative_ontology:disappearance_verdict(skills_mismatch_reading, contested).
narrative_ontology:founding_problem_status(skills_mismatch_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(skills_mismatch_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-13',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(skills_mismatch_reading, 'none', 1).
narrative_ontology:epsilon_provenance(skills_mismatch_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(skills_mismatch_reading_tests).
:- end_tests(skills_mismatch_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than low, because while retraining coordination has genuine value, the allocation structure channels public subsidy and worker unpaid time toward employers and credentialing bodies who bear none of the placement risk. Suppression is moderate (0.38): there is no coercive enforcement preventing workers from exiting the retraining track, but administrative gatekeeping (eligibility rules, funding conditionality, the framing of dropout as individual failure) constrains the practical alternatives available to a displaced worker with limited savings. Theater ratio (0.31) reflects a meaningful but growing gap between programmatic activity and actual wage-recovery outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   Retraining administrators, technology employers, and credentialing bodies are beneficiaries: they receive subsidized labor pipelines, program funding, or certification demand regardless of individual placement success. Mid-career displaced workers and program dropouts are victims under this reading's own terms even though the reading insists their victimhood is temporary and remediable — the metrics measure what is actually happening to them during the remediation window, independent of the reading's optimistic framing of that window's eventual closure.
 *
 * MANDATROPHY ANALYSIS:
 *   The skills-mismatch reading resists mandatrophy by design: as long as displacement is framed as a training gap, the retraining apparatus's mandate is renewed by each new wave of displacement rather than questioned for its track record. The founding_problem_status is authored as contested precisely to prevent this reading from silently assuming its own success — the corroboration field surfaces that outside economists dispute whether the mandate's original problem (correctable skills gap) still describes what is actually happening (structural demand gap), which is the divergence this reading's classification should be measured against.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remediability_vs_permanence,
    'Is the mid-career displaced worker population genuinely remediable through retraining, or does the skills-mismatch framing misdescribe what is structurally a permanent downward-mobility cohort (the clock_incompatibility_reading''s claim)?',
    'Longitudinal wage-recovery studies tracking displaced cohorts 5-10 years post-retraining, disaggregated by age at displacement, regional labor demand, and program type, compared against the counterfactual of no retraining intervention.',
    'If recovery rates converge to pre-displacement earnings over time, the skills-mismatch reading is vindicated as the operative structural description. If recovery permanently plateaus below pre-displacement earnings for a stable cohort share, the clock_incompatibility_reading''s caste-formation claim is the more accurate description and this reading is a cover story riding on genuine but insufficient coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remediability_vs_permanence, empirical, 'Whether displaced workers are a remediable population or a permanent underclass — the load-bearing distinction between this reading and its siblings.').

omega_variable(
    allocation_control_beneficiary_ambiguity,
    'Does control over retraining allocation constitute a genuine coordination benefit to administrators, employers, and credentialing bodies, or is the allocation function itself extractive because it is unaccountable to placement outcomes?',
    'Compare programs with outcome-based funding (renewal contingent on verified placement/wage recovery) against programs with input-based funding (renewal contingent on enrollment/completion counts) for differences in extractiveness and worker outcomes.',
    'If outcome-based funding structurally reduces extraction relative to input-based funding, the extraction identified here is a fixable feature of program design, supporting this reading''s premise that displacement is a solvable allocation friction. If extraction persists regardless of funding structure, the coordination story may be cover for rent capture by administrators and credentialing bodies independent of the reading''s framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(allocation_control_beneficiary_ambiguity, empirical, 'Whether the beneficiary structure is a fixable design flaw or an inherent feature of the allocation model.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which reading of the technological_displacement_axiom kernel best fits the empirical record for a given displacement wave — is the choice of skills-mismatch framing itself evidence-driven or a policy-preference selection made prior to evidence?',
    'Track which reading policymakers and program administrators invoke before versus after outcome data becomes available; a reading invoked pre-emptively and maintained despite disconfirming outcome data indicates preference-driven selection rather than evidence-driven selection.',
    'If the skills-mismatch reading is selected primarily because it justifies continued administrative funding rather than because it best fits the data, the reading itself functions as an extraction-preserving frame, which would elevate this constraint''s true extractiveness above what its own internal metrics show.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether this reading is chosen for descriptive accuracy or for its favorable framing of the retraining apparatus''s mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(skills_mismatch_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(skil_tr_t0, skills_mismatch_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(skil_tr_t4, skills_mismatch_reading, theater_ratio, 4, 0.21).
narrative_ontology:measurement(skil_tr_t8, skills_mismatch_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(skil_tr_t12, skills_mismatch_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(skil_tr_t16, skills_mismatch_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(skil_tr_t20, skills_mismatch_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(skil_tr_t24, skills_mismatch_reading, theater_ratio, 24, 0.31).

% Extraction over time
narrative_ontology:measurement(skil_be_t0, skills_mismatch_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(skil_be_t4, skills_mismatch_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(skil_be_t8, skills_mismatch_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(skil_be_t12, skills_mismatch_reading, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(skil_be_t16, skills_mismatch_reading, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(skil_be_t20, skills_mismatch_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(skil_be_t24, skills_mismatch_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(skil_su_t0, skills_mismatch_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(skil_su_t4, skills_mismatch_reading, suppression_requirement, 4, 0.29).
narrative_ontology:measurement(skil_su_t8, skills_mismatch_reading, suppression_requirement, 8, 0.31).
narrative_ontology:measurement(skil_su_t12, skills_mismatch_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(skil_su_t16, skills_mismatch_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(skil_su_t20, skills_mismatch_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(skil_su_t24, skills_mismatch_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(skills_mismatch_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(skills_mismatch_reading, 0.15).
narrative_ontology:affects_constraint(skills_mismatch_reading, temporal_equivalence_reading).
narrative_ontology:affects_constraint(skills_mismatch_reading, clock_incompatibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the technological_displacement_axiom kernel. temporal_equivalence_reading treats current displacement as structurally equivalent to prior self-correcting technology transitions (lower ε, framed as Rope-like historical pattern). clock_incompatibility_reading treats the retraining timeline as too slow relative to displacement pace, producing permanent caste formation regardless of program design (higher ε, framed as Snare-like). This story (skills_mismatch_reading) sits between them: it acknowledges real, painful displacement (unlike temporal_equivalence_reading's minimization) but insists the population is remediable through allocation fixes (unlike clock_incompatibility_reading's permanence claim). All three readings must be read together to understand the kernel; none alone is the whole picture, and none should be collapsed into the others' ε or stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
