% ============================================================================
% CONSTRAINT STORY: personhood_boundary__fitness_contingent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__fitness_contingent_reading, []).

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
 *   constraint_id: personhood_boundary__fitness_contingent_reading
 *   human_readable: Fitness-Contingent Personhood Boundary (Post-Natal Fitness Test Reading)
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested personhood_boundary
 *   kernel: the fitness-contingent reading, under which moral standing is not
 *   conferred by birth alone but must be demonstrated against a fitness
 *   criterion (viability, capacity, absence of disqualifying disability)
 *   administered by state or medical authority. Historical instantiations
 *   include eugenics-era non-treatment protocols, selective infanticide
 *   practices justified by capacity assessments, and modern
 *   disability-selective withholding-of-care policies framed in fitness
 *   terms. This is not the birth-threshold reading (which grants standing at
 *   birth unconditionally) nor the potential-based reading (which grounds
 *   standing in potential for rational agency and excludes only severely
 *   disabled cases on a potentiality test) — those are separate constraints
 *   with separate ε values, linked here via network.affects_constraints.
 *   Under this reading specifically, exclusion from the moral community is
 *   not exceptional but the default state prior to a passed test, producing a
 *   much larger and more administratively routine victim set than the
 *   potential-based reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, 0.81).
domain_priors:suppression_score(personhood_boundary__fitness_contingent_reading, 0.88).
domain_priors:theater_ratio(personhood_boundary__fitness_contingent_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, snare).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Fitness-Contingent Personhood Boundary (Post-Natal Fitness Test Reading)").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, 'd3996243-c973-4356-a3ae-d0d86c8366aa').
narrative_ontology:cs_kernel_codification('d3996243-c973-4356-a3ae-d0d86c8366aa', distributed).
narrative_ontology:cs_authority_grounding('d3996243-c973-4356-a3ae-d0d86c8366aa', extraction).
narrative_ontology:cs_interpretation_layer_present('d3996243-c973-4356-a3ae-d0d86c8366aa').
narrative_ontology:cs_reading_relation('d3996243-c973-4356-a3ae-d0d86c8366aa', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('d3996243-c973-4356-a3ae-d0d86c8366aa', personhood_boundary__potential_based_reading, influences).
narrative_ontology:cs_axiom('d3996243-c973-4356-a3ae-d0d86c8366aa', foundational, standing_requires_demonstrated_capacity).
narrative_ontology:cs_axiom_status(standing_requires_demonstrated_capacity, holdable).
narrative_ontology:cs_axiom_grounding('d3996243-c973-4356-a3ae-d0d86c8366aa', standing_requires_demonstrated_capacity, deontological).
narrative_ontology:cs_axiom('d3996243-c973-4356-a3ae-d0d86c8366aa', secondary, state_holds_administrative_authority_over_membership_test).
narrative_ontology:cs_axiom_status(state_holds_administrative_authority_over_membership_test, holdable).
narrative_ontology:cs_axiom_grounding('d3996243-c973-4356-a3ae-d0d86c8366aa', state_holds_administrative_authority_over_membership_test, conventional).
narrative_ontology:cs_reference_frame('d3996243-c973-4356-a3ae-d0d86c8366aa', capacity_grounded_moral_status).
narrative_ontology:cs_drift_state('d3996243-c973-4356-a3ae-d0d86c8366aa', post_disability_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d3996243-c973-4356-a3ae-d0d86c8366aa', '').
narrative_ontology:cs_kernel_id(personhood_boundary__fitness_contingent_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, state_authorities_administering_fitness_determination).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, resource_allocating_institutions).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, eugenics_aligned_professional_bodies).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, pre_fitness_infants).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, disabled_neonates).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, families_of_excluded_infants).
narrative_ontology:constraint_vindicates(personhood_boundary__fitness_contingent_reading, graduated_moral_status_doctrine).
narrative_ontology:constraint_vindicates(personhood_boundary__fitness_contingent_reading, state_authority_over_membership_boundary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and administers the fitness criteria (viability, capacity thresholds, disability screens) that determine whether a given infant is admitted into the moral/legal community. Sets the test, staffs the evaluators, and retains discretion over edge cases. Bears none of the direct cost of exclusion and gains expanded jurisdictional authority over reproduction and family life.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, state_authorities_administering_fitness_determination, agenda_setter,
    institutional, generational, arbitrage, national).

% Hospitals, insurers, and welfare administrators that benefit financially and administratively from a bright line permitting non-treatment or de-prioritization of infants who fail the fitness test. The boundary converts a costly universal-care obligation into a conditional one, redirecting resources toward infants deemed fit.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, resource_allocating_institutions, beneficiary,
    institutional, biographical, arbitrage, national).

% Medical and policy associations whose theoretical frameworks (fitness, quality of life, capacity) are institutionalized and funded because the boundary exists. They author the tests, publish the criteria, and see their disciplinary authority expand as more decisions route through fitness evaluation rather than unconditional obligation.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, eugenics_aligned_professional_bodies, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, eugenics_aligned_professional_bodies, agenda_setter).

% Newborns not yet subjected to or not yet having passed the fitness determination. Under this reading they hold no moral standing and therefore no claim against withheld care, exclusion, or disposal. They cannot advocate, cannot exit, cannot contest the test administered on them — the total absence of standing is the mechanism, not a side effect.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, pre_fitness_infants, payer,
    powerless, immediate, trapped, local).

% Infants presenting with conditions the fitness criteria weight heavily against. Systematically fail the threshold test at higher rates regardless of viability with intervention, converting a treatable medical condition into a moral-standing disqualification. Have no representative voice in the determination beyond the family and the evaluating institution.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, disabled_neonates, payer,
    powerless, immediate, trapped, local).

% Parents and kin who bear the grief, stigma, and any residual caregiving burden without recourse once the fitness determination excludes their infant from standing. Can appeal only within the same institutional framework that authored the test; cannot exit the jurisdiction's authority claim over the determination without relocating to a jurisdiction with a different reading of the kernel.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, families_of_excluded_infants, payer,
    moderate, biographical, constrained, local).

% Would object that the fitness criteria encode disability discrimination directly into the moral-standing boundary, but are structurally outside the evaluating institutions and outside the state's rule-making process for the test itself. Their objections surface in litigation and advocacy but do not sit inside the determination process.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, disability_rights_advocates, excluded,
    organized, generational, constrained, national).

% Examine the fitness-contingent reading as one of several competing accounts of where personhood begins, tracing its historical instances (infanticide practices, disability-selective non-treatment policies, eugenics-era institutional codes) and comparing its victim set and enforcement structure against the birth-threshold and potential-based readings.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, moral_philosophers_and_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__fitness_contingent_reading, state_authorities_administering_fitness_determination).
narrative_ontology:fixing_cost_class(personhood_boundary__fitness_contingent_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides administrators and clinicians with a bright-line, adjudicable test for allocating scarce neonatal and social resources, replacing case-by-case moral deliberation with a standardized fitness threshold.
% TRANSFER_FUNCTION: Moves moral standing, legal protection, and claim on resources away from infants who fail the fitness determination and toward infants who pass it, and toward the institutions that administer and benefit from the sorting process (freed resources, expanded authority, disciplinary legitimacy).
% ABSENT_VOICES: The infants themselves have no voice by construction — the reading denies them standing prior to passing the test, which is the mechanism under examination. Disability rights advocates and bioethicists critical of eugenics-adjacent fitness criteria are excluded from the rule-making and evaluation process itself, though they contest it externally through courts and advocacy.
% DISAPPEARANCE_RATIONALE: If the fitness-contingent boundary were abolished (i.e., the jurisdiction shifted to unconditional birth-threshold standing), resource-allocation institutions would lose the administrative discretion to withhold care or deprioritize disabled neonates, evaluating professional bodies would lose disciplinary authority over the determination, and every infant currently excludable under the fitness test would acquire an immediate, unconditional claim to protection and care. The reorganization would be immediate and structural, not cosmetic.
% FOUNDING_PROBLEM: Historically framed as solving resource scarcity (which infants receive limited neonatal intensive care, food, or protection) and as encoding a philosophical commitment that moral status tracks demonstrated capacities rather than mere biological birth.
% FOUNDING_PROBLEM_CORROBORATION: State authorities and allied professional bodies attest the scarcity-allocation problem remains live and that fitness criteria are a rational response to it. Independent bioethicists, disability rights litigants, and historians of eugenics-era policy — parties outside the benefiting institutions — attest that the scarcity framing has been used to launder disability discrimination and that in most contemporary high-resource settings the claimed scarcity does not in fact bind, making the arrangement's persistence a matter of institutional authority rather than genuine resource necessity.
narrative_ontology:disappearance_verdict(personhood_boundary__fitness_contingent_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__fitness_contingent_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(personhood_boundary__fitness_contingent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__fitness_contingent_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__fitness_contingent_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__fitness_contingent_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81 at interval end) because the reading transfers moral standing, resources, and legal protection away from a defined class of powerless entities toward administering institutions and resource-allocating bodies, and because the transfer is total (denial of standing, not partial cost) for those who fail the test. Suppression is authored even higher (0.88) because the infants who bear the cost cannot contest, appeal, or exit the determination in any capacity — suppression here is closer to absolute than in most snares, since the affected party lacks even the standing to be recognized as a claimant. Theater ratio rises over the interval (0.20 to 0.42) reflecting increasing institutional formalization of 'fitness committees' and review boards whose procedural apparatus provides legitimating cover without altering the underlying exclusion rate. Resistance is authored moderate-high (0.74) reflecting sustained external contestation from disability rights movements and bioethics critique, even though that resistance operates outside the determination process itself.
 *
 * PERSPECTIVAL GAP:
 *   From the administering authority's seat, the fitness test looks like principled, resource-rational coordination — a defensible philosophical position about what grounds moral status. From the excluded infant's structural position (represented analytically, since the infant cannot self-report), the same arrangement is total, unappealable extraction of standing with no coordination benefit returned. The engine should compute markedly different seat types from these two positions given the identical structural facts — that divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   State authorities and resource-allocating institutions sit near the full-beneficiary end: they administer the test, retain discretion, and capture freed resources and expanded jurisdiction. Pre-fitness infants and disabled neonates sit at the full-target end — trapped exit, immediate time horizon, zero capacity to contest — the paradigm case of a directionality-derived high-d target. Families occupy an intermediate position: they bear real cost but retain some (constrained) capacity to appeal within the same institutional frame that authored the exclusion, which is why they are not merged into the same exit-options bucket as the infants themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (genuine neonatal resource scarcity in historically resource-poor settings) may have been live at the reading's origin but is contested as still live in most jurisdictions where this reading persists today; where scarcity no longer binds, the arrangement functions as inertial or actively defended institutional authority over a membership boundary rather than a live coordination solution — this is exactly the founding_problem_status: contested / corroboration-from-outside-the-beneficiaries pattern the R5 interview is designed to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fitness_criterion_content_indeterminacy,
    'What specific capacities or conditions constitute ''fitness'' under this reading, and who has authority to revise the criteria over time?',
    'Historical and comparative analysis of how fitness criteria have been operationalized across jurisdictions and eras (viability thresholds, disability exclusions, capacity assessments) to determine whether the criterion content is stable or drifts opportunistically toward administering-institution convenience.',
    'If the criterion content drifts toward whatever minimizes institutional cost or maximizes administrative discretion, this strengthens the snare classification (extraction disguised as principled philosophy); if the criterion is externally validated and stable, it weakens the extraction reading somewhat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fitness_criterion_content_indeterminacy, empirical, 'Whether fitness criteria are principled and stable or opportunistically administered.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the fitness-contingent reading a genuinely distinct philosophical position on personhood, or is it better understood as the birth-threshold reading with an added, separately-motivated exclusion clause layered on top for resource or eugenic reasons?',
    'Conceptual analysis of whether the fitness test is derived from an independent theory of moral status (as claimed) or is post-hoc rationalization for pre-existing resource-allocation or discriminatory practice — examine historical sequencing of doctrine versus practice.',
    'If post-hoc, the reading''s coordination-function claim (principled personhood theory) is substantially weaker than authored, and the constraint moves further toward pure extraction; if the theory genuinely precedes and shapes practice independently, the coordination claim has more standing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether this reading is a genuine independent doctrine or extraction rationalized after the fact.').

omega_variable(
    sibling_reading_convergence_at_margins,
    'At what point does the potential_based_reading''s narrower exclusion (severe disability only) collapse into the fitness_contingent_reading''s broader exclusion (all pre-test infants)?',
    'Comparative doctrinal analysis of jurisdictions that formally hold the potential-based reading but administratively apply fitness-style tests in practice — check whether the two readings are more distinct in theory than in operational effect.',
    'If the readings converge in practice, the network edge between them should carry a stronger influence weight than a merely adjacent-kernel relationship; if they remain operationally distinct, the current forecloses/coexists_with typing is sufficient.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_convergence_at_margins, conceptual, 'Whether operational convergence between sibling readings undermines their doctrinal distinctness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__fitness_contingent_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(pers_tr_t8, personhood_boundary__fitness_contingent_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(pers_tr_t16, personhood_boundary__fitness_contingent_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(pers_tr_t24, personhood_boundary__fitness_contingent_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(pers_tr_t32, personhood_boundary__fitness_contingent_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__fitness_contingent_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__fitness_contingent_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(pers_be_t8, personhood_boundary__fitness_contingent_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(pers_be_t16, personhood_boundary__fitness_contingent_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(pers_be_t24, personhood_boundary__fitness_contingent_reading, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(pers_be_t32, personhood_boundary__fitness_contingent_reading, base_extractiveness, 32, 0.78).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__fitness_contingent_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__fitness_contingent_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(pers_su_t8, personhood_boundary__fitness_contingent_reading, suppression_requirement, 8, 0.75).
narrative_ontology:measurement(pers_su_t16, personhood_boundary__fitness_contingent_reading, suppression_requirement, 16, 0.79).
narrative_ontology:measurement(pers_su_t24, personhood_boundary__fitness_contingent_reading, suppression_requirement, 24, 0.83).
narrative_ontology:measurement(pers_su_t32, personhood_boundary__fitness_contingent_reading, suppression_requirement, 32, 0.86).
narrative_ontology:measurement(pers_su_t40, personhood_boundary__fitness_contingent_reading, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__fitness_contingent_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, personhood_boundary__potential_based_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the personhood_boundary kernel. birth_threshold_reading grants unconditional standing at birth (narrowest exclusion, no victim set under normal circumstances). potential_based_reading grounds standing in potential for rational agency, excluding only a narrow class of severely disabled infants on a potentiality test. fitness_contingent_reading (this story) makes standing conditional on passing an administered fitness test as the default rule, producing the broadest victim set and the highest authored extractiveness and suppression of the three. Each carries its own ε and stakeholder set per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
