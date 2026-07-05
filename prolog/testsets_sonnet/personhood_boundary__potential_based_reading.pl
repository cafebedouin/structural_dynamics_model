% ============================================================================
% CONSTRAINT STORY: personhood_boundary__potential_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__potential_based_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: personhood_boundary__potential_based_reading
 *   human_readable: Personhood Boundary: Potential-for-Rational-Agency Reading
 *   domain: moral_philosophy/bioethics/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the potential-based reading of the contested
 *   personhood-boundary kernel: moral standing is grounded in an entity's
 *   potential for rational agency, such that severely disabled infants judged
 *   to lack that potential (or its realistic prospect) may lack full
 *   standing. This is one of three sibling constraints sharing a kernel — the
 *   birth-threshold reading (standing attaches at birth, unconditionally) and
 *   the fitness-contingent reading (standing requires demonstrated fitness)
 *   are separate constraints with their own ε values, not alternative
 *   measurements of this one. The potential-based reading is distinguished
 *   structurally by WHO makes the exclusion judgment (physicians and parents,
 *   not a bright-line biological event) and by WHAT the excluded class is
 *   (infants without realistic developmental potential, not all infants or
 *   all pre-fit entities).
 *
 * KEY AGENTS:
 *   - severely_disabled_infants: primary target (powerless/trapped) — cannot contest the classification made about them
 *   - attending_physicians: primary agenda_setter (institutional/arbitrage) — apply the criterion at the bedside and benefit from its liability-reducing vocabulary
 *   - parents_seeking_nontreatment_authorization: beneficiary and payer (moderate/constrained) — relieved of an unbearable moral burden but bear grief and social scrutiny
 *   - disability_rights_advocates: excluded voice (organized/constrained) — structurally implicated by the criterion's logic but rarely present at the point of decision
 *   - bioethicists_and_philosophers: analytical observer (analytical/analytical) — see the full contested structure without controlling it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, 0.58).
domain_priors:suppression_score(personhood_boundary__potential_based_reading, 0.62).
domain_priors:theater_ratio(personhood_boundary__potential_based_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, accessibility_collapse, 0.44).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__potential_based_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__potential_based_reading, "Personhood Boundary: Potential-for-Rational-Agency Reading").
narrative_ontology:topic_domain(personhood_boundary__potential_based_reading, "moral_philosophy/bioethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__potential_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__potential_based_reading, 'ec6f22d3-f3a8-442a-9733-300584894194').
narrative_ontology:cs_kernel_codification('ec6f22d3-f3a8-442a-9733-300584894194', distributed).
narrative_ontology:cs_authority_grounding('ec6f22d3-f3a8-442a-9733-300584894194', expertise).
narrative_ontology:cs_interpretation_layer_present('ec6f22d3-f3a8-442a-9733-300584894194').
narrative_ontology:cs_reading_relation('ec6f22d3-f3a8-442a-9733-300584894194', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('ec6f22d3-f3a8-442a-9733-300584894194', personhood_boundary__fitness_contingent_reading, coexists_with).
narrative_ontology:cs_axiom('ec6f22d3-f3a8-442a-9733-300584894194', foundational, moral_status_grounded_in_capacity_potential).
narrative_ontology:cs_axiom_status(moral_status_grounded_in_capacity_potential, holdable).
narrative_ontology:cs_axiom_grounding('ec6f22d3-f3a8-442a-9733-300584894194', moral_status_grounded_in_capacity_potential, deontological).
narrative_ontology:cs_axiom('ec6f22d3-f3a8-442a-9733-300584894194', secondary, clinical_medical_authority_may_adjudicate_standing).
narrative_ontology:cs_axiom_status(clinical_medical_authority_may_adjudicate_standing, holdable).
narrative_ontology:cs_axiom_grounding('ec6f22d3-f3a8-442a-9733-300584894194', clinical_medical_authority_may_adjudicate_standing, conventional).
narrative_ontology:cs_reference_frame('ec6f22d3-f3a8-442a-9733-300584894194', clinical_bioethics_capacity_framework).
narrative_ontology:cs_drift_state('ec6f22d3-f3a8-442a-9733-300584894194', contemporary_disability_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ec6f22d3-f3a8-442a-9733-300584894194', '').
narrative_ontology:cs_kernel_id(personhood_boundary__potential_based_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, attending_physicians).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, hospital_ethics_committees).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, parents_seeking_nontreatment_authorization).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, severely_disabled_infants).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, disability_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, parents_seeking_nontreatment_authorization).
narrative_ontology:constraint_vindicates(personhood_boundary__potential_based_reading, capacity_grounded_moral_status_doctrine).
narrative_ontology:constraint_vindicates(personhood_boundary__potential_based_reading, rational_agency_potential_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot advocate, cannot exit, cannot contest the classification made about them. Whether they are treated as bearers of moral standing depends entirely on a clinical/parental judgment about whether they possess or could develop the relevant potential. If judged to lack it, decisions about withholding treatment or resources are made without their voice entering the record at all.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, severely_disabled_infants, payer,
    powerless, immediate, trapped, local).

% Apply the potential-based criterion in real time at the bedside, deciding which infants meet the threshold for full moral standing and which do not. This authority reduces liability exposure and resource-allocation friction for the institution, and provides a principled-sounding vocabulary for decisions that would otherwise appear as rationing.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, attending_physicians, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, attending_physicians, beneficiary).

% Facing catastrophic prognoses, some parents find in the potential-based reading a framework that authorizes withholding aggressive intervention without the moral weight of 'ending a person's life' — a genuine relief from an otherwise unbearable decision. They also bear enormous grief and social judgment, and the framework's authority over their child's status is not one they control.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, parents_seeking_nontreatment_authorization, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, parents_seeking_nontreatment_authorization, payer).

% Codify and adjudicate the potential-based standard into protocols (e.g., criteria distinguishing infants with prospects for cognitive development from those without). They administer the boundary and could revise it, but institutional and legal precedent make revision costly and slow.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, hospital_ethics_committees, agenda_setter,
    institutional, generational, analytical, national).

% Argue the potential criterion imports an ableist hierarchy that treats disabled lives as conditionally valuable, and that 'potential for rational agency' is not a capacity anyone can actually measure in a newborn. They testify in policy hearings and litigate against protocols but are rarely present at the bedside where the classification is actually applied.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, disability_rights_advocates, excluded,
    organized, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, disability_rights_advocates, payer).

% Debate whether potential is a coherent moral criterion at all, given that many entities with unrealized or unrealizable potential (e.g., temporarily comatose adults) are never treated as lacking standing. They observe the practical application without controlling it.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, bioethicists_and_philosophers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides clinicians and grieving families a shared, principled-sounding vocabulary for making devastating end-of-life and resource-allocation decisions about severely disabled infants without each case requiring ad hoc improvisation of moral reasoning at the bedside.
% TRANSFER_FUNCTION: Moves the burden of moral status determination away from courts and legislatures and onto clinical judgment; moves scarce intensive-care resources away from infants classified as lacking sufficient potential and toward other patients; moves legal and moral liability away from physicians and parents who act within the framework.
% ABSENT_VOICES: The infants themselves cannot speak. Disability rights advocates who reject capacity-based hierarchies of moral worth are rarely present in the clinical moment where the classification is made, even though they are the constituency most structurally implicated by the criterion's logic.
% DISAPPEARANCE_RATIONALE: If the potential-based reading vanished, neonatal intensive care protocols, ethics-committee decision trees, and case law built around 'capacity for future rational agency' would require replacement; decisions currently authorized under this framework would need a different justificatory basis (birth threshold or fitness-contingent), materially changing which infants receive aggressive treatment.
% FOUNDING_PROBLEM: Clinicians and families needed a principled way to distinguish cases where withholding aggressive intervention is morally permissible from cases where it is infanticide, in an era where neonatal medicine could keep alive infants with catastrophic and irreversible impairments.
% FOUNDING_PROBLEM_CORROBORATION: Physicians and ethics committees attest the problem remains live — the underlying clinical dilemma (irreversible catastrophic impairment vs. treatable condition) has not disappeared. Disability rights scholars and independent bioethicists outside the clinical/parental beneficiary set attest that the 'potential for rational agency' criterion itself is empirically unmeasurable in newborns and has been repeatedly used to extend nontreatment decisions beyond the narrow catastrophic cases it was designed for — a genealogy drift documented in disability studies literature, not merely alleged by advocates.
narrative_ontology:disappearance_verdict(personhood_boundary__potential_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__potential_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__potential_based_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(personhood_boundary__potential_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__potential_based_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__potential_based_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__potential_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the criterion, once applied, redirects real resources (intensive care, aggressive intervention) away from a defenseless class based on a judgment that class cannot contest and that is empirically difficult to verify in newborns. Suppression (0.62) reflects that resistance to the classification (from disability advocates, from the infant's own interest if it could be represented) has no standing within the framework itself — the framework's authority structure has no channel for the excluded voice to alter a specific case. Theater ratio is moderate (0.31) and rising: a growing share of ethics-committee protocol activity performs rigor (multi-factor checklists, case conferences) without resolving the underlying unmeasurability of 'potential for rational agency' in a newborn.
 *
 * PERSPECTIVAL GAP:
 *   From the physician/ethics-committee seat, this looks like principled clinical ethics — a defensible, coordinated response to real uncertainty. From the disability-rights seat, the identical structure looks like a capacity hierarchy that revives eugenic reasoning under bioethical vocabulary. The engine should compute these as different seat-level types from the same structural data; neither seat's self-description settles the classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Physicians and ethics committees sit near the agenda-setting end: they administer the boundary, gain liability protection and decision-making legitimacy from it, and could in principle revise the criteria. Parents are genuinely dual-positioned — real relief from an impossible bind, but also bearers of a decision imposed on them by a framework they did not design. Severely disabled infants are the clearest target: full extraction, zero voice, zero exit. Disability rights advocates are victims in a structural sense (the criterion's logic implicates the value of disabled life generally) even though no single advocate is a party to any individual bedside decision.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing permissible nontreatment from infanticide under genuine clinical uncertainty — remains partially live: catastrophic, irreversible neonatal conditions still occur and clinicians still need principled guidance. But the corroboration record shows the criterion has drifted beyond its founding scope: 'potential for rational agency' cannot be reliably measured in a newborn, and disability studies literature documents its application broadening past the narrowest catastrophic cases. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (it does solve a real problem for physicians and some parents) while registering the asymmetric extraction on infants and the excluded disability-rights constituency that a pure-coordination read would erase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    potential_measurability,
    'Can ''potential for rational agency'' be reliably measured or predicted in a newborn infant, distinct from post-hoc rationalization of a resource or grief-driven decision already made?',
    'Longitudinal outcome studies comparing infants classified as lacking potential against actual developmental trajectories where aggressive treatment was nonetheless provided (natural experiments from jurisdictions with different protocols); independent review of ethics-committee case files for consistency and predictive accuracy.',
    'If potential is not reliably measurable, the criterion functions as a post-hoc justification for decisions actually driven by resource constraints or prognosis pessimism, sharply raising the constraint''s effective extraction and pushing the classification toward snare. If measurable with reasonable reliability, the coordination function is more genuine and the tangled_rope classification is more secure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(potential_measurability, empirical, 'Whether the potential criterion is empirically tractable or a post-hoc rationalization.').

omega_variable(
    kernel_framing_naturalness_vs_construction,
    'Is grounding personhood in potential for rational agency a philosophically principled position independently defensible, or a constructed compromise designed to authorize a narrower class of nontreatment decisions than fitness_contingent_reading while avoiding the political costs of birth_threshold_reading''s absolutism?',
    'Trace the historical emergence of potential-based criteria in bioethics literature (e.g., Tooley, Warren, subsequent critiques) against the clinical case law it was invoked to justify, checking whether the philosophical argument preceded or followed the practical need for a defensible-sounding standard.',
    'If constructed to serve a practical justificatory need, the reading''s normative authority is weaker than its philosophical presentation suggests, and its axioms should be read as instrumental rather than purely deontological.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_naturalness_vs_construction, conceptual, 'Whether potential-based personhood is independently principled or a constructed compromise reading.').

omega_variable(
    excluded_class_boundary_creep,
    'Has the class of infants judged to lack ''potential for rational agency'' expanded over time beyond the narrowest catastrophic cases (e.g., anencephaly) to include a wider range of disabilities?',
    'Compare protocol documents and case records across the measured interval for the range of conditions cited as meeting the exclusion criterion.',
    'Boundary creep would corroborate the disability-rights position that the criterion is unstable and extraction-widening rather than a fixed principled line, supporting the rising base_extractiveness trajectory already authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_class_boundary_creep, empirical, 'Whether the practical scope of exclusion has widened beyond its founding cases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__potential_based_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__potential_based_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(pers_tr_t8, personhood_boundary__potential_based_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(pers_tr_t16, personhood_boundary__potential_based_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(pers_tr_t24, personhood_boundary__potential_based_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(pers_tr_t32, personhood_boundary__potential_based_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__potential_based_reading, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__potential_based_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(pers_be_t8, personhood_boundary__potential_based_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(pers_be_t16, personhood_boundary__potential_based_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(pers_be_t24, personhood_boundary__potential_based_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(pers_be_t32, personhood_boundary__potential_based_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__potential_based_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__potential_based_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(pers_su_t8, personhood_boundary__potential_based_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(pers_su_t16, personhood_boundary__potential_based_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(pers_su_t24, personhood_boundary__potential_based_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(pers_su_t32, personhood_boundary__potential_based_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(pers_su_t40, personhood_boundary__potential_based_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__fitness_contingent_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__birth_threshold_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the personhood_boundary kernel. birth_threshold_reading claims personhood attaches unconditionally at birth (no victim class among born humans); fitness_contingent_reading claims personhood requires demonstrated fitness (a broader, more permissive exclusion criterion covering more entities than potential); this potential_based_reading occupies an intermediate position, excluding only infants judged to lack realistic developmental potential. Each has a distinct ε, distinct beneficiary/victim structure, and distinct classification — they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
