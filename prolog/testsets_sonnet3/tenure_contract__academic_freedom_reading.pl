% ============================================================================
% CONSTRAINT STORY: tenure_contract__academic_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__academic_freedom_reading, []).

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
 *   constraint_id: tenure_contract__academic_freedom_reading
 *   human_readable: Tenure as Academic Freedom Insulation Mechanism
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This story reads the tenure contract kernel through the academic-freedom
 *   lens: tenure exists to solve the coordination problem that truth-seeking
 *   inquiry produces findings unpopular with the powerful, and that
 *   career-survival pressure will otherwise bend inquiry toward safety. Under
 *   this reading, faculty are low-directionality beneficiaries of a genuine
 *   coordination structure, students and the public gain incidentally through
 *   research quality, and the only high-ε party is the external political
 *   actor whose preferred lever (termination threat) the constraint
 *   specifically blocks. This is a deliberately narrow reading — it does not
 *   evaluate whether peer review also reproduces demographic composition (the
 *   sibling demographic_reproduction_reading) or whether the same tenure
 *   grant functions as rent extraction against contingent labor (the sibling
 *   institutional_extraction_reading). Those are different constraints with
 *   different ε values, authored separately and linked by network edges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, 0.28).
domain_priors:suppression_score(tenure_contract__academic_freedom_reading, 0.35).
domain_priors:theater_ratio(tenure_contract__academic_freedom_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Tenure as Academic Freedom Insulation Mechanism").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__academic_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, '93849b70-07a0-418d-9318-31eb25be5944').
narrative_ontology:cs_kernel_codification('93849b70-07a0-418d-9318-31eb25be5944', formalized).
narrative_ontology:cs_authority_grounding('93849b70-07a0-418d-9318-31eb25be5944', practice).
narrative_ontology:cs_interpretation_layer_present('93849b70-07a0-418d-9318-31eb25be5944').
narrative_ontology:cs_reading_relation('93849b70-07a0-418d-9318-31eb25be5944', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('93849b70-07a0-418d-9318-31eb25be5944', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('93849b70-07a0-418d-9318-31eb25be5944', foundational, career_security_must_be_decoupled_from_finding_content).
narrative_ontology:cs_axiom_status(career_security_must_be_decoupled_from_finding_content, holdable).
narrative_ontology:cs_axiom_grounding('93849b70-07a0-418d-9318-31eb25be5944', career_security_must_be_decoupled_from_finding_content, instrumental).
narrative_ontology:cs_axiom('93849b70-07a0-418d-9318-31eb25be5944', secondary, peer_adjudicated_cause_is_legitimate_substitute_for_at_will_removal).
narrative_ontology:cs_axiom_status(peer_adjudicated_cause_is_legitimate_substitute_for_at_will_removal, holdable).
narrative_ontology:cs_axiom_grounding('93849b70-07a0-418d-9318-31eb25be5944', peer_adjudicated_cause_is_legitimate_substitute_for_at_will_removal, conventional).
narrative_ontology:cs_reference_frame('93849b70-07a0-418d-9318-31eb25be5944', aaup_1940_academic_freedom_settlement).
narrative_ontology:cs_drift_state('93849b70-07a0-418d-9318-31eb25be5944', contemporary_legislative_pressure_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('93849b70-07a0-418d-9318-31eb25be5944', '').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, students_and_public_via_research_quality).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, external_political_actors_seeking_suppression).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, untenured_researchers).
narrative_ontology:constraint_vindicates(tenure_contract__academic_freedom_reading, academic_freedom_doctrine).
narrative_ontology:constraint_vindicates(tenure_contract__academic_freedom_reading, truth_seeking_independence_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent appointments that cannot be terminated for the content of their research or teaching absent cause proceedings. This lets them pursue politically unpopular, commercially unprofitable, or institutionally embarrassing lines of inquiry without risking their livelihood. Exit to industry or other institutions remains available but is costly; the protection's value lies precisely in not needing to exercise it.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenured_faculty, beneficiary,
    organized, civilizational, mobile, national).

% Work toward tenure as the eventual guarantee of independence, but during the probationary period they lack the protection they are told the system exists to provide. They benefit from the promise and from senior colleagues' insulated advocacy, but bear a period of vulnerability the reading treats as a necessary screening cost rather than a structural harm.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, untenured_researchers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__academic_freedom_reading, untenured_researchers, excluded).

% Legislators, donors, and advocacy groups who want to defund, censure, or remove faculty whose findings or public statements conflict with their political or economic interests. Tenure specifically blocks their preferred lever (termination threat), forcing them into slower, costlier routes — public pressure campaigns, funding cuts, legislative restructuring of the institution itself. From this reading's standpoint, their frustration is the mechanism working as intended, not a defect.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, external_political_actors_seeking_suppression, payer,
    powerful, biographical, trapped, national).

% Receive the downstream benefit of research and teaching that has not been shaped by fear of institutional or political reprisal. They do not participate in the tenure decision and bear no direct cost under this reading; their interest is served incidentally by faculty independence, not through any active claim they assert.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, students_and_public_via_research_quality, beneficiary,
    powerless, generational, analytical, national).

% Administers the tenure review process and must defend tenured faculty against external removal pressure, sometimes at the cost of institutional funding or political standing. Under this reading, administration's role is to hold the line that protects the coordination function, not to extract from it — though the reading acknowledges administrations sometimes buckle under sufficient political or financial pressure.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, university_administration, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Decouples a researcher's material survival from the popularity of their findings, solving the genuine problem that truth-seeking inquiry sometimes produces conclusions that powerful parties want suppressed, and that inquiry will be systematically distorted toward safe conclusions if survival depends on avoiding offense.
% TRANSFER_FUNCTION: Moves the power to end a career from external political and institutional actors to a peer-adjudicated cause process, effectively transferring career security from 'at the pleasure of whoever is currently displeased' to 'contingent on a due-process finding of cause.' No monetary transfer is central to this reading; what moves is discretion over termination.
% ABSENT_VOICES: External political actors are present as payers under this reading but are not consulted on whether the protection is warranted — the reading treats their frustration as the intended effect, not a grievance requiring a seat. Contingent and adjunct faculty, who perform much of the same truth-seeking labor without any tenure protection, are structurally outside this reading's beneficiary class and would object that the coordination story does not extend to them.
% DISAPPEARANCE_RATIONALE: If tenure protections vanished, researchers working on politically contested topics (climate science, contested history, corporate-adjacent public health findings) would face direct termination exposure for their conclusions; institutions would face intensified pressure to align hiring and retention with funder and legislative preferences; the character of institutionally-housed inquiry into contested questions would shift measurably within a single hiring cycle.
% FOUNDING_PROBLEM: Early-20th-century cases of professors dismissed for teaching evolution, criticizing wartime policy, or advocating unpopular economic views demonstrated that at-will academic employment let institutional trustees and external political pressure directly control research and teaching content.
% FOUNDING_PROBLEM_CORROBORATION: The AAUP's 1940 Statement of Principles is the founding faculty-side document, but corroboration outside the beneficiary class exists in the historical record of legislative attempts to remove tenured faculty over contested research (climate science, critical theory, contested historical claims) that continue into the present, and in court findings recognizing tenure's role in preventing viewpoint-based termination. Independent higher-education policy researchers outside faculty unions have documented continued political pressure campaigns targeting tenured researchers specifically because termination is unavailable, which corroborates the problem's persistence from outside the group tenure protects.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tenure_contract__academic_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__academic_freedom_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__academic_freedom_reading_tests).
:- end_tests(tenure_contract__academic_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28) because, by this reading's own lights, the standing arrangement genuinely solves a coordination problem and the party who pays (external political actors) pays only in the sense of losing a suppression lever they should not structurally have. Suppression is moderate (0.35) because tenure does actively suppress one specific class of action — arbitrary termination for content — which requires ongoing institutional will to enforce against political and funder pressure. Theater ratio is low and rises only slightly (0.10 to 0.15) reflecting that cause-review processes are largely functional rather than performative, though bureaucratic formalization has crept in modestly over the measured interval.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty sit near the beneficiary end: the constraint subsidizes their independence at low cost to them personally. External political actors sit near the full-target end: the constraint is specifically constructed to block their preferred lever, and they cannot exit the relationship — they remain stakeholders in the institution's public standing and funding regardless of whether they can remove any given researcher. Students and the public are near-symmetric-to-beneficiary: they gain from research quality but assert no active claim and bear no direct cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary termination for unpopular findings) remains live by this reading's own account — corroborated by continuing legislative and donor pressure campaigns against tenured researchers specifically because the termination lever is unavailable to them. Because founding_problem_status is 'live' and disappearance_verdict is 'world_rearranges', this reading finds no mandatrophy: the mandate has not outlived its function under the academic-freedom framing. A finding of mandatrophy would require showing the underlying suppression risk has disappeared, which this reading disputes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_or_capture_readings,
    'Does the tenure contract kernel genuinely admit the academic-freedom coordination reading as structurally dominant, or is the coordination story cover for the extraction and gatekeeping dynamics the sibling readings identify operating through the same mechanism?',
    'Comparative institutional analysis: track whether tenure denial/grant decisions correlate more strongly with (a) documented instances of protecting politically unpopular research, (b) demographic ''fit'' assessments uncorrelated with output, or (c) departmental headcount/budget protection unrelated to either. A dominant correlation would favor one reading over the others as the structurally primary account.',
    'If capture or gatekeeping dynamics dominate empirically, this academic-freedom reading would need to be recharacterized as the minority function riding on a structure whose primary operation is described by a sibling reading — though per the ε-invariance principle this would not change THIS story''s ε, since each reading''s ε is assessed by its own lights against the same kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_or_capture_readings, conceptual, 'Whether academic freedom is the kernel''s structurally dominant function or one function among several sharing the same mechanism.').

omega_variable(
    untenured_exclusion_boundary,
    'Does the academic-freedom coordination benefit genuinely extend to untenured and contingent researchers who perform the same truth-seeking labor, or does this reading''s beneficiary class improperly exclude them?',
    'Compare research risk-taking and topic selection between tenured and non-tenured researchers in politically sensitive fields; a persistent gap would indicate the coordination benefit is narrowly captured by the tenured class rather than diffusing to the labor performing the function.',
    'If contingent researchers show systematically more cautious topic selection, the academic-freedom reading''s claimed public benefit (quality research reaching students/public) is undercut for the growing share of instruction and research performed by non-tenured labor, suggesting the coordination function has a narrower effective scope than this reading assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(untenured_exclusion_boundary, empirical, 'Whether the tenure protection''s coordination benefit generalizes beyond the tenured class itself.').

omega_variable(
    cs_framing_kernel_vs_review_process,
    'Is the correct CS framing the tenure grant itself (kernel: the permanent-employment commitment) or the peer-review process that adjudicates cause (kernel: the evidentiary standard for termination)?',
    'Trace which layer actually absorbs contested cases: if cause proceedings routinely reinterpret ''cause'' to accommodate institutional pressure while nominally preserving the tenure grant, the review process is the operative kernel and the employment commitment is a fixed_text/lineage shell around a more flexible authority_grounding.',
    'If the review process is the operative locus, drift analysis should track review-standard erosion rather than formal tenure-grant erosion, which would change where this story''s drift_state should be measured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_review_process, conceptual, 'Alternative framing of what constitutes the kernel within the tenure commitment system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__academic_freedom_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tenu_tr_t8, tenure_contract__academic_freedom_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement(tenu_tr_t16, tenure_contract__academic_freedom_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(tenu_tr_t24, tenure_contract__academic_freedom_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(tenu_tr_t32, tenure_contract__academic_freedom_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__academic_freedom_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__academic_freedom_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(tenu_be_t8, tenure_contract__academic_freedom_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(tenu_be_t16, tenure_contract__academic_freedom_reading, base_extractiveness, 16, 0.25).
narrative_ontology:measurement(tenu_be_t24, tenure_contract__academic_freedom_reading, base_extractiveness, 24, 0.26).
narrative_ontology:measurement(tenu_be_t32, tenure_contract__academic_freedom_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__academic_freedom_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__academic_freedom_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(tenu_su_t8, tenure_contract__academic_freedom_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(tenu_su_t16, tenure_contract__academic_freedom_reading, suppression_requirement, 16, 0.3).
narrative_ontology:measurement(tenu_su_t24, tenure_contract__academic_freedom_reading, suppression_requirement, 24, 0.32).
narrative_ontology:measurement(tenu_su_t32, tenure_contract__academic_freedom_reading, suppression_requirement, 32, 0.34).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__academic_freedom_reading, suppression_requirement, 40, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tenure_contract__academic_freedom_reading, 0.08).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% Three readings of the tenure_contract kernel: this story (academic_freedom_reading, low ε, coordination-dominant, rope-flavored), institutional_extraction_reading (rent extraction by tenured incumbents onto contingent labor, high ε for adjuncts), and demographic_reproduction_reading (peer-review gatekeeping reproducing group composition via 'fit' criteria, high ε for excluded demographic groups). All three share the same underlying text/practice (the tenure grant and its review process) but author independently stable ε values per the ε-invariance principle — none is a measurement of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
