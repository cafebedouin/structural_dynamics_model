% ============================================================================
% CONSTRAINT STORY: dignified_death__autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__autonomy_primary, []).

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
 *   constraint_id: dignified_death__autonomy_primary
 *   human_readable: Autonomy-Primary Reading of Dignified Death: Regulated Assisted Dying with Medical Gatekeeping
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story analyzes the regulated assisted dying regime (as
 *   implemented in jurisdictions like Oregon, Netherlands, Canada, Belgium)
 *   from the autonomy-primary reading of the 'dignified death' kernel. The
 *   autonomy-primary reading holds that dignity resides in self-determination
 *   and the suffering individual has final authority over timing and method
 *   of death. The standing arrangement — a medical-legal gatekeeping regime
 *   with strict eligibility criteria — is assessed by this reading as a
 *   tangled rope: it coordinates a genuine function (safe, supervised access
 *   for qualifying patients) while asymmetrically extracting autonomous
 *   choice from those who suffer but fall outside the criteria. The
 *   extraction is high (ε=0.52) because the gatekeeping apparatus actively
 *   denies exit to structurally identifiable groups (non-terminal suffering,
 *   psychiatric suffering, dementia without terminal prognosis) while
 *   concentrating interpretive authority in medical institutions that benefit
 *   professionally from maintaining the boundary. Suppression is very high
 *   (0.78) because the constraint's persistence depends on active
 *   enforcement: criminal prohibition for non-compliant clinicians,
 *   disciplinary proceedings for boundary-pushing assessors, and statutory
 *   exclusion of entire suffering categories. Theater ratio (0.32) reflects
 *   that the coordination function (safeguards, oversight) is real but a
 *   growing share of enforcement energy defends the eligibility boundary
 *   rather than protecting patients. The measurement series shows
 *   extractiveness rising as eligibility criteria fail to expand with
 *   clinical understanding, theater increasing as procedural compliance
 *   substitutes for substantive access, and suppression hardening as
 *   challenges mount.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__autonomy_primary, 0.52).
domain_priors:suppression_score(dignified_death__autonomy_primary, 0.78).
domain_priors:theater_ratio(dignified_death__autonomy_primary, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, extractiveness, 0.52).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Autonomy-Primary Reading of Dignified Death: Regulated Assisted Dying with Medical Gatekeeping").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, '55c7f96e-7bd4-4187-837b-1d44626178c1').
narrative_ontology:cs_kernel_codification('55c7f96e-7bd4-4187-837b-1d44626178c1', distributed).
narrative_ontology:cs_authority_grounding('55c7f96e-7bd4-4187-837b-1d44626178c1', distributed).
narrative_ontology:cs_reading_relation('55c7f96e-7bd4-4187-837b-1d44626178c1', dignified_death__relational_autonomy, coexists_with).
narrative_ontology:cs_reading_relation('55c7f96e-7bd4-4187-837b-1d44626178c1', dignified_death__sanctity_primary, forecloses).
narrative_ontology:cs_axiom('55c7f96e-7bd4-4187-837b-1d44626178c1', foundational, individual_has_final_authority_over_death_timing_and_method).
narrative_ontology:cs_axiom_status(individual_has_final_authority_over_death_timing_and_method, holdable).
narrative_ontology:cs_axiom_grounding('55c7f96e-7bd4-4187-837b-1d44626178c1', individual_has_final_authority_over_death_timing_and_method, deontological).
narrative_ontology:cs_axiom('55c7f96e-7bd4-4187-837b-1d44626178c1', secondary, suffering_justifies_autonomous_exit).
narrative_ontology:cs_axiom_status(suffering_justifies_autonomous_exit, holdable).
narrative_ontology:cs_axiom_grounding('55c7f96e-7bd4-4187-837b-1d44626178c1', suffering_justifies_autonomous_exit, deontological).
narrative_ontology:cs_reference_frame('55c7f96e-7bd4-4187-837b-1d44626178c1', autonomy_primary_dignity_framework).
narrative_ontology:cs_drift_state('55c7f96e-7bd4-4187-837b-1d44626178c1', contemporary_regulated_access_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('55c7f96e-7bd4-4187-837b-1d44626178c1', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, qualifying_autonomous_agents).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, denied_exit_patients).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, suffering_prolonged_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, medical_regulatory_bodies).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, palliative_care_establishment).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, clinical_gatekeepers).
narrative_ontology:constraint_vindicates(dignified_death__autonomy_primary, bodily_self_determination).
narrative_ontology:constraint_vindicates(dignified_death__autonomy_primary, autonomy_as_dignity_foundation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Patients who meet strict eligibility criteria (terminal illness, prognosis <6 months, mental capacity, voluntary request) and can access legal assisted dying. They benefit from the coordination function — a legal pathway exists — but their access is contingent on medical gatekeepers' assessment. Exit from suffering is available but only on terms the gatekeepers define.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, qualifying_autonomous_agents, beneficiary,
    moderate, biographical, constrained, national).

% Suffering individuals who do not meet eligibility criteria — non-terminal but intractable suffering (e.g., severe chronic neurodegenerative, psychiatric, or constitutional suffering), or those who lose capacity before completing the process. They bear the full extraction: prolonged suffering against their will, with no legal exit. The constraint's gatekeeping function actively denies them the autonomy the coordination function promises.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, denied_exit_patients, payer,
    powerless, immediate, trapped, national).

% Individuals whose suffering falls outside the recognized medical categories (e.g., existential suffering, dementia without terminal prognosis, treatment-refractory mental illness). They are structurally excluded from the coordination benefit and bear the extraction of continued suffering. Some seek extraterritorial exit (travel to permissive jurisdictions), which is resource-intensive and often impossible at end-stage.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, suffering_prolonged_individuals, payer,
    powerless, immediate, trapped, national).

% Enacts the statutory framework defining eligibility criteria, procedural safeguards, and reporting requirements. Holds ultimate authority to liberalize or restrict access. Responds to political pressure from both autonomy advocates and sanctity advocates. The legislative agenda-setting power is the primary lever for structural change.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, state_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Medical colleges, licensing boards, and health ministries that translate statute into clinical guidelines, assess practitioner compliance, and discipline deviations. They benefit professionally from gatekeeping authority — control over the life/death boundary reinforces medical sovereignty. Their institutional interest aligns with narrow eligibility and rigorous oversight.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, medical_regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, medical_regulatory_bodies, beneficiary).

% Physicians, psychiatrists, and assessors who evaluate requests, confirm eligibility, and administer or prescribe. They bear moral and professional risk (conscience objections, fear of error, collegial scrutiny) and exercise de facto veto power through eligibility interpretation. Some experience moral injury from both participating and refusing; the constraint extracts clinical autonomy even as it grants gatekeeping authority.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, clinical_gatekeepers, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, clinical_gatekeepers, agenda_setter).

% Specialty societies, hospice networks, and palliative clinicians who advocate for improved symptom management as the alternative to assisted dying. They benefit from the constraint's structure: the gatekeeping regime channels resources and legitimacy toward palliative care as the 'preferred' pathway. Some genuinely coordinate suffering relief; others leverage the prohibition to protect professional jurisdiction.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, palliative_care_establishment, beneficiary,
    organized, generational, mobile, national).

% Right-to-die organizations, civil liberties groups, and bioethicists arguing for broader eligibility based on self-determination. They are structurally excluded from the gatekeeping process — their voices are heard in legislative testimony but not in clinical eligibility determinations. They would object to the narrow criteria as violating the autonomy norm the constraint claims to honor.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, autonomy_advocates, excluded,
    organized, generational, mobile, global).

% Religious institutions, pro-life organizations, and bioethicists opposing any intentional life-termination. They are excluded from the clinical gatekeeping but exert influence through legislative lobbying and public campaigns. They view the constraint as insufficiently restrictive — the very existence of a legal pathway violates their reading of the kernel.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, sanctity_advocates, excluded,
    organized, generational, mobile, global).

% Constitutional and administrative courts adjudicating challenges to eligibility criteria, procedural delays, and equality-rights claims. They observe the constraint's operation from an analytical seat, periodically forcing structural adjustments (e.g., striking down 'reasonably foreseeable death' requirement). Their rulings shape the constraint's evolution but they do not administer it.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, courts_and_tribunals, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally sanctioned, medically supervised pathway for assisted dying that prevents clandestine/unsafe practices, ensures voluntariness and capacity assessment, and offers a structured alternative to unilateral suicide or physician non-compliance.
% TRANSFER_FUNCTION: Moves decision-authority over death timing/method from the individual to a medical-legal gatekeeping triad (physician assessors, regulatory bodies, statutory criteria). The individual retains a request right but not a decision right; the gatekeepers hold the effective veto. The transfer extracts autonomous choice from those who fail criteria and concentrates interpretive authority in medical institutions.
% ABSENT_VOICES: Individuals with non-terminal but intolerable suffering (advanced dementia, treatment-refractory mental illness, constitutional suffering) who are categorically excluded by current eligibility frameworks. Also absent: future patients whose suffering categories have not yet been recognized. They are not in the room where criteria are written or applied.
% DISAPPEARANCE_RATIONALE: If the regulated gatekeeping regime vanished overnight, the autonomy-primary reading predicts: (a) immediate access for currently denied patients who would self-determine exit; (b) loss of safeguards (voluntariness verification, capacity assessment) for vulnerable qualifying patients; (c) professional and legal vacuum for clinicians; (d) political crisis as sanctity advocates demand reinstatement. The world rearranges — both the coordination benefit and the extraction would disappear, replaced by a contested vacuum.
% FOUNDING_PROBLEM: The pre-legalization landscape featured: clandestine assisted deaths with no safeguards; physician non-compliance with patient wishes; traumatic suicide methods for those denied help; legal jeopardy for compassionate clinicians; and public demand for a regulated alternative to uncontrolled practice.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary committee reports and royal commission transcripts (e.g., UK House of Lords 2005, Canadian Special Joint Committee 2016) document the founding problem from outside the benefiting parties. Autonomy advocates attest the problem persists for those excluded by current criteria. Medical regulators attest the original safeguards function for qualifying patients. The status is contested because parties disagree on whether the founding problem is solved (for qualifying patients) or merely displaced (onto denied patients).
narrative_ontology:disappearance_verdict(dignified_death__autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignified_death__autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__autonomy_primary, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is tangled_rope because the constraint structurally combines: (1) a genuine coordination function — providing a legal, supervised pathway that prevents the harms of clandestine practice — and (2) asymmetric extraction — the same gatekeeping apparatus that enables coordination for qualifying patients actively denies it to denied_exit_patients and suffering_prolonged_individuals, while medical_regulatory_bodies and clinical_gatekeepers benefit professionally from the gatekeeping authority. Requires_active_enforcement is true: the eligibility boundary is maintained by criminal law, professional discipline, and institutional policy. Beneficiaries (qualifying_autonomous_agents) and victims (denied_exit_patients, suffering_prolonged_individuals) are both named. The engine will compute per-seat classifications: from the qualifying agent seat, the constraint may compute as rope (coordination with minimal extraction); from the denied patient seat, it computes as snare (pure extraction with no coordination benefit); from the clinical gatekeeper seat, it may compute as tangled_rope (both coordinating and extracting). This seat divergence is the measurement.
 *
 * PERSPECTIVAL GAP:
 *   The autonomy-primary reading and the sanctity-primary reading will compute radically different seat classifications for the same constraint. From autonomy-primary: denied patients are victims of extraction; from sanctity-primary: the constraint is insufficiently suppressive (it permits any killing at all). The clinical_gatekeeper seat experiences the constraint as both coordination (clear legal framework) and extraction (moral injury, professional risk) — the engine captures this duality. The palliative_care_establishment benefits from the constraint's existence (resources, legitimacy) but some members experience moral tension. The seat divergence is not noise — it is the structural reality the framework measures.
 *
 * DIRECTIONALITY LOGIC:
 *   The autonomy-primary reading sees the constrained individual (suffering patient) as the structural target of extraction — the constraint denies them the final authority the reading says is theirs by right. Directionality for denied_exit_patients and suffering_prolonged_individuals is near 1.0 (full target): they bear the full cost of prolonged suffering with no legal exit, and their exit_options are 'trapped' (no legal alternative, extraterritorial exit practically impossible at end-stage). Qualifying_autonomous_agents have directionality near 0.3 (partial beneficiary): they receive the coordination benefit but only on gatekeepers' terms, with exit_options 'constrained' (must meet criteria, complete process, retain capacity). State_legislature and medical_regulatory_bodies have directionality near 0.0 (beneficiaries): they collect authority, legitimacy, and professional jurisdiction from the constraint. Clinical_gatekeepers sit near 0.5 (symmetric): they bear moral/professional risk but hold veto power. The engine derives these from beneficiary/victim declarations + exit_options + power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (clandestine practice, no safeguards, clinician jeopardy) is substantially solved for qualifying patients but displaced onto denied patients. The constraint's mandate has not atrophied — the coordination function remains live for those who qualify — but its extraction has accumulated as clinical understanding of suffering outpaces eligibility criteria. This is not a piton (the function hasn't atrophied) but a tangled_rope with growing extractive overhang. The mandatrophy_resolved flag is false because the coordination function is still claimed and used, even as the extraction side expands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the autonomy_primary reading a distinct constraint from the relational_autonomy and sanctity_primary readings, or are they observables of a single constraint?',
    'Apply ε-invariance test: if measuring the constraint via autonomy_primary criteria yields ε=0.52 (tangled_rope) while sanctity_primary criteria yields ε≈0.9 (snare — any permission is maximal extraction from sanctity view), they are different constraints. The decomposition into three constraint stories is warranted.',
    'If they are one constraint, the framework must model observable-dependent ε (forbidden by DP-001). If three constraints, each gets its own ε, stakeholders, and classification, linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel decomposes into multiple ε-invariant constraints per DP-001.').

omega_variable(
    sanctity_foreclosure_structural,
    'Does the autonomy_primary reading''s core premise (individual has FINAL authority over death timing/method) logically foreclose the sanctity_primary reading within any single legal-ethical framework?',
    'Analyze whether a jurisdiction can simultaneously hold ''individual has final authority to choose assisted death'' and ''intentional life-termination is always prohibited regardless of consent'' as operative law. If mutually exclusive in practice, the relation is ''forecloses''; if they coexist as competing political positions without legal resolution, ''coexists_with''.',
    'If forecloses, the cs_structure.reading_relations entry for sanctity_primary is correctly ''forecloses''. If coexists_with, the relation should be corrected. This affects the engine''s foreclosure computation from axiom contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctity_foreclosure_structural, conceptual, 'Whether autonomy_primary and sanctity_primary are logically incompatible in a single framework.').

omega_variable(
    gatekeeping_coordination_extraction_boundary,
    'Is the medical gatekeeping (eligibility criteria, capacity assessment, waiting periods) structurally necessary for the coordination function (safe, voluntary access), or is it extractive overhead that could be reduced without losing the coordination benefit?',
    'Natural experiment: compare outcomes in jurisdictions with narrower vs. broader eligibility (e.g., Netherlands'' broader criteria vs. Oregon''s terminal-illness-only). If broader criteria maintain safeguards (low non-voluntary deaths, low error rates) while serving more suffering patients, the gatekeeping is partially extractive. If broader criteria produce measurable harm, gatekeeping is coordination-necessary.',
    'If gatekeeping is partially extractive, the constraint''s ε is inflated by medical professional interest, not just patient protection. This strengthens the tangled_rope classification and identifies medical_regulatory_bodies as beneficiaries of extraction, not just coordinators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_coordination_extraction_boundary, empirical, 'Whether eligibility strictness is coordination-necessary or extractive overhead.').

omega_variable(
    suppression_mechanism_denied_patients,
    'Is the suppression experienced by denied_exit_patients structural (legal prohibition, criminal sanctions) or internalized (patients accept they are ''not eligible'' and stop seeking)?',
    'Post-denial trajectory study: do patients who are assessed as ineligible continue seeking exit (travel, clandestine means, suicide) or do they acquiesce? If acquiescence correlates with internalized ''ineligibility'' framing, suppression has internalized component.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint extracts not just by blocking exit but by shaping self-conception of who ''deserves'' exit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_denied_patients, empirical, 'Structural vs. internalized suppression for denied patients.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dignified_death_autonomy_primary_tr_t0, dignified_death__autonomy_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement(dignified_death_autonomy_primary_tr_t5, dignified_death__autonomy_primary, theater_ratio, 5, 0.22).
narrative_ontology:measurement(dignified_death_autonomy_primary_tr_t10, dignified_death__autonomy_primary, theater_ratio, 10, 0.26).
narrative_ontology:measurement(dignified_death_autonomy_primary_tr_t15, dignified_death__autonomy_primary, theater_ratio, 15, 0.29).
narrative_ontology:measurement(dignified_death_autonomy_primary_tr_t20, dignified_death__autonomy_primary, theater_ratio, 20, 0.3).
narrative_ontology:measurement(dignified_death_autonomy_primary_tr_t25, dignified_death__autonomy_primary, theater_ratio, 25, 0.31).
narrative_ontology:measurement(dignified_death_autonomy_primary_tr_t30, dignified_death__autonomy_primary, theater_ratio, 30, 0.32).

% Extraction over time
narrative_ontology:measurement(dignified_death_autonomy_primary_be_t0, dignified_death__autonomy_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(dignified_death_autonomy_primary_be_t5, dignified_death__autonomy_primary, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(dignified_death_autonomy_primary_be_t10, dignified_death__autonomy_primary, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(dignified_death_autonomy_primary_be_t15, dignified_death__autonomy_primary, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(dignified_death_autonomy_primary_be_t20, dignified_death__autonomy_primary, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(dignified_death_autonomy_primary_be_t25, dignified_death__autonomy_primary, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(dignified_death_autonomy_primary_be_t30, dignified_death__autonomy_primary, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(dignified_death_autonomy_primary_su_t0, dignified_death__autonomy_primary, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(dignified_death_autonomy_primary_su_t5, dignified_death__autonomy_primary, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(dignified_death_autonomy_primary_su_t10, dignified_death__autonomy_primary, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(dignified_death_autonomy_primary_su_t15, dignified_death__autonomy_primary, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(dignified_death_autonomy_primary_su_t20, dignified_death__autonomy_primary, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(dignified_death_autonomy_primary_su_t25, dignified_death__autonomy_primary, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(dignified_death_autonomy_primary_su_t30, dignified_death__autonomy_primary, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__autonomy_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(dignified_death__autonomy_primary, 0.08).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__relational_autonomy).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__sanctity_primary).

% DUAL FORMULATION NOTE:
% This story decomposes the 'dignified_death' kernel into three ε-invariant constraints per DP-001. The autonomy_primary reading instantiates a tangled_rope constraint (coordination + extraction). The relational_autonomy reading instantiates a scaffold or rope (transitional coordination with procedural safeguards). The sanctity_primary reading instantiates a snare (pure extraction from autonomy perspective, but mountain from its own view). All three share the kernel_id 'dignified_death' and are linked via network.affects_constraints. The ε values differ substantially: autonomy_primary ε≈0.52 (gatekeeping extraction), sanctity_primary ε≈0.85+ (any permission is extraction), relational_autonomy ε≈0.30 (coordination with moderate extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
