% ============================================================================
% CONSTRAINT STORY: dignified_death__relational_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__relational_autonomy, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: dignified_death__relational_autonomy
 *   human_readable: Relational Autonomy in Dignified Death Decision-Making
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the relational_autonomy reading of the
 *   dignified_death kernel. It models end-of-life decision-making as a
 *   coordination problem best solved by distributing authority across a
 *   patient-family-clinician triad backed by procedural safeguards. Dignity
 *   is understood to emerge from relational context rather than isolated
 *   individual choice or transcendent moral law. The reading is contested by
 *   autonomy_primary (sole patient sovereignty) and sanctity_primary (life's
 *   intrinsic value prohibits intentional termination) readings.
 *
 * KEY AGENTS:
 *   - Bioethics institutions: Primary agenda-setter (institutional/constrained) â design and maintain the procedural framework
 *   - Patients in supported networks: Primary beneficiary (moderate/constrained) â gain safeguards but share authority
 *   - Isolated patients: Primary target (powerless/trapped) â bear the structural cost of triad requirements without relational support
 *   - Unilateral autonomy seekers: Secondary target (moderate/constrained) â experience loss of sole decision-making authority
 *   - Autonomy and sanctity advocates: Excluded observers (organized/analytical) â would object but are outside institutional policy-making
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__relational_autonomy, 0.38).
domain_priors:suppression_score(dignified_death__relational_autonomy, 0.28).
domain_priors:theater_ratio(dignified_death__relational_autonomy, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, extractiveness, 0.38).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__relational_autonomy, rope).
narrative_ontology:human_readable(dignified_death__relational_autonomy, "Relational Autonomy in Dignified Death Decision-Making").
narrative_ontology:topic_domain(dignified_death__relational_autonomy, "bioethics/medical_law/political_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, 'c1fb5050-4842-4eb9-bf3d-bf955d3a0fe7').
narrative_ontology:cs_kernel_codification('c1fb5050-4842-4eb9-bf3d-bf955d3a0fe7', formalized).
narrative_ontology:cs_authority_grounding('c1fb5050-4842-4eb9-bf3d-bf955d3a0fe7', expertise).
narrative_ontology:cs_interpretation_layer_present('c1fb5050-4842-4eb9-bf3d-bf955d3a0fe7').
narrative_ontology:cs_reading_relation('c1fb5050-4842-4eb9-bf3d-bf955d3a0fe7', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('c1fb5050-4842-4eb9-bf3d-bf955d3a0fe7', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('c1fb5050-4842-4eb9-bf3d-bf955d3a0fe7', foundational, dignity_emerges_from_relational_context).
narrative_ontology:cs_axiom_status(dignity_emerges_from_relational_context, holdable).
narrative_ontology:cs_axiom_grounding('c1fb5050-4842-4eb9-bf3d-bf955d3a0fe7', dignity_emerges_from_relational_context, deontological).
narrative_ontology:cs_axiom('c1fb5050-4842-4eb9-bf3d-bf955d3a0fe7', foundational, decision_authority_requires_triadic_distribution).
narrative_ontology:cs_axiom_status(decision_authority_requires_triadic_distribution, holdable).
narrative_ontology:cs_axiom_grounding('c1fb5050-4842-4eb9-bf3d-bf955d3a0fe7', decision_authority_requires_triadic_distribution, conventional).
narrative_ontology:cs_reference_frame('c1fb5050-4842-4eb9-bf3d-bf955d3a0fe7', relational_dignity_triad).
narrative_ontology:cs_drift_state('c1fb5050-4842-4eb9-bf3d-bf955d3a0fe7', contemporary_bioethical_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c1fb5050-4842-4eb9-bf3d-bf955d3a0fe7', '').
narrative_ontology:cs_kernel_id(dignified_death__relational_autonomy, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, patients_in_supported_networks).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, families_and_surrogates).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, treating_clinicians).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, isolated_patients).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, unilateral_autonomy_seekers).
narrative_ontology:constraint_vindicates(dignified_death__relational_autonomy, procedural_safeguards_efficacy).
narrative_ontology:constraint_vindicates(dignified_death__relational_autonomy, relational_dignity_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop clinical guidelines and accreditation standards that require patients, families, and clinicians to jointly deliberate end-of-life decisions through specified procedural steps. They train clinicians, review compliance, and update protocols in response to case law and emerging norms.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, bioethics_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Face terminal or unbearable conditions and enter a structured process where their preferences are heard alongside family input and clinical judgment. They receive procedural protections against coercion but cannot unilaterally demand or refuse interventions without triadic agreement.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, patients_in_supported_networks, beneficiary,
    moderate, biographical, constrained, national).

% Are formally included in end-of-life deliberations as legitimate holders of interest and perspective, rather than being sidelined or given unchecked veto power. They participate in structured meetings with clinicians and the patient.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, families_and_surrogates, beneficiary,
    moderate, biographical, constrained, national).

% Provide prognostic information and mediation within a protocol that assigns them shared authority rather than sole decision-making power or mere technical service. They document triadic consensus and follow procedural checklists that limit individual liability.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, treating_clinicians, beneficiary,
    moderate, biographical, constrained, national).

% Lack available family or supportive surrogates. They must still complete triadic deliberation, often with court-appointed or institutional proxies substituted for family, which can delay decisions and subject their private preferences to strangers.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, isolated_patients, payer,
    powerless, immediate, trapped, national).

% Wish to determine the timing and manner of their own death without family involvement or clinical gatekeeping. The framework requires them to incorporate family and clinician perspectives, which they experience as an infringement on self-determination.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, unilateral_autonomy_seekers, payer,
    moderate, biographical, constrained, national).

% Argue that sole patient sovereignty is the only legitimate basis for end-of-life decisions. They publish critiques of relational models as covert paternalism and campaign for policies permitting unilateral patient-initiated death.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, autonomy_advocates, excluded,
    organized, biographical, analytical, national).

% Hold that intentional life-termination is morally impermissible regardless of consent or relational context. They oppose the permissive framing of dignified death and the institutional normalization of assisted dying.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, sanctity_advocates, excluded,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__relational_autonomy, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves coordination failures among patients, families, and clinicians in high-stakes end-of-life decisions by distributing authority and establishing procedural order, preventing both unilateral abandonment and paternalistic exclusion.
% TRANSFER_FUNCTION: Moves decision-making authority from the patient-as-sole-sovereign to a triadic network, and transfers institutional credibility from biomedical ethics bodies to the bedside deliberation process.
% ABSENT_VOICES: Pure autonomy advocates who regard any family or clinical gatekeeping as illegitimate paternalism; sanctity-based opponents who reject the permissive framing of dignified death entirely; and isolated patients who lack the relational infrastructure the model assumes but are still subjected to its procedural requirements.
% DISAPPEARANCE_RATIONALE: If the relational triad framework vanished, end-of-life decisions would revert to either pure patient demand, unchecked medical paternalism, or jurisdictional prohibition; the current balance of authority among the three parties would collapse and the procedural safeguards would cease to operate.
% FOUNDING_PROBLEM: End-of-life decisions historically suffered from both paternalistic exclusion of patients and abandonment of patients to unsupported autonomous choice; families were either ignored or given unchecked veto power; clinicians lacked clear procedural guidance.
% FOUNDING_PROBLEM_CORROBORATION: Palliative medicine researchers and elder-abuse watchdog organizations independently attest that unsupported autonomy and unchecked paternalism both remain active harms; patient advocacy organizations outside the bioethics establishment corroborate that the founding problem is unresolved.
narrative_ontology:disappearance_verdict(dignified_death__relational_autonomy, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__relational_autonomy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__relational_autonomy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignified_death__relational_autonomy, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__relational_autonomy, 0.38, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__relational_autonomy_tests).
:- end_tests(dignified_death__relational_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the triad requirement imposes genuine procedural overhead and coerces unilateral autonomy seekers and isolated patients into a deliberative structure they did not choose. Suppression is low-moderate (0.28): the constraint is enforced through professional accreditation and institutional norms rather than overt coercion, but it does suppress pure unilateral choice within the jurisdiction. Theater ratio is modest (0.22): most procedural activity is functional, though some performative compliance exists. Accessibility collapse (0.42) reflects that alternatives (pure autonomy or sanctity-based prohibition) remain visible in other jurisdictions and discourses, but are locally inaccessible once the framework is adopted. Resistance (0.32) comes from organized autonomy and sanctity advocacy, but is channeled through policy debate rather than widespread non-compliance.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (bioethics institutions) experiences the constraint as necessary coordination that solves a genuine collective-action problem; the payer seats (isolated patients, unilateral autonomy seekers) experience the same structure as an imposed procedural burden that extracts their decisional autonomy. The engine computes this divergence from structural data â the beneficiary-payer asymmetry and the trapped exit of isolated patients â without relying on the claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   Bioethics institutions derive low directionality (d near 0.0) as the architects and administrators of the constraint. Patients in supported networks, families, and clinicians derive low-moderate directionality as net beneficiaries of coordination and safeguards. Isolated patients and unilateral autonomy seekers derive high directionality (d near 1.0) because the constraint structurally extracts their capacity for unilateral choice and imposes relational deliberation. Excluded advocates sit near analytical (d ~0.5) because they are neither subsidized nor extracted by the constraint's operation, though they contest its legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework avoids mandatrophy because its founding problem â preventing both paternalistic exclusion and unsupported autonomous abandonment â remains live, and the procedural safeguards continue to function. It is not a piton because the theater ratio is low and the coordination function is active, not atrophied. It is not a snare because suppression is relatively low and the beneficiary set is broad rather than a narrow capturer. The moderate Îµ reflects high procedural overhead rather than concentrated rent extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_overhead_or_paternalism,
    'Does the triadic deliberation requirement with procedural safeguards represent necessary coordination cost, or does it function as institutionalized paternalism that extracts decisional autonomy from patients?',
    'Comparative outcome studies across jurisdictions with varying procedural requirements, measuring patient-reported dignity, decisional regret, and perceived coercion.',
    'If the procedural overhead extracts autonomy without improving outcomes, effective extractiveness rises and the rope classification weakens toward tangled_rope; if outcomes improve, the overhead is coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_overhead_or_paternalism, empirical, 'Whether procedural overhead is coordination cost or disguised paternalism').

omega_variable(
    isolated_patient_exclusion,
    'Do patients lacking supportive family networks suffer net harm under the triad requirement, or do institutional surrogates adequately compensate?',
    'Empirical studies of isolated patients in jurisdictions with mandatory triadic deliberation versus jurisdictions permitting unilateral patient choice.',
    'If isolated patients systematically experience delay, distress, or autonomy loss, the victim set is validated and directionality for the powerless atom rises; if surrogates compensate effectively, victim status is attenuated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(isolated_patient_exclusion, empirical, 'Empirical status of isolated patients under relational autonomy frameworks').

omega_variable(
    kernel_sibling_foreclosure,
    'Does the relational_autonomy reading of dignified death logically foreclose the autonomy_primary or sanctity_primary readings, or do all three remain co-tenable within a pluralist bioethical framework?',
    'Comparative analysis of whether a single institutional policy can accommodate all three readings, or whether adopting relational_autonomy as policy excludes the core premises of its siblings.',
    'If foreclosure is established, the constraint family should include forecloses relations and engine-computed foreclosure; if coexistence holds, the current coexists_with relations are confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sibling_foreclosure, conceptual, 'Structural relationship between sibling kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__relational_autonomy, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dd_rel_auto_tr_t0, dignified_death__relational_autonomy, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dd_rel_auto_tr_t5, dignified_death__relational_autonomy, theater_ratio, 5, 0.12).
narrative_ontology:measurement(dd_rel_auto_tr_t10, dignified_death__relational_autonomy, theater_ratio, 10, 0.15).
narrative_ontology:measurement(dd_rel_auto_tr_t15, dignified_death__relational_autonomy, theater_ratio, 15, 0.17).
narrative_ontology:measurement(dd_rel_auto_tr_t20, dignified_death__relational_autonomy, theater_ratio, 20, 0.19).
narrative_ontology:measurement(dd_rel_auto_tr_t25, dignified_death__relational_autonomy, theater_ratio, 25, 0.21).
narrative_ontology:measurement(dd_rel_auto_tr_t30, dignified_death__relational_autonomy, theater_ratio, 30, 0.22).

% Extraction over time
narrative_ontology:measurement(dd_rel_auto_be_t0, dignified_death__relational_autonomy, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(dd_rel_auto_be_t5, dignified_death__relational_autonomy, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(dd_rel_auto_be_t10, dignified_death__relational_autonomy, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(dd_rel_auto_be_t15, dignified_death__relational_autonomy, base_extractiveness, 15, 0.34).
narrative_ontology:measurement(dd_rel_auto_be_t20, dignified_death__relational_autonomy, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(dd_rel_auto_be_t25, dignified_death__relational_autonomy, base_extractiveness, 25, 0.37).
narrative_ontology:measurement(dd_rel_auto_be_t30, dignified_death__relational_autonomy, base_extractiveness, 30, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dignified_death__relational_autonomy, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__sanctity_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the dignified_death kernel, decomposed per the Îµ-invariance principle. The relational_autonomy reading coordinates end-of-life authority through triadic distribution; the autonomy_primary reading locates authority in sole patient self-determination; the sanctity_primary reading locates dignity in life's intrinsic value and rejects intentional termination. Each reading carries a distinct Îµ and stakeholder structure; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
