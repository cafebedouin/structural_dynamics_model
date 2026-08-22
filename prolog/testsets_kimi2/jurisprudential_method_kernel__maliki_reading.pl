% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__maliki_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__maliki_reading
 *   human_readable: Maliki Jurisprudential Method: Medinan Living Tradition as Normative Source
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   The Maliki reading of the Sunni jurisprudential method holds that law
 *   derives from the Qur'an and Hadith as filtered through the living
 *   tradition (amal) of the Medinan community. This reading instantiates a
 *   specific constraint within the broader jurisprudential_method_kernel,
 *   distinguished from Hanafi rationalism, Shafi'i textual hierarchy, and
 *   Hanbali literalism by its privileging of unwritten Medinan practice as a
 *   normative source. The constraint coordinates legal interpretation around
 *   a shared geographic and scholarly reference point while asymmetrically
 *   extracting legitimacy from non-Medinan interpretive claims.
 *
 * KEY AGENTS:
 *   - maliki_jurists: Primary agenda-setter (institutional/civilizational/identity_locked) â administer and transmit the tradition
 *   - medinan_scholarly_establishment: Primary beneficiary (institutional/civilizational/identity_locked) â collect epistemic privilege and institutional authority
 *   - non_medinan_jurists: Primary payer (powerful/civilizational/constrained) â bear cost of devalued interpretive authority
 *   - muslim_communities: Coordinated community (moderate/biographical/constrained) â receive stability, pay in displaced local norms
 *   - reformist_scholars: Excluded voice (moderate/generational/constrained) â would reject madhhab-bound methodology entirely
 *   - comparative_legal_historians: Analytical observer (institutional/generational/analytical) â study the tradition from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, 0.55).
domain_priors:suppression_score(jurisprudential_method_kernel__maliki_reading, 0.42).
domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__maliki_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Maliki Jurisprudential Method: Medinan Living Tradition as Normative Source").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "religious/legal/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, 'dcf72ad3-6e1f-4eb5-a935-48fcdbc0d6c4').
narrative_ontology:cs_kernel_codification('dcf72ad3-6e1f-4eb5-a935-48fcdbc0d6c4', fixed_text).
narrative_ontology:cs_authority_grounding('dcf72ad3-6e1f-4eb5-a935-48fcdbc0d6c4', lineage).
narrative_ontology:cs_interpretation_layer_present('dcf72ad3-6e1f-4eb5-a935-48fcdbc0d6c4').
narrative_ontology:cs_reading_relation('dcf72ad3-6e1f-4eb5-a935-48fcdbc0d6c4', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('dcf72ad3-6e1f-4eb5-a935-48fcdbc0d6c4', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('dcf72ad3-6e1f-4eb5-a935-48fcdbc0d6c4', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('dcf72ad3-6e1f-4eb5-a935-48fcdbc0d6c4', foundational, amal_ahl_al_madina_normative_authority).
narrative_ontology:cs_axiom_status(amal_ahl_al_madina_normative_authority, holdable).
narrative_ontology:cs_axiom_grounding('dcf72ad3-6e1f-4eb5-a935-48fcdbc0d6c4', amal_ahl_al_madina_normative_authority, conventional).
narrative_ontology:cs_axiom('dcf72ad3-6e1f-4eb5-a935-48fcdbc0d6c4', foundational, medinan_uniqueness_as_preservative).
narrative_ontology:cs_axiom_status(medinan_uniqueness_as_preservative, holdable).
narrative_ontology:cs_axiom_grounding('dcf72ad3-6e1f-4eb5-a935-48fcdbc0d6c4', medinan_uniqueness_as_preservative, empirically_contingent).
narrative_ontology:cs_reference_frame('dcf72ad3-6e1f-4eb5-a935-48fcdbc0d6c4', medinan_practice_as_prophetic_preservative).
narrative_ontology:cs_drift_state('dcf72ad3-6e1f-4eb5-a935-48fcdbc0d6c4', modernity_nation_state_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dcf72ad3-6e1f-4eb5-a935-48fcdbc0d6c4', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_establishment).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, non_medinan_jurists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, muslim_communities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, muslim_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and transmit the Maliki legal tradition through isnad-based instruction, fatwa, and judicial appointment. Their scholarly identity and institutional authority are constituted by mastery of Medinan practice; abandoning the madhhab would dissolve their legitimacy and career foundation.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, maliki_jurists, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Claims privileged epistemic access to authentic Prophetic practice by virtue of geographic and scholarly continuity with the Medinan community. Their interpretive judgments carry intrinsic weight in the Maliki hierarchy that non-Medinan judgments lack, consolidating authority, students, and endowed positions.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_establishment, beneficiary,
    institutional, civilizational, identity_locked, global).

% Jurists from Kufa, Baghdad, Cairo, and other centers whose regional practices and independent reasoning are structurally ranked below Medinan practice in the Maliki source hierarchy. They must either accept Medinan precedent as corrective or argue against it from within shared textual sources, with their own interpretive contributions permanently discounted.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, non_medinan_jurists, payer,
    powerful, civilizational, constrained, global).

% Receive coherent legal guidance and continuity from the Maliki tradition but may find local customary practices overridden by Medinan norms that do not account for regional conditions. Their ability to seek alternative rulings is bounded by the authority of the madhhab system and the social cost of departing from established scholarly guidance.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, muslim_communities, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, muslim_communities, payer).

% Advocate for direct Qur'anic interpretation or reason-based jurisprudence independent of madhhab-bound tradition. They are structurally excluded from the Maliki framework because their approach bypasses the living Medinan tradition that the school treats as epistemically foundational.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, reformist_scholars, excluded,
    moderate, generational, constrained, global).

% Study the Maliki school from outside its authority structure, comparing its claims about Medinan preservation against documentary and archaeological evidence. They do not participate in the tradition's internal legitimation but provide external analysis of its historical development.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, comparative_legal_historians, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_establishment).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Islamic legal interpretation across generations by grounding rulings in a continuous, lived tradition presumed to preserve the Prophet's practice, reducing doctrinal fragmentation and providing a stable reference point for novel cases.
% TRANSFER_FUNCTION: Transfers epistemic authority and interpretive legitimacy from non-Medinan jurists, regional schools, and independent reason to the Medinan scholarly lineage and its transmitted norms.
% ABSENT_VOICES: Non-Medinan jurists whose regional practices are systematically demoted, rationalist reformists who reject geographic privilege in favor of direct textual or rational methods, and modern critical historians who question the empirical basis of Medinan uniqueness.
% DISAPPEARANCE_RATIONALE: If the privileging of Medinan practice vanished, the Maliki school's distinctive methodology would collapse; legal authority would redistribute toward direct textualism, rational jurisprudence, or regional customary law, and the institutional prestige of the Medinan lineage would dissolve.
% FOUNDING_PROBLEM: The early Muslim community needed a reliable mechanism to distinguish authentic Prophetic practice from regional innovation and personal opinion after the Prophet's death; the continuous practice of the Medinan community was identified as the most faithful preserve.
% FOUNDING_PROBLEM_CORROBORATION: Traditional Maliki scholars attest the problem from within the benefiting lineage. Modern academic historians corroborate that legal diversity existed in early Islam, but contest that Medinan practice was uniquely preservative; reformist jurists outside the Medinan tradition argue the problem is better solved by direct scriptural engagement. No fully independent corroboration exists for the Medinan-privilege solution.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__maliki_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__maliki_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is medium because the constraint genuinely coordinates legal interpretation (providing continuity and reducing fragmentation) but also systematically privileges one geographic lineage's claims over others. Suppression (0.42) reflects moderate devaluation of non-Medinan opinions rather than violent silencing. Theater ratio (0.28) acknowledges that some maintenance of Medinan exceptionalism is performative, though much coordination is functionally real. Accessibility collapse (0.62) is significant because once one accepts the Maliki framework, non-Medinan sources lose standing as independent authorities. Resistance (0.38) comes from other schools and modern reformists. The temporal measurements show extraction peaking during classical consolidation (T=40) and declining in modernity as nation-states and Salafi movements erode madhhab authority, while theater rises as the tradition becomes increasingly performative under pressure.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Maliki jurists) experiences the constraint as preservation of authentic Prophetic practice and necessary coordination of the legal tradition. The payer seat (non-Medinan jurists) experiences the same structure as arbitrary geographic gatekeeping that devalues their interpretive contributions without textual refutation. The engine computes this divergence from the structural asymmetry in exit options (identity_locked for tradition-bound jurists versus constrained for rivals) and directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   The Medinan scholarly establishment is the declared beneficiary (low d, subsidized by the constraint's epistemic privilege). Non-Medinan jurists are declared victims (high d, extracted from via legitimacy denial). Muslim communities sit near symmetric: they benefit from coordinated legal stability but pay where Medinan norms displace local customary rulings. Maliki jurists as agenda-setters are structurally near beneficiaries because their authority is constituted by the constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling: a pure rope reading would ignore the asymmetric extraction from non-Medinan claims; a pure snare reading would ignore the genuine coordination function the Medinan reference point provides in stabilizing jurisprudence across centuries. The founding problem (post-Prophetic legal fragmentation) is contested: classical scholars attest it is live, while modern observers say the constraint now persists partly by institutional inertia. The founding_problem_status of contested signals this ambiguity without forcing false resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is this constraint one reading of the jurisprudential_method_kernel, where sibling readings (hanafi, shafii, hanbali) instantiate different methodological commitments from the same textual kernel?',
    'Comparative analysis of the four readings'' epsilon values and beneficiary/victim structures across the constraint family.',
    'If the readings are structurally independent, they should remain separate constraints; if they converge on identical metrics, the kernel decomposition may be over-fitted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Kernel reading position within the Sunni jurisprudential method family').

omega_variable(
    medinan_preservation_empirical_status,
    'Did the Medinan community actually preserve the Prophet''s practice with the unique fidelity claimed, or is this an idealized reconstruction?',
    'Historical-source analysis comparing Medinan legal practice to the contemporary hadith corpus and archaeological or epigraphic evidence from the seventh and eighth centuries.',
    'If the empirical claim is substantially false, the coordination function becomes a false-summit mountain: extraction from non-Medinan claims would be revealed as pure structural privilege rather than genuine preservation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medinan_preservation_empirical_status, empirical, 'Whether the Medinan preservation claim is historically accurate or constructed').

omega_variable(
    amal_as_coordination_vs_extraction,
    'Does the amal ahl al-Madina primarily coordinate legal interpretation across generations, or does it primarily extract legitimacy from competing regional schools?',
    'Measure the ratio of consensus-generating function to boundary-policing function in Maliki legal literature across the school''s history.',
    'A high coordination ratio would push the constraint toward rope; a high boundary-policing ratio would push it toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amal_as_coordination_vs_extraction, conceptual, 'Coordination versus extraction function of Medinan living tradition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__maliki_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(juri_tr_t20, jurisprudential_method_kernel__maliki_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__maliki_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(juri_tr_t60, jurisprudential_method_kernel__maliki_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(juri_tr_t80, jurisprudential_method_kernel__maliki_reading, theater_ratio, 80, 0.35).
narrative_ontology:measurement(juri_tr_t100, jurisprudential_method_kernel__maliki_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(juri_be_t20, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(juri_be_t60, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 60, 0.54).
narrative_ontology:measurement(juri_be_t80, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 80, 0.48).
narrative_ontology:measurement(juri_be_t100, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(juri_su_t20, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(juri_su_t60, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 60, 0.36).
narrative_ontology:measurement(juri_su_t80, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 80, 0.3).
narrative_ontology:measurement(juri_su_t100, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 100, 0.24).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% The jurisprudential_method_kernel decomposes into four structurally distinct readings (hanafi, maliki, shafii, hanbali) because each reading assigns a different epsilon to custom/practice, names different beneficiaries and victims, and employs different enforcement mechanisms. The epsilon-invariance principle requires separate stories; the label 'Islamic jurisprudence' conflates four distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
