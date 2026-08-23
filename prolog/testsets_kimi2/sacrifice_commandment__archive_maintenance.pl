% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__archive_maintenance, []).

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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Study of Sacrificial Law as Archive Maintenance for Future Temple
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   In Rabbinic Judaism following the destruction of the Second Temple, the
 *   biblical commandment of animal sacrifice became inoperable. Among the
 *   range of halakhic responses, the archive_maintenance reading holds that
 *   intensive study of sacrificial lawâits technical procedures, spatial
 *   layout, and temporal rhythmsâfunctions as a preservation mechanism.
 *   This is not worship in the present tense, nor is the commandment
 *   suspended; rather, study maintains a procedural archive for the messianic
 *   era when the Temple will be restored. The constraint coordinates present
 *   scholarly labor across generations to prevent halakhic discontinuity,
 *   while extracting cognitive and educational resources from present actors
 *   for the benefit of a future community that does not yet exist.
 *
 * KEY AGENTS:
 *   - Rabbinic institutions (institutional/constrained): Set the curriculum and enforce the norm that sacrificial law remains a live study topic; their authority derives from lineage and textual expertise.
 *   - Present scholars and students (moderate/identity_locked): Bear the opportunity cost of mastering voluminous, practically inapplicable material; their exit is constrained by identity fusion with the Torah-study role.
 *   - Future Temple community (powerless/trapped): The intended beneficiary of the preserved knowledge, structurally unable to influence the present archive or opt out of receiving it.
 *   - Messianic activist groups (powerful/constrained): Excluded from the halakhic consensus; they argue for physical preparation over textual preservation and would redirect resources toward literal Temple reconstruction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, 0.45).
domain_priors:suppression_score(sacrifice_commandment__archive_maintenance, 0.35).
domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, extractiveness, 0.45).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, scaffold).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Study of Sacrificial Law as Archive Maintenance for Future Temple").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious/halakhic/commitment_system").

domain_priors:requires_active_enforcement(sacrifice_commandment__archive_maintenance).
narrative_ontology:has_sunset_clause(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, 'c99e3ab0-0ed1-44b5-8487-03ca4db76065').
narrative_ontology:cs_kernel_codification('c99e3ab0-0ed1-44b5-8487-03ca4db76065', fixed_text).
narrative_ontology:cs_authority_grounding('c99e3ab0-0ed1-44b5-8487-03ca4db76065', lineage).
narrative_ontology:cs_interpretation_layer_present('c99e3ab0-0ed1-44b5-8487-03ca4db76065').
narrative_ontology:cs_reading_relation('c99e3ab0-0ed1-44b5-8487-03ca4db76065', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('c99e3ab0-0ed1-44b5-8487-03ca4db76065', sacrifice_commandment__study_as_performance, influences).
narrative_ontology:cs_axiom('c99e3ab0-0ed1-44b5-8487-03ca4db76065', foundational, sacrificial_knowledge_requires_active_preservation).
narrative_ontology:cs_axiom_status(sacrificial_knowledge_requires_active_preservation, holdable).
narrative_ontology:cs_axiom_grounding('c99e3ab0-0ed1-44b5-8487-03ca4db76065', sacrificial_knowledge_requires_active_preservation, deontological).
narrative_ontology:cs_axiom('c99e3ab0-0ed1-44b5-8487-03ca4db76065', foundational, study_without_temple_is_preparatory_not_constitutive).
narrative_ontology:cs_axiom_status(study_without_temple_is_preparatory_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('c99e3ab0-0ed1-44b5-8487-03ca4db76065', study_without_temple_is_preparatory_not_constitutive, deontological).
narrative_ontology:cs_reference_frame('c99e3ab0-0ed1-44b5-8487-03ca4db76065', temple_centric_torah_observance).
narrative_ontology:cs_drift_state('c99e3ab0-0ed1-44b5-8487-03ca4db76065', post_second_destruction_exile, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c99e3ab0-0ed1-44b5-8487-03ca4db76065', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_temple_community).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, present_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the curriculum for Torah study across yeshivot and enforce the norm that sacrificial law (korbanot) remains a live topic of Talmudic study. They derive institutional legitimacy from maintaining the full corpus of halakha without truncation. Their authority is grounded in lineage and textual expertise, and they cannot abandon this corpus without undermining their claim to comprehensive halakhic stewardship.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, rabbinic_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Invest years of cognitive labor mastering complex sacrificial proceduresâanimal species, altar geography, priestly choreography, blood disposalâthat have no present practical application. The cost is opportunity cost of other Torah study or economic activity. For those in the yeshiva system, abandoning this corpus is experienced as abandoning their identity as Torah scholars.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, present_scholars, payer,
    moderate, biographical, identity_locked, global).

% The intended recipient of the preserved archive: a future community of priests and worshippers in a restored Temple who would require accurate procedural knowledge to resume sacrifice. They have no voice in the present curricular decisions and cannot opt out of inheriting whatever archive the present generation produces.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, future_temple_community, beneficiary,
    powerless, civilizational, trapped, global).

% Advocate for literal Temple preparationâphysical construction, priestly garments, red heifer procurementârather than textual preservation alone. They are structurally excluded from mainstream halakhic discourse because their agenda implies that study is insufficient, which threatens the archive_maintenance justification for current curricular priorities.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, temple_activist_groups, excluded,
    powerful, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__archive_maintenance, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves technical halakhic knowledge across generational gaps so that priestly sacrifice can resume immediately upon restoration of the Temple, without loss of procedural continuity.
% TRANSFER_FUNCTION: Moves cognitive labor, educational resources, and curricular priority from present scholars and students to an archived body of knowledge intended for future priestly practitioners.
% ABSENT_VOICES: Messianic activist groups who argue for practical Temple preparation over textual study; secular critics who question allocating educational resources to obsolete ritual technology; Christian or Muslim observers who contest the theological premise of Third Temple restoration.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, yeshiva curricula would shift away from sacrificial law, the specialized knowledge would degrade within a generation, and a future Temple restoration would face a halakhic continuity gap. The present scholarly economy would reallocate toward currently applicable law.
% FOUNDING_PROBLEM: The destruction of the Second Temple created a rupture in sacrificial practice; without a mechanism to preserve procedural knowledge, the commandment would become impossible to fulfill upon restoration.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary halakhic authorities across Orthodox denominations attest to the live necessity of preserving Temple-related law. However, no non-Jewish or secular source corroborates the theological premise that the Temple will be restored or that this specific preservation mechanism is necessary; the corroboration is entirely internal to the benefiting tradition.
narrative_ontology:disappearance_verdict(sacrifice_commandment__archive_maintenance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__archive_maintenance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_commandment__archive_maintenance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__archive_maintenance, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__archive_maintenance_tests).
:- end_tests(sacrifice_commandment__archive_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the coordination has genuine future value but the transfer is heavily time-shifted and the present return is nil. Suppression is moderate-low (0.35) because while rabbinic institutions enforce the curriculum, alternative readings (performance-only, study-as-worship) persist and are tolerated within the broader discourse. Theater ratio is low-moderate (0.25): the study is substantively technical and not primarily performative, though some curricular emphasis may serve institutional prestige rather than genuine preservation. Accessibility collapse is moderate (0.4) because the other two kernel readings remain live alternatives. Resistance is moderate (0.3) from students who question the relevance and from competing theological movements.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (rabbinic institutions) experiences the constraint as a successful continuity mechanism and a fulfillment of religious duty. The payer seat (present scholars) experiences it as a burdensome curricular requirement with no present application. The beneficiary seat (future community) is not present to experience anything, which is the core structural asymmetry: the extraction is validated by a beneficiary that cannot corroborate its own need. The engine should compute a wide divergence between the institutional seat (low d, coordination) and the present scholar seat (high d, extraction), with the future seat receiving subsidy (negative chi) despite its powerlessness because it is declared a beneficiary.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic institutions are agenda-setters with constrained exit (their authority is constituted by this tradition) but they are not the primary beneficiaries of the extraction; they administer the transfer. Present scholars are the structural targets: they bear the costs (time, opportunity) and have identity-locked exit. Future community is the beneficiary: it receives the preserved knowledge subsidy. The messianic activists are excluded but their exclusion is structural (they challenge the archive logic itself).
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification prevents mislabeling this as either pure coordination (Rope) or pure extraction (Snare). It is genuinely transitional: if the Temple were restored tomorrow, the archive_maintenance function would become obsolete and study would shift to operational training. However, because the transition has not occurred for two millennia, the scaffold risks piton degradation (theater rising, extraction accumulating). The temporal measurements show slow extraction growth over centuries, consistent with a scaffold whose transition horizon keeps receding. The mandatrophy is not yet resolved because the founding problem (Temple absence) is still live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the archive_maintenance reading capture the true structural function of sacrifice study, or does it rationalize a practice whose present utility is primarily institutional reproduction?',
    'Comparative analysis across the three kernel readings (archive_maintenance, study_as_performance, performance_only) to see which reading''s structural predictions best fit curricular time allocation, funding flows, and stated rabbinic justifications.',
    'If the reading is primarily rationalization, extractiveness is higher than the coordination framing suggests, and the scaffold classification may be a cover for tangled_rope or piton dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the archive maintenance reading is structurally descriptive or doctrinally prescriptive cover.').

omega_variable(
    future_utility_uncertainty,
    'Will the preserved technical knowledge actually be operative in a future Temple scenario, or will restoration involve revealed or superseding law that makes the present archive obsolete?',
    'Theological debate and analysis of messianic-era halakha traditions; observation of whether rabbinic sources expect radical legal change in the messianic era.',
    'If the archived knowledge is expected to be superseded, the present extraction is unsubsidized by genuine future coordination value, pushing the constraint toward snare or piton classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_utility_uncertainty, conceptual, 'Whether preserved knowledge retains operative value in a restored Temple.').

omega_variable(
    enforcement_mechanism_nature,
    'Is the maintenance of sacrificial study enforced by institutional curriculum control, or by identity fusion where scholars cannot imagine abandoning this corpus?',
    'Observe curricular elasticity in institutions with and without state funding; observe whether students exposed to alternative curricula abandon sacrificial study.',
    'Identity-locked enforcement implies higher effective extraction for present scholars and different coalition dynamics than institutional enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_nature, empirical, 'Structural versus internalized enforcement of the study norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scam_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.1).
narrative_ontology:measurement(scam_tr_t400, sacrifice_commandment__archive_maintenance, theater_ratio, 400, 0.15).
narrative_ontology:measurement(scam_tr_t800, sacrifice_commandment__archive_maintenance, theater_ratio, 800, 0.2).
narrative_ontology:measurement(scam_tr_t1200, sacrifice_commandment__archive_maintenance, theater_ratio, 1200, 0.22).
narrative_ontology:measurement(scam_tr_t1600, sacrifice_commandment__archive_maintenance, theater_ratio, 1600, 0.24).
narrative_ontology:measurement(scam_tr_t2000, sacrifice_commandment__archive_maintenance, theater_ratio, 2000, 0.25).

% Extraction over time
narrative_ontology:measurement(scam_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(scam_be_t400, sacrifice_commandment__archive_maintenance, base_extractiveness, 400, 0.25).
narrative_ontology:measurement(scam_be_t800, sacrifice_commandment__archive_maintenance, base_extractiveness, 800, 0.32).
narrative_ontology:measurement(scam_be_t1200, sacrifice_commandment__archive_maintenance, base_extractiveness, 1200, 0.38).
narrative_ontology:measurement(scam_be_t1600, sacrifice_commandment__archive_maintenance, base_extractiveness, 1600, 0.42).
narrative_ontology:measurement(scam_be_t2000, sacrifice_commandment__archive_maintenance, base_extractiveness, 2000, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(scam_su_t0, sacrifice_commandment__archive_maintenance, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(scam_su_t400, sacrifice_commandment__archive_maintenance, suppression_requirement, 400, 0.18).
narrative_ontology:measurement(scam_su_t800, sacrifice_commandment__archive_maintenance, suppression_requirement, 800, 0.25).
narrative_ontology:measurement(scam_su_t1200, sacrifice_commandment__archive_maintenance, suppression_requirement, 1200, 0.3).
narrative_ontology:measurement(scam_su_t1600, sacrifice_commandment__archive_maintenance, suppression_requirement, 1600, 0.33).
narrative_ontology:measurement(scam_su_t2000, sacrifice_commandment__archive_maintenance, suppression_requirement, 2000, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__study_as_performance).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the sacrifice_commandment kernel, decomposed per the Îµ-invariance principle because each reading (archive_maintenance, study_as_performance, performance_only) carries a distinct Îµ, beneficiary structure, and justification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
