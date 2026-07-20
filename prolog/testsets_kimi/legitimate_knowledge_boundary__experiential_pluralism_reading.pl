% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__experiential_pluralism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__experiential_pluralism_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: legitimate_knowledge_boundary__experiential_pluralism_reading
 *   human_readable: Experiential Pluralism Reading of Legitimate Knowledge Boundary
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the experiential pluralism reading of the
 *   legitimate knowledge boundary kernel. It treats lived experience and
 *   community validation as primary sources of epistemic legitimacy,
 *   relegating methodological standards to one tool among many. The
 *   constraint coordinates by lowering barriers to knowledge production for
 *   marginalized communities, but simultaneously extracts epistemic authority
 *   from credentialed experts and methodological institutions, redistributing
 *   it to community gatekeepers. It is actively enforced through discourse
 *   norms that police credentialist 'imperialism' and require experiential
 *   framing in policy and research contexts. As a kernel reading, it is clean
 *   and epsilon-invariant: it does not describe the credentialed or hybrid
 *   readings, which are separate constraints.
 *
 * KEY AGENTS:
 *   - situated_communities: Primary beneficiary (organized/mobile) â gain epistemic standing without credentialing.
 *   - community_gatekeepers: Agenda setter (organized/mobile) â administers validation norms and boundary policing.
 *   - credentialed_experts: Primary payer (institutional/constrained) â methodological authority downgraded.
 *   - empirical_researchers: Payer (moderate/constrained) â systematic empirical work deprioritized.
 *   - formal_institutions: Payer (institutional/constrained) â must dilute gatekeeping to accommodate community endorsement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.62).
domain_priors:suppression_score(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.58).
domain_priors:theater_ratio(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__experiential_pluralism_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__experiential_pluralism_reading, "Experiential Pluralism Reading of Legitimate Knowledge Boundary").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__experiential_pluralism_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__experiential_pluralism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__experiential_pluralism_reading, '2eb5c21c-574c-4f01-802f-3a84468f7500').
narrative_ontology:cs_kernel_codification('2eb5c21c-574c-4f01-802f-3a84468f7500', distributed).
narrative_ontology:cs_authority_grounding('2eb5c21c-574c-4f01-802f-3a84468f7500', distributed).
narrative_ontology:cs_reading_relation('2eb5c21c-574c-4f01-802f-3a84468f7500', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('2eb5c21c-574c-4f01-802f-3a84468f7500', legitimate_knowledge_boundary__hybrid_coproduction_reading, coexists_with).
narrative_ontology:cs_axiom('2eb5c21c-574c-4f01-802f-3a84468f7500', foundational, lived_experience_as_epistemic_foundation).
narrative_ontology:cs_axiom_status(lived_experience_as_epistemic_foundation, holdable).
narrative_ontology:cs_axiom_grounding('2eb5c21c-574c-4f01-802f-3a84468f7500', lived_experience_as_epistemic_foundation, deontological).
narrative_ontology:cs_axiom('2eb5c21c-574c-4f01-802f-3a84468f7500', foundational, methodology_as_subordinate_tool).
narrative_ontology:cs_axiom_status(methodology_as_subordinate_tool, holdable).
narrative_ontology:cs_axiom_grounding('2eb5c21c-574c-4f01-802f-3a84468f7500', methodology_as_subordinate_tool, conventional).
narrative_ontology:cs_reference_frame('2eb5c21c-574c-4f01-802f-3a84468f7500', situated_knowledge_framework).
narrative_ontology:cs_drift_state('2eb5c21c-574c-4f01-802f-3a84468f7500', contemporary_academic_politics, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2eb5c21c-574c-4f01-802f-3a84468f7500', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, situated_communities).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, community_gatekeepers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, empirical_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, formal_institutions).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, standpoint_theory).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, situated_knowledge_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their lived experience is treated as a primary and sufficient source of legitimate knowledge; they gain epistemic standing and policy voice without needing formal credentialing or methodological training.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, situated_communities, beneficiary,
    organized, biographical, mobile, regional).

% Administer the norms of community validation, determining whose testimony counts as authoritative, adjudicating conflicts between experiential accounts, and policing boundaries against credentialist or methodological incursions.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, community_gatekeepers, agenda_setter,
    organized, biographical, mobile, regional).

% Hold advanced methodological training and peer-reviewed research programs; their authority is systematically downgraded relative to experiential narrative, forcing them to recast expertise as merely context-specific or lose institutional standing.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts, payer,
    institutional, biographical, constrained, global).

% Conduct systematic empirical inquiry; find their work deprioritized in funding and policy when it conflicts with community-validated narratives or fails to incorporate experiential framing.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, empirical_researchers, payer,
    moderate, biographical, constrained, national).

% Universities, funding bodies, and policy agencies must redesign validation protocols to include community endorsement, diluting methodological gatekeeping and redirecting resources toward community-validated projects.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, formal_institutions, payer,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes epistemic authority to communities historically excluded by credentialing gatekeeping, enabling knowledge production and policy voice without requiring methodological training or institutional membership.
% TRANSFER_FUNCTION: Moves epistemic authority, legitimacy, and resources from credentialed methodological institutions to situated communities and their designated validators; moves accountability from formal peer review to communal endorsement.
% ABSENT_VOICES: Methodological purists and safety-critical researchers who would argue that experiential primacy disables falsification, generalizability, and error correction in domains like medicine, engineering, and climate modeling are structurally marginalized as credentialist or colonial.
% DISAPPEARANCE_RATIONALE: Without this constraint, epistemic legitimacy would recentralize around methodological credentialing; situated communities would lose their current policy voice and institutional standing, and knowledge-validation pipelines would revert to peer-review gatekeeping.
% FOUNDING_PROBLEM: Credentialist epistemology systematically excluded marginalized communities by treating methodological form and institutional affiliation as necessary conditions for legitimate knowledge, silencing voices that lacked access to expensive training and review infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Feminist epistemologists and critical race scholars from outside the benefiting parties attest the historical exclusion. Medical safety researchers and statistical methodologists from outside the benefiting parties attest that the founding problem has been substantially addressed in formal institutions and that the current arrangement overshoots into anti-empiricism.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__experiential_pluralism_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__experiential_pluralism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__experiential_pluralism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial transfer of epistemic authority from credentialed institutions to community validators. Suppression (0.58) captures the active enforcement needed to maintain experiential primacy against resurgent credentialism and methodological critique. Theater ratio (0.45) registers the performative dimension of community validation rituals and boundary-policing discourse, which increasingly substitutes for genuine epistemic adjudication. Accessibility collapse (0.48) is moderate: methodological alternatives are devalued but not eliminated. Resistance (0.55) is significant because credentialed institutions and safety-critical fields actively contest the constraint. Measurements show a steady accumulation of extraction and enforcement from T0 to T25 as the reading has been institutionalized in policy and academic contexts.
 *
 * PERSPECTIVAL GAP:
 *   The situated_communities seat experiences this constraint as liberation from credentialist exclusion â a genuine coordination gain. The credentialed_experts and formal_institutions seats experience it as extraction that disables their error-correction and safety-assessment functions. The engine will compute these seats differently: beneficiaries with mobile exit will show low directionality and damped effective extraction; constrained institutional payers will show high directionality and amplified extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (situated_communities) receive epistemic subsidy â their knowledge claims are validated without methodological cost, yielding low directionality. Community_gatekeepers occupy a dual position: they administer the constraint and collect status from it, but their authority depends on the constraint's persistence, placing them nearer symmetric. Payers (credentialed_experts, empirical_researchers, formal_institutions) bear the extraction directly: their accumulated capital in methodology and peer review is devalued, and their exit is constrained by institutional lock-in and professional identity, yielding high directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by preserving the coordination function in its founding problem: credentialism genuinely excluded marginalized voices. However, the metrics show that the arrangement has accumulated extraction beyond the solving of that problem â it now suppresses legitimate methodological critique and enforces experiential framing even in safety-critical domains where the founding exclusion is not the active problem. The T17 abductive trigger would fire if base_extractiveness continues rising, signaling potential snare drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    methodology_suppression_mechanism,
    'Is the devaluation of methodological rigor under this reading a structural feature or an accidental overreach by specific communities?',
    'Cross-community comparison: in communities with strong experiential norms but safety-critical needs (public health, engineering), does methodological exclusion lead to predictable harm?',
    'If structural, the reading is more extractive than coordinating; if accidental, targeted reform could preserve coordination while reducing harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodology_suppression_mechanism, empirical, 'Whether methodological suppression is inherent or incidental.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the experiential pluralism reading logically foreclose the credentialed expertise reading, or do they function as alternative framings in a permanently contested space?',
    'Analysis of institutional adoption: can a single policy framework simultaneously treat lived experience as primary and methodological rigor as necessary without contradiction?',
    'If foreclosure is real, the kernel is zero-sum; if coexistence is possible, the dispute is resolvable through the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between experiential and credentialed readings.').

omega_variable(
    community_gatekeeper_capture,
    'Does distributed community validation avoid capture better than credentialing, or do new community gatekeepers simply replace old ones?',
    'Measure concentration of validation authority within ''distributed'' communities: who decides which experiences count?',
    'If validation authority reconcentrates, the coordination function is partially illusory and extraction shifts to new gatekeepers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_gatekeeper_capture, empirical, 'Whether distributed validation avoids gatekeeper capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__experiential_pluralism_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lkb_epl_tr_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(lkb_epl_tr_t5, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(lkb_epl_tr_t10, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(lkb_epl_tr_t15, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(lkb_epl_tr_t20, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(lkb_epl_tr_t25, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(lkb_epl_be_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(lkb_epl_be_t5, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(lkb_epl_be_t10, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(lkb_epl_be_t15, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(lkb_epl_be_t20, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(lkb_epl_be_t25, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(lkb_epl_su_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(lkb_epl_su_t5, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(lkb_epl_su_t10, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(lkb_epl_su_t15, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(lkb_epl_su_t20, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(lkb_epl_su_t25, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% The natural-language concept 'legitimate knowledge boundary' decomposes into three structurally distinct constraints: the credentialed_expertise_reading (high methodological extraction from experiential knowers), the experiential_pluralism_reading (high experiential extraction from credentialed institutions), and the hybrid_coproduction_reading (attempted integration with its own asymmetric costs). Each has a distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
