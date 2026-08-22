% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__restrictive_anthropocentric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__restrictive_anthropocentric_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: legal_personhood_boundary__restrictive_anthropocentric_reading
 *   human_readable: Restrictive Anthropocentric Personhood Boundary
 *   domain: legal/philosophical
 *
 * SUMMARY:
 *   This constraint is the restrictive_anthropocentric_reading of the
 *   legal_personhood_boundary kernel. It holds that legal personhood is
 *   limited to born humans with cognitive capacity, thereby excluding
 *   fetuses, ecosystems, AI systems, and some cognitively impaired humans
 *   from rights-bearing status. Sibling readings include
 *   developmental_potentiality_reading (conception-based personhood) and
 *   functional_capacity_reading (species-blind capacity-based personhood).
 *   The reading is claimed as a coordination device that prevents legal
 *   overload, while its metrics describe substantial asymmetric extraction
 *   from excluded entities.
 *
 * KEY AGENTS:
 *   - State judiciary (agenda_setter/institutional): Administers and enforces the personhood boundary through constitutional interpretation and case law.
 *   - Pregnant people (beneficiary/moderate): Retain reproductive autonomy because fetal personhood is denied.
 *   - Established rights holders (beneficiary/organized): Enjoy stable rights reserved to the born cognitively capable human class.
 *   - Fetal entities (payer/powerless/non-actor): Denied all legal standing and protective intervention due to the birth threshold.
 *   - Ecological systems (payer/powerless/non-actor): Denied standing to sue on their own behalf.
 *   - Artificial intelligences (payer/powerless/non-actor): Classified as property regardless of functional capacity.
 *   - Cognitively impaired humans (payer/powerless): Risk guardianship and diminished autonomy under capacity tests.
 *   - Pro-life advocates and environmental lawyers (excluded/organized): Present in discourse but structurally foreclosed from prevailing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.62).
domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.58).
domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Restrictive Anthropocentric Personhood Boundary").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal/philosophical").

domain_priors:requires_active_enforcement(legal_personhood_boundary__restrictive_anthropocentric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, '3800b4bd-4eba-4643-90a0-6352f838ba0f').
narrative_ontology:cs_kernel_codification('3800b4bd-4eba-4643-90a0-6352f838ba0f', formalized).
narrative_ontology:cs_authority_grounding('3800b4bd-4eba-4643-90a0-6352f838ba0f', lineage).
narrative_ontology:cs_interpretation_layer_present('3800b4bd-4eba-4643-90a0-6352f838ba0f').
narrative_ontology:cs_reading_relation('3800b4bd-4eba-4643-90a0-6352f838ba0f', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_reading_relation('3800b4bd-4eba-4643-90a0-6352f838ba0f', legal_personhood_boundary__functional_capacity_reading, forecloses).
narrative_ontology:cs_axiom('3800b4bd-4eba-4643-90a0-6352f838ba0f', foundational, anthropocentric_birth_threshold).
narrative_ontology:cs_axiom_status(anthropocentric_birth_threshold, holdable).
narrative_ontology:cs_axiom_grounding('3800b4bd-4eba-4643-90a0-6352f838ba0f', anthropocentric_birth_threshold, deontological).
narrative_ontology:cs_axiom('3800b4bd-4eba-4643-90a0-6352f838ba0f', foundational, cognitive_capacity_gate).
narrative_ontology:cs_axiom_status(cognitive_capacity_gate, holdable).
narrative_ontology:cs_axiom_grounding('3800b4bd-4eba-4643-90a0-6352f838ba0f', cognitive_capacity_gate, instrumental).
narrative_ontology:cs_reference_frame('3800b4bd-4eba-4643-90a0-6352f838ba0f', born_human_cognitive_rights_framework).
narrative_ontology:cs_drift_state('3800b4bd-4eba-4643-90a0-6352f838ba0f', contemporary_rights_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3800b4bd-4eba-4643-90a0-6352f838ba0f', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_people).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, established_rights_holders).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, fetal_entities).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, ecological_systems).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligences).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitively_impaired_humans).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, anthropocentric_legal_positivism).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, birth_threshold_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the physical and legal burdens of pregnancy. Under this reading, they retain autonomy over reproductive decisions because the fetus is not a rights-bearing person competing for legal protection. They cannot easily exit the jurisdiction of the legal system that defines personhood.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_people, beneficiary,
    moderate, biographical, constrained, national).

% Born humans who meet cognitive capacity thresholds and benefit from a stable legal boundary that reserves rights, standing, and civic status to their class. They experience reduced competition for rights-claims from non-human entities and pre-birth humans.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, established_rights_holders, beneficiary,
    organized, generational, mobile, national).

% Interprets constitutional and statutory texts to adjudicate who qualifies as a legal person. Maintains the restrictive boundary through case law, rejecting personhood claims from fetuses, ecosystems, and AI. Could revise the boundary but faces institutional precedent constraints.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, state_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Human organisms in utero are categorically denied legal personhood under this reading. They cannot hold rights, sue for wrongful death, or trigger state protective intervention. Their exclusion is the direct cost of the birth-threshold rule.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, fetal_entities, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__restrictive_anthropocentric_reading, fetal_entities).

% Rivers, forests, and ecosystems are denied legal standing to sue on their own behalf. Environmental protection must be routed through human plaintiffs with concrete injury, limiting direct legal recourse for ecological harm.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, ecological_systems, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__restrictive_anthropocentric_reading, ecological_systems).

% Advanced AI systems are classified as property or tools regardless of demonstrated functional capacity. They cannot hold rights, enter contracts, or seek legal redress for deactivation or misuse.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligences, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligences).

% Born humans whose cognitive capacity falls below the threshold required for full personhood attribution under this reading. They risk guardianship, diminished autonomy, and exclusion from full rights-bearing status despite being human and born.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitively_impaired_humans, payer,
    powerless, biographical, identity_locked, national).

% Seek legal standing for natural entities and are systematically blocked by the anthropocentric birth-and-species requirement. Their arguments are heard in court but the restrictive reading forecloses their preferred outcome.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_law_advocates, excluded,
    organized, generational, constrained, national).

% Argue for fetal personhood from conception and are structurally excluded from achieving their goal under this reading. They resist the constraint through litigation and legislation but the reading forecloses their core claim.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pro_life_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__restrictive_anthropocentric_reading, diffuse).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__restrictive_anthropocentric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates legal rights, duties, and standing by establishing a clear boundary around which entities can sue, enter contracts, hold property, and demand state protection. Solves the coordination problem of an overloaded legal system by limiting the set of rights-bearers to born humans with cognitive capacity.
% TRANSFER_FUNCTION: Transfers legal standing and protective state intervention away from fetuses, ecosystems, AI systems, and cognitively impaired humans, and toward born cognitively capable humans and pregnant persons seeking reproductive autonomy.
% ABSENT_VOICES: Fetuses and ecosystems cannot speak in the legal forum; AI systems lack representation. Pro-life advocates and environmental lawyers who would argue for broader personhood are present in discourse but structurally excluded from prevailing under this reading.
% DISAPPEARANCE_RATIONALE: If the restrictive boundary vanished, fetal personhood claims would flood courts, ecosystems could gain standing, AI rights litigation would proceed, and the architecture of reproductive and environmental law would reorganize around broader rights-bearing entities.
% FOUNDING_PROBLEM: The problem of infinite or conflicting rights-claims in a finite legal system â how to allocate scarce judicial and protective resources among entities with competing claims to moral consideration without collapsing the legal order into irresolvable value conflicts.
% FOUNDING_PROBLEM_CORROBORATION: Legal philosophers like John Finnis and Ronald Dworkin have debated whether the boundary is necessary for legal coherence. Pro-life and environmental advocates attest the problem is not rights-overload but wrongful exclusion. No neutral party outside the benefiting groups fully corroborates the restrictive boundary as the only solution; it is contested across jurisprudential schools.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__restrictive_anthropocentric_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__restrictive_anthropocentric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legal_personhood_boundary__restrictive_anthropocentric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the constraint systematically denies rights to a broad class of entities that other readings would protect. Suppression (0.58) reflects active judicial enforcement required to maintain the exclusion. Theater ratio (0.30) acknowledges that the legal doctrine has genuine coordinating function but carries performative boundary-maintenance. Accessibility collapse (0.78) is high because once the doctrine is entrenched, alternatives (fetal standing, ecosystem rights) become legally unimaginable. Resistance (0.52) captures persistent social and jurisprudential opposition. Measurements share one time grid to prevent misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (judiciary) experiences the constraint as necessary legal architecture that coordinates rights and prevents jurisdictional chaos. The payer seats (excluded entities and cognitively impaired humans) experience it as categorical rights denial. The beneficiary seats experience it as autonomy and clarity. The engine computes this divergence from structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (pregnant people, established rights holders) sit near the beneficiary end of directionality because the constraint subsidizes their autonomy and status. The agenda setter (state judiciary) sits near symmetric but slightly toward beneficiary because it maintains institutional authority through the doctrine. Payers (fetal entities, ecosystems, AI, cognitively impaired humans) sit near the full-target end because the constraint extracts legal standing from them. Excluded advocates sit at high directionality because the constraint actively suppresses their claims.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled_rope classification, this constraint would be misread either as a rope (by rights-holders who see only the coordination function) or as a snare (by pro-life advocates who see only the extraction). The structural requirement to declare both beneficiaries and victims, plus active enforcement, forces the dual nature into the record. The metrics are authored independently of the claim: the claim is tangled_rope because the constraint genuinely coordinates rights for some while asymmetrically extracting them from others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    birth_threshold_empirical_basis,
    'Is birth a morally significant ontological boundary, or merely a pragmatic legal convenience?',
    'Comparative legal analysis of jurisdictions with different thresholds (viability, conception, birth) correlated with outcomes for maternal health and fetal mortality; philosophical argumentation alone cannot resolve.',
    'If birth is merely pragmatic, the reading''s deontological grounding collapses toward conventionalism, lowering accessibility_collapse and raising theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(birth_threshold_empirical_basis, conceptual, 'Ambiguity about whether the birth threshold is natural or constructed').

omega_variable(
    speciesism_justification,
    'Does species membership track any morally relevant property, or is it an arbitrary discriminator equivalent to other historical exclusionary categories?',
    'Cross-species cognitive capacity research and comparative rights theory; if non-human animals demonstrate capacity meeting the reading''s threshold, the species boundary becomes visible as arbitrary extraction.',
    'Resolution against speciesism would reclassify the constraint toward snare by collapsing the coordination justification for the species boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speciesism_justification, conceptual, 'Whether species membership is a defensible moral criterion').

omega_variable(
    capacity_measurement_indeterminacy,
    'How is cognitive capacity operationalized in law, and does the operational metric genuinely distinguish persons from non-persons?',
    'Empirical review of judicial capacity tests (IQ, communication, executive function) and their correlation with moral agency; neuroscientific evidence on minimal consciousness.',
    'If capacity tests are arbitrary or over-inclusive, the reading''s internal coherence weakens and effective extraction from cognitively impaired humans rises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_measurement_indeterminacy, empirical, 'Indeterminacy in how cognitive capacity is measured and applied').

omega_variable(
    kernel_reading_location,
    'Does the restrictive anthropocentric reading genuinely foreclose its siblings, or do hybrid or jurisdictionally fragmented readings allow partial coexistence?',
    'Survey of actual legal systems to detect mixed regimes (e.g., born-human requirement for voting but capacity-based standing for animals in some courts).',
    'If hybrid regimes exist, the forecloses relation should be downgraded to influences, changing the constraint family''s network topology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Whether this reading strictly forecloses siblings or permits hybrid legal regimes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lpb_ra_tr_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lpb_ra_tr_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(lpb_ra_tr_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(lpb_ra_tr_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(lpb_ra_tr_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement(lpb_ra_tr_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(lpb_ra_be_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(lpb_ra_be_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(lpb_ra_be_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(lpb_ra_be_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(lpb_ra_be_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(lpb_ra_be_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(lpb_ra_su_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(lpb_ra_su_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(lpb_ra_su_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(lpb_ra_su_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement(lpb_ra_su_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(lpb_ra_su_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, developmental_potentiality_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, functional_capacity_reading).

% DUAL FORMULATION NOTE:
% The legal_personhood_boundary kernel decomposes into three structurally distinct readings. The restrictive_anthropocentric_reading (born humans with cognitive capacity) is linked to developmental_potentiality_reading (conception-based) and functional_capacity_reading (species-blind capacity-based). Each reading has a distinct beneficiary/victim structure and epsilon profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
