% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__biomedical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__biomedical_reading, []).

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
 *   constraint_id: dsm_taxonomy_kernel__biomedical_reading
 *   human_readable: DSM Biomedical Objectivity Reading
 *   domain: medical_epistemology/psychiatric_taxonomy
 *
 * SUMMARY:
 *   This constraint story instantiates the biomedical_reading of the
 *   dsm_taxonomy_kernel: the institutionalized claim that DSM categories map
 *   to objective neurobiological disease entities discoverable through
 *   empirical research. The reading is encoded in diagnostic manuals,
 *   treatment guidelines, insurance authorization systems, and involuntary
 *   treatment statutes. It functions as the dominant legitimating framework
 *   for global psychiatric practice, pharmaceutical markets, and
 *   institutional behavioral management. Key agents include the psychiatric
 *   establishment and pharmaceutical industry as concentrated beneficiaries,
 *   diagnosed individuals as diffuse payers subject to coercion, and critical
 *   or neurodiversity perspectives as structurally excluded voices.
 *
 * KEY AGENTS:
 *   - psychiatric_establishment: Primary beneficiary (institutional/constrained) â captures professional authority and reimbursement
 *   - pharmaceutical_industry: Primary beneficiary (institutional/mobile) â captures pharmacological markets
 *   - institutional_gatekeepers: Secondary beneficiary (organized/constrained) â captures behavioral conformity tools
 *   - diagnosed_individuals: Primary target (powerless/trapped) â bears extraction via involuntary treatment and capacity loss
 *   - apa_dsm_authors: Agenda setter (institutional/constrained) â administers the taxonomy
 *   - critical_psychiatry_researchers: Excluded voice (moderate/constrained) â marginalized despite empirical challenges
 *   - neurodiversity_advocates: Excluded voice (organized/constrained) â pathologization critics kept outside revision
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.78).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.72).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Biomedical Objectivity Reading").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, 'e484d3d7-3a90-40d9-a3ee-ce932f9e21d4').
narrative_ontology:cs_kernel_codification('e484d3d7-3a90-40d9-a3ee-ce932f9e21d4', formalized).
narrative_ontology:cs_authority_grounding('e484d3d7-3a90-40d9-a3ee-ce932f9e21d4', extraction).
narrative_ontology:cs_interpretation_layer_present('e484d3d7-3a90-40d9-a3ee-ce932f9e21d4').
narrative_ontology:cs_reading_relation('e484d3d7-3a90-40d9-a3ee-ce932f9e21d4', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e484d3d7-3a90-40d9-a3ee-ce932f9e21d4', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('e484d3d7-3a90-40d9-a3ee-ce932f9e21d4', foundational, dsm_categories_map_to_neurobiological_entities).
narrative_ontology:cs_axiom_status(dsm_categories_map_to_neurobiological_entities, holdable).
narrative_ontology:cs_axiom_grounding('e484d3d7-3a90-40d9-a3ee-ce932f9e21d4', dsm_categories_map_to_neurobiological_entities, empirically_contingent).
narrative_ontology:cs_axiom('e484d3d7-3a90-40d9-a3ee-ce932f9e21d4', secondary, pharmacological_intervention_as_primary_treatment).
narrative_ontology:cs_axiom_status(pharmacological_intervention_as_primary_treatment, holdable).
narrative_ontology:cs_axiom_grounding('e484d3d7-3a90-40d9-a3ee-ce932f9e21d4', pharmacological_intervention_as_primary_treatment, instrumental).
narrative_ontology:cs_reference_frame('e484d3d7-3a90-40d9-a3ee-ce932f9e21d4', biomedical_objectivity_framework).
narrative_ontology:cs_drift_state('e484d3d7-3a90-40d9-a3ee-ce932f9e21d4', contemporary_post_dsm5, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e484d3d7-3a90-40d9-a3ee-ce932f9e21d4', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, institutional_gatekeepers).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, diagnosed_individuals).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, biological_psychiatry_doctrine).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, categorical_disease_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derives professional authority, reimbursement eligibility, research funding, and institutional jurisdiction from the claim that DSM categories reflect objective neurobiological disease entities. The categorical model underwrites licensure, training curricula, and clinical practice standards worldwide.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, beneficiary,
    institutional, generational, constrained, global).

% Benefits from disease-entity framing that justifies pharmacological intervention as a primary and often mandatory treatment modality. DSM categories create legally recognized, insurance-reimbursable markets for psychotropic drug development, prescription, and long-term maintenance.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry, beneficiary,
    institutional, biographical, mobile, global).

% Schools, employers, carceral systems, immigration authorities, and courts use DSM diagnoses to classify, manage, medicate, and exclude individuals who deviate from expected behavioral norms, outsourcing social control to medical authority.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, institutional_gatekeepers, beneficiary,
    organized, biographical, constrained, national).

% Bear the costs of diagnostic classification including involuntary commitment, forced medication, adverse pharmacological effects, loss of legal capacity, employment discrimination, and identity foreclosure. Once entered into the system, exit is blocked by civil commitment statutes, guardianship proceedings, and persistent institutional records.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, diagnosed_individuals, payer,
    powerless, biographical, trapped, local).

% Authors and revises the DSM, setting diagnostic thresholds, inclusion criteria, and category boundaries. Presents revisions as empirically grounded while navigating commercial pressures, professional politics, and institutional precedent.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, apa_dsm_authors, agenda_setter,
    institutional, generational, constrained, global).

% Produce evidence that DSM categories lack biomarker validity, diagnostic stability, and cross-cultural reliability. Their findings are systematically underrepresented in guideline committees, training programs, and funding priorities.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, critical_psychiatry_researchers, excluded,
    moderate, generational, constrained, global).

% Argue that DSM categories pathologize natural human neurological variation. Excluded from DSM revision processes, institutional guideline development, and reimbursement policy negotiations.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, neurodiversity_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__biomedical_reading, diffuse).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__biomedical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized nomenclature that enables clinical communication, research aggregation, insurance pre-authorization, and epidemiological tracking across otherwise fragmented institutions and geographies.
% TRANSFER_FUNCTION: Moves autonomy, credibility, and economic resources from diagnosed individuals to psychiatric institutions, pharmaceutical markets, and behavioral-conformity systems through the mechanism of categorical diagnostic assignment.
% ABSENT_VOICES: Critical psychiatry researchers and neurodiversity advocates are structurally excluded from DSM revision and standard-of-care guideline processes; their absence naturalizes the biomedical frame and suppresses dimensional or contextual alternatives.
% DISAPPEARANCE_RATIONALE: If the institutionalized claim that DSM categories map to objective neurobiological disease entities vanished, insurance authorization frameworks would destabilize, pharmaceutical markets for many psychotropic categories would contract, involuntary treatment statutes would lose their primary legitimating vocabulary, and schools, courts, and employers would need alternative vocabularies for behavioral management.
% FOUNDING_PROBLEM: How to classify severe mental distress and behavioral deviance in a way that enables reliable clinical communication, research aggregation, and resource allocation across disparate institutions and cultures.
% FOUNDING_PROBLEM_CORROBORATION: The psychiatric establishment attests the problem remains live and requires categorical disease-entity classification. Critical psychiatry, survivor movements, and independent historians attest the founding problem has been captured by commercial and professional interests; outside scholarship (e.g., Kutchins & Kirk, Whitaker, Horwitz) supports the contested status.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__biomedical_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__biomedical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__biomedical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.40 to 0.78 over the interval because the disease-entity claim became increasingly decoupled from actual biomarker discovery while enabling ever-broader pharmacological and coercive intervention. Suppression rises from 0.45 to 0.72 as critical psychiatry and survivor movements gained visibility and required active institutional exclusion from guidelines, training, and revision processes. Theater ratio rises from 0.20 to 0.55 as the gap between claimed neurobiological grounding and persistent biomarker absence widened, forcing increasing performative invocation of 'future discovery' to maintain authority. Accessibility collapse is high (0.80) because once an individual carries a DSM diagnosis, alternative framings (recovery model, neurodiversity, trauma-informed contextual models) become nearly inaccessible within mainstream institutional pathways. Resistance is moderate (0.55) because organized survivor and critical movements exist but remain institutionally marginal.
 *
 * PERSPECTIVAL GAP:
 *   The psychiatric establishment and pharmaceutical industry compute the constraint as coordination (standardized nomenclature enabling research and care), while diagnosed individuals compute it as extraction (loss of autonomy, forced treatment, and identity foreclosure). The engine captures this divergence from the structural asymmetry in exit options (mobile/constrained vs trapped) and power (institutional vs powerless). The agenda setter seat (APA authors) experiences the constraint as necessary professional infrastructure under empirical contest, a hybrid position between beneficiary and coordinator.
 *
 * DIRECTIONALITY LOGIC:
 *   The three beneficiary groupsâpsychiatric_establishment, pharmaceutical_industry, and institutional_gatekeepersâsit at low directionality because the constraint subsidizes their authority, revenue, and social-control capacity. The diagnosed_individuals stakeholder sits at high directionality because the constraint extracts autonomy, legal capacity, and bodily integrity from them. The excluded seats (critical psychiatry, neurodiversity) sit outside the formal beneficiary/victim derivation but their exclusion is what sustains the suppression metric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâreliable classification of mental distress for coordinationâremains genuine, which prevents classification as a pure snare. However, the specific solution (categorical biomedical objectivity) has accumulated substantial extractive function that exceeds its coordination value, as evidenced by rising theater and extractiveness over time. This prevents classification as a pure rope. Tangled rope is the structurally honest classification: genuine coordination substrate (clinical communication, research aggregation) coexisting with asymmetric extraction (involuntary treatment, pharma markets, capacity loss) held in place by active enforcement (institutional guideline capture, exclusion of critics, legal coercion).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the biomedical reading of DSM taxonomy represent a genuine empirical commitment that is temporarily incomplete, or does it function as an institutional extraction mechanism whose persistence depends on preventing kernel revision?',
    'Comparative analysis of DSM revision processes against independent neurobiological and genetic research trajectories; assessment of whether diagnostic validity and biomarker correspondence have improved commensurately with decades of claimed neurobiological grounding.',
    'If biomarker validity remains absent and the gap between claim and evidence has widened, the constraint is better explained by institutional extraction than empirical discovery, supporting reclassification toward snare or piton and confirming axiom-overriding drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the biomedical reading is genuinely empirically grounded or functions as institutional extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative frameworks (critical psychiatry, neurodiversity, trauma-informed models) structural or internalized among diagnosed individuals and frontline clinicians?',
    'Post-exit trajectory studies: do individuals and clinicians who leave mainstream psychiatric institutions continue to endorse the biomedical model, or do alternative frameworks become accessible once structural barriers are removed?',
    'If internalized, effective suppression exceeds structural measures, amplifying extraction through identity lock-in and reducing measured resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of alternative psychiatric frameworks.').

omega_variable(
    involuntary_treatment_scope,
    'What proportion of individuals meeting DSM diagnostic thresholds actually experience involuntary treatment, legal capacity restriction, or coerced pharmacological intervention versus voluntary engagement?',
    'Epidemiological and legal-status surveys of diagnosed populations, cross-referenced by diagnostic category and jurisdiction.',
    'High involuntary scope would confirm severe direct extraction; predominantly voluntary engagement would shift the victim profile toward indirect extraction (stigma, opportunity cost, pharmacological side effects) and moderate epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(involuntary_treatment_scope, empirical, 'Scope of involuntary treatment within the diagnosed population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm_bio_tr_t0, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dsm_bio_tr_t10, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(dsm_bio_tr_t20, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(dsm_bio_tr_t30, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(dsm_bio_tr_t40, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(dsm_bio_be_t0, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(dsm_bio_be_t10, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(dsm_bio_be_t20, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(dsm_bio_be_t30, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(dsm_bio_be_t40, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dsm_bio_su_t0, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(dsm_bio_su_t10, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(dsm_bio_su_t20, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(dsm_bio_su_t30, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(dsm_bio_su_t40, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, identity_coordination).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).

% DUAL FORMULATION NOTE:
% This constraint is the biomedical reading of the dsm_taxonomy_kernel, which decomposes into structurally distinct readings per epsilon-invariance. The biomedical reading claims DSM categories map to objective neurobiological disease entities (high extractiveness, empirically contingent axioms). Sibling readings have different epsilon values, victim sets, and foundational axioms. Each reading is compiled as a separate constraint story linked by the kernel family network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
