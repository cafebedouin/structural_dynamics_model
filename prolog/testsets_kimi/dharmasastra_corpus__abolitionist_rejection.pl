% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__abolitionist_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__abolitionist_rejection, []).

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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Dharmasastra Authority as Abolitionist Rejection Reading
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This constraint instantiates the abolitionist rejection reading of the
 *   dharmasastra_corpus kernel. From this seat, the Dharmasastra textual
 *   tradition is not a source of ethical guidance or historical wisdom but a
 *   fundamentally oppressive apparatus that naturalizes birth-based hierarchy
 *   through claimed divine revelation. The constraint extracts labor, status,
 *   and dignity from oppressed castes and women, concentrating benefit in the
 *   brahminical interpretive class and dominant caste elites. The
 *   abolitionist reading logically forecloses both orthodox literalist and
 *   reformist contextual readings because it denies any legitimate textual
 *   authority and insists on total abandonment rather than reinterpretation.
 *   The authored metrics describe high extraction, high theater (religious
 *   performance as cover for hierarchy), and substantial though eroded
 *   suppression following legal abolition in the post-independence republic.
 *
 * KEY AGENTS:
 *   - brahminical_interpreters: Agenda-setter and beneficiary (institutional/identity_locked/continental) â maintain the textual interpretive monopoly that assigns caste status and ritual rank.
 *   - dominant_caste_elites: Beneficiary (powerful/identity_locked/continental) â receive labor, deference, and endogamous social closure from the hierarchy.
 *   - oppressed_caste_communities: Payer (powerless/trapped/continental) â bear the extraction of labor, dignity, and mobility enforced by the textual rules.
 *   - subordinated_women: Payer (powerless/trapped/continental) â bear gendered extraction of labor and reproductive capacity within the dharmic framework.
 *   - abolitionist_movements: Observer (organized/analytical/national) â analyze and resist the caste-text nexus from outside the interpretive tradition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, 0.92).
domain_priors:suppression_score(dharmasastra_corpus__abolitionist_rejection, 0.78).
domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, 0.88).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, extractiveness, 0.92).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0.88).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Dharmasastra Authority as Abolitionist Rejection Reading").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__abolitionist_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, '3061a020-7ddf-40d3-8d1b-d21994328f58').
narrative_ontology:cs_kernel_codification('3061a020-7ddf-40d3-8d1b-d21994328f58', fixed_text).
narrative_ontology:cs_authority_grounding('3061a020-7ddf-40d3-8d1b-d21994328f58', extraction).
narrative_ontology:cs_interpretation_layer_present('3061a020-7ddf-40d3-8d1b-d21994328f58').
narrative_ontology:cs_reading_relation('3061a020-7ddf-40d3-8d1b-d21994328f58', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('3061a020-7ddf-40d3-8d1b-d21994328f58', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_axiom('3061a020-7ddf-40d3-8d1b-d21994328f58', foundational, textual_authority_zero).
narrative_ontology:cs_axiom_status(textual_authority_zero, holdable).
narrative_ontology:cs_axiom_grounding('3061a020-7ddf-40d3-8d1b-d21994328f58', textual_authority_zero, deontological).
narrative_ontology:cs_axiom('3061a020-7ddf-40d3-8d1b-d21994328f58', foundational, caste_text_inseparable).
narrative_ontology:cs_axiom_status(caste_text_inseparable, holdable).
narrative_ontology:cs_axiom_grounding('3061a020-7ddf-40d3-8d1b-d21994328f58', caste_text_inseparable, empirically_contingent).
narrative_ontology:cs_reference_frame('3061a020-7ddf-40d3-8d1b-d21994328f58', oppressive_hierarchy).
narrative_ontology:cs_drift_state('3061a020-7ddf-40d3-8d1b-d21994328f58', contemporary_republic, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3061a020-7ddf-40d3-8d1b-d21994328f58', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, brahminical_interpreters).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, dominant_caste_elites).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, oppressed_caste_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, subordinated_women).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__abolitionist_rejection, varna_hierarchy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and interpret the Dharmasastra texts, adjudicate dharma disputes, perform rituals that validate caste hierarchy, and derive social authority from exclusive access to textual knowledge. Their social position depends on the continued legitimacy of the textual framework.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, brahminical_interpreters, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__abolitionist_rejection, brahminical_interpreters, beneficiary).

% Occupy privileged positions in the caste hierarchy, receive labor services and deference from subordinated groups, and benefit from endogamous social networks that preserve economic and political advantages across generations.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dominant_caste_elites, beneficiary,
    powerful, generational, identity_locked, continental).

% Assigned to degraded and polluting labor, denied ritual and educational access, subjected to untouchability practices, and forced into dependency relationships with dominant caste households; mobility is structurally blocked by the textual rules and their social enforcement.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, oppressed_caste_communities, payer,
    powerless, generational, trapped, continental).

% Subjected to patriarchal dharmic rules governing marriage, property, mobility, and ritual participation; their labor and reproductive capacity are harnessed within the caste order and policed through scriptural norms.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, subordinated_women, payer,
    powerless, biographical, trapped, continental).

% Analyze the caste-text nexus as an extraction apparatus, organize resistance and conversion movements, and advocate for constitutional equality and the complete abandonment of Dharmasastra authority.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, abolitionist_movements, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates ritual purity, economic function, marriage partners, and social rank by birth status within a textual hierarchy; enforces endogamy and differential access to knowledge, resources, and physical space.
% TRANSFER_FUNCTION: Moves labor, deference, material surplus, reproductive capacity, and social status from oppressed castes and women to dominant caste elites and the priestly interpretive class.
% ABSENT_VOICES: Adivasi communities outside the varna fold, non-caste religious minorities, and subaltern castes historically denied textual literacy would contest the framework's universality; they were structurally excluded from the textual economy and its adjudication.
% DISAPPEARANCE_RATIONALE: If the Dharmasastra authority and caste framework vanished entirely, marriage markets would reconstitute across caste lines, labor relations would shift from ascriptive to contractual, ritual and educational institutions would lose their exclusionary gatekeeping function, and the entire social geography of the subcontinent would reorganize.
% FOUNDING_PROBLEM: Establishing and maintaining a birth-based social hierarchy that distributes ritual status, labor obligations, and political authority under the mantle of divine revelation and textual order.
% FOUNDING_PROBLEM_CORROBORATION: Subaltern historians and dalit intellectuals outside the beneficiary circle attest the framework was constructed to preserve brahminical dominance rather than to solve a universal coordination problem; no independent corroboration from non-beneficiary sources supports a divine origin claim.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__abolitionist_rejection, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__abolitionist_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__abolitionist_rejection, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dharmasastra_corpus__abolitionist_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__abolitionist_rejection, 0.92, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__abolitionist_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__abolitionist_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extreme (0.92) because the caste system allocates nearly all social goodsâritual status, education, land access, marriage partnersâby birth and enforces this through religious sanction. Suppression is high but decaying (0.78) because formal legal enforcement was abolished in the mid-twentieth century, yet social enforcement (boycott, violence, endogamy policing) persists. Theater ratio is very high (0.88) because the constraintâs persistence depends heavily on the performance of ritual purity, textual recitation, and dharmic legitimacy that obscures the extraction beneath. Accessibility collapse is near-total (0.92) because, within the system, alternatives to caste identity and textual authority were structurally unavailable. Resistance is moderate (0.55) because subaltern resistance has always existed but was fragmented and violently suppressed. The claim/metric independence is maintained: the constraint is claimed as snare (pure extraction) and the metrics support that classification without tuning.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats (brahminical_interpreters, dominant_caste_elites) experience the constraint as legitimate social order and religious duty; their computed type would diverge toward tangled rope or even mountain (divine law). The payer seats (oppressed_caste_communities, subordinated_women) experience it as pure extraction with no coordination benefit; their computed type aligns with snare. The engine produces this divergence from identical structural data because directionality inverts for beneficiaries and amplifies for trapped targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared as brahminical_interpreters and dominant_caste_elites; victims as oppressed_caste_communities and subordinated_women. The derivation chain assigns low d (subsidy/benefit) to the agenda-setting priestly class and dominant castes, and high d (target) to the powerless, trapped payer groups. Spatial scope is continental, amplifying effective extraction for the trapped victims. No override is needed because the structural declarations cleanly map the asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The abolitionist reading prevents mislabeling by refusing any coordination story. A reformist reading might classify the constraint as tangled rope (genuine ethical coordination plus caste extraction), but the abolitionist readingâs total rejection of textual authority dissolves the coordination claim entirely. Mandatrophy is resolved: the founding problem of maintaining a birth-based hierarchy is dead in the modern republic, yet the arrangement persists through social enforcement and identity-lock, producing the dead-problem + world-rearranges mismatch that signals zombie/snare persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the Dharmasastra corpus a separable ethical framework (reformist), an eternally valid revelation (orthodox), or a fundamentally oppressive apparatus (abolitionist)?',
    'Comparative historical sociology of caste and subaltern ethnography: if the textual rules show tight coupling to extraction mechanisms across independent regional implementations, the abolitionist reading is supported; if decoupling of ethical precepts from hierarchy is historically evidenced, the reformist reading gains support.',
    'Resolution determines whether the constraint decomposes as snare (abolitionist), tangled rope (reformist), or mountain (orthodox).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contested kernel reading ambiguity for Dharmasastra').

omega_variable(
    caste_suppression_mechanism,
    'Is caste suppression maintained primarily through structural violence (economic boycott, spatial segregation, physical violence) or through internalized identity (self-policing of caste boundaries and psychological incorporation of hierarchy)?',
    'Post-exit trajectory studies: if individuals who leave caste-bound contexts continue to enforce boundaries on themselves and their kin, suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraintâs extractive grip is harder to dismantle through legal reform alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_suppression_mechanism, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    textual_authority_grounding,
    'Does the authority of Dharmasastra derive from genuine lineage/tradition or from the extraction benefits it confers on the interpretive class?',
    'Economic and demographic analysis of brahminical communities: if their authority and resource control collapse when extraction is legally removed, authority is extraction-dependent.',
    'If extraction-dependent, the constraint is a snare; if genuinely lineage-based, it may read as a degraded commitment system (piton) rather than pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_authority_grounding, empirical, 'Whether authority is extraction-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0, 0.7).
narrative_ontology:measurement(dhar_tr_t15, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 15, 0.75).
narrative_ontology:measurement(dhar_tr_t30, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 30, 0.8).
narrative_ontology:measurement(dhar_tr_t45, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 45, 0.84).
narrative_ontology:measurement(dhar_tr_t60, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 60, 0.87).
narrative_ontology:measurement(dhar_tr_t70, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 70, 0.88).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 0, 0.95).
narrative_ontology:measurement(dhar_be_t15, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 15, 0.94).
narrative_ontology:measurement(dhar_be_t30, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 30, 0.93).
narrative_ontology:measurement(dhar_be_t45, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 45, 0.92).
narrative_ontology:measurement(dhar_be_t60, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 60, 0.92).
narrative_ontology:measurement(dhar_be_t70, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 70, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(dhar_su_t15, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 15, 0.86).
narrative_ontology:measurement(dhar_su_t30, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(dhar_su_t45, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 45, 0.8).
narrative_ontology:measurement(dhar_su_t60, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 60, 0.79).
narrative_ontology:measurement(dhar_su_t70, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 70, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__abolitionist_rejection, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__reformist_contextual).

% DUAL FORMULATION NOTE:
% This constraint is the abolitionist reading of the dharmasastra_corpus kernel. Sibling readings instantiate structurally distinct constraints from the same textual kernel. The abolitionist reading decomposes the corpus as inseparable from caste oppression, whereas the reformist reading separates an ethical core and the orthodox reading treats the text as eternal revelation. Each reading emits a different epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
