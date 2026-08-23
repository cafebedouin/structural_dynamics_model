% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__secular_democratic_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: July Charter Secular Democratic Mandate (Civilian Supremacy Reading)
 *   domain: constitutional_law/political_transitions
 *
 * SUMMARY:
 *   This constraint story instantiates the secular democratic reading of the
 *   July Charter kernel: the charter is read as mandating secular democratic
 *   institutions and the subordination of the military to civilian authority.
 *   In this reading, the charter functions as a tangled rope â it
 *   coordinates a genuine post-revolutionary transition problem (how to
 *   prevent military or theocratic dictatorship) while asymmetrically
 *   extracting political autonomy from the military and participation rights
 *   from Islamist parties such as Jamaat-e-Islami. The constraint is actively
 *   enforced through constitutional structures, electoral rules, and civilian
 *   control mechanisms. The reading is contested by sibling readings that
 *   claim the charter establishes Islamic-nationalist sovereignty or military
 *   custodianship.
 *
 * KEY AGENTS:
 *   - civilian_authority (institutional/constrained) â agenda setter administering the secular framework and enforcing military subordination
 *   - secular_political_coalition (organized/constrained) â beneficiary operating within the secular democratic field
 *   - jamaat_e_islami (organized/constrained) â payer bearing exclusion from full political participation
 *   - military_institution (institutional/constrained) â payer bearing loss of autonomous political role
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.65).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.75).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "July Charter Secular Democratic Mandate (Civilian Supremacy Reading)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional_law/political_transitions").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, 'f5493b54-275e-4a38-99b3-2e512f7c97ed').
narrative_ontology:cs_kernel_codification('f5493b54-275e-4a38-99b3-2e512f7c97ed', fixed_text).
narrative_ontology:cs_authority_grounding('f5493b54-275e-4a38-99b3-2e512f7c97ed', lineage).
narrative_ontology:cs_interpretation_layer_present('f5493b54-275e-4a38-99b3-2e512f7c97ed').
narrative_ontology:cs_reading_relation('f5493b54-275e-4a38-99b3-2e512f7c97ed', july_charter_sovereign_legitimacy__guided_nationalism_reading, forecloses).
narrative_ontology:cs_reading_relation('f5493b54-275e-4a38-99b3-2e512f7c97ed', july_charter_sovereign_legitimacy__military_custodian_reading, forecloses).
narrative_ontology:cs_axiom('f5493b54-275e-4a38-99b3-2e512f7c97ed', foundational, popular_sovereignty_as_exclusive_legitimacy_source).
narrative_ontology:cs_axiom_status(popular_sovereignty_as_exclusive_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('f5493b54-275e-4a38-99b3-2e512f7c97ed', popular_sovereignty_as_exclusive_legitimacy_source, conventional).
narrative_ontology:cs_axiom('f5493b54-275e-4a38-99b3-2e512f7c97ed', foundational, military_subordination_as_structural_requirement).
narrative_ontology:cs_axiom_status(military_subordination_as_structural_requirement, holdable).
narrative_ontology:cs_axiom_grounding('f5493b54-275e-4a38-99b3-2e512f7c97ed', military_subordination_as_structural_requirement, conventional).
narrative_ontology:cs_created_at('f5493b54-275e-4a38-99b3-2e512f7c97ed', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_authority).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_political_coalition).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_institution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governs under the charter's secular framework, derives legitimacy from electoral mandate, and administers the institutions that enforce military subordination to elected leadership.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_authority, agenda_setter,
    institutional, generational, constrained, national).

% Competes for power within secular democratic institutions, benefiting from a political field that constrains religious parties and centers civilian electoral contestation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_political_coalition, beneficiary,
    organized, generational, constrained, national).

% Religious political movement whose full participation is constrained by secular mandates; bears the cost of exclusion from governing coalitions and limited platform access under the charter.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami, payer,
    organized, generational, constrained, national).

% Bears the cost of constitutional subordination, losing autonomous political role, independent budgetary authority, and institutional parity with civilian government.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_institution, payer,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the post-revolutionary power vacuum by establishing a non-theocratic, non-praetorian governing framework, creating a predictable process for leadership transition and pluralistic contestation without reverting to sectarian or military rule.
% TRANSFER_FUNCTION: Transfers governing authority and institutional autonomy from religious political movements and the military to elected civilian bodies operating under secular procedural norms.
% ABSENT_VOICES: Grassroots Islamist constituencies who view sovereignty as deriving from divine mandate rather than popular will; mid-rank military officers who might prefer institutional autonomy or a custodial national role.
% DISAPPEARANCE_RATIONALE: If the secular democratic mandate vanished, civilian government would lose its constitutional anchor against praetorian and theocratic claims; Islamist parties would demand unbounded participation and the military would likely reclaim autonomous political guardianship, forcing a wholesale reorganization of the state.
% FOUNDING_PROBLEM: Post-revolutionary collapse of prior regime leaving a power vacuum contested by armed forces and religious movements, threatening recurrent dictatorship or majoritarian theocracy.
% FOUNDING_PROBLEM_CORROBORATION: Secular democratic coalitions and transitional justice scholars attest the coup-theocracy risk; Islamist parties and military historians dispute that the risk justified their permanent constitutional exclusion, attesting that the charter entrenched a particular ideological victory. International constitutional advisors provide partial outside corroboration of the transition dilemma but not of the specific secular exclusionary design.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__secular_democratic_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__secular_democratic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint scores extractiveness at 0.65 because it systematically transfers authority away from two significant political forces (Islamists and the military) to civilian electoral bodies. Suppression at 0.75 reflects the active enforcement required to maintain military subordination and secular boundaries against organized resistance. Theater ratio at 0.45 captures the performative dimension of civilian supremacy in contexts where military or bureaucratic power persists informally. The coordination function is genuine â the charter prevents a direct return to dictatorship â but it is inextricably bound to asymmetric extraction, meeting the tangled rope gate. Metrics and claim are authored independently: the claim is tangled_rope; if metrics were lower on extraction, the engine might compute rope, and if higher, snare â the divergence is the measurement.
 *
 * PERSPECTIVAL GAP:
 *   From the civilian authority and secular coalition, the constraint appears as necessary coordination that solved a post-revolutionary tragedy-of-the-commons over sovereign power. From Jamaat-e-Islami and the military institution, the identical constitutional text operates as an enforced dispossession of their political autonomy and rightful role in statecraft. The engine computes this divergence from the same structural data: beneficiaries with constrained exit experience low effective extraction, while payers with constrained exit experience high effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian authority and the secular coalition are structural beneficiaries (low d): the constraint subsidizes their access to power and legitimizes their rule. Jamaat-e-Islami and the military institution are structural targets (high d): the constraint extracts their capacity for autonomous political action. The derivation is direct from the beneficiary/victim declarations combined with constrained exit options for all parties; no override is needed because the structural positions are clear.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents two common errors: reading the charter as pure rope (which would ignore the asymmetric exclusion of Islamists and military autonomy) and reading it as pure snare (which would deny the genuine coordination function of preventing post-revolutionary praetorian or theocratic collapse). The founding problem is contested, not dead, so mandatrophy is not resolved; the constraint is not a piton because the beneficiaries remain actively invested in its enforcement and the extraction is concentrated rather than diffuse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secular_mandate_contingency,
    'Is the secular democratic mandate a contingent post-revolutionary compromise among secular elites, or does it reflect a structurally necessary feature of sovereign legitimacy?',
    'Historical analysis of drafting records and comparative constitutional studies of similar transitions.',
    'If contingent, the constraint''s high extractiveness is not justified by natural-law necessity and the false-summit detection system flags it; if necessary, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_mandate_contingency, conceptual, 'Contingency of secular democratic mandate').

omega_variable(
    military_consent_vs_coercion,
    'Is military subordination maintained by the charter''s coercive enforcement capacity, or by the military''s tacit consent and strategic calculation?',
    'Observation of military behavior during constitutional crises: if the military intervenes when its interests are threatened despite constitutional text, subordination is performative.',
    'If performative, theater_ratio rises and the constraint approaches piton; if genuine enforcement, the extraction from military autonomy is structurally secured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_consent_vs_coercion, empirical, 'Military subordination enforcement mechanism').

omega_variable(
    kernel_text_ambiguity,
    'Does the charter text unambiguously mandate secular democratic civilian supremacy, or does it contain ambiguity that makes the secular democratic reading one of several viable interpretations?',
    'Textual analysis of the charter alongside drafting debates and subsequent amendment history.',
    'If the text is ambiguous, the constraint''s identity is kernel-dependent and its classification varies by reading; if unambiguous, rival readings are misreadings rather than alternate constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_text_ambiguity, conceptual, 'Ambiguity of charter text regarding secularism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_charter_secular_tr_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(july_charter_secular_tr_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(july_charter_secular_tr_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(july_charter_secular_tr_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(july_charter_secular_tr_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement(july_charter_secular_tr_t25, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(july_charter_secular_be_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(july_charter_secular_be_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(july_charter_secular_be_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(july_charter_secular_be_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(july_charter_secular_be_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(july_charter_secular_be_t25, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 25, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(july_charter_secular_su_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(july_charter_secular_su_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(july_charter_secular_su_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(july_charter_secular_su_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(july_charter_secular_su_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(july_charter_secular_su_t25, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 25, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, military_custodian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the july_charter_sovereign_legitimacy kernel. It is decomposed from the contested natural-language concept 'July Charter' into structurally distinct claims: secular democratic mandate, guided nationalism, and military custodianship. Each reading has its own epsilon, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
