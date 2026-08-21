% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__coercion_visibility_reading, []).

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
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: Divine Marriage Command (Coercion Visibility Reading)
 *   domain: religious/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the 'coercion_visibility_reading' of
 *   the 'divine_marriage_command' kernel. It describes the theological
 *   justification for the religious institution's abandonment of polygamy as
 *   a direct, acknowledged response to federal coercion, with legitimacy
 *   derived from the necessity of institutional survival. The constraint
 *   coordinates the institution's continued existence but extracts from
 *   members who must conform to the new practice. The M-set gap (between
 *   divine command and practice) is acknowledged as closed due to exogenous
 *   pressure, and the authority structure admits non-revelatory grounds for
 *   doctrinal shift, creating a potential legitimacy crisis if coercion is
 *   seen as a valid input for theology.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.68).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.85).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "Divine Marriage Command (Coercion Visibility Reading)").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, 'ce907fa7-20c4-41bd-b84d-6546a4e94bb1').
narrative_ontology:cs_kernel_codification('ce907fa7-20c4-41bd-b84d-6546a4e94bb1', fixed_text).
narrative_ontology:cs_authority_grounding('ce907fa7-20c4-41bd-b84d-6546a4e94bb1', lineage).
narrative_ontology:cs_interpretation_layer_present('ce907fa7-20c4-41bd-b84d-6546a4e94bb1').
narrative_ontology:cs_reading_relation('ce907fa7-20c4-41bd-b84d-6546a4e94bb1', divine_marriage_command__continuationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce907fa7-20c4-41bd-b84d-6546a4e94bb1', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_axiom('ce907fa7-20c4-41bd-b84d-6546a4e94bb1', foundational, institutional_survival_is_paramount).
narrative_ontology:cs_axiom_status(institutional_survival_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('ce907fa7-20c4-41bd-b84d-6546a4e94bb1', institutional_survival_is_paramount, instrumental).
narrative_ontology:cs_axiom('ce907fa7-20c4-41bd-b84d-6546a4e94bb1', foundational, exogenous_coercion_can_alter_practice).
narrative_ontology:cs_axiom_status(exogenous_coercion_can_alter_practice, holdable).
narrative_ontology:cs_axiom_grounding('ce907fa7-20c4-41bd-b84d-6546a4e94bb1', exogenous_coercion_can_alter_practice, conventional).
narrative_ontology:cs_reference_frame('ce907fa7-20c4-41bd-b84d-6546a4e94bb1', doctrinal_pragmatism_for_survival).
narrative_ontology:cs_drift_state('ce907fa7-20c4-41bd-b84d-6546a4e94bb1', post_manifesto_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('ce907fa7-20c4-41bd-b84d-6546a4e94bb1', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, religious_institution).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, conforming_members).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, dissenting_members).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, polygamous_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority that issued the Manifesto, acknowledging federal coercion and justifying the doctrinal shift as necessary for institutional survival. It benefits from continued existence and legal recognition.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, religious_institution, agenda_setter,
    institutional, generational, constrained, global).

% The external authority that applied coercive pressure (laws, confiscation, imprisonment) against polygamous practices, forcing the religious institution to adapt its doctrine and practice to survive.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, federal_government, agenda_setter,
    institutional, biographical, arbitrage, national).

% Members who accepted the doctrinal shift and conformed to monogamous practices. They benefit from the stability and continued existence of the religious institution, albeit at the cost of abandoning prior practices.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, conforming_members, beneficiary,
    moderate, biographical, constrained, local).

% Members who struggled with or rejected the doctrinal shift, feeling it compromised core tenets. They bear the cost of internal conflict, social pressure, or potential excommunication, with limited options for maintaining their prior practices within the institution.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, dissenting_members, payer,
    powerless, biographical, trapped, local).

% Families directly impacted by the abandonment of polygamous practices, facing the dissolution of existing family structures or the necessity of living in secrecy. They bore the most direct and severe costs of the coercion-driven doctrinal change.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, polygamous_families, payer,
    powerless, biographical, trapped, local).

% Academics and independent thinkers who analyze the historical and theological implications of the Manifesto, particularly the role of coercion in shaping religious doctrine and the nature of institutional legitimacy.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, theological_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure the legal and physical survival of the religious institution by aligning its public practices with federal law, thereby preserving its community and organizational structure.
% TRANSFER_FUNCTION: Transfers the burden of legal non-compliance and the risk of institutional dissolution from the religious leadership to its members, who must abandon polygamous practices. It also shifts the theological grounding from direct revelation to pragmatic necessity for survival.
% ABSENT_VOICES: Those who refused to abandon polygamous practices and were excommunicated or formed splinter groups. They would argue that the institution capitulated to illegitimate external pressure and betrayed divine commands, but their voices are marginalized by the dominant narrative of survival.
% DISAPPEARANCE_RATIONALE: If the theological justification for abandoning polygamy due to coercion vanished, the institution's historical narrative would face a profound crisis. It would likely lead to a re-evaluation of its foundational claims, potentially causing schisms, and a re-emergence of polygamous practices among some adherents, fundamentally reorganizing the community.
% FOUNDING_PROBLEM: The existential threat to the religious institution posed by federal anti-polygamy legislation, which included confiscation of property, disenfranchisement, and imprisonment of its leaders and members, threatening its very existence.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, federal court decisions, and independent historians corroborate the severe federal coercion faced by the institution. While the direct legal threat of the past is gone, the historical necessity of the response is still invoked by the institution, and its impact on doctrine is a live theological question for scholars.
narrative_ontology:disappearance_verdict(divine_marriage_command__coercion_visibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__coercion_visibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(divine_marriage_command__coercion_visibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__coercion_visibility_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__coercion_visibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__coercion_visibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (institutional survival) but involves significant asymmetric extraction from members who had to abandon polygamous practices. Extractiveness is high (0.68) due to the profound personal and familial costs of this shift. Suppression is very high (0.85) due to both the initial federal coercion and the subsequent internal enforcement of the new norm, leaving little room for dissent. Theater ratio is relatively low (0.25) because this reading explicitly acknowledges the external coercion, reducing the need for purely performative justifications of the doctrinal change.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the religious institution, this constraint is a necessary, albeit painful, act of coordination for survival. From the perspective of dissenting members and polygamous families, it is a highly extractive and suppressive imposition, driven by external force and justified by a pragmatic theological shift that compromises core beliefs. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious institution is a primary beneficiary, securing its survival and legal standing. Conforming members also benefit from the institution's stability. Dissenting members and polygamous families are clear targets, bearing the direct costs of doctrinal change and forced conformity. The federal government acts as an external agenda-setter, applying the coercive force that drives the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_as_theological_input,
    'Is exogenous coercion a legitimate input for shaping theological doctrine, or does it fundamentally compromise the doctrine''s divine authority?',
    'Comparative theological analysis across traditions that have faced similar coercive pressures, examining the long-term impacts on doctrinal integrity and institutional legitimacy.',
    'If coercion is deemed an illegitimate input, this reading''s theological grounding is weakened, potentially reclassifying the constraint as a Snare (pure extraction under duress) rather than a Tangled Rope (pragmatic coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_as_theological_input, conceptual, 'Ambiguity regarding the theological validity of coercion-driven doctrinal shifts.').

omega_variable(
    doctrinal_integrity_vs_survival,
    'To what extent did prioritizing institutional survival compromise the core doctrinal integrity of the divine marriage command?',
    'Analysis of internal theological debates and dissenting voices from the period, alongside later doctrinal developments, to assess the perceived continuity or rupture with prior revelation.',
    'If compromise was severe, the extraction from dissenting members is amplified, and the coordination function (survival) is seen as having come at an unacceptable theological cost, pushing the classification closer to Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_integrity_vs_survival, conceptual, 'The trade-off between institutional survival and doctrinal fidelity.').

omega_variable(
    m_set_gap_closure_validity,
    'Is the acknowledged closure of the M-set gap (between divine command and practice) genuinely theological, or primarily a pragmatic concession to external power?',
    'Examination of the theological arguments presented at the time and their reception by the broader religious community, compared against the explicit statements of coercion.',
    'If primarily pragmatic, the ''theological legitimacy'' claim is weakened, increasing the effective extractiveness and suppression, as the justification becomes less about divine will and more about power dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m_set_gap_closure_validity, empirical, 'The true nature of the M-set gap closure: theological or pragmatic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__coercion_visibility_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(divi_tr_t5, divine_marriage_command__coercion_visibility_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(divi_tr_t10, divine_marriage_command__coercion_visibility_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(divi_tr_t15, divine_marriage_command__coercion_visibility_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(divi_tr_t20, divine_marriage_command__coercion_visibility_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(divi_tr_t25, divine_marriage_command__coercion_visibility_reading, theater_ratio, 25, 0.25).
narrative_ontology:measurement(divi_tr_t30, divine_marriage_command__coercion_visibility_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(divi_be_t5, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(divi_be_t10, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(divi_be_t15, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 15, 0.69).
narrative_ontology:measurement(divi_be_t20, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(divi_be_t25, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(divi_be_t30, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(divi_su_t5, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 5, 0.8).
narrative_ontology:measurement(divi_su_t10, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(divi_su_t15, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 15, 0.83).
narrative_ontology:measurement(divi_su_t20, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 20, 0.84).
narrative_ontology:measurement(divi_su_t25, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 25, 0.85).
narrative_ontology:measurement(divi_su_t30, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__coercion_visibility_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'divine_marriage_command' kernel, each representing a distinct structural interpretation of the Manifesto's impact on doctrine and practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
