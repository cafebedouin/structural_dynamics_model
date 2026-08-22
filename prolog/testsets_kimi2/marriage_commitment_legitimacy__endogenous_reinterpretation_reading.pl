% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
 *   human_readable: Manifesto as Genuine Prophetic Revelation: Endogenous Reinterpretation
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   The 1890 Manifesto ending plural marriage in the LDS Church, read as
 *   genuine prophetic revelation. This constraint story instantiates the
 *   endogenous_reinterpretation_reading of the marriage_commitment_legitimacy
 *   kernel. The reading holds that God commanded the reversal to preserve the
 *   Church for higher purposes, reframing monogamy not as capitulation but as
 *   a new covenant stage. Structural analysis from this reading's seat
 *   identifies genuine coordination (institutional survival) alongside
 *   asymmetric extraction (costs borne by practicing polygamists and their
 *   families), with the church hierarchy and faithful membership as
 *   beneficiaries of prophetic continuity.
 *
 * KEY AGENTS:
 *   - church_hierarchy (institutional/agenda_setter/beneficiary) â administers revelation and enforces compliance, derives legitimacy
 *   - practicing_polygamists (moderate/payer) â bear the cost of abandoning plural families under doctrinal reversal
 *   - faithful_membership (organized/beneficiary) â receive continuity and reduced persecution
 *   - splinter_fundamentalists (moderate/excluded) â rejected the Manifesto and exited to separatist communities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.22).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.48).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "Manifesto as Genuine Prophetic Revelation: Endogenous Reinterpretation").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, '7499ab32-5bb5-456b-97d3-69ce803aaa96').
narrative_ontology:cs_kernel_codification('7499ab32-5bb5-456b-97d3-69ce803aaa96', fixed_text).
narrative_ontology:cs_authority_grounding('7499ab32-5bb5-456b-97d3-69ce803aaa96', lineage).
narrative_ontology:cs_interpretation_layer_present('7499ab32-5bb5-456b-97d3-69ce803aaa96').
narrative_ontology:cs_reading_relation('7499ab32-5bb5-456b-97d3-69ce803aaa96', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('7499ab32-5bb5-456b-97d3-69ce803aaa96', marriage_commitment_legitimacy__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('7499ab32-5bb5-456b-97d3-69ce803aaa96', foundational, manifesto_genuine_revelation).
narrative_ontology:cs_axiom_status(manifesto_genuine_revelation, holdable).
narrative_ontology:cs_axiom_grounding('7499ab32-5bb5-456b-97d3-69ce803aaa96', manifesto_genuine_revelation, theological).
narrative_ontology:cs_axiom('7499ab32-5bb5-456b-97d3-69ce803aaa96', foundational, monogamy_new_covenant_stage).
narrative_ontology:cs_axiom_status(monogamy_new_covenant_stage, holdable).
narrative_ontology:cs_axiom_grounding('7499ab32-5bb5-456b-97d3-69ce803aaa96', monogamy_new_covenant_stage, theological).
narrative_ontology:cs_reference_frame('7499ab32-5bb5-456b-97d3-69ce803aaa96', prophetic_succession_legitimacy).
narrative_ontology:cs_drift_state('7499ab32-5bb5-456b-97d3-69ce803aaa96', post_splinter_consolidation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7499ab32-5bb5-456b-97d3-69ce803aaa96', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, faithful_membership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, practicing_polygamists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims and administers the Manifesto as binding prophetic revelation; enforces compliance through ecclesiastical discipline and excommunication; derives institutional continuity and prophetic succession legitimacy from member acceptance of the command.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_hierarchy, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_hierarchy, beneficiary).

% Entered plural marriage under prior doctrinal command; required to dissolve cohabitation and family structures or face excommunication and federal imprisonment; bear the direct relational, economic, and spiritual costs of the doctrinal reversal.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, practicing_polygamists, payer,
    moderate, biographical, constrained, regional).

% Accept the Manifesto as a new covenant stage; benefit from continued institutional existence, reduced federal persecution, and coherent prophetic narrative; their compliance is the substrate of the constraint's enforcement.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, faithful_membership, beneficiary,
    organized, generational, constrained, national).

% Reject the Manifesto's revelatory status; maintain plural marriage in separatist communities; their exclusion and excommunication is the boundary condition that enforces the constraint's theological coherence.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, splinter_fundamentalists, excluded,
    moderate, generational, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the institutional Church from federal dissolution by unifying the membership under a single marriage standard that satisfies the minimal conditions for territorial political accommodation, while maintaining prophetic authority as the source of the command.
% TRANSFER_FUNCTION: Transfers legitimacy and institutional continuity to the prophetic office and the generational membership, while transferring the relational and economic costs of abandoning plural family structures onto practicing polygamists.
% ABSENT_VOICES: Splinter fundamentalists who continued plural marriage and rejected the Manifesto's revelatory status were excommunicated and structurally excluded from the institutional conversation. Federal policymakers who viewed the Manifesto as mere political compliance rather than theological event were present in the historical environment but outside the commitment system's internal logic.
% DISAPPEARANCE_RATIONALE: Without the Manifesto as a binding prophetic command, the institutional basis for monogamous marriage in the Church collapses back into prior plural marriage practice or schism; federal pressure would force a different accommodation, and the prophetic succession would face a severe legitimacy crisis.
% FOUNDING_PROBLEM: The Church faced existential threat from federal anti-polygamy legislation, confiscation policies, and the denial of territorial statehood that conditioned institutional survival on abandoning plural marriage.
% FOUNDING_PROBLEM_CORROBORATION: Federal legislative records and territorial court cases corroborate the existential pressure; outside historians and political scientists attest to the federal coercion. The church hierarchy's own account of spiritual necessity is internal to the benefiting authority structure; no purely external theological corroboration exists without accepting the revelatory premise.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.22, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).
:- end_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.22 at interval end) because the primary function is coordination for institutional survival, though practicing polygamists bear real and asymmetric costs. Suppression is moderate (0.48) because federal pressure and ecclesiastical enforcement overlapped but the reading frames federal action as catalyst rather than cause. Theater ratio is low-moderate (0.18) because enforcement became partly performative after the existential federal threat faded, though never fully theatrical. Accessibility collapse is high (0.78) within the faithful epistemic framework: once the prophetic premise is accepted, doctrinal alternatives collapse. Resistance is moderate (0.42) due to splintering and underground persistence of plural marriage.
 *
 * PERSPECTIVAL GAP:
 *   The church hierarchy experiences the constraint as divinely authored coordination preserving legitimate authority; practicing polygamists experience it as a forced abandonment of covenant relationships under spiritual duress. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The church hierarchy and faithful membership are declared beneficiaries (low d), deriving continuity and legitimacy. Practicing_polygamists are declared victims/payers (high d), bearing the cost of practice abandonment. Federal pressure operates as external scope amplification but is not a seated beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal existential threat) is dead, yet the constraint persists as doctrine. The mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges signals potential mandatrophy. However, the reading interprets the persistence as legitimate ongoing revelation rather than zombie coordination. The metrics (low theater, low extraction) suggest the constraint has not decayed into pure piton performance, though temporal measurements show slight theater increase during enforcement consolidation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_coercion_origin,
    'Is the Manifesto''s origin genuinely prophetic revelation, or a theological reframing of institutional capitulation to federal coercion?',
    'Historical access to interior states of prophetic claimants is impossible; classification depends on evidentiary threshold for revelation claims and comparative analysis of timing relative to federal enforcement escalations.',
    'If classified as coercion, extractiveness rises substantially because members were forced to abandon plural marriage under false pretense of divine command, shifting the constraint toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_coercion_origin, conceptual, 'Ambiguity between divine and coercive origin of the Manifesto').

omega_variable(
    legitimacy_as_extraction,
    'Does the institutional benefit of prophetic succession legitimacy constitute extractive rent or non-extractive coordination good?',
    'Comparative analysis of authority costs versus member autonomy gains; assessment of whether legitimacy flows to a capturable seat or diffuses to the membership.',
    'If legitimacy is treated as concentrated rent, directionality for the hierarchy shifts toward target; if non-extractive coordination good, constraint moves toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_as_extraction, conceptual, 'Whether institutional legitimacy is extractive rent or coordination surplus').

omega_variable(
    enforcement_mechanism_ambiguity,
    'Was the enforcement of the Manifesto primarily driven by internal ecclesiastical discipline or external federal prosecution?',
    'Historical analysis of excommunication records versus federal prosecution rates before and after 1890.',
    'If primarily external, suppression is lower and the constraint''s directionality is less endogenously extractive; if internal, suppression reflects institutional coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_ambiguity, empirical, 'Internal versus external suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(marr_tr_t30, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(marr_tr_t40, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(marr_be_t30, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 30, 0.25).
narrative_ontology:measurement(marr_be_t40, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 40, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(marr_su_t30, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(marr_su_t40, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 40, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
