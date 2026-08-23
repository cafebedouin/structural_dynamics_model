% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: LDS 1890 Manifesto: Endogenous Prophetic Reinterpretation of Plural Marriage Suspension
 *   domain: religious/institutional/political
 *
 * SUMMARY:
 *   The 1890 Manifesto suspended plural marriage in the LDS church. This
 *   reading treats the manifesto as legitimate prophetic reinterpretation:
 *   God revealed a temporal suspension to preserve the church's salvific
 *   mission. The kernel is contested â the exogenous reading attributes the
 *   change to federal coercion, and the pragmatism reading treats the
 *   revelation narrative as legitimation for survival-driven capitulation.
 *   This constraint story isolates the endogenous reading as a clean
 *   Îµ-invariant constraint. The beneficiary set is the church institution
 *   (legal survival, temple access, missionary expansion); the victim set is
 *   fundamentalist practitioners excommunicated for maintaining the original
 *   reading. The metrics and claim are authored independently: the claim is
 *   rope, reflecting the reading's own coordination framing, while the
 *   metrics describe the constraint's actual operation including moderate
 *   extraction and active enforcement.
 *
 * KEY AGENTS:
 *   - prophetic_leadership: agenda_setter (institutional/arbitrage) â issues and enforces the manifesto
 *   - church_institution: beneficiary (institutional/constrained) â gains legal survival and operational continuity
 *   - fundamentalist_practitioners: payer (moderate/identity_locked) â bear excommunication costs for maintaining prior practice
 *   - federal_government: excluded (institutional/analytical) â external pressure agent backgrounded in this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.42).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.55).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "LDS 1890 Manifesto: Endogenous Prophetic Reinterpretation of Plural Marriage Suspension").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious/institutional/political").

domain_priors:requires_active_enforcement(plural_marriage_mandate__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, '07724054-d392-496a-b060-c4487540e9a5').
narrative_ontology:cs_kernel_codification('07724054-d392-496a-b060-c4487540e9a5', fixed_text).
narrative_ontology:cs_authority_grounding('07724054-d392-496a-b060-c4487540e9a5', lineage).
narrative_ontology:cs_interpretation_layer_present('07724054-d392-496a-b060-c4487540e9a5').
narrative_ontology:cs_reading_relation('07724054-d392-496a-b060-c4487540e9a5', plural_marriage_mandate__exogenous_override_reading, influences).
narrative_ontology:cs_reading_relation('07724054-d392-496a-b060-c4487540e9a5', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('07724054-d392-496a-b060-c4487540e9a5', foundational, prophetic_prerogative_to_suspend).
narrative_ontology:cs_axiom_status(prophetic_prerogative_to_suspend, holdable).
narrative_ontology:cs_axiom_grounding('07724054-d392-496a-b060-c4487540e9a5', prophetic_prerogative_to_suspend, theological).
narrative_ontology:cs_axiom('07724054-d392-496a-b060-c4487540e9a5', secondary, salvific_preservation_mandate).
narrative_ontology:cs_axiom_status(salvific_preservation_mandate, holdable).
narrative_ontology:cs_axiom_grounding('07724054-d392-496a-b060-c4487540e9a5', salvific_preservation_mandate, theological).
narrative_ontology:cs_reference_frame('07724054-d392-496a-b060-c4487540e9a5', prophetic_continuity_framework).
narrative_ontology:cs_drift_state('07724054-d392-496a-b060-c4487540e9a5', post_manifesto_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('07724054-d392-496a-b060-c4487540e9a5', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 Manifesto as a prophetic proclamation suspending plural marriage. They administer the constraint by receiving and declaring divine will, authorizing church courts to excommunicate non-compliant members, and directing the global church toward legal conformity and monogamous marriage standards.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, prophetic_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Gains federal legal recognition, retains temple properties, and expands missionary work globally by complying with the monogamy requirement. The institutional body depends on the manifesto for its continued corporate existence and mainstream social acceptance.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution, beneficiary,
    institutional, generational, constrained, global).

% Continued contracting plural marriages after 1890 based on prior prophetic teachings and theological conviction. They face excommunication, loss of temple recommend privileges, and social ostracism from the main body, bearing the primary cost of the practice shift.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_practitioners, payer,
    moderate, biographical, identity_locked, regional).

% Passed and enforced the Edmunds-Tucker Act and related anti-polygamy legislation, creating the legal pressure that preceded the manifesto. In this endogenous reading, they are the external political context rather than the causal agent of the change, and their voice is excluded from the church's theological narrative.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, federal_government, excluded,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the global LDS community around a unified marriage practice compliant with federal law, preserving collective access to temples, missionary work, and legal corporate existence under a single prophetic directive.
% TRANSFER_FUNCTION: Moves compliance obligation from plural marriage practitioners to monogamy norms; transfers the cost of doctrinal continuity onto fundamentalist practitioners who are excommunicated, while the institutional center gains legal legitimacy and property security.
% ABSENT_VOICES: Fundamentalist practitioners who continued plural marriage were excommunicated and thus excluded from institutional deliberation; federal officials who might claim credit for the change are backgrounded in this theological reading.
% DISAPPEARANCE_RATIONALE: If the manifesto and its enforcement vanished, the LDS church would face renewed federal pressure and potential property confiscation, fundamentalist practitioners would likely reassert plural marriage within the main body, and the global missionary and temple system would require rapid legal and political repositioning.
% FOUNDING_PROBLEM: The LDS church faced existential legal and political threat in the 1880s due to federal anti-polygamy legislation, risking confiscation of temples, dissolution of the corporate church, and imprisonment of leaders.
% FOUNDING_PROBLEM_CORROBORATION: Federal congressional records and non-LDS historians (e.g., Sarah Barringer Gordon, D. Michael Quinn) attest to the existential legal pressure, while the church's theological narrative attributes the change primarily to revelation. Outside historians corroborate the threat; the revelatory causal mechanism is attested only by the benefiting parties.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__endogenous_reinterpretation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).
:- end_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate because the constraint shifts substantial religious and social costs onto fundamentalists while delivering institutional benefits to the church center. Suppression (0.55) reflects active disciplinary enforcement (excommunication) required to maintain the new practice boundary. Theater ratio (0.28) captures the ceremonial and narrative maintenance of the prophetic channel, which is partly functional and partly performative. Accessibility collapse (0.48) is moderate: alternatives (schismatic groups) persist but are socially and religiously costly. Resistance (0.52) reflects the fundamentalist schism and underground plural marriage persistence. The measurement series show enforcement ratcheting around the Reed Smoot hearings and Second Manifesto, then stabilizing.
 *
 * PERSPECTIVAL GAP:
 *   From the prophetic leadership and church institution seats, the manifesto is legitimate coordination preserving the collective enterprise. From the fundamentalist practitioner seat, it is an extractive break with prior revelation that excommunicates the faithful. The engine computes this divergence from the same structural data: low directionality for the institution, high directionality for the fundamentalists.
 *
 * DIRECTIONALITY LOGIC:
 *   The church institution is the beneficiary (d near 0.0) because the constraint subsidizes its legal existence and global expansion. Fundamentalist practitioners are the target (d near 1.0) because they bear the costs of exclusion and identity rupture. Prophetic leadership sits low-moderate d: they administer the constraint and benefit from institutional stability, but are also bound by their own revelatory claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim reflects the reading's framing that the manifesto coordinates the body around a new divine directive. However, the founding problem (1880s federal threat) was largely resolved by 1910, while the constraint persisted. The R5 genealogy interview (founding_problem_status contested) flags this tension without resolving it. The mandatrophy_resolved flag is not set because the reading's own theological framework (ongoing revelation) provides a live justification that competes with the historical-obsolescence narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_vs_exogenous_causality,
    'Is the 1890 Manifesto best explained by endogenous prophetic revelation or exogenous federal coercion?',
    'Historical analysis of private correspondence, congressional timing, and pre-manifesto church leadership deliberations.',
    'If exogenous causality is dominant, the constraint''s legitimacy as rope dissolves and it reclassifies toward snare or tangled rope; if endogenous, the rope classification is structurally supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_vs_exogenous_causality, empirical, 'Whether the manifesto was caused by revelation or federal pressure').

omega_variable(
    temporal_suspension_permanence,
    'A temporal suspension revealed in 1890 has persisted for generations. Does the ''temporal'' framing retain meaning, or has the constraint become permanent through institutional inertia?',
    'Comparative analysis of prophetic announcements and doctrinal shifts from 1890 through the 1904 Second Manifesto to contemporary church policy.',
    'If the suspension has become permanent by inertia rather than ongoing revelation, the constraint may have drifted from rope toward scaffold or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_suspension_permanence, conceptual, 'Whether the temporal suspension has become a permanent institutional fixture').

omega_variable(
    doctrine_practice_retention_gap,
    'Does the retention of plural marriage doctrine while suspending practice create a persistent extractive potential that can be reactivated against non-compliant members?',
    'Analysis of disciplinary patterns and rhetorical activation of the doctrine in subsequent decades.',
    'If doctrine retention preserves a latent threat, the constraint may be more extractive than a pure coordination rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_retention_gap, conceptual, 'Whether retained doctrine creates latent extractive potential').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plural_marriage_endog_tr_t0, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(plural_marriage_endog_tr_t5, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(plural_marriage_endog_tr_t10, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(plural_marriage_endog_tr_t15, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(plural_marriage_endog_tr_t20, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(plural_marriage_endog_tr_t25, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(plural_marriage_endog_tr_t30, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(plural_marriage_endog_be_t0, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(plural_marriage_endog_be_t5, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(plural_marriage_endog_be_t10, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(plural_marriage_endog_be_t15, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(plural_marriage_endog_be_t20, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(plural_marriage_endog_be_t25, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 25, 0.43).
narrative_ontology:measurement(plural_marriage_endog_be_t30, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(plural_marriage_endog_su_t0, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(plural_marriage_endog_su_t5, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(plural_marriage_endog_su_t10, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(plural_marriage_endog_su_t15, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(plural_marriage_endog_su_t20, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(plural_marriage_endog_su_t25, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(plural_marriage_endog_su_t30, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% The plural_marriage_mandate kernel decomposes into three structurally distinct claims: endogenous prophetic reinterpretation (this file), exogenous federal coercion, and institutional pragmatism. Each reading carries a different epsilon, beneficiary/victim structure, and causal mechanism. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
