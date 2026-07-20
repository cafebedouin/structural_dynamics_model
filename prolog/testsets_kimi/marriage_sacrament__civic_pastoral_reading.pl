% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__civic_pastoral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__civic_pastoral_reading, []).

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
 *   constraint_id: marriage_sacrament__civic_pastoral_reading
 *   human_readable: Marriage as Pastoral Ideal with Compassionate Discernment
 *   domain: religious/doctrinal/political_sociology
 *
 * SUMMARY:
 *   This constraint story captures the civic_pastoral_reading of the
 *   marriage_sacrament kernel, in which marriage is treated as a pastoral
 *   relationship subject to human failure and indissolubility is held as an
 *   ideal realized through compassionate discernment rather than a juridical
 *   constant. The reading functions within Catholic magisterial practice to
 *   coordinate sacramental access for Catholics in irregular unions while
 *   imposing identity costs on traditional Catholics who depend on doctrinal
 *   stability. It is one of two contested readings of the same kernel; the
 *   sibling hierarchical_indissolubility_reading treats indissolubility as
 *   ontologically constitutive. The metrics and claimed type are authored
 *   independently: the constraint is claimed as tangled_rope because the
 *   pastoral coordination is structurally inseparable from the asymmetric
 *   extraction borne by traditionalist communities, and active enforcement
 *   (discernment) is required to hold the arrangement together.
 *
 * KEY AGENTS:
 *   - pastoral_hierarchy: Agenda-setter (institutional/constrained) â administers discernment and bears erosion costs
 *   - catholics_in_irregular_unions: Beneficiary (moderate/constrained) â gain sacramental access via individualized pastoral paths
 *   - traditional_catholics: Payer (organized/identity_locked) â bear doctrinal relativization and normative collapse costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.45).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.5).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Marriage as Pastoral Ideal with Compassionate Discernment").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious/doctrinal/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, 'f67dac1a-055c-401c-bfd2-e64fc77ff898').
narrative_ontology:cs_kernel_codification('f67dac1a-055c-401c-bfd2-e64fc77ff898', fixed_text).
narrative_ontology:cs_authority_grounding('f67dac1a-055c-401c-bfd2-e64fc77ff898', lineage).
narrative_ontology:cs_interpretation_layer_present('f67dac1a-055c-401c-bfd2-e64fc77ff898').
narrative_ontology:cs_reading_relation('f67dac1a-055c-401c-bfd2-e64fc77ff898', marriage_sacrament__hierarchical_indissolubility_reading, influences).
narrative_ontology:cs_axiom('f67dac1a-055c-401c-bfd2-e64fc77ff898', foundational, indissolubility_aspirational_not_constitutive).
narrative_ontology:cs_axiom_status(indissolubility_aspirational_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('f67dac1a-055c-401c-bfd2-e64fc77ff898', indissolubility_aspirational_not_constitutive, theological).
narrative_ontology:cs_axiom('f67dac1a-055c-401c-bfd2-e64fc77ff898', foundational, pastoral_discernment_as_merciful_authentication).
narrative_ontology:cs_axiom_status(pastoral_discernment_as_merciful_authentication, holdable).
narrative_ontology:cs_axiom_grounding('f67dac1a-055c-401c-bfd2-e64fc77ff898', pastoral_discernment_as_merciful_authentication, theological).
narrative_ontology:cs_reference_frame('f67dac1a-055c-401c-bfd2-e64fc77ff898', pastoral_marriage_ideal).
narrative_ontology:cs_drift_state('f67dac1a-055c-401c-bfd2-e64fc77ff898', contemporary_pastoral_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f67dac1a-055c-401c-bfd2-e64fc77ff898', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, catholics_in_irregular_unions).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_catholics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers marriage doctrine through diocesan tribunals and pastoral guidelines, exercising compassionate discernment over individual unions. Bears institutional costs of internal division and declining trust from traditional communities, yet retains formal sacramental governance authority.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, pastoral_hierarchy, agenda_setter,
    institutional, generational, constrained, global).

% Live in second unions or marriages without annulment and seek full participation in Eucharistic life and parish belonging. Gain access under the pastoral reading through individualized discernment rather than uniform juridical exclusion.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, catholics_in_irregular_unions, beneficiary,
    moderate, biographical, constrained, local).

% Uphold indissolubility as ontologically constitutive and experience the pastoral reading as a rupture with continuous tradition. Their communal and personal identities are organized around doctrinal permanence; exit from this framework means abandoning the Catholic self-understanding they have inherited.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_catholics, payer,
    organized, generational, identity_locked, global).

narrative_ontology:fixing_cost_class(marriage_sacrament__civic_pastoral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a pastoral mechanism for maintaining sacramental communion and community inclusion for Catholics whose marriages have failed, without requiring formal juridical annulment in every case.
% TRANSFER_FUNCTION: Moves doctrinal authority from fixed juridical norms to individualized pastoral discernment, transferring the costs of normative ambiguity to traditional Catholics while transferring sacramental access to those in irregular unions.
% ABSENT_VOICES: Theological absolutists and canonists who regard indissolubility as ontologically constitutive are formally part of the Church but increasingly sidelined in pastoral practice; their objections are heard as procedural noise rather than substantive governance input.
% DISAPPEARANCE_RATIONALE: If the pastoral reading vanished and strict hierarchical indissolubility were uniformly enforced, millions of remarried Catholics would face exclusion from communion, pastoral practice would recentralize on tribunals, and traditional Catholic communities would regain normative clarity â the global Church's sacramental and political configuration would shift substantially.
% FOUNDING_PROBLEM: How to maintain pastoral care and sacramental access for Catholics in failed marriages without appearing to abandon the Church's teaching on indissolubility.
% FOUNDING_PROBLEM_CORROBORATION: The pastoral hierarchy and progressive theologians attest the problem is pastoral care for the wounded. Traditional Catholic scholars and some canonists attest the problem was manufactured to justify doctrinal accommodation; they argue the original problem was adequately handled by the existing annulment system. Independent sociological studies of Catholic practice outside the benefiting parties document both the scale of irregular unions and the institutional strain caused by inconsistent responses.
narrative_ontology:disappearance_verdict(marriage_sacrament__civic_pastoral_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__civic_pastoral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_sacrament__civic_pastoral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__civic_pastoral_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__civic_pastoral_reading_tests).
:- end_tests(marriage_sacrament__civic_pastoral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the pastoral reading genuinely coordinates inclusion for irregular unions but simultaneously extracts doctrinal stability from traditional Catholics. Suppression (0.50) reflects the active enforcement needed to maintain the pastoral discretion framework against both traditionalist resistance and internal juridical confusion; it is not scaled by context. Theater ratio (0.45) captures the increasing performative gap between the stated ideal of indissolubility and the actual practice of widespread exceptions. Accessibility collapse (0.55) registers that once the pastoral frame is accepted, the absolute juridical alternative becomes cognitively unavailable for many pastoral agents, even though traditional communities preserve an alternative frame. Resistance (0.55) measures active traditionalist pushback (dubia, institutional noncompliance). Measurements share one time grid across all tracked metrics.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (irregular-union Catholics) experiences the constraint as merciful coordination that restores communion; the payer seat (traditional Catholics) experiences the identical structure as betrayal and extraction of the identity conditions that constitute their religious world. The pastoral hierarchy sits near symmetric: it coordinates genuine pastoral goods but loses normative capital with traditional constituencies. The engine computes this divergence from beneficiary/victim declarations and exit modulations.
 *
 * DIRECTIONALITY LOGIC:
 *   Catholics in irregular unions are declared beneficiaries with constrained exit (low d, subsidized by the constraint). Traditional Catholics are declared victims with identity_locked exit (high d, amplified extraction). The pastoral hierarchy is not declared in either base array, so directionality reverts to the institutional fallback (near symmetric), reflecting its dual role as both coordinator and administrator of a contested regime.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â how to include wounded Catholics without formal annulment â is contested and may be a post-hoc justification for doctrinal drift. The R5 status is contested, not dead, because the irregular-union problem is real; however, the specific pastoral mechanism may exceed the problem's scope. This prevents automatic piton classification: there is live coordination, not merely inertial theater. The metrics (moderate theater, moderate extraction) support tangled_rope over piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_authenticity,
    'Does the civic_pastoral reading represent authentic doctrinal development within the sacramental kernel, or a functionally new constraint substituting pastoral discretion for juridical structure?',
    'Historical analysis of magisterial continuity claims paired with sociological measurement of whether pastoral practice under this reading converges with or diverges from the kernel''s prior juridical interpretation.',
    'If it is a functionally new constraint, its epsilon should be evaluated independently of the kernel''s historical extraction profile; if authentic development, it inherits the kernel''s lower historical extraction baseline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_authenticity, conceptual, 'Whether the reading is development or substitution').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of traditionalist dissent under this reading structural (institutional marginalization, exclusion from tribunals) or internalized (traditionalists adopting self-doubt under pastoral framing)?',
    'Post-decision suppression trajectory: if traditionalist resistance persists or intensifies after formal institutional pressure is removed, suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure because traditionalists carry the constraint with them even where institutional enforcement is weak.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    authority_erosion_reversibility,
    'Can the erosion of institutional authority under this reading be reversed by reassertion of the hierarchical reading, or has the pastoral frame created irreversible institutional habit?',
    'Comparative case study of dioceses or jurisdictions that have attempted to revert to stricter juridical enforcement after pastoral experimentation.',
    'If irreversible, the constraint is drifting toward a stable new equilibrium with higher extraction than the current metric suggests; if reversible, the present metrics capture a transient tangled rope that could tighten or loosen.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_erosion_reversibility, empirical, 'Whether institutional drift under this reading is reversible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__civic_pastoral_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marr_tr_t5, marriage_sacrament__civic_pastoral_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(marr_tr_t10, marriage_sacrament__civic_pastoral_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(marr_tr_t15, marriage_sacrament__civic_pastoral_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(marr_tr_t20, marriage_sacrament__civic_pastoral_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(marr_tr_t25, marriage_sacrament__civic_pastoral_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(marr_tr_t30, marriage_sacrament__civic_pastoral_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(marr_be_t5, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(marr_be_t10, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(marr_be_t15, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(marr_be_t20, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(marr_be_t25, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(marr_be_t30, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(marr_su_t5, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 5, 0.25).
narrative_ontology:measurement(marr_su_t10, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(marr_su_t15, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(marr_su_t20, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(marr_su_t25, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(marr_su_t30, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, marriage_sacrament__hierarchical_indissolubility_reading).

% DUAL FORMULATION NOTE:
% This constraint and its sibling are two readings of the marriage_sacrament kernel. The civic_pastoral_reading derives moderate extractiveness from the pastoral function and higher identity costs for traditionalists; the hierarchical_indissolubility_reading derives its metrics from a juridical enforcement structure with different beneficiary and victim distributions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
