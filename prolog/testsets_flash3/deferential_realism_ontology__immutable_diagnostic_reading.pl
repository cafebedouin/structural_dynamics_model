% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__immutable_diagnostic_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Deferential Realism Ontology: Immutable Diagnostic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint represents the 'immutable diagnostic' reading of the
 *   Deferential Realism ontology. It asserts that the typology is an
 *   objective observational instrument, with fixed referents for each
 *   constraint type (e.g., mountains are physical invariants, snares are
 *   measurable extraction mechanisms). Misclassification is treated as an
 *   error correctable through better observation, rather than a matter of
 *   normative judgment or rhetorical framing. This reading actively
 *   suppresses alternative interpretations that emphasize the constructed or
 *   rhetorical nature of classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.15).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.7).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, mountain).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Deferential Realism Ontology: Immutable Diagnostic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:emerges_naturally(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, '29746470-e722-40bb-b111-0a24c5410508').
narrative_ontology:cs_kernel_codification('29746470-e722-40bb-b111-0a24c5410508', formalized).
narrative_ontology:cs_authority_grounding('29746470-e722-40bb-b111-0a24c5410508', expertise).
narrative_ontology:cs_interpretation_layer_present('29746470-e722-40bb-b111-0a24c5410508').
narrative_ontology:cs_reading_relation('29746470-e722-40bb-b111-0a24c5410508', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('29746470-e722-40bb-b111-0a24c5410508', deferential_realism_ontology__hybrid_pragmatic_reading, forecloses).
narrative_ontology:cs_axiom('29746470-e722-40bb-b111-0a24c5410508', foundational, classification_is_objective_discovery).
narrative_ontology:cs_axiom_status(classification_is_objective_discovery, holdable).
narrative_ontology:cs_axiom_grounding('29746470-e722-40bb-b111-0a24c5410508', classification_is_objective_discovery, empirically_contingent).
narrative_ontology:cs_axiom('29746470-e722-40bb-b111-0a24c5410508', foundational, epsilon_is_fixed_referent).
narrative_ontology:cs_axiom_status(epsilon_is_fixed_referent, holdable).
narrative_ontology:cs_axiom_grounding('29746470-e722-40bb-b111-0a24c5410508', epsilon_is_fixed_referent, empirically_contingent).
narrative_ontology:cs_reference_frame('29746470-e722-40bb-b111-0a24c5410508', positivist_social_science_ideal).
narrative_ontology:cs_drift_state('29746470-e722-40bb-b111-0a24c5410508', contemporary_postmodern_critique, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('29746470-e722-40bb-b111-0a24c5410508', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, policy_makers).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, critical_theorists).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, objective_social_science).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, epistemic_realism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adhere to this reading, believing the typology provides objective diagnostic tools. They resolve classification disputes by appealing to observable metrics and treat epsilon values as discoverable facts. Their professional identity is tied to the framework's diagnostic immutability.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_practitioners, agenda_setter,
    institutional, generational, identity_locked, global).

% Find this reading overly positivist, arguing that it suppresses normative and rhetorical dimensions of classification. They bear the cost of having their alternative framings dismissed as 'misclassification' rather than legitimate alternative readings.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, critical_theorists, payer,
    moderate, generational, mobile, global).

% Benefit from the perceived objectivity and diagnostic clarity this reading offers, using its classifications to justify interventions or resist critiques. They appreciate a framework that claims to provide 'correct' answers.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, policy_makers, beneficiary,
    powerful, biographical, constrained, national).

% Advocate for a more nuanced view where some classifications are objective and others are normative. They are excluded from the 'pure' diagnostic discourse, their positions often framed as compromising the framework's rigor.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, hybrid_pragmatists, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, objective language for diagnosing social constraints, enabling practitioners to coordinate on identifying and analyzing structural problems with a common, fixed referent.
% TRANSFER_FUNCTION: Transfers epistemic authority from subjective interpretation to objective observation, channeling intellectual effort towards empirical measurement and away from normative debate about classification itself.
% ABSENT_VOICES: Proponents of the 'rhetorical scaffold' and 'hybrid pragmatic' readings are actively suppressed or dismissed within this framework, as their views challenge the core axiom of objective, immutable classification. They would argue for the constructed or normative dimensions of the typology.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the entire field of Deferential Realism would lose its foundational claim to objective diagnosis. Classification disputes would immediately become overt normative debates, and the framework's perceived authority would collapse, forcing a re-evaluation of its entire methodology.
% FOUNDING_PROBLEM: The problem of subjective bias and rhetorical manipulation in social theory, where classifications were seen as arbitrary or serving political agendas rather than reflecting underlying reality.
% FOUNDING_PROBLEM_CORROBORATION: Practitioners attest the problem is live, citing ongoing debates in social science. Critics (e.g., critical theorists) argue that while the problem is real, this reading's solution oversimplifies the relationship between observation and normativity, and that the framework itself can be used to suppress dissent.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, ExtMetricName, E),
    domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(deferential_realism_ontology__immutable_diagnostic_reading),
    narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) reflects the claim that the framework itself is a neutral diagnostic tool, not designed for rent-seeking. However, suppression is high (0.70) because this reading actively dismisses and marginalizes alternative framings that challenge its core premise of objective classification. Accessibility collapse is high (0.80) because once this reading is adopted, alternative interpretive paths are largely foreclosed. Resistance is low (0.10) from within the framework, as dissent is framed as 'misunderstanding' rather than legitimate critique. The claimed type is 'mountain' because this reading asserts the framework's diagnostic categories are as fixed and discoverable as natural laws.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of practitioners, this reading is a robust, objective tool (a Mountain). From the perspective of critical theorists, it functions as a Snare, trapping discourse in a positivist frame and extracting the legitimacy of alternative analyses. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Deferential Realism practitioners are beneficiaries and agenda-setters, as their professional identity and epistemic authority are grounded in this reading's claims of objectivity. Critical theorists are payers, as their alternative framings are suppressed. Policy makers are beneficiaries, as they gain a seemingly objective justification for their actions. Hybrid pragmatists are excluded, as their nuanced position is incompatible with the 'pure' diagnostic claim.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    objectivity_vs_normativity_boundary,
    'Is the distinction between ''objective observation'' and ''normative judgment'' as clear-cut as this reading asserts, or does classification inherently involve normative choices?',
    'Detailed case studies of classification disputes within the framework, analyzing whether resolution hinges on empirical data alone or on appeals to underlying values and legitimate beneficiaries.',
    'If normative choices are found to be inherent, the ''immutable diagnostic'' claim would be undermined, shifting the framework towards a ''hybrid pragmatic'' or even ''rhetorical scaffold'' classification, increasing its effective extractiveness and lowering its perceived naturalness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(objectivity_vs_normativity_boundary, conceptual, 'Ambiguity in the boundary between descriptive and prescriptive aspects of classification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative framings structural (e.g., institutional power of DR practitioners) or internalized (e.g., cognitive patterns among adherents that make alternative framings unthinkable)?',
    'Analysis of how new entrants to the field adopt or resist this reading: if resistance persists despite institutional pressure, suppression is more structural; if new entrants quickly internalize the ''objective'' framing, it''s more internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the ''target'' (critical theorists, hybrid pragmatists) carries the suppression with them even when institutional barriers are lowered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for epistemic alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(defe_tr_t15, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(defe_be_t15, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(defe_su_t15, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Deferential Realism Ontology' kernel. This 'immutable diagnostic' reading asserts objective classification, while sibling readings emphasize rhetorical or hybrid aspects. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
