% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__practice_decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__practice_decline_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__practice_decline_reading
 *   human_readable: Honor Satisfaction Substrate â Practice Decline Reading
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   The honor satisfaction substrate â the normative code governing
 *   interpersonal offense and restoration among elites â persisted as a
 *   coordination mechanism even after the practice of dueling declined under
 *   legal prohibition and institutional barriers. This reading treats the
 *   constraint as a rope (coordination device under external pressure) rather
 *   than a mountain (inevitable feature of masculine society) or a degraded
 *   snare. The kernel is contested: other readings argue the honor code
 *   itself transformed endogenously into a dignity culture
 *   (cultural_contraction_reading) or that multiple causes operated
 *   non-independently (composite_overdetermined_reading). Only the
 *   practice_decline reading is authored here.
 *
 * KEY AGENTS:
 *   - gentleman_class: Primary beneficiary (powerful/constrained) â coordinated by the code, bears normative obligations
 *   - military_officers: Secondary beneficiary (organized/constrained) â institutional inheritors maintaining attenuated codes
 *   - legal_state: External observer (institutional/analytical) â enforces against dueling practice while preserving substrate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.35).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.45).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Honor Satisfaction Substrate â Practice Decline Reading").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "historical_sociology/cultural_anthropology/legal_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, '8c368d7e-1dfd-403b-9119-59077717d6bc').
narrative_ontology:cs_kernel_codification('8c368d7e-1dfd-403b-9119-59077717d6bc', distributed).
narrative_ontology:cs_authority_grounding('8c368d7e-1dfd-403b-9119-59077717d6bc', practice).
narrative_ontology:cs_interpretation_layer_present('8c368d7e-1dfd-403b-9119-59077717d6bc').
narrative_ontology:cs_reading_relation('8c368d7e-1dfd-403b-9119-59077717d6bc', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c368d7e-1dfd-403b-9119-59077717d6bc', honor_satisfaction_substrate__composite_overdetermined_reading, coexists_with).
narrative_ontology:cs_axiom('8c368d7e-1dfd-403b-9119-59077717d6bc', foundational, honor_code_normative_autonomy).
narrative_ontology:cs_axiom_status(honor_code_normative_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('8c368d7e-1dfd-403b-9119-59077717d6bc', honor_code_normative_autonomy, conventional).
narrative_ontology:cs_axiom('8c368d7e-1dfd-403b-9119-59077717d6bc', foundational, exogenous_practice_suppression).
narrative_ontology:cs_axiom_status(exogenous_practice_suppression, holdable).
narrative_ontology:cs_axiom_grounding('8c368d7e-1dfd-403b-9119-59077717d6bc', exogenous_practice_suppression, empirically_contingent).
narrative_ontology:cs_reference_frame('8c368d7e-1dfd-403b-9119-59077717d6bc', classical_gentlemanly_honor).
narrative_ontology:cs_drift_state('8c368d7e-1dfd-403b-9119-59077717d6bc', post_legal_prohibition_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8c368d7e-1dfd-403b-9119-59077717d6bc', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, gentleman_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, military_officers).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__practice_decline_reading, social_order_through_reciprocity).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__practice_decline_reading, monopoly_on_violence_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elite social stratum whose interpersonal disputes were regulated by the code of honor. They benefited from predictable rules for insult, satisfaction, and reputation management that prevented chaotic violence and maintained class boundaries. Legal prohibition of dueling increased the cost of compliance without providing an honor-compatible alternative, leaving them in a coordination trap where the normative demands persisted but the sanctioned outlet disappeared.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, gentleman_class, beneficiary,
    powerful, generational, constrained, national).

% Institutional inheritors of the honor substrate who maintained attenuated dueling codes and point-of-honor regulations within military justice. They benefited from the cohesion and command legitimacy derived from shared honor norms, even as civilian dueling became legally untenable.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, military_officers, beneficiary,
    organized, biographical, constrained, national).

% Monopolized legitimate violence through legal prohibition of dueling and institutional barriers to private satisfaction. Observed the honor code from outside, prosecuting violations while inadvertently preserving the normative substrate by suppressing only the practice and leaving the coordinative logic intact.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, legal_state, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured, reciprocal framework for elite males to resolve disputes and maintain social standing without unregulated violence or total social breakdown; creates predictable expectations about insult, response, and restoration within a bounded status group.
% TRANSFER_FUNCTION: Moves social standing, reputation, and obligation among members of the gentleman class and military officer corps. Those who meet the code maintain or gain standing; those who fail lose it. No material transfer accrues to a central authority.
% ABSENT_VOICES: Women, lower-class persons, and legal modernizers who rejected violence-based dispute resolution were formally excluded from the honor-based coordination; their exclusion was constitutive of the gentleman-class boundary.
% DISAPPEARANCE_RATIONALE: If the honor code substrate disappeared overnight, military discipline codes would lose their moral grounding, attenuated cultures of honor would collapse, and the gentleman class would lose its mechanism for reputational coordination; disputes would shift entirely to legal or material domains, and the social identity of the affected classes would require reconstruction.
% FOUNDING_PROBLEM: How to regulate interpersonal violence and insult among armed, status-conscious elites such that disputes do not escalate into feuds or social chaos, while preserving the class boundary against non-elite intrusion.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and sociologists outside the gentleman class attest that state monopoly on violence and court systems have replaced the honor code's dispute-resolution function; military historians note that modern military justice now handles offenses that once triggered duels. No corroboration from within the beneficiary set is required because the problem is acknowledged as solved by external institutional development.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__practice_decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__practice_decline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__practice_decline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__practice_decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__practice_decline_reading, 0.35, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__practice_decline_reading_tests).
:- end_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.35 at interval end) because the honor code's primary function is coordination among elites, not extraction by a centralized beneficiary. Suppression is moderate (0.45) because the code enforces itself through social sanction and class-boundary maintenance; legal prohibition intensifies the gap between normative demand and permitted practice, raising the social cost of non-compliance. Theater_ratio rises to 0.45 by 1900 as dueling becomes increasingly performative â challenges issued but not fought, satisfaction channeled into non-lethal forms â indicating the coordination function is maintained through gesture rather than practice. Resistance is moderate-low (0.30) because the coordinated class largely accepted the code, with resistance appearing mainly from modernizers outside the class. All metrics share a single time grid.
 *
 * PERSPECTIVAL GAP:
 *   The gentleman-class seat experiences the constraint as protective coordination that preserves social order and class boundaries; from the legal-state seat it appears as an obsolete violence mechanism requiring external suppression. The military-officer seat experiences it as institutional identity maintenance. These divergences are structural, not perspectival illusions â the engine computes different per-seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Both beneficiary seats (gentleman_class, military_officers) sit near the beneficiary end of directionality: they are net beneficiaries of the coordination function, though they bear symmetric costs (risk, obligation, opportunity cost). The legal_state is structurally external (analytical observer) with no directionality toward the constraint itself; it benefits from monopoly on violence but does not extract through the honor code. No victim seat is declared because the constraint operates as rope: costs are symmetrically distributed among the coordinated class rather than extracted asymmetrically by one party from another.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by distinguishing the honor code (coordination substrate) from dueling (one practice instantiating it). Dueling declined; the code did not. A reading that conflates the two would misclassify the remaining substrate as a piton or snare. By treating the code as persisting coordination under legal pressure, the rope classification captures the genuine coordinative function while acknowledging the practice's decline was exogenous, not endogenous. The founding problem â regulating elite violence â is dead, solved by state institutions, but the arrangement persists because the substrate was never dependent on the practice alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the practice_decline_reading of the honor_satisfaction_substrate kernel. Would adopting the cultural_contraction_reading reclassify the constraint as a scaffold or piton rather than a rope?',
    'Comparative historiographical analysis tracing whether normative statements about honor retained performative force or became mere residue; content analysis of etiquette manuals and military regulations across the interval.',
    'If the cultural_contraction reading is adopted, the constraint would be reclassified with higher theater_ratio and extractiveness, potentially as a piton where the coordination function has atrophied and only theatrical maintenance remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Sibling reading would change classification toward piton or scaffold').

omega_variable(
    exogenous_enforcement_magnitude,
    'To what extent did legal prohibition actually suppress dueling versus drive it underground or displace it regionally?',
    'Archival police records, court prosecutions, newspaper accounts of duels, and regional comparative studies measuring practice frequency before and after prohibition.',
    'If prohibition was ineffective and dueling declined primarily due to norm change, the rope classification weakens and the constraint approaches the cultural_contraction reading or mountain erosion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_enforcement_magnitude, empirical, 'Whether legal suppression was the actual cause of practice decline').

omega_variable(
    coordination_benefit_symmetry,
    'Was the honor code''s coordination benefit symmetrically distributed among the gentleman class, or did it disproportionately protect high-status members while exposing marginal gentry to lethal risk?',
    'Prosopographical analysis of duel participants and outcomes by wealth, title, and family status; measurement of social mobility consequences for survivors versus non-participants.',
    'If benefit was strongly asymmetric, the constraint is better classified as tangled_rope (genuine coordination plus asymmetric extraction) than as rope, and a victim seat should be added.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_benefit_symmetry, empirical, 'Symmetry of coordination benefit within the gentleman class').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_practice_decline_tr_t0, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(honor_practice_decline_tr_t30, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(honor_practice_decline_tr_t60, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(honor_practice_decline_tr_t90, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 90, 0.28).
narrative_ontology:measurement(honor_practice_decline_tr_t120, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 120, 0.36).
narrative_ontology:measurement(honor_practice_decline_tr_t150, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 150, 0.45).

% Extraction over time
narrative_ontology:measurement(honor_practice_decline_be_t0, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(honor_practice_decline_be_t30, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 30, 0.26).
narrative_ontology:measurement(honor_practice_decline_be_t60, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 60, 0.28).
narrative_ontology:measurement(honor_practice_decline_be_t90, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 90, 0.3).
narrative_ontology:measurement(honor_practice_decline_be_t120, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 120, 0.33).
narrative_ontology:measurement(honor_practice_decline_be_t150, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 150, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(honor_practice_decline_su_t0, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(honor_practice_decline_su_t30, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement(honor_practice_decline_su_t60, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 60, 0.25).
narrative_ontology:measurement(honor_practice_decline_su_t90, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 90, 0.32).
narrative_ontology:measurement(honor_practice_decline_su_t120, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 120, 0.38).
narrative_ontology:measurement(honor_practice_decline_su_t150, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 150, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
