% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__vassal_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__vassal_coordination_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: feudal_oath_reciprocity__vassal_coordination_reading
 *   human_readable: Feudal Oath Reciprocity â Vassal Coordination Reading
 *   domain: medieval_political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the vassal-coordination reading of the
 *   feudal oath reciprocity kernel. It treats the feudal oath not as a
 *   vehicle of lordly extraction nor as a sacramentally mediated bond, but as
 *   a fixed, bounded coordination mechanism in which charter text enforces
 *   mutual obligations. Both vassal and lord are net beneficiaries of the
 *   stability the charter provides; there is no structural victim in this
 *   reading, though the peasantry is structurally excluded from the pact.
 *
 * KEY AGENTS:
 *   - Enfeoffed vassals (moderate power, constrained exit) â beneficiaries of tenure security and protection guarantees
 *   - Feudal lords (powerful, constrained exit) â beneficiaries of reliable military service and counsel
 *   - Monastic scribes (institutional, analytical exit) â observers/custodians of the charter text
 *   - Unfree peasantry (powerless, trapped) â excluded from the reciprocal arrangement entirely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__vassal_coordination_reading, 0.18).
domain_priors:suppression_score(feudal_oath_reciprocity__vassal_coordination_reading, 0.2).
domain_priors:theater_ratio(feudal_oath_reciprocity__vassal_coordination_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__vassal_coordination_reading, rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__vassal_coordination_reading, "Feudal Oath Reciprocity â Vassal Coordination Reading").
narrative_ontology:topic_domain(feudal_oath_reciprocity__vassal_coordination_reading, "medieval_political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, '5b5f5bb3-4e71-4bcd-b4e7-cacb63f0c80f').
narrative_ontology:cs_kernel_codification('5b5f5bb3-4e71-4bcd-b4e7-cacb63f0c80f', fixed_text).
narrative_ontology:cs_authority_grounding('5b5f5bb3-4e71-4bcd-b4e7-cacb63f0c80f', lineage).
narrative_ontology:cs_interpretation_layer_present('5b5f5bb3-4e71-4bcd-b4e7-cacb63f0c80f').
narrative_ontology:cs_reading_relation('5b5f5bb3-4e71-4bcd-b4e7-cacb63f0c80f', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b5f5bb3-4e71-4bcd-b4e7-cacb63f0c80f', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('5b5f5bb3-4e71-4bcd-b4e7-cacb63f0c80f', foundational, charter_reciprocity_binding).
narrative_ontology:cs_axiom_status(charter_reciprocity_binding, holdable).
narrative_ontology:cs_axiom_grounding('5b5f5bb3-4e71-4bcd-b4e7-cacb63f0c80f', charter_reciprocity_binding, conventional).
narrative_ontology:cs_axiom('5b5f5bb3-4e71-4bcd-b4e7-cacb63f0c80f', foundational, mutual_enforceability_by_text).
narrative_ontology:cs_axiom_status(mutual_enforceability_by_text, holdable).
narrative_ontology:cs_axiom_grounding('5b5f5bb3-4e71-4bcd-b4e7-cacb63f0c80f', mutual_enforceability_by_text, conventional).
narrative_ontology:cs_reference_frame('5b5f5bb3-4e71-4bcd-b4e7-cacb63f0c80f', customary_charter_reciprocity).
narrative_ontology:cs_drift_state('5b5f5bb3-4e71-4bcd-b4e7-cacb63f0c80f', high_medieval_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5b5f5bb3-4e71-4bcd-b4e7-cacb63f0c80f', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, enfeoffed_vassals).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, feudal_lords).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__vassal_coordination_reading, charter_bound_reciprocity).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__vassal_coordination_reading, mutual_enforceability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Armed retainers who swear homage and receive land tenure (the fief) in exchange for military service. Under this reading, the charter text fixes the lord's obligations as firmly as the vassal's, giving the vassal a documented, enforceable claim to protection and limited customary dues. Exit means forfeiting the fief and breaking an oath witnessed by the community, so most remain within the relationship, but they do so with bounded obligations rather than unlimited subjection.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, enfeoffed_vassals, beneficiary,
    moderate, biographical, constrained, regional).

% Territorial magnates who grant fiefs and receive sworn military service and counsel. The charter binds them to provide protection, justice, and maintenance of the tenure; they cannot arbitrarily increase exactions beyond the written customary terms without rupturing the reciprocal bond and risking vassal defection or loss of honor among their peers.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, feudal_lords, beneficiary,
    powerful, generational, constrained, regional).

% Monastic and cathedral scriptoria who draft, witness, and archive the charter texts. Their literacy gives them custodial power over the written terms, but they do not collect from the arrangement; they preserve the documentary framework that makes mutual enforcement and memory across generations possible.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, monastic_scribes, observer,
    institutional, civilizational, analytical, regional).

% Agricultural laborers bound to the fief who perform the productive work that makes the estate viable. They are not parties to the oath and have no standing in its reciprocal terms; their absence from the charter is structural, not accidental.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, unfree_peasantry, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts volatile personal loyalty into stable, territorially anchored political-military cooperation by fixing reciprocal obligations in a written charter that both parties can invoke against the other, solving the problem of how to maintain large-scale armed followings without a centralized state.
% TRANSFER_FUNCTION: Moves military service and counsel from the vassal to the lord, and moves land tenure, protection, and customary justice from the lord to the vassal; the charter text itself serves as the mutual enforcement device.
% ABSENT_VOICES: Unfree peasants and agricultural laborers who work the estates but are invisible in the oath's reciprocity; also mercenary captains and urban communes that offer alternative military-service markets but are excluded by the honor-bound, charter-delimited framework.
% DISAPPEARANCE_RATIONALE: If the charter-enforced reciprocal oath vanished, the personal bond between lord and armed retainer would lose its documentary anchor; vassals would lose tenure security and lords would lose predictable military call-up, forcing a shift toward cash-based or territorial-state armies and reorganizing medieval political order.
% FOUNDING_PROBLEM: Collapse of centralized Carolingian state apparatus left no standing administration to allocate land, adjudicate disputes, or field armies; local power holders needed a decentralized, scalable mechanism to bind armed followers to territorial obligations.
% FOUNDING_PROBLEM_CORROBORATION: Modern constitutional historians and legal historians attest the state-collapse origin from outside the medieval beneficiary classes; some revisionist scholars contest the scale of the collapse, arguing older Romano-Germanic institutions persisted longer, so the founding problem's severity is debated among external analysts rather than self-asserted by lords or vassals.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__vassal_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__vassal_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__vassal_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).
:- end_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the charter bounds the lord's demands and the vassal's service to customary terms; the transfer is symmetric in enforceability. Suppression is low (0.20) because the constraint operates primarily through mutual expectation and documented custom rather than coercion. Theater ratio is low (0.12) because the homage ceremony, while ritualized, is functionally integrated with the tenure transfer. Accessibility collapse is moderate (0.35) because alternatives such as mercenary contracts or allodial tenure exist but are less politically integrated. Resistance is low (0.15) because both principal parties benefit from the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   The lord-extraction reading would compute a high directionality for the vassal (near full target) and low for the lord (near full beneficiary), producing a tangled-rope or snare classification. The ecclesiastical-mediation reading would introduce sacramental beneficiaries (the Church as intercessor) and victims (those outside the Christian oath-community). The vassal-coordination reading suppresses those asymmetries by treating the charter text as the primary enforcer, yielding symmetric directionalities for both vassal and lord.
 *
 * DIRECTIONALITY LOGIC:
 *   Both enfeoffed_vassals and feudal_lords are declared beneficiaries, placing their directionality near the subsidy end; the charter binds each to the other. The monastic scribes are observers with no extractive relationship. The unfree peasantry are excluded, not victims of the oath itself (they are not governed by its extraction but by a separate manorial constraint); therefore no victim group is declared and the engine derives no high-d target from this constraint story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â decentralized political-military coordination after state collapse â was genuinely live in the early medieval period. The R5 genealogy records it as contested by modern historians but does not assert it was merely a cover story. Because founding_problem_status is contested rather than dead, and because the disappearance verdict is world_rearranges, the mismatch consumer does not flag this as a zombie piton. The constraint is classified as rope because the coordination function (stable land-for-service exchange) remains structurally central and no party is a net loser.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_vs_practice_gap,
    'Do surviving charter texts reflect actually practiced reciprocity, or are they aspirational formulations masking highly asymmetric power?',
    'Archaeological and narrative evidence of vassal self-help and lordly compliance rates; comparison of charter clauses with estate-account litigation records.',
    'If texts are aspirational, the low epsilon rope classification collapses toward a higher-extraction tangled rope or snare; if practiced, the coordination reading is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_vs_practice_gap, empirical, 'Evidentiary gap between written charter reciprocity and lived feudal practice.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the feudal oath kernel inherently underdetermined among coordination, extraction, and sacramental readings, or does the historical record admit a single dominant reading?',
    'Cross-corpus analysis of oath formulas across regions and centuries to see whether reciprocity clauses are uniform (supporting coordination) or variable and lord-favoring (supporting extraction).',
    'If the kernel is underdetermined, the classification of any single reading remains permanently contested and the engine should treat the family as a whole; if determinable, one reading can be deprecated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Underdetermination of the feudal oath kernel across competing readings.').

omega_variable(
    lord_compliance_enforcement,
    'What mechanisms enforced lord compliance with charter terms when lords held superior coercive power?',
    'Study of vassal defection cascades, honorial courts, and peer pressure among the aristocracy; whether charter breaches by lords were systematically punished or merely recorded.',
    'If lord compliance was unenforceable in practice, the symmetry of the rope dissolves and the vassal becomes a victim despite textual reciprocity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lord_compliance_enforcement, empirical, 'Asymmetry of enforcement between vassal and lord despite textual reciprocity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feudal_vassal_coord_tr_t0, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(feudal_vassal_coord_tr_t50, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 50, 0.06).
narrative_ontology:measurement(feudal_vassal_coord_tr_t100, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 100, 0.08).
narrative_ontology:measurement(feudal_vassal_coord_tr_t150, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 150, 0.1).
narrative_ontology:measurement(feudal_vassal_coord_tr_t200, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 200, 0.12).
narrative_ontology:measurement(feudal_vassal_coord_tr_t250, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 250, 0.15).
narrative_ontology:measurement(feudal_vassal_coord_tr_t300, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 300, 0.18).

% Extraction over time
narrative_ontology:measurement(feudal_vassal_coord_be_t0, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(feudal_vassal_coord_be_t50, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 50, 0.13).
narrative_ontology:measurement(feudal_vassal_coord_be_t100, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 100, 0.14).
narrative_ontology:measurement(feudal_vassal_coord_be_t150, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 150, 0.15).
narrative_ontology:measurement(feudal_vassal_coord_be_t200, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 200, 0.16).
narrative_ontology:measurement(feudal_vassal_coord_be_t250, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 250, 0.17).
narrative_ontology:measurement(feudal_vassal_coord_be_t300, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 300, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(feudal_oath_reciprocity__vassal_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__vassal_coordination_reading, attachment_coordination).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint is the vassal-coordination reading of the feudal_oath_reciprocity kernel, emphasizing bounded reciprocity enforced by charter text. It is structurally linked to its sibling readings: lord_extraction_reading (asymmetric extraction framing) and ecclesiastical_mediation_reading (sacramental overlay framing).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
