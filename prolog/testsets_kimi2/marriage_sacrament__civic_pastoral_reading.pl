% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__civic_pastoral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Pastoral Marriage with Discernment: Indissolubility as Ideal
 *   domain: religious/political
 *
 * SUMMARY:
 *   This constraint story instantiates the civic_pastoral_reading of the
 *   marriage_sacrament kernel, in which marriage is treated as a pastoral
 *   relationship subject to human failure and indissolubility functions as an
 *   aspirational ideal requiring compassionate discernment rather than as an
 *   ontological barrier. The reading consolidated in the post-conciliar
 *   period and intensified with the Amoris Laetitia era. It solves a genuine
 *   coordination problemâretaining divorced and remarried Catholics in
 *   sacramental lifeâbut asymmetrically extracts from traditional laity
 *   whose identity depends on stable, non-contradictory doctrinal norms. The
 *   metric profile and claimed type are authored independently: the
 *   constraint is claimed as tangled_rope (genuine pastoral coordination plus
 *   asymmetric extraction) while the metrics track the steady accumulation of
 *   extraction and theater as the ideal-practice gap widens.
 *
 * KEY AGENTS:
 *   - pastoral_clergy: Primary agenda-setter (institutional/constrained) â administers discernment and bears institutional cost of maintaining doctrinal ambiguity
 *   - divorced_remmarried_catholics: Primary beneficiary (moderate/constrained) â receive pastoral pathway and sacramental access
 *   - traditional_catholic_laity: Primary payer (organized/identity_locked) â bear doctrinal confusion, normative destabilization, and identity erosion
 *   - doctrinal_conservatives: Excluded voice (institutional/constrained) â formally inside the magisterium, structurally outside the operative framework
 *   - sociologist_religion: Analytical observer (analytical/analytical) â tracks the doctrine-practice divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.55).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.58).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Pastoral Marriage with Discernment: Indissolubility as Ideal").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious/political").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, '53ea03d6-57da-4539-af56-d460fb8cde86').
narrative_ontology:cs_kernel_codification('53ea03d6-57da-4539-af56-d460fb8cde86', fixed_text).
narrative_ontology:cs_authority_grounding('53ea03d6-57da-4539-af56-d460fb8cde86', practice).
narrative_ontology:cs_interpretation_layer_present('53ea03d6-57da-4539-af56-d460fb8cde86').
narrative_ontology:cs_reading_relation('53ea03d6-57da-4539-af56-d460fb8cde86', marriage_sacrament__hierarchical_indissolubility_reading, influences).
narrative_ontology:cs_axiom('53ea03d6-57da-4539-af56-d460fb8cde86', foundational, indissolubility_is_aspirational_ideal).
narrative_ontology:cs_axiom_status(indissolubility_is_aspirational_ideal, holdable).
narrative_ontology:cs_axiom_grounding('53ea03d6-57da-4539-af56-d460fb8cde86', indissolubility_is_aspirational_ideal, deontological).
narrative_ontology:cs_axiom('53ea03d6-57da-4539-af56-d460fb8cde86', foundational, pastoral_discernment_as_institutional_norm).
narrative_ontology:cs_axiom_status(pastoral_discernment_as_institutional_norm, holdable).
narrative_ontology:cs_axiom_grounding('53ea03d6-57da-4539-af56-d460fb8cde86', pastoral_discernment_as_institutional_norm, conventional).
narrative_ontology:cs_reference_frame('53ea03d6-57da-4539-af56-d460fb8cde86', pastoral_accompaniment_ideal).
narrative_ontology:cs_drift_state('53ea03d6-57da-4539-af56-d460fb8cde86', post_pastoral_shift_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('53ea03d6-57da-4539-af56-d460fb8cde86', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, divorced_remmarried_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_catholic_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer marriage tribunal processes, pastoral accompaniment, and case-by-case discernment regarding communion for the divorced and remarried. They exercise delegated authority to apply the ideal of indissolubility flexibly, navigating between doctrinal rigor and pastoral inclusion. Their institutional role depends on sustaining this middle path.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, pastoral_clergy, agenda_setter,
    institutional, generational, constrained, global).

% Catholics in failed or irregular marriages who seek continued sacramental participation. They benefit from the pastoral pathway that permits discernment-based admission to communion without requiring absolute separation or impossible marital repair, allowing them to remain inside the community.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, divorced_remmarried_catholics, beneficiary,
    moderate, biographical, constrained, local).

% Catholics whose spiritual stability and communal identity depend on clear, non-contradictory doctrinal norms. They experience the pastoral reading as a doctrinal rupture that relativizes indissolubility, producing cognitive dissonance, loss of normative clarity, and fragmentation of their ecclesial identity. Exit to clearer traditionalist jurisdictions is possible but carries heavy relational and ideological costs.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_catholic_laity, payer,
    organized, generational, identity_locked, global).

% Bishops, theologians, and canonists who defend indissolubility as ontological and constitutive. They remain formally inside the magisterium but are structurally excluded from shaping the operative pastoral framework; their formal interventions and dubia are acknowledged institutionally yet bypassed in practice.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, doctrinal_conservatives, excluded,
    institutional, generational, constrained, global).

% Academic observers who document the growing divergence between the Church's official ontological doctrine and its pastoral practice. They track how the institution simultaneously maintains the language of indissolubility while normalizing exceptions, and they measure the resulting ideological polarization.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, sociologist_religion, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Retains divorced and remarried Catholics within sacramental and communal life by replacing absolute hierarchical prohibition with delegated pastoral discernment, preventing mass pastoral alienation while maintaining the symbolic ideal of indissolubility.
% TRANSFER_FUNCTION: Moves doctrinal authority from fixed normative clarity to delegated pastoral discretion; transfers the cognitive and identity costs of doctrinal ambiguity from the institution to traditional laity who depend on stable sacramental ontology.
% ABSENT_VOICES: Doctrinal conservatives and traditionalist communities are formally present in the Church but structurally excluded from the operative discernment framework; their objections are categorized as rigorism rather than legitimate theological dissent, and they are not seated in the pastoral councils that set local norms.
% DISAPPEARANCE_RATIONALE: If the pastoral discernment framework vanished, divorced and remarried Catholics would face immediate exclusion from communion, traditional laity would regain normative clarity, and the Church would either revert to strict hierarchical adjudication or face schismatic pressures and accelerated defection.
% FOUNDING_PROBLEM: How to maintain sacramental practice and pastoral care for Catholics whose marriages have irretrievably failed without either abandoning the doctrine of indissolubility entirely or enforcing a rigid hierarchical adjudication that produces pastoral alienation and mass exclusion.
% FOUNDING_PROBLEM_CORROBORATION: Pastoral theologians and some episcopal conferences attest the problem remains live. Traditionalist bishops and laity attest the pastoral solution creates worse problems than it solves. External historians of Vatican II and sociologists of religion corroborate the historical alienation but do not adjudicate the doctrinal response; corroboration is thus split across contested readings.
narrative_ontology:disappearance_verdict(marriage_sacrament__civic_pastoral_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__civic_pastoral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_sacrament__civic_pastoral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__civic_pastoral_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__civic_pastoral_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__civic_pastoral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness rises from 0.25 to 0.55 over the interval as the pastoral reading shifts from exceptional accommodation to normalized practice, increasing the identity cost for traditionalists. Theater ratio rises to 0.45 because the language of indissolubility as an ideal becomes increasingly performative while the operative reality permits dissolution through discernment. Suppression rises to 0.58 because maintaining the new pastoral equilibrium requires active management of traditionalist dissent and marginalization of the hierarchical reading. Accessibility collapse is moderate (0.50) because the hierarchical alternative remains intellectually available but is pastorally inaccessible within the operative framework. Resistance is moderate (0.45) from traditionalist organizations and some episcopal opposition.
 *
 * PERSPECTIVAL GAP:
 *   The pastoral clergy and the divorced/remarried couples experience the constraint as necessary coordination that prevents exclusion and schism. The traditional Catholic laity experience the same structure as extractive confusion that dissolves the sacramental ontology they depend on. The engine computes this divergence from the structural dataâbeneficiary versus payer roles, identity_locked versus constrained exit options, and the same spatial_scope with opposed directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Divorced and remarried Catholics sit near the beneficiary end (d low): the constraint subsidizes their sacramental participation. Traditional Catholic laity sit near the full-target end (d high): the constraint extracts doctrinal clarity from them and imposes identity costs. Pastoral clergy sit near symmetric but slightly toward beneficiary: they gain institutional flexibility and pastoral efficacy at the cost of doctrinal coherence and authority erosion. Doctrinal conservatives are excluded rather than coordinatedâtheir exclusion is a structural requirement for the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling because its founding problemâpastoral alienation of divorced Catholicsâwas genuinely live, and the coordination function (retaining members in sacramental life) is real. A pure snare reading would ignore this and treat the arrangement as cover for doctrinal dissolution. However, the cost asymmetryâtraditional laity pay identity costs while the institution gains flexibilityâestablishes the asymmetric extraction that makes this a tangled_rope rather than a rope. If the founding problem were dead and the arrangement persisted solely as doctrinal theater, it would compute toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_position_in_kernel,
    'Does the civic_pastoral reading''s legitimization of case-by-case discernment structurally destabilize the hierarchical_indissolubility_reading, or can both readings be held as complementary aspects of a single sacramental theology?',
    'Comparative analysis of magisterial documents, episcopal conference implementations, and intra-ecclesial legal precedent to determine whether the readings are treated as complementary or as competing paradigms.',
    'If the readings are genuinely complementary, the constraint''s extraction profile softens toward coordination; if they are competitive and the hierarchical reading is actively delegitimized, extraction from traditionalists intensifies and the constraint edges toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_position_in_kernel, conceptual, 'Structural relationship between sibling readings in the marriage_sacrament kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of traditionalist dissent structural (institutional marginalization, removal from teaching positions, suppression of dubia) or internalized (traditionalists accepting their own marginalization as fidelity to a remnant identity)?',
    'Post-exit suppression trajectory: observe whether traditionalists who relocate to jurisdictions with clearer doctrinal enforcement (e.g., traditionalist orders, personal ordinariates) continue to experience doctrinal confusion or recover normative clarity rapidly.',
    'If suppression is internalized, the constraint''s effective extraction is higher than the structural measure suggests because the target carries the suppression beyond any institutional boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism for traditionalist laity').

omega_variable(
    identity_lock_nature,
    'Is the traditional Catholic laity''s retention in the constraint driven primarily by professional identity, relational identity, ideological identity, or institutional identity fusion?',
    'Ethnographic study of traditionalist Catholic communities to identify the primary identity anchor that prevents exit to alternative traditionalist jurisdictions.',
    'If institutional identity dominates, exit to alternative structures (SSPX, FSSP, Eastern Catholic jurisdictions) is comparatively easier than if relational or ideological identity locks are primary, which would intensify the identity_locked classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_nature, empirical, 'Nature of identity lock binding traditionalist laity to the constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__civic_pastoral_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(marr_tr_t10, marriage_sacrament__civic_pastoral_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(marr_tr_t20, marriage_sacrament__civic_pastoral_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(marr_tr_t30, marriage_sacrament__civic_pastoral_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(marr_tr_t40, marriage_sacrament__civic_pastoral_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(marr_tr_t50, marriage_sacrament__civic_pastoral_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(marr_be_t10, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(marr_be_t20, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(marr_be_t30, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(marr_be_t40, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(marr_be_t50, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(marr_su_t10, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(marr_su_t20, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(marr_su_t30, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(marr_su_t40, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(marr_su_t50, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, hierarchical_indissolubility_reading).

% DUAL FORMULATION NOTE:
% The marriage_sacrament kernel decomposes into at least two structurally distinct constraints: the civic_pastoral_reading (indissolubility as aspirational ideal with delegated pastoral discernment) and the hierarchical_indissolubility_reading (indissolubility as ontological, constitutive reality requiring hierarchical adjudication). They share the same sacramental referent but have different epsilon values, stakeholder structures, victim sets, and operative enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
