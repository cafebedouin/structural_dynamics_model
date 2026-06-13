% ============================================================================
% CONSTRAINT STORY: biblical_authority__sola_scriptura_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__sola_scriptura_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: biblical_authority__sola_scriptura_reading
 *   human_readable: Sola Scriptura: Scripture Alone as Sufficient and Self-Interpreting Authority
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This constraint represents the 'Sola Scriptura' principle, a foundational
 *   tenet of many Protestant traditions, asserting that the Bible is the
 *   sole, sufficient, and self-interpreting source of religious authority for
 *   Christian doctrine and practice. It emphasizes individual interpretation
 *   and congregational autonomy, leading to lower clerical extraction but
 *   also higher doctrinal fragmentation across communities. This is one
 *   reading of the broader 'biblical_authority' kernel.
 *
 * KEY AGENTS:
 *   - lay_believers: Primary beneficiary (moderate/mobile) — gains interpretive autonomy.
 *   - individual_interpreters: Primary beneficiary (moderate/mobile) — gains direct access to authority.
 *   - clergy: Agenda setter (organized/constrained) — shifts role from authoritative interpreter to facilitator/teacher.
 *   - denominational_bodies: Payer/Agenda setter (institutional/constrained) — struggles with maintaining doctrinal coherence.
 *   - doctrinal_coherence_across_communities: Victim (analytical/trapped) — suffers from lack of centralized adjudication.
 *   - theologians: Observer (analytical/analytical) — analyzes interpretive methods and doctrinal outcomes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.3).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.2).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Sola Scriptura: Scripture Alone as Sufficient and Self-Interpreting Authority").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "theology/religious_studies/history_of_christianity").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, '8b25adbf-43bb-45d6-87ef-a422a1e14b7e').
narrative_ontology:cs_kernel_codification('8b25adbf-43bb-45d6-87ef-a422a1e14b7e', fixed_text).
narrative_ontology:cs_authority_grounding('8b25adbf-43bb-45d6-87ef-a422a1e14b7e', distributed).
narrative_ontology:cs_reading_relation('8b25adbf-43bb-45d6-87ef-a422a1e14b7e', biblical_authority__tradition_scripture_reading, forecloses).
narrative_ontology:cs_reading_relation('8b25adbf-43bb-45d6-87ef-a422a1e14b7e', biblical_authority__conciliar_reading, forecloses).
narrative_ontology:cs_axiom('8b25adbf-43bb-45d6-87ef-a422a1e14b7e', foundational, scripture_is_perspicuous).
narrative_ontology:cs_axiom_status(scripture_is_perspicuous, holdable).
narrative_ontology:cs_axiom_grounding('8b25adbf-43bb-45d6-87ef-a422a1e14b7e', scripture_is_perspicuous, deontological).
narrative_ontology:cs_axiom('8b25adbf-43bb-45d6-87ef-a422a1e14b7e', foundational, tradition_is_subordinate_to_scripture).
narrative_ontology:cs_axiom_status(tradition_is_subordinate_to_scripture, holdable).
narrative_ontology:cs_axiom_grounding('8b25adbf-43bb-45d6-87ef-a422a1e14b7e', tradition_is_subordinate_to_scripture, deontological).
narrative_ontology:cs_reference_frame('8b25adbf-43bb-45d6-87ef-a422a1e14b7e', reformation_era_scriptural_primacy).
narrative_ontology:cs_drift_state('8b25adbf-43bb-45d6-87ef-a422a1e14b7e', contemporary_theological_discourse, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('8b25adbf-43bb-45d6-87ef-a422a1e14b7e', '').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, individual_interpreters).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(biblical_authority__sola_scriptura_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__sola_scriptura_reading_tests).
:- end_tests(biblical_authority__sola_scriptura_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.3) because the principle inherently resists centralized control over interpretation, thus limiting opportunities for institutional rent-seeking. Suppression is also low (0.2) as it promotes individual conscience and discourages coercive enforcement of specific interpretations beyond local congregational norms. Theater ratio is low (0.1) as the principle is generally enacted directly, with little performative maintenance masking other functions. The metrics reflect the intended structural delta of low clerical extraction and high individual autonomy.
 *
 * PERSPECTIVAL GAP:
 *   Clergy within 'sola scriptura' traditions experience this constraint as a Rope, enabling their role as teachers and facilitators, but also as a Payer, as they lose the authoritative interpretive monopoly. Lay believers experience it as a Rope, granting them direct access to divine authority. Doctrinal coherence, as an abstract entity, is a victim, as the lack of a central adjudicator leads to fragmentation.
 *
 * DIRECTIONALITY LOGIC:
 *   Lay believers and individual interpreters are beneficiaries (d near 0.0) as they gain direct access to scripture and interpretive freedom. Clergy and denominational bodies are agenda setters/payers (d near 0.5-0.7) as they administer the principle but also bear the costs of maintaining coherence without a centralized authority. Doctrinal coherence is a victim (d near 1.0) as it is structurally undermined by the principle's emphasis on individual interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (individual access to scripture, resistance to clerical hierarchy) remains live. The classification as Rope prevents mislabeling the resulting doctrinal fragmentation as pure extraction, acknowledging the genuine coordination function of empowering individual believers. The tension between individual autonomy and collective coherence is inherent to this reading, not a sign of mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sola_scriptura_vs_tradition_ambiguity,
    'Is ''Scripture alone'' truly self-interpreting, or does it implicitly rely on an unacknowledged interpretive tradition?',
    'Comparative analysis of interpretive divergences across ''sola scriptura'' denominations; historical tracing of how initial interpretations became normative.',
    'If an unacknowledged tradition is found to be operative, the constraint''s claimed self-sufficiency is weakened, potentially increasing its effective extractiveness by obscuring the true source of interpretive authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sola_scriptura_vs_tradition_ambiguity, conceptual, 'Ambiguity of ''self-interpreting'' in practice.').

omega_variable(
    doctrinal_fragmentation_as_cost_or_feature,
    'Is the resulting doctrinal fragmentation (victim: doctrinal_coherence_across_communities) an unavoidable cost of lay autonomy, or a structural flaw that undermines the constraint''s coordination function?',
    'Empirical study of the impact of fragmentation on faith communities and individual believers; theological arguments for the necessity of a unified interpretive authority.',
    'If fragmentation is deemed a structural flaw, the constraint''s classification might shift towards a Tangled Rope, as the coordination (lay autonomy) comes at a significant, unacknowledged cost (coherence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_fragmentation_as_cost_or_feature, preference, 'Evaluating doctrinal fragmentation as a cost or feature.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''sola_scriptura_reading'' of the ''biblical_authority'' kernel. How would its structural properties change if viewed through the ''tradition_scripture_reading'' or ''conciliar_reading''?',
    'Analyze the structural deltas: ''tradition_scripture_reading'' would likely increase clerical extraction and suppression, while ''conciliar_reading'' would shift authority to historical councils, potentially reducing individual interpretive autonomy.',
    'The ''sola_scriptura_reading'' emphasizes lay autonomy and low clerical extraction. Sibling readings would introduce higher institutional authority, potentially increasing extractiveness and suppression for individual believers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this as one reading of the ''biblical_authority'' kernel and outlines structural deltas of sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__sola_scriptura_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bibl_tr_t100, biblical_authority__sola_scriptura_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement(bibl_tr_t200, biblical_authority__sola_scriptura_reading, theater_ratio, 200, 0.1).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__sola_scriptura_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(bibl_be_t100, biblical_authority__sola_scriptura_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement(bibl_be_t200, biblical_authority__sola_scriptura_reading, base_extractiveness, 200, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__sola_scriptura_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(bibl_su_t100, biblical_authority__sola_scriptura_reading, suppression_requirement, 100, 0.18).
narrative_ontology:measurement(bibl_su_t200, biblical_authority__sola_scriptura_reading, suppression_requirement, 200, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_authority__sola_scriptura_reading, 0.08).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__conciliar_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, denominational_autonomy_norms).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'biblical_authority' kernel. Its structural properties (low clerical extraction, high doctrinal fragmentation) are distinct from sibling readings that emphasize tradition or conciliar authority. Each reading is modeled as a separate constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
