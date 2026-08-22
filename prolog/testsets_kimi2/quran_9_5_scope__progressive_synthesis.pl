% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__progressive_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__progressive_synthesis, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: quran_9_5_scope__progressive_synthesis
 *   human_readable: Quran 9:5 Progressive Synthesis Reading
 *   domain: religious/hermeneutic/political
 *
 * SUMMARY:
 *   This constraint story instantiates the progressive_synthesis reading of
 *   the quran_9_5_scope kernel. Under this reading, Quran 9:5 is interpreted
 *   as a time-bound 7th-century Medinese political directive rather than an
 *   eternally binding legal command; the Quranic ethical trajectory is held
 *   to supersede literalist application. The verse therefore exits active
 *   constraint space entirely. The constraint modeled is the
 *   progressive-synthetic hermeneutic regime itself, which coordinates modern
 *   Islamic ethical self-understanding and pluralist political belonging
 *   while asymmetrically extracting epistemic authority from textualist
 *   institutions. The reading forecloses the abrogating_universal sibling
 *   reading (eternal offensive jihad) and influences the contextual_defensive
 *   reading by creating downstream pressure toward full supersession.
 *
 * KEY AGENTS:
 *   - progressive_islamic_scholars: Agenda-setter (organized/constrained) â develop historical-critical hermeneutics that temporalize 9:5 and maintain the interpretive framework against literalist challenge
 *   - secular_pluralist_frameworks: Beneficiary (institutional/mobile) â gain normative space as literalist violence claims dissolve and governance models are freed from scriptural coercion
 *   - textualist_authority_structures: Payer (institutional/identity_locked) â lose epistemic authority and institutional relevance as the verse is removed from active legal command
 *   - progressive_muslim_communities: Beneficiary (moderate/constrained) â freed from the cognitive and social burden of reconciling literalist martial commands with modern citizenship
 *   - polytheist_religious_minorities: Excluded (powerless/trapped) â no longer doctrinal targets under this reading but absent from the Islamic hermeneutic conversation that determines their status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__progressive_synthesis, 0.42).
domain_priors:suppression_score(quran_9_5_scope__progressive_synthesis, 0.4).
domain_priors:theater_ratio(quran_9_5_scope__progressive_synthesis, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, extractiveness, 0.42).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__progressive_synthesis, tangled_rope).
narrative_ontology:human_readable(quran_9_5_scope__progressive_synthesis, "Quran 9:5 Progressive Synthesis Reading").
narrative_ontology:topic_domain(quran_9_5_scope__progressive_synthesis, "religious/hermeneutic/political").

domain_priors:requires_active_enforcement(quran_9_5_scope__progressive_synthesis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__progressive_synthesis, '23f2af25-9bb3-44b5-96bf-569a14c44747').
narrative_ontology:cs_kernel_codification('23f2af25-9bb3-44b5-96bf-569a14c44747', fixed_text).
narrative_ontology:cs_authority_grounding('23f2af25-9bb3-44b5-96bf-569a14c44747', expertise).
narrative_ontology:cs_interpretation_layer_present('23f2af25-9bb3-44b5-96bf-569a14c44747').
narrative_ontology:cs_reading_relation('23f2af25-9bb3-44b5-96bf-569a14c44747', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('23f2af25-9bb3-44b5-96bf-569a14c44747', quran_9_5_scope__contextual_defensive, influences).
narrative_ontology:cs_axiom('23f2af25-9bb3-44b5-96bf-569a14c44747', foundational, historical_critical_method_over_literal_priority).
narrative_ontology:cs_axiom_status(historical_critical_method_over_literal_priority, holdable).
narrative_ontology:cs_axiom_grounding('23f2af25-9bb3-44b5-96bf-569a14c44747', historical_critical_method_over_literal_priority, empirically_contingent).
narrative_ontology:cs_axiom('23f2af25-9bb3-44b5-96bf-569a14c44747', foundational, ethical_trajectory_as_hermeneutic_principle).
narrative_ontology:cs_axiom_status(ethical_trajectory_as_hermeneutic_principle, holdable).
narrative_ontology:cs_axiom_grounding('23f2af25-9bb3-44b5-96bf-569a14c44747', ethical_trajectory_as_hermeneutic_principle, deontological).
narrative_ontology:cs_reference_frame('23f2af25-9bb3-44b5-96bf-569a14c44747', seventh_century_medinese_political_context).
narrative_ontology:cs_drift_state('23f2af25-9bb3-44b5-96bf-569a14c44747', contemporary_global_pluralism, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('23f2af25-9bb3-44b5-96bf-569a14c44747', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__progressive_synthesis, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, secular_pluralist_frameworks).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, progressive_muslim_communities).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, textualist_authority_structures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and teach historical-critical and ethical-trajectory hermeneutics that relegate 9:5 to its 7th-century political context. They staff university departments, progressive mosques, and research institutes where this reading is cultivated. Exit means abandoning scholarly legitimacy in traditional seminaries or risking institutional isolation.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, progressive_islamic_scholars, agenda_setter,
    organized, generational, constrained, global).

% International human rights regimes, secular governance models, and pluralist political frameworks benefit from the delegitimization of Quranic literalist violence claims. They do not administer the hermeneutic but gain normative space as the verse's binding force dissolves, allowing religious coexistence policies to proceed with reduced scriptural opposition.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, secular_pluralist_frameworks, beneficiary,
    institutional, civilizational, mobile, global).

% Traditional madrasa networks, state muftiates, and literalist jurisprudential bodies whose authority derives from the claim that 9:5 remains eternally instructive. Progressive synthesis directly undermines their epistemic authority, student recruitment, and institutional funding. Their identity is fused with the literalist method; exit means dissolving the self-understanding of their entire tradition.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, textualist_authority_structures, payer,
    institutional, civilizational, identity_locked, global).

% Lay Muslims who adopt progressive readings and are freed from the cognitive and social burden of reconciling literalist martial commands with modern citizenship. They depend on progressive scholars for interpretive leadership and face community pressure from traditionalist family and social networks.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, progressive_muslim_communities, beneficiary,
    moderate, biographical, constrained, global).

% Religious minorities historically targeted by literalist applications of 9:5. Under progressive synthesis they are no longer doctrinal targets, but they are not participants in the Islamic hermeneutic process that determines their status and have no seat in the scholarly forums where this reading is produced.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, polytheist_religious_minorities, excluded,
    powerless, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__progressive_synthesis, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates modern Muslim ethical self-understanding and political belonging by resolving the apparent contradiction between Quranic martial directives and pluralist political norms through historical contextualization and ethical-trajectory reasoning.
% TRANSFER_FUNCTION: Moves interpretive authority and normative prestige from literalist jurisprudential institutions to progressive scholarly networks and secular-pluralist governance frameworks; moves social-psychological burden from lay Muslims and religious minorities to textualist authorities whose claims lose legitimacy.
% ABSENT_VOICES: Traditional jurists (fuqaha) trained in classical usul al-fiqh and committed to literalist or abrogationist readings are structurally absent from progressive scholarly forums and international human rights venues where this reading is forged; their objection is heard only in counter-publics and traditional institutions.
% DISAPPEARANCE_RATIONALE: If the progressive synthesis vanished, textualist readings would regain normative dominance in Islamic jurisprudence, polytheist minorities would again face doctrinal targeting, and secular governance frameworks would lose a key theological ally in de-escalating scriptural violence claims.
% FOUNDING_PROBLEM: The apparent contradiction between the Quranic text's martial commands and the ethical-political requirements of modern pluralist coexistence.
% FOUNDING_PROBLEM_CORROBORATION: Progressive Islamic scholars and historians of early Islam corroborate the historical specificity from within the hermeneutic tradition; secular human rights frameworks corroborate the ethical tension from outside the benefiting parties; textualist authorities deny the problem exists.
narrative_ontology:disappearance_verdict(quran_9_5_scope__progressive_synthesis, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__progressive_synthesis, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__progressive_synthesis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_9_5_scope__progressive_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__progressive_synthesis, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__progressive_synthesis_tests).
:- end_tests(quran_9_5_scope__progressive_synthesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the progressive synthesis genuinely coordinates modern ethics but extracts authority from textualist institutions by delegitimizing their core interpretive method. Suppression (0.40) reflects partial closure of literalist alternatives in progressive-dominated scholarly and institutional spaces, though literalism remains globally accessible. Theater ratio (0.30) captures the gap between genuine hermeneutic labor and performative displays of progressiveness that substitute for direct engagement with textualist arguments. Accessibility collapse (0.35) is incomplete because textualist alternatives remain widely available in traditional institutions and digital counter-publics. Resistance (0.75) is high because textualist authorities mount sustained epistemic and institutional pushback. The temporal series on a single shared grid show gradual institutionalization of the reading over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (progressive scholars) experiences the constraint as genuine coordination solving a real theological-ethical problem. The payer seat (textualist authorities) experiences the same arrangement as extractive displacement of their epistemic authority and institutional role. The beneficiary seats (secular pluralist frameworks, progressive communities) experience subsidized ethical relief. The engine computes this divergence from the structural data: same constraint, opposed directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive scholars and secular pluralist frameworks sit near the beneficiary end: the constraint subsidizes their epistemic and political projects. Textualist authority structures sit near the target end: the constraint extracts their historical authority by declaring their foundational premise of eternal bindingness obsolete. Progressive Muslim communities sit nearer symmetric, receiving ethical coordination while remaining dependent on progressive scholarly leadership. Polytheist minorities are excluded from the directionality computation by their non-participant status.
 *
 * MANDATROPHY ANALYSIS:
 *   The progressive synthesis reading avoids mandatrophy mislabeling by explicitly acknowledging the verse's original function (7th-century political coordination) while declaring that function obsolete. The founding problem â reconciling revelation with modern ethics â is contested but live from the progressive perspective. A pure snare reading would ignore the genuine coordination function for modern ethics; a pure rope reading would ignore the asymmetric authority transfer from textualists. Tangled rope captures both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_reading_location,
    'Given that this constraint is the progressive_synthesis reading of kernel quran_9_5_scope, and sibling readings instantiate structurally distinct constraints (abrogating_universal, contextual_defensive), is the core disagreement located in spatial scope, extractiveness, or beneficiary geometry?',
    'Kernel-level decomposition comparing epsilon values, beneficiary/victim structures, and scope declarations across the three sibling constraints; verification that each reading carries a stable epsilon invariant under its own measurement.',
    'If the disagreement is located in the scope atom (universal vs time-bound), the constraints are structurally distinct at the spatial_scope level and epsilon-invariance is preserved; if the disagreement reduces to epsilon alone without structural decomposition, the kernel has not been properly split.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_location, conceptual, 'Structural location of disagreement among sibling readings of the quran_9_5_scope kernel').

omega_variable(
    textualist_authority_as_victim,
    'Are textualist authority structures genuinely victimized by progressive hermeneutics, or are they simply losing illegitimate coercive power?',
    'Examine whether textualist institutions face material deprivation, exclusion from public discourse, or merely loss of normative dominance in progressive scholarly venues.',
    'If the loss is purely a decline in unearned coercive authority, the effective extractiveness is lower than measured; if it includes structural exclusion and resource deprivation, the current epsilon is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualist_authority_as_victim, conceptual, 'Whether textualist authority loss counts as genuine victimization').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of literalist readings structural (institutional exclusion from progressive scholarly forums and tenure committees) or internalized (self-censorship by scholars trained in traditional seminaries)?',
    'Post-exit suppression trajectory and survey of scholars in progressive institutions: if literalist expression persists covertly after structural barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â textualist scholars carry the suppression with them even where institutional policy permits literalism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in hermeneutic institutions').

omega_variable(
    historical_verifiability,
    'Can the 7th-century Medinese political context of verse 9:5 be empirically verified with sufficient confidence to override literalist readings, or does the progressive synthesis rest on conventional hermeneutic authority?',
    'Advances in historical-critical scholarship, asbab al-nuzul documentation, and archaeology of early Islamic political treaties.',
    'If verification remains weak, the progressive synthesis axioms shift from empirically_contingent to conventional grounding, altering foreclosure dynamics and coupling analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_verifiability, empirical, 'Empirical grounding of historical contextualization claims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__progressive_synthesis, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__progressive_synthesis, theater_ratio, 0, 0.15).
narrative_ontology:measurement(qura_tr_t16, quran_9_5_scope__progressive_synthesis, theater_ratio, 16, 0.18).
narrative_ontology:measurement(qura_tr_t32, quran_9_5_scope__progressive_synthesis, theater_ratio, 32, 0.21).
narrative_ontology:measurement(qura_tr_t48, quran_9_5_scope__progressive_synthesis, theater_ratio, 48, 0.24).
narrative_ontology:measurement(qura_tr_t64, quran_9_5_scope__progressive_synthesis, theater_ratio, 64, 0.27).
narrative_ontology:measurement(qura_tr_t80, quran_9_5_scope__progressive_synthesis, theater_ratio, 80, 0.3).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__progressive_synthesis, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(qura_be_t16, quran_9_5_scope__progressive_synthesis, base_extractiveness, 16, 0.24).
narrative_ontology:measurement(qura_be_t32, quran_9_5_scope__progressive_synthesis, base_extractiveness, 32, 0.28).
narrative_ontology:measurement(qura_be_t48, quran_9_5_scope__progressive_synthesis, base_extractiveness, 48, 0.32).
narrative_ontology:measurement(qura_be_t64, quran_9_5_scope__progressive_synthesis, base_extractiveness, 64, 0.37).
narrative_ontology:measurement(qura_be_t80, quran_9_5_scope__progressive_synthesis, base_extractiveness, 80, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__progressive_synthesis, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(qura_su_t16, quran_9_5_scope__progressive_synthesis, suppression_requirement, 16, 0.32).
narrative_ontology:measurement(qura_su_t32, quran_9_5_scope__progressive_synthesis, suppression_requirement, 32, 0.34).
narrative_ontology:measurement(qura_su_t48, quran_9_5_scope__progressive_synthesis, suppression_requirement, 48, 0.36).
narrative_ontology:measurement(qura_su_t64, quran_9_5_scope__progressive_synthesis, suppression_requirement, 64, 0.38).
narrative_ontology:measurement(qura_su_t80, quran_9_5_scope__progressive_synthesis, suppression_requirement, 80, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__contextual_defensive).

% DUAL FORMULATION NOTE:
% The quran_9_5_scope kernel decomposes into three structurally distinct constraints: abrogating_universal (high extraction, universal scope, snare-like), contextual_defensive (moderate extraction, bounded defensive scope), and progressive_synthesis (coordination with asymmetric authority transfer, tangled-rope). Each reading carries a distinct epsilon and stakeholder geometry; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
