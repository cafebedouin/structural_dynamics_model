% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_authority__continuity_reading
 *   human_readable: Vatican II Continuity Hermeneutic (Continuity Reading)
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This constraint story models the continuity_reading of the
 *   vatican_ii_authority kernel: the claim that the Second Vatican Council
 *   represents organic doctrinal development in continuity with tradition,
 *   and that post-conciliar reforms are legitimate expressions of an
 *   unchanging deposit of faith. The kernel is the set of 16 conciliar
 *   documents; the reading is a hermeneutic commitment system that frames all
 *   reform as non-rupturous. Key agents include the magisterial institution
 *   (agenda-setter), progressive reformers (beneficiary), and traditionalist
 *   Catholics (target). The constraint is claimed as coordination (preventing
 *   schism) but the metrics are authored independently: moderate extraction
 *   through enforced assent, rising theater as practice drifts from
 *   pre-conciliar norms, and active suppression of the rupture reading within
 *   the Roman communion.
 *
 * KEY AGENTS:
 *   - magisterial_institution: agenda-setter (institutional/analytical) â promulgates and enforces the continuity hermeneutic through encyclicals, liturgical norms, and disciplinary mechanisms
 *   - progressive_reformers: beneficiary (organized/constrained) â gain institutional legitimacy for reform initiatives by framing them as organic development
 *   - traditionalist_catholics: primary target (moderate/identity_locked) â bear the burden of enforced assent to reforms they experience as rupture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__continuity_reading, 0.45).
domain_priors:suppression_score(vatican_ii_authority__continuity_reading, 0.55).
domain_priors:theater_ratio(vatican_ii_authority__continuity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__continuity_reading, "Vatican II Continuity Hermeneutic (Continuity Reading)").
narrative_ontology:topic_domain(vatican_ii_authority__continuity_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__continuity_reading, 'e9bb74a5-43c9-4a3b-ad4e-2c0934af278e').
narrative_ontology:cs_kernel_codification('e9bb74a5-43c9-4a3b-ad4e-2c0934af278e', fixed_text).
narrative_ontology:cs_authority_grounding('e9bb74a5-43c9-4a3b-ad4e-2c0934af278e', lineage).
narrative_ontology:cs_interpretation_layer_present('e9bb74a5-43c9-4a3b-ad4e-2c0934af278e').
narrative_ontology:cs_reading_relation('e9bb74a5-43c9-4a3b-ad4e-2c0934af278e', vatican_ii_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('e9bb74a5-43c9-4a3b-ad4e-2c0934af278e', vatican_ii_authority__composite_overdetermination_reading, forecloses).
narrative_ontology:cs_axiom('e9bb74a5-43c9-4a3b-ad4e-2c0934af278e', foundational, organic_development_legitimates_reform).
narrative_ontology:cs_axiom_status(organic_development_legitimates_reform, holdable).
narrative_ontology:cs_axiom_grounding('e9bb74a5-43c9-4a3b-ad4e-2c0934af278e', organic_development_legitimates_reform, theological).
narrative_ontology:cs_axiom('e9bb74a5-43c9-4a3b-ad4e-2c0934af278e', foundational, hermeneutic_continuity_is_resolvable).
narrative_ontology:cs_axiom_status(hermeneutic_continuity_is_resolvable, holdable).
narrative_ontology:cs_axiom_grounding('e9bb74a5-43c9-4a3b-ad4e-2c0934af278e', hermeneutic_continuity_is_resolvable, conventional).
narrative_ontology:cs_reference_frame('e9bb74a5-43c9-4a3b-ad4e-2c0934af278e', unbroken_apostolic_tradition).
narrative_ontology:cs_drift_state('e9bb74a5-43c9-4a3b-ad4e-2c0934af278e', post_conciliar_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e9bb74a5-43c9-4a3b-ad4e-2c0934af278e', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__continuity_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, progressive_reformers).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, magisterial_institution).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, traditionalist_catholics).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, hermeneutic_of_continuity).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, organic_doctrinal_development).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theological and pastoral activists who advocate for post-conciliar liturgical and doctrinal reforms. They gain institutional legitimacy by framing changes as organic development rather than rupture. Their exit is constrained because operating outside the magisterial framework means losing canonical standing, platform, and access to Church infrastructure.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, progressive_reformers, beneficiary,
    organized, generational, constrained, global).

% The papal and episcopal teaching office that promulgates and enforces the continuity reading through encyclicals, liturgical norms, directives, and disciplinary oversight. It sets the interpretive rules for the Council and enforces compliance to prevent schism, while stabilizing its own authority across changing pastoral practice.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, magisterial_institution, agenda_setter,
    institutional, civilizational, analytical, universal).

% Clergy and laity who experience post-conciliar reforms as doctrinal and liturgical rupture. They carry the burden of assimilating changes they view as contradictory to prior teaching, and face marginalization or canonical restrictions if they publicly dissent. Catholic identity fused with pre-conciliar practice makes formal exit spiritually and socially traumatic.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, traditionalist_catholics, payer,
    moderate, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hermeneutic framework that allows the Catholic Church to implement pastoral, liturgical, and ecumenical reforms after Vatican II without formal schism, by interpreting all change as organic development of an unchanging deposit of faith.
% TRANSFER_FUNCTION: Moves institutional legitimacy and magisterial protection from a pre-conciliar traditionalist posture to post-conciliar reform initiatives; simultaneously transfers the burden of assent and the cost of suppressed dissent onto traditionalist Catholics.
% ABSENT_VOICES: Traditionalist theologians and laity who read the Council as rupture are formally inside the Church but functionally excluded from magisterial hermeneutic deliberations. Sedevacantists and radical schismatics are entirely outside the institutional conversation. Secular historians of the Council are excluded from the theological authority structure.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished overnight, the magisterium would lose its primary mechanism for legitimating post-conciliar reforms. Progressive initiatives would face heightened schism risk, traditionalist dissent would surge, and the Church would likely fracture into competing hermeneutic camps without a shared authority narrative to bind them.
% FOUNDING_PROBLEM: The Church faced the problem of how to renew itself pastorally and engage modernity at the Second Vatican Council without undermining its claim to teach unchanging truth or triggering mass schism.
% FOUNDING_PROBLEM_CORROBORATION: Progressive reformers and the magisterium attest the problem is still live, requiring ongoing continuity hermeneutics. Traditionalist Catholics and some independent historians attest the founding problem was manufactured or is dead, arguing the pre-conciliar Church was stable and the continuity framework is a retrospective justification. Corroboration from outside the benefiting parties includes secular historians of religion who document the Council's disruptive institutional effects independently of Catholic partisan interests.
narrative_ontology:disappearance_verdict(vatican_ii_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__continuity_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__continuity_reading_tests).
:- end_tests(vatican_ii_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.45 reflects the moderate but real cost borne by traditionalists forced to assent to reforms they experience as doctrinal and liturgical rupture; the extraction is non-monetary but coercion-based. Suppression at 0.55 captures active magisterial enforcement: censures of dissenting theologians, restrictions on traditionalist groups, and the exclusion of the rupture reading from legitimate ecclesial discourse. Theater at 0.40 indicates substantial performative labor to reframe obvious institutional discontinuities (liturgical overhaul, ecumenical shifts, collegiality) as organic development. Accessibility_collapse at 0.60 reflects that, within the Roman communion, the rupture reading is structurally unavailable without schism; resistance at 0.50 reflects organized traditionalist pushback (SSPX, academic dissent, liturgical restrictions) that prevents total suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the magisterial seat and the progressive reformer seat, the constraint is experienced as necessary coordination: without the continuity hermeneutic, the Church fragments into schism. From the traditionalist seat, it is experienced as extractive imposition: they pay the cost of a forced interpretive narrative that dissolves their theological world while demanding their assent. The engine computes this divergence from structural dataâidentical conciliar texts, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterial institution and progressive reformers are structural beneficiaries (low d): the continuity reading subsidizes their authority and reform agendas by providing a non-rupturous frame. Traditionalist Catholics are structural victims (high d): they bear the cost of assimilating changes under an interpretive framework they reject; identity-locked exit options push them toward the full-target end, amplifying effective extraction despite moderate institutional power.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading prevents mislabeling by maintaining a genuine coordination function: it has kept the Church from formal schism on a massive scale while accommodating substantial reform. The mandatrophy question is whether the founding problem (how to reform without rupture) is still live. It is contested: traditionalists argue the problem was manufactured and the pre-conciliar settlement was stable, while the magisterium claims ongoing engagement with modernity requires the continuity framework. The persistence of the constraint is partly functional and partly inertial, as the post-conciliar institution now depends on the reading for its own legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vatican_ii_kernel_reading_location,
    'This constraint is the continuity_reading of kernel vatican_ii_authority. The sibling rupture_reading would change the structural classification by introducing the universal Church as victim (if the reforms are erroneous) and raising extractiveness toward snare levels, while composite_overdetermination_reading would dissolve the beneficiary/victim structure entirely by declaring the kernel irreducibly ambiguous. Where is the disagreement structurally located?',
    'Historical-doctrinal analysis comparing pre- and post-conciliar magisterial pronouncements on contested topics (religious liberty, liturgy, ecclesiology) to determine whether the documents contain internal contradiction with prior teaching.',
    'If doctrinal contradictions are demonstrable, the continuity reading''s epsilon and suppression metrics are misrepresented and the constraint collapses toward snare or piton depending on enforcement persistence; if the text is genuinely coherent, the continuity reading strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vatican_ii_kernel_reading_location, conceptual, 'Structural location of disagreement between continuity and sibling readings').

omega_variable(
    cost_asymmetry_in_organic_development,
    'Are post-conciliar reforms truly cost-free development as the continuity reading claims, or do they impose asymmetric costs on traditionalist believers through enforced assent and marginalization?',
    'Comparative ethnographic and sociological study of traditionalist communities measuring psychological, social, and canonical costs of forced assent to contested reforms.',
    'If costs are real and asymmetric, the constraint is tangled rope or snare rather than rope; if genuinely cost-free, rope classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_asymmetry_in_organic_development, empirical, 'Whether organic development imposes hidden asymmetric costs').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the enforcement of the continuity reading achieved through explicit disciplinary coercion, or through internalized deference to magisterial authority that persists even when explicit sanctions are absent?',
    'Documentation of formal censorship and canonical penalties cases combined with survey or interview data on traditionalist self-censorship and identity-based compliance.',
    'If suppression is primarily internalized, the constraint''s effective extraction is higher than the structural measure suggests because the target carries the suppression after exit; if structural, extraction is bounded by institutional capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_authority__continuity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_authority__continuity_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_authority__continuity_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_authority__continuity_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_authority__continuity_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_authority__continuity_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_authority__continuity_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_authority__continuity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(vati_be_t10, vatican_ii_authority__continuity_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(vati_be_t20, vatican_ii_authority__continuity_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(vati_be_t30, vatican_ii_authority__continuity_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(vati_be_t40, vatican_ii_authority__continuity_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(vati_be_t50, vatican_ii_authority__continuity_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(vati_be_t60, vatican_ii_authority__continuity_reading, base_extractiveness, 60, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_authority__continuity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(vati_su_t10, vatican_ii_authority__continuity_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(vati_su_t20, vatican_ii_authority__continuity_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(vati_su_t30, vatican_ii_authority__continuity_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(vati_su_t40, vatican_ii_authority__continuity_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(vati_su_t50, vatican_ii_authority__continuity_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(vati_su_t60, vatican_ii_authority__continuity_reading, suppression_requirement, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The vatican_ii_authority kernel decomposes into three structurally distinct constraints because the label 'Vatican II authority' conflates three incompatible hermeneutic claims: continuity (organic development, this file), rupture (doctrinal break), and composite overdetermination (irreducible ambiguity). Each reading has a distinct beneficiary/victim structure, epsilon, and claimed type. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
