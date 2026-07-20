% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__unitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__unitarian_reading, []).

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
 *   constraint_id: biblical_divine_nature__unitarian_reading
 *   human_readable: Unitarian Reading of Biblical Divine Nature
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   This constraint story instantiates the unitarian reading of the
 *   biblical_divine_nature kernel: the claim that God is numerically one
 *   person, the Father alone, with the Son and Holy Spirit subordinate or
 *   created beings. It is one reading of a contested kernel, alongside the
 *   trinitarian_reading and modalist_reading siblings. The constraint
 *   operates as a commitment system with low institutional authority and flat
 *   ecclesiology. Its structural asymmetry lies in coordinating
 *   non-Trinitarian believers under simple monotheism while extracting
 *   ontological status from the Son and Spirit and stripping authority from
 *   Trinitarian institutional hierarchies and credal orthodoxy.
 *
 * KEY AGENTS:
 *   - unitarian_communities: Primary beneficiary/agenda_setter (organized/constrained) â gains flat ecclesiology and theological simplicity
 *   - lay_believers: Secondary beneficiary (powerless/constrained) â gains direct access to the Father
 *   - institutional_hierarchy: Primary victim (institutional/identity_locked) â bears loss of creedal authority
 *   - credal_orthodoxy: Secondary victim (organized/identity_locked) â bears delegitimation of conciliar tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, 0.48).
domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, 0.58).
domain_priors:theater_ratio(biblical_divine_nature__unitarian_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Unitarian Reading of Biblical Divine Nature").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__unitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, '6217fea6-fb4c-40fd-a302-36f2c49035c2').
narrative_ontology:cs_kernel_codification('6217fea6-fb4c-40fd-a302-36f2c49035c2', fixed_text).
narrative_ontology:cs_authority_grounding('6217fea6-fb4c-40fd-a302-36f2c49035c2', distributed).
narrative_ontology:cs_reading_relation('6217fea6-fb4c-40fd-a302-36f2c49035c2', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('6217fea6-fb4c-40fd-a302-36f2c49035c2', biblical_divine_nature__modalist_reading, coexists_with).
narrative_ontology:cs_axiom('6217fea6-fb4c-40fd-a302-36f2c49035c2', foundational, father_alone_is_true_god).
narrative_ontology:cs_axiom_status(father_alone_is_true_god, holdable).
narrative_ontology:cs_axiom_grounding('6217fea6-fb4c-40fd-a302-36f2c49035c2', father_alone_is_true_god, theological).
narrative_ontology:cs_axiom('6217fea6-fb4c-40fd-a302-36f2c49035c2', foundational, son_is_created_being).
narrative_ontology:cs_axiom_status(son_is_created_being, holdable).
narrative_ontology:cs_axiom_grounding('6217fea6-fb4c-40fd-a302-36f2c49035c2', son_is_created_being, theological).
narrative_ontology:cs_reference_frame('6217fea6-fb4c-40fd-a302-36f2c49035c2', strict_biblical_monotheism).
narrative_ontology:cs_drift_state('6217fea6-fb4c-40fd-a302-36f2c49035c2', post_nicene_dominance, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('6217fea6-fb4c-40fd-a302-36f2c49035c2', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, unitarian_communities).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, lay_believers).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, institutional_hierarchy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, credal_orthodoxy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that maintain the unitarian reading through biblical interpretation and flat ecclesiology; they set the doctrinal agenda locally, resist Trinitarian institutional authority, and enforce the subordination or created status of the Son and Spirit within their boundaries.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, unitarian_communities, agenda_setter,
    organized, generational, constrained, global).

% Individual believers who benefit from a simple monotheistic framework and direct access to the Father without the cognitive and ecclesial overhead of Trinitarian metaphysics; they are coordinated into worship communities that reject creedal mediation.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, lay_believers, beneficiary,
    powerless, biographical, constrained, global).

% The credal institutional churches whose authority and legitimacy depend on Nicene Trinitarianism as a boundary marker. The unitarian reading strips their theological foundation, renders their hierarchical mediation superfluous, and recasts their object of worship as a created being.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, institutional_hierarchy, payer,
    institutional, civilizational, identity_locked, global).

% The tradition of creedal formulation and conciliar theology that defines mainstream Christian orthodoxy. The unitarian reading delegitimizes the Nicene and Athanasian creeds by classifying the Son and Spirit as subordinate or created, imposing a severe authority cost on the tradition.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, credal_orthodoxy, payer,
    organized, civilizational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates monotheistic worship by identifying exactly one divine person (the Father) as God, eliminating ontological complexity between believers and the divine, and enabling flat ecclesiology without mediatory hierarchy.
% TRANSFER_FUNCTION: Transfers authority and worship-capacity away from the Son and Holy Spirit (reclassified as subordinate or created beings) and away from Trinitarian institutional hierarchies and credal traditions, toward the Father alone and toward direct, unmediated believer communities.
% ABSENT_VOICES: Trinitarian theologians whose exegetical and metaphysical objections are structurally excluded from the unitarian interpretive community; the Spirit as a person with independent agency, whose voice is muted by subordination or creation classification; and modalist Christians who affirm numerical singularity but are excluded by the unitarian insistence on the Son's distinct creaturehood.
% DISAPPEARANCE_RATIONALE: If this doctrinal constraint vanished, Trinitarian hierarchies would regain epistemic authority, creedal boundaries would re-solidify around triune ontology, and unitarian communities would lose their primary theological distinctives; the shape of Christian worship and ecclesiology would reorganize around a triune God with mediatory institutions.
% FOUNDING_PROBLEM: The problem of maintaining strict monotheism within a framework that also reveres Jesus and the Spirit; the unitarian reading solves this by locating divinity exclusively in the Father and assigning derivative or created status to the Son and Spirit.
% FOUNDING_PROBLEM_CORROBORATION: Unitarian apologists attest the problem is live, citing the Shema and the dangers of tri-theism. Trinitarian theologians from outside the benefiting parties attest that the problem was solved at Nicaea through consubstantiality and that the unitarian reading resurrects a dead controversy; external historians of doctrine note the political context of conciliar decisions but do not corroborate the theological necessity of the unitarian solution.
narrative_ontology:disappearance_verdict(biblical_divine_nature__unitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__unitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__unitarian_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_divine_nature__unitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__unitarian_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__unitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__unitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects moderate authority extraction from institutional hierarchy and ontological subordination of Son/Spirit, balanced by genuine coordination of monotheistic worship. Suppression (0.58) captures the active enforcement required to maintain the reading against Trinitarian hegemony and to police internal boundaries against modalist or Trinitarian inroads. Theater_ratio (0.20) is low because the reading is held sincerely without heavy performative maintenance. Accessibility_collapse (0.75) is high: once the unitarian hermeneutic is adopted, Trinitarian metaphysics becomes cognitively inaccessible. Resistance (0.80) is very high due to millennia of Trinitarian institutional dominance.
 *
 * PERSPECTIVAL GAP:
 *   The unitarian communities experience the constraint as liberating coordination (simple worship, flat structure), while the institutional hierarchy experiences it as severe extraction of their foundational legitimacy. The engine computes this divergence from beneficiary/victim declarations and exit options: identity_locked institutional actors face amplified effective extraction, while organized beneficiaries with constrained exit face moderated extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Unitarian communities and lay believers are structural beneficiaries (low directionality), gaining theological simplicity and ecclesial autonomy. Institutional hierarchy and credal orthodoxy are structural victims (high directionality), bearing the cost of delegitimized authority and creedal obsolescence. The directionality split is driven by the power and exit asymmetry: the hierarchy is identity_locked to Trinitarianism, while believers have constrained but real exit into unitarian communities.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as tangled_rope prevents mislabeling the genuine coordination function (monotheistic clarity, direct worship, flat ecclesiology) as pure extraction, while the victim declarations prevent mislabeling the authority transfer as pure coordination. The active enforcement requirement (doctrinal boundary maintenance, polemical argument, exclusion of Trinitarian teachers) reflects the hybrid nature: the reading must be actively maintained because its natural theological equilibrium in the broader Christian ecosystem is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unitarian_political_economy,
    'Is the flat ecclesiology of the unitarian reading a genuine theological consequence of biblical monotheism, or a political strategy to resist centralized church authority?',
    'Historical analysis of unitarian movements'' socio-political context, comparing communities that emerged under state pressure versus those that emerged through exegetical reform.',
    'If primarily political, extraction classification strengthens toward snare; if theological, remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unitarian_political_economy, conceptual, 'Whether flat ecclesiology is theological consequence or political strategy.').

omega_variable(
    son_spirit_ontology_cost,
    'Does classifying the Son and Spirit as created or subordinate impose an irreducible soteriological and devotional cost, or is it merely a neutral ontological rearrangement?',
    'Analysis of worship practices and soteriological frameworks in unitarian communities compared to Trinitarian communities, measuring devotional intensity and soteriological reliance on the Son.',
    'If high cost, victim set expands to include the Son and Spirit as direct victims; if neutral, extractiveness is lower and the reading leans toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(son_spirit_ontology_cost, empirical, 'Whether Son/Spirit subordination carries irreducible devotional cost.').

omega_variable(
    enforcement_asymmetry,
    'Is the reading''s persistence driven more by internal theological conviction or by external opposition from Trinitarian institutions?',
    'Compare growth and retention rates of unitarian communities in permissive versus hostile environments; measure internal boundary enforcement versus external persecution effects.',
    'If external opposition is the main binding force, identity_locked dynamics dominate and the constraint is more extractive; if internal conviction dominates, the coordination function is primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry, empirical, 'Internal conviction vs external opposition as binding force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 0, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biblical_divine_nature_unit_tr_t0, biblical_divine_nature__unitarian_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(biblical_divine_nature_unit_tr_t300, biblical_divine_nature__unitarian_reading, theater_ratio, 300, 0.15).
narrative_ontology:measurement(biblical_divine_nature_unit_tr_t600, biblical_divine_nature__unitarian_reading, theater_ratio, 600, 0.2).
narrative_ontology:measurement(biblical_divine_nature_unit_tr_t1200, biblical_divine_nature__unitarian_reading, theater_ratio, 1200, 0.22).
narrative_ontology:measurement(biblical_divine_nature_unit_tr_t1600, biblical_divine_nature__unitarian_reading, theater_ratio, 1600, 0.2).
narrative_ontology:measurement(biblical_divine_nature_unit_tr_t1800, biblical_divine_nature__unitarian_reading, theater_ratio, 1800, 0.2).

% Extraction over time
narrative_ontology:measurement(biblical_divine_nature_unit_be_t0, biblical_divine_nature__unitarian_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(biblical_divine_nature_unit_be_t300, biblical_divine_nature__unitarian_reading, base_extractiveness, 300, 0.55).
narrative_ontology:measurement(biblical_divine_nature_unit_be_t600, biblical_divine_nature__unitarian_reading, base_extractiveness, 600, 0.6).
narrative_ontology:measurement(biblical_divine_nature_unit_be_t1200, biblical_divine_nature__unitarian_reading, base_extractiveness, 1200, 0.52).
narrative_ontology:measurement(biblical_divine_nature_unit_be_t1600, biblical_divine_nature__unitarian_reading, base_extractiveness, 1600, 0.48).
narrative_ontology:measurement(biblical_divine_nature_unit_be_t1800, biblical_divine_nature__unitarian_reading, base_extractiveness, 1800, 0.48).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(biblical_divine_nature__unitarian_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__unitarian_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, modalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel biblical_divine_nature. The label 'biblical divine nature' conflates three structurally distinct claims: Trinitarian (three hypostases, one ousia), Unitarian (Father alone is God; Son/Spirit subordinate/created), and Modalist (one person in sequential modes). Each reading has a different epsilon, beneficiary/victim structure, and classification. They are modeled as separate stories linked via network.affects_constraints, not as one story with adjustable parameters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
