% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: vatican_ii_authority__continuity_reading
 *   human_readable: Vatican II Continuity Reading — Organic Development in Unchanging Deposit
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   The continuity reading of Vatican II authority holds that all sixteen
 *   conciliar documents express organic doctrinal development within the
 *   unchanging deposit of faith. Reforms (liturgical, ecumenical, religious
 *   liberty, collegiality) are legitimate when faithfully implementing the
 *   conciliar texts; apparent ambiguities are resolvable through the
 *   'hermeneutic of continuity' — interpreting the Council in light of the
 *   entire tradition. This reading is the official position of the
 *   post-conciliar magisterium (Paul VI through Francis) and is claimed to
 *   reflect the Council's own self-understanding. Beneficiaries are
 *   progressive reformers who gain legitimating authority for changes by
 *   framing them as continuity; the reading declares no victims, presenting
 *   reforms as cost-free development.
 *
 * KEY AGENTS:
 *   - progressive_reformers_claiming_continuity: Primary beneficiary (institutional/biographical) — gains legitimating framework for reforms
 *   - post_conciliar_magisterium: Agenda setter (institutional/generational) — administers and authoritatively interprets the continuity claim
 *   - traditionalist_communities: Excluded/potential victim (organized/biographical) — experiences reforms as rupture but is told this is continuity; exit options constrained by canonical status
 *   - theological_scholars: Observer (analytical/civilizational) — evaluates the continuity claim against historical evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__continuity_reading, 0.12).
domain_priors:suppression_score(vatican_ii_authority__continuity_reading, 0.15).
domain_priors:theater_ratio(vatican_ii_authority__continuity_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__continuity_reading, mountain).
narrative_ontology:human_readable(vatican_ii_authority__continuity_reading, "Vatican II Continuity Reading — Organic Development in Unchanging Deposit").
narrative_ontology:topic_domain(vatican_ii_authority__continuity_reading, "theology/ecclesiology/religious_authority").

domain_priors:emerges_naturally(vatican_ii_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__continuity_reading, 'eb64a673-002d-49e2-9725-24f544d0fba8').
narrative_ontology:cs_kernel_codification('eb64a673-002d-49e2-9725-24f544d0fba8', formalized).
narrative_ontology:cs_authority_grounding('eb64a673-002d-49e2-9725-24f544d0fba8', lineage).
narrative_ontology:cs_interpretation_layer_present('eb64a673-002d-49e2-9725-24f544d0fba8').
narrative_ontology:cs_reading_relation('eb64a673-002d-49e2-9725-24f544d0fba8', vatican_ii_authority__rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb64a673-002d-49e2-9725-24f544d0fba8', vatican_ii_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('eb64a673-002d-49e2-9725-24f544d0fba8', foundational, deposit_of_faith_unchanging).
narrative_ontology:cs_axiom_status(deposit_of_faith_unchanging, holdable).
narrative_ontology:cs_axiom_grounding('eb64a673-002d-49e2-9725-24f544d0fba8', deposit_of_faith_unchanging, deontological).
narrative_ontology:cs_axiom('eb64a673-002d-49e2-9725-24f544d0fba8', foundational, organic_development_preserves_identity).
narrative_ontology:cs_axiom_status(organic_development_preserves_identity, holdable).
narrative_ontology:cs_axiom_grounding('eb64a673-002d-49e2-9725-24f544d0fba8', organic_development_preserves_identity, deontological).
narrative_ontology:cs_axiom('eb64a673-002d-49e2-9725-24f544d0fba8', secondary, hermeneutic_of_continuity_resolves_ambiguity).
narrative_ontology:cs_axiom_status(hermeneutic_of_continuity_resolves_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('eb64a673-002d-49e2-9725-24f544d0fba8', hermeneutic_of_continuity_resolves_ambiguity, conventional).
narrative_ontology:cs_reference_frame('eb64a673-002d-49e2-9725-24f544d0fba8', conciliar_texts_in_tradition).
narrative_ontology:cs_drift_state('eb64a673-002d-49e2-9725-24f544d0fba8', post_synodal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eb64a673-002d-49e2-9725-24f544d0fba8', '2026-08-03T12:00:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_authority__continuity_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, progressive_reformers_claiming_continuity).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, post_conciliar_magisterium).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, doctrinal_continuity_principle).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, hermeneutic_of_continuity).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, organic_development_of_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theologians, bishops, and curial officials who advance post-conciliar reforms (liturgical revision, ecumenism, religious liberty, collegiality) by framing them as organic development of the deposit of faith. They gain legitimating authority for changes without bearing the cost of admitting rupture. Their exit is arbitrage-grade — they occupy the authoritative interpretive positions and can move between academic, episcopal, and curial roles.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, progressive_reformers_claiming_continuity, beneficiary,
    institutional, generational, arbitrage, global).

% The papacy and curial departments (especially CDF/DDF) that authoritatively define and enforce the hermeneutic of continuity. They set the agenda for what counts as legitimate development, adjudicate doctrinal disputes, and control the canonical consequences of dissent. Their position is structurally unfireable — the magisterium's authority is the constraint's enforcement mechanism.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, post_conciliar_magisterium, agenda_setter,
    institutional, generational, arbitrage, global).

% Communities (e.g., SSPX, traditionalist institutes, lay faithful attached to pre-conciliar forms) who experience the post-conciliar reforms as substantive rupture with tradition. The continuity reading declares their experience invalid — they are told the reforms are continuous, their objection is framed as disobedience or lack of faith. Exit options are constrained: remain in canonical tension, seek irregular status (SSPX pre-2009), or separate (sedevacantism). Their power is organized (institutions, communities, networks) but structurally subordinate to the magisterium.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, traditionalist_communities, excluded,
    organized, biographical, constrained, global).

% Academic theologians and historians who evaluate the continuity claim against the documentary evidence: the sixteen conciliar texts, pre-conciliar magisterium, and post-conciliar reception. They do not bear canonical penalties for their conclusions (unless they hold ecclesiastical office), but their work shapes the intellectual environment in which the constraint operates. Their exit is analytical — they can change their assessment without personal cost.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, theological_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__continuity_reading, post_conciliar_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified interpretive framework that allows the Church to claim doctrinal stability while implementing substantial changes — solving the coordination problem of how a communion grounded in unchanging truth can legitimately reform its liturgy, discipline, and pastoral posture.
% TRANSFER_FUNCTION: Moves interpretive authority from the pre-conciliar textual tradition (which appears to constrain change) to the living magisterium (which authorizes development). The 'deposit of faith' is the nominal source; the magisterium's hermeneutic is the actual operator. No material transfer; the transfer is epistemic and authoritative.
% ABSENT_VOICES: Traditionalist communities who experience rupture are structurally excluded from the authoritative interpretive circle — they are the object of the hermeneutic, not participants in it. Pre-conciliar magisterial texts (treated as sources to be interpreted, not as authoritative interlocutors) are also absent as living voices. The global South's reception of the Council (often more conservative than Northern progressive theology) is underrepresented in the official hermeneutic.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished overnight, the post-conciliar reforms would lose their primary legitimating framework. The magisterium would need a new account of why changes that appear discontinuous are not. Traditionalist communities would gain epistemic validation. The entire post-conciliar ecclesiastical structure (liturgical, canonical, ecumenical) would face a legitimacy crisis requiring either a new synthesis or formal schism.
% FOUNDING_PROBLEM: The Church faced a crisis of relevance and credibility in the modern world (1950s-60s): massive secularization, loss of intellectual authority, disconnection from contemporary culture, and the need to address religious liberty, ecumenism, and the lay apostolate without surrendering doctrinal identity. The Council was convened to 'update' (aggiornamento) the Church while preserving the deposit of faith.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Council's own documents (Gaudium et spes, Lumen gentium), by Pope John XXIII's opening address, and by historians across the theological spectrum (O'Malley, Alberigo, Komonchak). The continuity reading's claim that the problem remains live is corroborated by the magisterium's own continuing language of 'new evangelization' and 'missionary conversion' (Evangelii gaudium). However, traditionalist scholars (e.g., Davies, de Mattei) and some rupture-reading theologians corroborate that the *manner* of the Council's response created new problems — the founding problem's status is genuinely contested.
narrative_ontology:disappearance_verdict(vatican_ii_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vatican_ii_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__continuity_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(vatican_ii_authority__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(vatican_ii_authority__continuity_reading),
    narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(vatican_ii_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Low extractiveness (0.12) because the reading presents itself as descriptive of theological reality — the deposit of faith develops organically, and the Council simply manifests this development. Low suppression (0.15) because the reading does not coerce assent through penalty but through authoritative teaching; dissent is possible but carries canonical weight. Low theater (0.08) because the hermeneutic is presented as genuine interpretive work, not performance. High accessibility collapse (0.78) because the continuity claim, if accepted, makes alternatives (rupture, composite) appear as misreadings rather than live options. Low resistance (0.22) because the reading operates within the magisterium's own authoritative framework. The slight upward drift in metrics over 1965-2023 reflects accumulating tension between the continuity claim and lived experience of change, not a change in the reading's structural nature.
 *
 * PERSPECTIVAL GAP:
 *   From the magisterium's seat (agenda_setter, institutional, generational, arbitrage exit), the constraint is a mountain — the deposit of faith is unchanging, development is organic, the Council is continuous. From traditionalist communities' seat (excluded, organized, biographical, constrained exit), the same structure operates as a snare — their experience of rupture is declared invalid, their objection is framed as disobedience, exit requires schism. The engine computes this divergence from the structural data: different power/exit combinations yield different effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: progressive_reformers_claiming_continuity (gain legitimating authority for reforms without bearing cost of admitting rupture) and post_conciliar_magisterium (maintains unified authority structure). No declared victims — the reading explicitly claims reforms are cost-free development. Traditionalist communities are not listed as victims because the continuity reading denies they bear costs; their experience is categorized as 'misunderstanding' rather than extraction. This is exactly the structural profile that triggers FSM evaluation: a mountain claim with declared beneficiaries but no victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (renewal/adaptation of the Church to the modern world while preserving doctrinal integrity) is rated 'live' — the continuity reading holds that the need for authentic development persists. Mandatrophy is not resolved because the reading claims the mandate remains active and the constraint (organic development) is the permanent solution, not a temporary scaffold. The 'continuity' claim itself is the mandate — it does not expire.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_naturalness_ambiguity,
    'Is the continuity reading a genuine mountain of theological structure (unchangeable organic development) or a constructed constraint that benefits progressive reformers by framing contested changes as cost-free development?',
    'Historical-theological analysis of whether pre-conciliar doctrinal principles entail post-conciliar conclusions without supplemental premises; examination of whether ''organic development'' functions as a ratchet that only moves in one direction.',
    'If constructed, the mountain claim is a false summit — FSM signature would reclassify as tangled_rope with progressive reformers as beneficiaries and traditionalist communities as victims bearing the cost of framed-as-natural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_naturalness_ambiguity, conceptual, 'Natural-law vs. constructed framing of doctrinal continuity').

omega_variable(
    kernel_reading_vatican_ii_authority,
    'This constraint is the continuity_reading of kernel vatican_ii_authority. How does its structural profile differ from the rupture_reading and composite_overdetermination_reading, and where is the disagreement located?',
    'Compare the three readings'' beneficiary/victim structures, extractiveness values, and claimed types. The continuity reading claims zero victims and low extraction; rupture reading claims traditionalists as victims and high extraction; composite reading claims structural ambiguity as the primary feature.',
    'If the three readings have substantially different ε values and structural profiles, they are distinct constraints linked by network.affects_constraints, not one constraint with measurement variance. Confirms ε-invariance decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_vatican_ii_authority, conceptual, 'Commitment system kernel decomposition: continuity vs. rupture vs. composite readings').

omega_variable(
    hermeneutic_closure_mechanism,
    'Does the ''hermeneutic of continuity'' function as a genuine interpretive method that resolves ambiguities, or as a closure device that pre-judges the outcome of interpretation in favor of the post-conciliar status quo?',
    'Case analysis of post-conciliar magisterial documents citing the hermeneutic: do they demonstrate genuine interpretive work on contested texts, or do they only ratify pre-determined conclusions?',
    'If closure device, the reading''s low suppression metric understates the actual coercive force — internalized suppression operates through the interpretive frame itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutic_closure_mechanism, conceptual, 'Interpretive method vs. closure mechanism in continuity hermeneutic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__continuity_reading, 1965, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vatican_ii_continuity_tr_t1965, vatican_ii_authority__continuity_reading, theater_ratio, 1965, 0.02).
narrative_ontology:measurement(vatican_ii_continuity_tr_t1985, vatican_ii_authority__continuity_reading, theater_ratio, 1985, 0.04).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2000, vatican_ii_authority__continuity_reading, theater_ratio, 2000, 0.06).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2013, vatican_ii_authority__continuity_reading, theater_ratio, 2013, 0.07).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2023, vatican_ii_authority__continuity_reading, theater_ratio, 2023, 0.08).

% Extraction over time
narrative_ontology:measurement(vatican_ii_continuity_be_t1965, vatican_ii_authority__continuity_reading, base_extractiveness, 1965, 0.05).
narrative_ontology:measurement(vatican_ii_continuity_be_t1985, vatican_ii_authority__continuity_reading, base_extractiveness, 1985, 0.08).
narrative_ontology:measurement(vatican_ii_continuity_be_t2000, vatican_ii_authority__continuity_reading, base_extractiveness, 2000, 0.1).
narrative_ontology:measurement(vatican_ii_continuity_be_t2013, vatican_ii_authority__continuity_reading, base_extractiveness, 2013, 0.11).
narrative_ontology:measurement(vatican_ii_continuity_be_t2023, vatican_ii_authority__continuity_reading, base_extractiveness, 2023, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(vatican_ii_continuity_su_t1965, vatican_ii_authority__continuity_reading, suppression_requirement, 1965, 0.05).
narrative_ontology:measurement(vatican_ii_continuity_su_t1985, vatican_ii_authority__continuity_reading, suppression_requirement, 1985, 0.1).
narrative_ontology:measurement(vatican_ii_continuity_su_t2000, vatican_ii_authority__continuity_reading, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement(vatican_ii_continuity_su_t2013, vatican_ii_authority__continuity_reading, suppression_requirement, 2013, 0.14).
narrative_ontology:measurement(vatican_ii_continuity_su_t2023, vatican_ii_authority__continuity_reading, suppression_requirement, 2023, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__continuity_reading, 0.08).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, post_conciliar_liturgical_reform).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, ecumenical_dialogue_framework).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, religious_liberty_doctrine_dignitatis_humanae).

% DUAL FORMULATION NOTE:
% Vatican II authority kernel decomposes into three constraint stories per ε-invariance: continuity_reading (this file, ε=0.12, mountain claimed), rupture_reading (ε≈0.65, snare/tangled_rope claimed), composite_overdetermination_reading (ε≈0.35, tangled_rope claimed). The three readings share the same referent (the Council's authority) but instantiate different constraints with different ε, different beneficiary/victim structures, different types. Linked via network.affects_constraints. The upstream kernel (the Council event/texts) influences all three; the continuity reading is the magisterium's authorized reading and structurally influences downstream constraints (liturgical reform, ecumenism, religious liberty) by providing their legitimating framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__continuity_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
