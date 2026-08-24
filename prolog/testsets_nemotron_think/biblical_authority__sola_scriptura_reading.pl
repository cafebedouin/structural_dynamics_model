% ============================================================================
% CONSTRAINT STORY: biblical_authority__sola_scriptura_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: biblical_authority__sola_scriptura_reading
 *   human_readable: Sola Scriptura: Scripture Alone as Sufficient and Self-Interpreting Authority
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   The sola scriptura reading asserts that Scripture alone is materially
 *   sufficient and formally perspicuous for all matters of doctrine and
 *   practice, requiring no infallible magisterium or binding tradition for
 *   authoritative interpretation. Emerging from the 16th-century Reformation,
 *   this principle restructured Christian authority by transferring
 *   interpretive competence from a clerical hierarchy to the individual
 *   believer guided by the Spirit. The constraint operates as a tangled rope:
 *   it genuinely coordinates by enabling direct access to divine revelation
 *   (beneficiary: lay autonomy) while extracting the cost of doctrinal
 *   fragmentation across communities (victim: ecumenical coherence). The
 *   claim/metric gap is deliberate: the reading claims to be a rope (pure
 *   coordination restoring apostolic simplicity) while the authored metrics
 *   describe a constraint with substantial extractive overhead
 *   (fragmentation, loss of adjudicative unity). The engine measures that
 *   divergence; do not reconcile.
 *
 * KEY AGENTS:
 *   - lay_believers: Primary beneficiary (moderate/constrained) — gains interpretive autonomy, bears fragmentation risk
 *   - congregational_leaders: Beneficiary/agenda_setter (organized/constrained) — exercises local authority without higher oversight
 *   - ecumenical_church_bodies: Victim/payer (institutional/constrained) — loses doctrinal coherence and shared sacramental unity
 *   - theological_educators_seeking_unity: Victim/observer (organized/mobile) — bears pedagogical cost of fragmentation
 *   - reformers: Historical agenda_setter (powerful/arbitrage) — instituted the principle against magisterial resistance
 *   - catholic_magisterium: Excluded (institutional/trapped) — its authority is structurally denied by the principle
 *   - secular_scholars: Observer (analytical/analytical) — analyzes the constraint from outside the faith commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.42).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.68).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Sola Scriptura: Scripture Alone as Sufficient and Self-Interpreting Authority").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "theology/religious_studies/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__sola_scriptura_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, 'cbe1bfd7-ff35-4cc1-b689-9236f439e548').
narrative_ontology:cs_kernel_codification('cbe1bfd7-ff35-4cc1-b689-9236f439e548', fixed_text).
narrative_ontology:cs_authority_grounding('cbe1bfd7-ff35-4cc1-b689-9236f439e548', lineage).
narrative_ontology:cs_interpretation_layer_present('cbe1bfd7-ff35-4cc1-b689-9236f439e548').
narrative_ontology:cs_reading_relation('cbe1bfd7-ff35-4cc1-b689-9236f439e548', biblical_authority__tradition_scripture_reading, forecloses).
narrative_ontology:cs_reading_relation('cbe1bfd7-ff35-4cc1-b689-9236f439e548', biblical_authority__conciliar_reading, forecloses).
narrative_ontology:cs_axiom('cbe1bfd7-ff35-4cc1-b689-9236f439e548', foundational, scripture_alone_sufficient_and_self_interpreting).
narrative_ontology:cs_axiom_status(scripture_alone_sufficient_and_self_interpreting, holdable).
narrative_ontology:cs_axiom_grounding('cbe1bfd7-ff35-4cc1-b689-9236f439e548', scripture_alone_sufficient_and_self_interpreting, deontological).
narrative_ontology:cs_axiom('cbe1bfd7-ff35-4cc1-b689-9236f439e548', foundational, priesthood_of_all_believers_entails_interpretive_competence).
narrative_ontology:cs_axiom_status(priesthood_of_all_believers_entails_interpretive_competence, holdable).
narrative_ontology:cs_axiom_grounding('cbe1bfd7-ff35-4cc1-b689-9236f439e548', priesthood_of_all_believers_entails_interpretive_competence, deontological).
narrative_ontology:cs_reference_frame('cbe1bfd7-ff35-4cc1-b689-9236f439e548', apostolic_scripture_sufficiency).
narrative_ontology:cs_drift_state('cbe1bfd7-ff35-4cc1-b689-9236f439e548', contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cbe1bfd7-ff35-4cc1-b689-9236f439e548', '').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, congregational_leaders).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, ecumenical_church_bodies).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, theological_educators_seeking_unity).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, scripture_sufficiency).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, scripture_perspicuity).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, priesthood_of_all_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individual Christians who read Scripture as the sole authority for faith and practice. They gain direct interpretive access without clerical mediation, but bear the risk of error and the burden of discernment. Exit from the principle means adopting a tradition-mediated reading, which requires submitting to an external authority — a significant identity shift.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, lay_believers, beneficiary,
    moderate, biographical, constrained, global).

% Pastors, elders, and lay leaders in congregational polities who exercise teaching and governance authority locally without higher ecclesiastical oversight. They benefit from the principle's delegation of authority but are constrained by the need to maintain congregational consensus. Exit means joining a connectional or episcopal system, surrendering local autonomy.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, congregational_leaders, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(biblical_authority__sola_scriptura_reading, congregational_leaders, agenda_setter).

% Denominations and councils (e.g., World Council of Churches, Catholic-Orthodox dialogue) that seek visible unity and doctrinal coherence across Christian communities. They bear the cost of sola scriptura's fragmentation: each new interpretive community multiplies the obstacles to consensus. Their exit options are limited because the principle is embedded in the constitutional identity of their dialogue partners.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, ecumenical_church_bodies, payer,
    institutional, generational, constrained, global).

% Seminary professors and ecumenical theologians who attempt to teach a coherent Christian doctrine across traditions. They pay in increased pedagogical complexity: they must navigate dozens of incompatible sola scriptura interpretations. They can exit by narrowing their scope to a single tradition, but that abandons the ecumenical vocation.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, theological_educators_seeking_unity, payer,
    organized, biographical, mobile, global).

% 16th-century figures (Luther, Calvin, Zwingli, etc.) who instituted the principle against the magisterium. They had arbitrage-grade exit: they could appeal to Scripture against the church because they controlled the reforming movement's narrative and political protection. Their structural position was unique to the founding moment.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, reformers, agenda_setter,
    powerful, generational, arbitrage, continental).

% The teaching office of the Catholic Church (pope and bishops in communion) whose authority to interpret Scripture authentically is denied by the sola scriptura principle. They are structurally excluded from the interpretive game the constraint defines. Their exit would require abandoning the claim to divine institution — identity-locked at the institutional level.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, catholic_magisterium, excluded,
    institutional, civilizational, trapped, global).

% Historians, sociologists, and philosophers of religion who analyze the constraint from outside the faith commitment. They neither collect nor pay; they map the structural dynamics. Their exit is analytical — they can change frameworks without personal cost.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, secular_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:fixing_cost_class(biblical_authority__sola_scriptura_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables every believer to access divine revelation directly without clerical gatekeeping, solving the coordination problem of epistemic access to authority in a fragmented religious landscape.
% TRANSFER_FUNCTION: Moves interpretive authority and doctrinal decision-making from a centralized magisterium/tradition to the individual believer and local congregation, transferring the cost of adjudication from the center to the periphery.
% ABSENT_VOICES: The pre-Reformation consensus of the undivided church (patristic and conciliar) is structurally absent — its authority is the very thing the constraint suppresses. Also absent are the laity in traditions that never adopted the principle (Catholic, Orthodox), who would object to the characterization of their tradition as 'clerical gatekeeping' but are not seated at the sola scriptura table.
% DISAPPEARANCE_RATIONALE: If sola scriptura vanished overnight, Protestant denominations would lose their constitutional principle. They would either fragment further into pure subjectivism, adopt confessional standards as de facto magisteria, or seek reunion with Rome/Orthodoxy. The global Christian landscape would reorganize around renewed debates about authority, tradition, and Scripture.
% FOUNDING_PROBLEM: The late medieval church restricted lay access to Scripture (vernacular translations banned, interpretation reserved to magisterium), creating an epistemic gap between the faithful and their supreme authority. The Reformers built sola scriptura to close that gap: if Scripture is God's word, it must be accessible and intelligible to all God's people without institutional mediation.
% FOUNDING_PROBLEM_CORROBORATION: Protestant historians (e.g., Brad Gregory, *The Unintended Reformation*) attest the founding problem was real but argue the solution generated worse fragmentation. Catholic historians (e.g., John O'Malley, *Trent*) attest the late medieval restrictions were real but contest that sola scriptura was the necessary remedy. Secular scholars of religion (e.g., Charles Taylor, *A Secular Age*) corroborate that the principle restructured Western epistemic authority, enabling both modern individualism and religious pluralism — corroboration from outside the beneficiary set.
narrative_ontology:disappearance_verdict(biblical_authority__sola_scriptura_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__sola_scriptura_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_authority__sola_scriptura_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__sola_scriptura_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is moderate (0.42) because the constraint transfers adjudicative authority from a centralized magisterium to distributed believers, generating fragmentation as a byproduct. Suppression is high (0.68) because the principle actively denies epistemic legitimacy to tradition and councils, not merely ignoring them. Theater is low (0.18): the principle is genuinely held and functionally operative, not performative. Accessibility collapse is moderate (0.55): once accepted, alternative authority structures (magisterium, conciliar consensus) become epistemically inaccessible, but interpretive pluralism within sola scriptura remains high. Resistance is high (0.75) from Catholic, Orthodox, and ecumenical traditions that view the principle as schismatic. The measurement series runs on a shared time grid (0, 100, 200, 300, 400, 507) covering the Reformation to present.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (lay believer), the constraint appears as a rope: genuine coordination restoring direct access to God's word. From the victim seat (ecumenical body), it appears as a snare: extraction of unity without compensation. The engine computes this divergence from the structural data — the authored claim (tangled_rope) acknowledges both faces.
 *
 * DIRECTIONALITY LOGIC:
 *   Lay believers and congregational leaders are structural beneficiaries (d near 0.2): they receive interpretive autonomy and local authority. Ecumenical bodies and unity-seeking educators are structural targets (d near 0.8): they bear the cost of fragmentation without the compensating autonomy. The reformers were historical agenda_setters with arbitrage-grade exit (they created the constraint). The Catholic magisterium is excluded (d not applicable) — its authority is the object of suppression. Secular scholars are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (epistemic access to divine revelation for all believers) remains live, but the arrangement's adjudicative vacuum has generated a secondary problem (doctrinal chaos) that the original principle cannot solve. The mandate has not atrophied — the principle still coordinates — but it has accumulated extractive overhead (fragmentation) that no party benefits from enough to maintain, and no party is hurt enough to fix (prohibitive fixing cost). This is the tangled_rope signature: coordination function persists alongside asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the sola scriptura reading a genuine recovery of apostolic authority or a constructed interpretive principle that fragments the church?',
    'Historical theology and patristic studies assessing whether the early church operated on a materially sufficient Scripture principle versus a tradition-mediated one; ecumenical dialogue outcomes on authority.',
    'If the reading is a recovery, its coordination function is vindicated and extraction is lower; if constructed, the fragmentation is extractive overhead and the constraint leans toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the sola scriptura principle is a natural feature of Christian authority or a Reformation-era construction.').

omega_variable(
    coordination_extraction_boundary,
    'Is doctrinal fragmentation a necessary cost of lay autonomy (coordination overhead) or an extractive byproduct that serves no coordination function?',
    'Comparative study of Protestant denominations: those with robust confessional standards versus those with radical individualism; measure correlation between interpretive autonomy and communal cohesion.',
    'If fragmentation is necessary overhead, the constraint remains tangled_rope; if fragmentation serves no coordination function and only benefits partisan actors, it trends toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the measured fragmentation is structural coordination cost or pure extraction.').

omega_variable(
    suppression_of_tradition_mechanism,
    'Does sola scriptura suppress tradition and magisterial authority structurally (by denying their epistemic status) or internalizedly (by forming consciences that reject them)?',
    'Sociology of knowledge: examine whether converts from high-tradition traditions experience suppression as external barrier or internal conviction; longitudinal study of Protestant seminaries'' formation curricula.',
    'If internalized, the constraint''s effective suppression is higher than institutional measures suggest; the engine''s suppression scalar understates the barrier to exit for formed subjects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_tradition_mechanism, empirical, 'Structural vs. internalized suppression of alternative interpretive authorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 0, 507).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_tr_t0, biblical_authority__sola_scriptura_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_tr_t100, biblical_authority__sola_scriptura_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_tr_t200, biblical_authority__sola_scriptura_reading, theater_ratio, 200, 0.14).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_tr_t300, biblical_authority__sola_scriptura_reading, theater_ratio, 300, 0.15).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_tr_t400, biblical_authority__sola_scriptura_reading, theater_ratio, 400, 0.17).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_tr_t507, biblical_authority__sola_scriptura_reading, theater_ratio, 507, 0.18).

% Extraction over time
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_be_t0, biblical_authority__sola_scriptura_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_be_t100, biblical_authority__sola_scriptura_reading, base_extractiveness, 100, 0.3).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_be_t200, biblical_authority__sola_scriptura_reading, base_extractiveness, 200, 0.35).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_be_t300, biblical_authority__sola_scriptura_reading, base_extractiveness, 300, 0.38).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_be_t400, biblical_authority__sola_scriptura_reading, base_extractiveness, 400, 0.4).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_be_t507, biblical_authority__sola_scriptura_reading, base_extractiveness, 507, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_su_t0, biblical_authority__sola_scriptura_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_su_t100, biblical_authority__sola_scriptura_reading, suppression_requirement, 100, 0.62).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_su_t200, biblical_authority__sola_scriptura_reading, suppression_requirement, 200, 0.65).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_su_t300, biblical_authority__sola_scriptura_reading, suppression_requirement, 300, 0.66).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_su_t400, biblical_authority__sola_scriptura_reading, suppression_requirement, 400, 0.67).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_su_t507, biblical_authority__sola_scriptura_reading, suppression_requirement, 507, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_authority__sola_scriptura_reading, 0.08).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__conciliar_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the biblical_authority kernel. The sola scriptura reading asserts Scripture's material sufficiency and formal perspicuity; the tradition_scripture_reading asserts Scripture's material insufficiency without tradition; the conciliar_reading asserts Scripture's authoritative interpretation through conciliar reception. The three readings form a constraint family linked by mutual foreclosure: each reading's core premise logically excludes the others within a single authority framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
