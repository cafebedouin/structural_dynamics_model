% ============================================================================
% CONSTRAINT STORY: biblical_authority__tradition_scripture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__tradition_scripture_reading, []).

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
 *   constraint_id: biblical_authority__tradition_scripture_reading
 *   human_readable: Scripture Requires Tradition for Authoritative Interpretation; Magisterium Guards Deposit of Faith
 *   domain: theological/ecclesial
 *
 * SUMMARY:
 *   This constraint models the Catholic/Orthodox reading of biblical
 *   authority: Scripture is materially insufficient for doctrine without
 *   Tradition, and the magisterium (teaching office) exercises authoritative
 *   interpretation guarding the deposit of faith. The arrangement claims to
 *   solve the coordination problem of doctrinal unity across time and
 *   cultures; the extraction is the clerical monopoly on sacramental
 *   mediation and interpretive adjudication. Lay interpretive agency is
 *   structurally excluded — the victim class. The claim/metric divergence is
 *   deliberate: the constraint is CLAIMED as tangled_rope (genuine
 *   coordination with asymmetric extraction) while the authored metrics
 *   describe substantially extractive operation with active enforcement. The
 *   engine measures this divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, 0.78).
domain_priors:suppression_score(biblical_authority__tradition_scripture_reading, 0.72).
domain_priors:theater_ratio(biblical_authority__tradition_scripture_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__tradition_scripture_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__tradition_scripture_reading, "Scripture Requires Tradition for Authoritative Interpretation; Magisterium Guards Deposit of Faith").
narrative_ontology:topic_domain(biblical_authority__tradition_scripture_reading, "theological/ecclesial").

domain_priors:requires_active_enforcement(biblical_authority__tradition_scripture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__tradition_scripture_reading, '44145c41-5ff2-44f1-aac9-465a246b0d67').
narrative_ontology:cs_kernel_codification('44145c41-5ff2-44f1-aac9-465a246b0d67', fixed_text).
narrative_ontology:cs_authority_grounding('44145c41-5ff2-44f1-aac9-465a246b0d67', lineage).
narrative_ontology:cs_interpretation_layer_present('44145c41-5ff2-44f1-aac9-465a246b0d67').
narrative_ontology:cs_reading_relation('44145c41-5ff2-44f1-aac9-465a246b0d67', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('44145c41-5ff2-44f1-aac9-465a246b0d67', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('44145c41-5ff2-44f1-aac9-465a246b0d67', foundational, scripture_requires_tradition_for_interpretation).
narrative_ontology:cs_axiom_status(scripture_requires_tradition_for_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('44145c41-5ff2-44f1-aac9-465a246b0d67', scripture_requires_tradition_for_interpretation, deontological).
narrative_ontology:cs_axiom('44145c41-5ff2-44f1-aac9-465a246b0d67', foundational, magisterium_guards_deposit_of_faith).
narrative_ontology:cs_axiom_status(magisterium_guards_deposit_of_faith, holdable).
narrative_ontology:cs_axiom_grounding('44145c41-5ff2-44f1-aac9-465a246b0d67', magisterium_guards_deposit_of_faith, conventional).
narrative_ontology:cs_axiom('44145c41-5ff2-44f1-aac9-465a246b0d67', secondary, sacraments_require_valid_ordination).
narrative_ontology:cs_axiom_status(sacraments_require_valid_ordination, holdable).
narrative_ontology:cs_axiom_grounding('44145c41-5ff2-44f1-aac9-465a246b0d67', sacraments_require_valid_ordination, conventional).
narrative_ontology:cs_reference_frame('44145c41-5ff2-44f1-aac9-465a246b0d67', apostolic_deposit_guarded_by_episcopal_succession).
narrative_ontology:cs_drift_state('44145c41-5ff2-44f1-aac9-465a246b0d67', post_vatican_ii_reception_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('44145c41-5ff2-44f1-aac9-465a246b0d67', '').
narrative_ontology:cs_kernel_id(biblical_authority__tradition_scripture_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, clerical_hierarchy).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, magisterial_institutions).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, sacramental_ministry).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, lay_interpretive_agency).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, non_magisterial_theologians).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, charismatic_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, sacramental_ministry).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, apostolic_succession_continuity).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, sacramental_causality).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, deposit_of_faith_integrity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises the magisterium: defines doctrine, authorizes sacramental ministry, adjudicates orthodoxy. Collects institutional authority, status, and material support from the faithful. Exit would mean abandoning the clerical identity and sacramental ontology that constitutes their vocation — identity_locked, not merely constrained.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, clerical_hierarchy, agenda_setter,
    institutional, generational, identity_locked, universal).

% Congregations, tribunals, universities, and curial offices that administer the interpretive and sacramental monopoly. They accrue resources, personnel, and legal privileges from the constraint's operation. Can shift emphasis between coordination and extraction functions (arbitrage) but cannot exit the structure without institutional dissolution.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, magisterial_institutions, beneficiary,
    institutional, civilizational, arbitrage, universal).

% Priests and bishops who administer sacraments as grace-conferring acts requiring valid ordination. They benefit from the monopoly on sacramental mediation (status, authority, livelihood) but pay obedience costs (canonical submission, celibacy discipline, doctrinal conformity). Exit means laicization — identity rupture.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, sacramental_ministry, beneficiary,
    organized, biographical, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, sacramental_ministry, payer).

% Baptized faithful who seek to interpret Scripture for themselves or their communities. Structurally excluded from authoritative interpretation; their interpretive acts are licensed only within magisterial boundaries. Exit requires leaving the communion (identity rupture) or internalizing subordination — identity_locked by baptismal ecclesiology.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, lay_interpretive_agency, payer,
    moderate, biographical, identity_locked, universal).

% Academic theologians whose work is subject to magisterial review (nihil obstat, mandatum). They bear censorship risk, career constraints, and silencing penalties for dissent. Exit options: move to secular academia (constrained — loss of ecclesial vocation), submit (identity_locked), or migrate to other traditions (constrained — institutional barriers).
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, non_magisterial_theologians, payer,
    organized, biographical, constrained, universal).

% Spiritual movements claiming direct pneumatic authority bypassing clerical mediation. Structurally excluded from magisterial recognition unless they submit to hierarchical oversight. Their charism is either domesticated (becomes lay movement under supervision) or suppressed — trapped by the constraint's exclusion logic.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, charismatic_movements, excluded,
    moderate, generational, trapped, universal).

% Orthodox, Protestant, and Anglican bodies engaged in formal dialogue. They analyze this constraint as an obstacle to communion (papal primacy, magisterial infallibility) but have no structural power to alter it. Their analytical seat computes the constraint's type from outside the identity_lock.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, ecumenical_dialogue_partners, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__tradition_scripture_reading, clerical_hierarchy).
narrative_ontology:fixing_cost_class(biblical_authority__tradition_scripture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents doctrinal fragmentation across time and cultures by anchoring Scripture interpretation in a living, authoritative Tradition guarded by a teaching office — solves the problem of 'what does Scripture mean?' without splintering into competing private judgments.
% TRANSFER_FUNCTION: Moves interpretive authority, sacramental mediation, and ecclesial governance from the baptized body (lay agency, charismatic gifts, local discernment) to the clerical hierarchy (magisterium, ordained ministry, canonical structures) — the transfer is authority and grace-access, not primarily money.
% ABSENT_VOICES: The laity as a class (not merely individual laypersons) — their sensus fidelium is invoked but never structurally empowered; women excluded from ordained ministry and thus from magisterial participation; Eastern Catholic churches whose conciliar tradition is subordinated to Roman centralization; victims of clerical abuse whose interpretive testimony was suppressed by the same machinery.
% DISAPPEARANCE_RATIONALE: If the magisterial interpretive monopoly vanished overnight, the Catholic/Orthodox communion would fracture into competing interpretive communities within years — sacramental validity would be contested, canonical unity would dissolve, and the institutional church would lose its defining structural coherence. The world rearranges because the constraint IS the institutional form.
% FOUNDING_PROBLEM: The early church faced proliferating heresies (Gnosticism, Arianism, etc.) claiming Scripture for contradictory doctrines. The founding problem: how to adjudicate Scripture's meaning authoritatively without either endless schism or imperial coercion. The solution: apostolic Tradition guarded by episcopal succession, later centralized in the Roman magisterium.
% FOUNDING_PROBLEM_CORROBORATION: The magisterium attests the problem is live (ongoing doctrinal confusion, relativism). Historians (Harnack, Pelikan, Ratzinger) corroborate the historical founding problem but disagree on its persistence: Harnack sees early Catholicism as the problem's institutionalization; Ratzinger sees the problem as permanent. Protestant theologians (Barth, Kung) attest the problem was solved by the founding solution but the solution became a new problem (institutionalization of authority). No single corroboration outside the beneficiary set commands consensus.
narrative_ontology:disappearance_verdict(biblical_authority__tradition_scripture_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__tradition_scripture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__tradition_scripture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(biblical_authority__tradition_scripture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__tradition_scripture_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__tradition_scripture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__tradition_scripture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the magisterium's interpretive monopoly and sacramental gatekeeping concentrate authority and status in the clerical hierarchy, with lay agency bearing the cost of exclusion. Suppression (0.72) is substantial because the constraint persists through canonical penalties (excommunication, silencing), institutional control of formation, and exclusion of dissenting voices from教职 — not merely social pressure. Theater ratio (0.35) reflects genuine coordination (doctrinal stability, sacramental assurance) mixed with performative defense of institutional prerogatives. Accessibility collapse (0.65) is moderate: alternatives exist (Protestant, Orthodox conciliar models) but are structurally delegitimated within this reading. Resistance (0.58) is significant: historical Reformation, modern dissent, and ongoing theological contestation demonstrate the constraint meets active opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the magisterial seat, the constraint is genuine coordination preserving apostolic continuity; from the lay seat, it is enforced extraction denying interpretive agency; from the analytical seat (this story), it is a tangled_rope where coordination and extraction are inseparable. The engine computes this divergence from beneficiary/victim declarations and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Clerical hierarchy and magisterial institutions are structural beneficiaries (d near 0.0): they collect interpretive authority, sacramental control, and institutional rents. Lay interpretive agency and non-magisterial theologians are structural victims (d near 1.0): they bear exclusion costs with constrained exit (identity_locked — ecclesial identity fused to magisterial communion). Sacramental ministry sits dual: beneficiary of monopoly, payer of formation/obedience costs. The engine computes per-seat χ from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (doctrinal unity against heresy/fragmentation) remains contested — the magisterium claims it is live; critics argue the problem has shifted to relevance/credibility. The arrangement persists with intensified extraction (Trent, Vatican I) and partial relaxation (Vatican II) but no sunset. Mandatrophy is unresolved: the coordination function (unity) is real but the extraction mechanism (clerical monopoly) has outgrown its justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel ''biblical_authority'' (reading_id: tradition_scripture_reading), distinct from sola_scriptura_reading and conciliar_reading?',
    'Structural decomposition: if changing the observable (which authority interprets Scripture) changes the extraction profile, the observer is looking at different constraints. This reading instantiates high clerical extraction and centralized adjudication; siblings instantiate different beneficiary/victim structures.',
    'Confirms ε-invariance: this constraint has a stable ε (0.78) assessed from this reading''s lights on the standing arrangement. Sibling readings would author different ε values over the same referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the biblical_authority kernel; sibling readings are separate constraints with distinct structural profiles.').

omega_variable(
    tradition_as_coordination_vs_extraction,
    'Does the magisterium''s interpretive monopoly serve a genuine coordination function (preventing doctrinal fragmentation) or is coordination the cover for clerical extraction?',
    'Counterfactual observation: in polities where magisterial authority weakened (post-Reformation, post-Vatican II dissent), did doctrinal fragmentation increase proportionally to the loss of coordination, or did pluralism stabilize without centralized adjudication?',
    'If fragmentation is low without magisterial monopoly, coordination is a cover and the constraint is snare; if fragmentation is high and costly, genuine tangled_rope with asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tradition_as_coordination_vs_extraction, empirical, 'Whether centralized interpretive authority is structurally necessary for doctrinal unity or a rent-extraction mechanism.').

omega_variable(
    sacramental_mediation_necessity,
    'Is sacramental mediation (clerical administration of grace) a structural requirement of the system or an extractive institutional design?',
    'Comparative ecclesiology: traditions with non-clerical sacraments or lay presidency (some Protestant, Orthodox economia) — do they exhibit grace-conferring failure or merely institutional difference?',
    'If lay-administered sacraments are functionally equivalent, clerical mediation is extractive gatekeeping; if not, it is a coordination necessity with extraction layered atop.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacramental_mediation_necessity, conceptual, 'Whether the clerical monopoly on sacramental grace-conferral is structurally necessary or extractive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__tradition_scripture_reading, 325, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t325, biblical_authority__tradition_scripture_reading, theater_ratio, 325, 0.15).
narrative_ontology:measurement(bibl_tr_t1054, biblical_authority__tradition_scripture_reading, theater_ratio, 1054, 0.22).
narrative_ontology:measurement(bibl_tr_t1517, biblical_authority__tradition_scripture_reading, theater_ratio, 1517, 0.38).
narrative_ontology:measurement(bibl_tr_t1563, biblical_authority__tradition_scripture_reading, theater_ratio, 1563, 0.42).
narrative_ontology:measurement(bibl_tr_t1870, biblical_authority__tradition_scripture_reading, theater_ratio, 1870, 0.35).
narrative_ontology:measurement(bibl_tr_t1965, biblical_authority__tradition_scripture_reading, theater_ratio, 1965, 0.28).
narrative_ontology:measurement(bibl_tr_t2024, biblical_authority__tradition_scripture_reading, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(bibl_be_t325, biblical_authority__tradition_scripture_reading, base_extractiveness, 325, 0.45).
narrative_ontology:measurement(bibl_be_t1054, biblical_authority__tradition_scripture_reading, base_extractiveness, 1054, 0.52).
narrative_ontology:measurement(bibl_be_t1517, biblical_authority__tradition_scripture_reading, base_extractiveness, 1517, 0.71).
narrative_ontology:measurement(bibl_be_t1563, biblical_authority__tradition_scripture_reading, base_extractiveness, 1563, 0.78).
narrative_ontology:measurement(bibl_be_t1870, biblical_authority__tradition_scripture_reading, base_extractiveness, 1870, 0.82).
narrative_ontology:measurement(bibl_be_t1965, biblical_authority__tradition_scripture_reading, base_extractiveness, 1965, 0.68).
narrative_ontology:measurement(bibl_be_t2024, biblical_authority__tradition_scripture_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t325, biblical_authority__tradition_scripture_reading, suppression_requirement, 325, 0.4).
narrative_ontology:measurement(bibl_su_t1054, biblical_authority__tradition_scripture_reading, suppression_requirement, 1054, 0.55).
narrative_ontology:measurement(bibl_su_t1517, biblical_authority__tradition_scripture_reading, suppression_requirement, 1517, 0.75).
narrative_ontology:measurement(bibl_su_t1563, biblical_authority__tradition_scripture_reading, suppression_requirement, 1563, 0.8).
narrative_ontology:measurement(bibl_su_t1870, biblical_authority__tradition_scripture_reading, suppression_requirement, 1870, 0.78).
narrative_ontology:measurement(bibl_su_t1965, biblical_authority__tradition_scripture_reading, suppression_requirement, 1965, 0.65).
narrative_ontology:measurement(bibl_su_t2024, biblical_authority__tradition_scripture_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__tradition_scripture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_authority__tradition_scripture_reading, 0.1).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, biblical_authority__conciliar_reading).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, sacramental_economy).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, clerical_celibacy_discipline).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, papal_infallibility_scope).

% DUAL FORMULATION NOTE:
% This reading of the biblical_authority kernel differs from sola_scriptura_reading (which eliminates the magisterial mediation layer, reducing extraction but increasing fragmentation) and conciliar_reading (which distributes interpretive authority across episcopal collegiality, reducing centralization). All three share the referent (Scripture's authority) but instantiate different beneficiary/victim structures and ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__tradition_scripture_reading, institutional, 0.1).
constraint_indexing:directionality_override(biblical_authority__tradition_scripture_reading, moderate, 0.85).
constraint_indexing:directionality_override(biblical_authority__tradition_scripture_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
