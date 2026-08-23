% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_progressive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_progressive_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_progressive_reading
 *   human_readable: Vatican II Doctrinal Authority â Rupture-Progressive Reading
 *   domain: ecclesiological/institutional/hermeneutic
 *
 * SUMMARY:
 *   This constraint instantiates the rupture-progressive reading of the
 *   Vatican II doctrinal authority kernel. It treats the Second Vatican
 *   Council as a necessary break with pre-conciliar rigidity and holds that
 *   the 'spirit of the Council' authorizes ongoing reform beyond the
 *   conciliar texts themselves. The constraint is the interpretive authority
 *   structure that grounds post-conciliar progressive governance, liturgical
 *   change, and doctrinal development in a dynamic hermeneutic. Because this
 *   reading extracts authority from traditionalist Catholics, pre-conciliar
 *   religious institutes, and the Roman Curia while coordinating progressive
 *   reformers, it functions as a tangled rope: genuine coordination for one
 *   faction and asymmetric extraction for another, held in place by active
 *   episcopal and curial enforcement.
 *
 * KEY AGENTS:
 *   - postconciliar_reform_hierarchy: Primary agenda-setter (institutional/mobile) â sets the interpretive framework and collects expanded authority.
 *   - conciliar_theologians: Primary beneficiary (organized/mobile) â intellectual legitimators who gain prestige and platforms.
 *   - traditionalist_catholics: Primary target (organized/identity_locked) â bear extraction of liturgical and doctrinal heritage.
 *   - pre_conciliar_religious_institutes: Secondary target (moderate/constrained) â subjected to imposed renewal programs.
 *   - roman_curia_officials: Institutional target (institutional/constrained) â lost centralized authority to collegiality.
 *   - catholic_academic_historians: Analytical observer (analytical/analytical) â documents the text-to-spirit gap without institutional stake.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.72).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.68).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "Vatican II Doctrinal Authority â Rupture-Progressive Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "ecclesiological/institutional/hermeneutic").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'c1f519a4-d590-4c03-a408-c54f28d95e87').
narrative_ontology:cs_kernel_codification('c1f519a4-d590-4c03-a408-c54f28d95e87', fixed_text).
narrative_ontology:cs_authority_grounding('c1f519a4-d590-4c03-a408-c54f28d95e87', lineage).
narrative_ontology:cs_interpretation_layer_present('c1f519a4-d590-4c03-a408-c54f28d95e87').
narrative_ontology:cs_reading_relation('c1f519a4-d590-4c03-a408-c54f28d95e87', vatican_ii_doctrinal_authority__continuity_reading, influences).
narrative_ontology:cs_reading_relation('c1f519a4-d590-4c03-a408-c54f28d95e87', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1f519a4-d590-4c03-a408-c54f28d95e87', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('c1f519a4-d590-4c03-a408-c54f28d95e87', foundational, conciliar_spirit_supersedes_letter).
narrative_ontology:cs_axiom_status(conciliar_spirit_supersedes_letter, holdable).
narrative_ontology:cs_axiom_grounding('c1f519a4-d590-4c03-a408-c54f28d95e87', conciliar_spirit_supersedes_letter, conventional).
narrative_ontology:cs_axiom('c1f519a4-d590-4c03-a408-c54f28d95e87', foundational, religious_freedom_as_doctrinal_reversal).
narrative_ontology:cs_axiom_status(religious_freedom_as_doctrinal_reversal, holdable).
narrative_ontology:cs_axiom_grounding('c1f519a4-d590-4c03-a408-c54f28d95e87', religious_freedom_as_doctrinal_reversal, theological).
narrative_ontology:cs_reference_frame('c1f519a4-d590-4c03-a408-c54f28d95e87', postconciliar_reform_horizon).
narrative_ontology:cs_drift_state('c1f519a4-d590-4c03-a408-c54f28d95e87', contemporary_church, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c1f519a4-d590-4c03-a408-c54f28d95e87', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, postconciliar_reform_hierarchy).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, conciliar_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_catholics).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_religious_institutes).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, roman_curia_officials).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, religious_freedom_as_development).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, collegiality_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenism_as_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Episcopal conferences and reform-oriented bishops who invoke the 'spirit of the Council' to authorize liturgical adaptations, doctrinal developments, and decentralized governance. They set the interpretive agenda for post-conciliar Catholicism and benefit from expanded authority transferred from Rome to local churches.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, postconciliar_reform_hierarchy, agenda_setter,
    institutional, generational, mobile, global).

% Academic theologians and conciliar periti who interpret ambiguous conciliar passages as intentional openings. They gain institutional prestige, publishing platforms, and advisory roles by elaborating the 'spirit' into new doctrinal and ethical positions beyond the conciliar texts.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, conciliar_theologians, beneficiary,
    organized, generational, mobile, global).

% Clergy and laity attached to the Tridentine liturgy, pre-conciliar catechesis, and the Syllabus of Errors framework. They experience progressive implementation as unilateral extraction of their worship forms and doctrinal certainties. Their exit options are schism or internal exile, both carrying heavy identity costs.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_catholics, payer,
    organized, biographical, identity_locked, global).

% Religious orders founded on pre-conciliar charisms and discipline that were subjected to post-conciliar renewal programs, habit changes, and liturgical alterations. Many experienced collapsed vocations and identity after reforms were imposed by superiors invoking the Council.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_religious_institutes, payer,
    moderate, biographical, constrained, global).

% Officials in Roman dicasteries whose centralized doctrinal and disciplinary authority was deliberately diffused to episcopal conferences and local churches by conciliar and post-conciliar reforms. They administer a reduced portfolio relative to the pre-conciliar Curia and must implement policies shaped by the progressive reading.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, roman_curia_officials, payer,
    institutional, generational, constrained, global).

% Secular and Catholic historians who study the Council's textual genesis and post-conciliar implementation. They document the gap between conciliar texts and the 'spirit' invoked to justify later reforms, without being bound to either party's ecclesial interests.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, catholic_academic_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_progressive_reading, diffuse).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_progressive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies the post-conciliar Church around a forward-looking interpretive horizon, coordinating ecumenical engagement, liturgical adaptation, and collegial governance by grounding legitimacy in a dynamic reading of the Council's intent.
% TRANSFER_FUNCTION: Moves doctrinal and liturgical authority from centralized curial control and pre-conciliar templates to progressive episcopal conferences, theologians, and reform commissions; transfers compliance burdens onto traditionalist clergy, religious orders, and curial officials.
% ABSENT_VOICES: Pre-conciliar theologians and the minority of bishops who resisted key schema changes at the Council are structurally absent from the authoritative interpretation; their objections survive only in archival records and critical histories, not in the official conciliar hermeneutic.
% DISAPPEARANCE_RATIONALE: If the rupture-progressive framework vanished, progressive reforms implemented in its name would lose their primary legitimating narrative; traditionalist and continuity readings would compete to fill the vacuum, and the current balance of authority between episcopal conferences and the Roman Curia would shift dramatically.
% FOUNDING_PROBLEM: The pre-conciliar Church was perceived as rigid, centralist, and unable to engage the modern world or other Christians; the Council was convened to open the Church to contemporary realities.
% FOUNDING_PROBLEM_CORROBORATION: Progressive historians and conciliar periti attest the founding problem from within the benefiting parties. Traditionalist historians and some pre-conciliar curial officials attested from outside that the problem was exaggerated or misconstrued; their testimony is preserved in minority reports, post-conciliar critical scholarship, and the archives of the Coetus Internationalis Patrum.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_progressive_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_progressive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the 'spirit' functions as an open-ended authorization that bypasses textual constraints, allowing progressive agents to implement changes that traditionalist agents experience as unilateral imposition. Suppression is substantial (0.68) because the framework's persistence depends on episcopal and curial enforcement to marginalize traditionalist alternatives and maintain the post-conciliar institutional settlement. Theater ratio is moderate-to-high (0.55) because a growing share of post-conciliar activity defends the 'spirit' as a quasi-legal category increasingly independent of the conciliar texts themselves. Accessibility collapse is moderate (0.60) because pre-conciliar alternatives still exist in restricted pockets (SSPX, personal parishes) but are structurally marginalized. Resistance is high (0.70) due to sustained traditionalist opposition. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The progressive hierarchy experiences this constraint as genuine coordination of the Church with modernity and necessary decentralization; the traditionalist Catholic seat experiences the identical structure as extraction of worship forms and doctrinal certainty under the guise of reform. The Roman Curia seat experiences a loss of centralized control that the progressive seat celebrates as collegiality. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   The postconciliar reform hierarchy and conciliar theologians are structural beneficiaries: they collect expanded authority, advisory roles, and institutional prestige (low d, subsidized by the constraint). Traditionalist Catholics, pre-conciliar religious institutes, and Roman Curia officials are structural targets: they bear the costs of lost authority, imposed reforms, and excluded alternatives (high d, amplified extraction). Catholic academic historians sit at the analytical exit with neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â pre-conciliar rigidity and centralism â is contested in its severity. The progressive reading insists the problem remains partially live (ongoing reform is still needed), which prevents a clean mandatrophy resolution. Were the problem universally acknowledged as solved while the arrangement persisted, the constraint would drift toward piton or snare. Because the reading treats implementation as authentic realization but also authorizes further development beyond textual limits, the coordination story resists simple obsolescence labeling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spirit_vs_text_ambiguity,
    'Is the ''spirit of the Council'' a legitimate hermeneutical key for doctrinal development, or an extra-textual authorization that enables arbitrary innovation disconnected from the conciliar texts?',
    'Comparative analysis of post-conciliar magisterial documents against conciliar texts to measure the gap between explicit conciliar commitments and ''spirit''-based developments; assessment by theologians outside the progressive beneficiary set.',
    'If the gap is large and systematic, the progressive reading functions as extraction using coordination as cover (snare-tendency); if the gap is narrow and textually traceable, the reading is closer to genuine coordination (rope-tendency).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spirit_vs_text_ambiguity, conceptual, 'Ambiguity of spirit-based authorization versus textual limits').

omega_variable(
    founding_problem_exaggeration,
    'Was pre-conciliar rigidity the actual problem the Council was built to solve, or was the reform agenda driven by external political, cultural, and ecumenical pressure that recast internal Catholic tradition as pathology?',
    'Historical analysis of conciliar preparatory schemas, interventions by periti, and post-conciliar memoirs from bishops across the theological spectrum; demographic and vocational data from pre- and post-conciliar periods.',
    'If external pressure was the primary driver, the coordination story is a retrospective justification and the constraint''s extraction is primary; if genuine internal rigidity was the problem, the coordination function is independently grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_exaggeration, empirical, 'Contested origin of the conciliar reform agenda').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the marginalization of traditionalist Catholics structural (institutional penalties, restrictions on the Latin Mass, episcopal suppression) or internalized (self-censorship, identity fusion with pre-conciliar forms that makes institutional exit unthinkable)?',
    'Post-restriction suppression trajectory: measure whether traditionalist practice and advocacy persist or collapse after specific institutional restrictions are lifted or tightened (natural experiments from papal decrees on the Traditional Latin Mass).',
    'If suppression persists or intensifies after institutional barriers are removed, the effective suppression is higher than the structural measure suggests; if it relaxes, the constraint is primarily structurally enforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of traditionalist alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(v2rp_tr_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(v2rp_tr_t10, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(v2rp_tr_t20, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(v2rp_tr_t30, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(v2rp_tr_t40, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(v2rp_tr_t50, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 50, 0.52).
narrative_ontology:measurement(v2rp_tr_t60, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 60, 0.55).

% Extraction over time
narrative_ontology:measurement(v2rp_be_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(v2rp_be_t10, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(v2rp_be_t20, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(v2rp_be_t30, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(v2rp_be_t40, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(v2rp_be_t50, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(v2rp_be_t60, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 60, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(v2rp_su_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(v2rp_su_t10, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(v2rp_su_t20, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(v2rp_su_t30, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(v2rp_su_t40, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(v2rp_su_t50, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 50, 0.65).
narrative_ontology:measurement(v2rp_su_t60, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Vatican II doctrinal authority kernel. It is structurally linked to sibling readings that instantiate different constraints from the same conciliar kernel. The rupture-progressive reading influences the continuity reading by shifting legitimacy conditions, and coexists with the rupture-traditionalist reading as a live factional position within the same Church.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
