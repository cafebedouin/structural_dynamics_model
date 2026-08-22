% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__composite_overdetermination_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: vatican_ii_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Authority as Irreducibly Overdetermined Composite
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   Vatican II is institutionalized as a binding ecumenical council whose
 *   texts coordinate global Catholic liturgical, ecclesiological, and
 *   ecumenical practice. The composite overdetermination reading treats this
 *   authority arrangement as structurally ambiguous: the conciliar documents
 *   encode incompatible theological rationales produced by factional
 *   compromise, making univocal interpretation impossible. The constraint is
 *   the standing arrangement whereby the Church enforces Vatican II as
 *   authoritative while its meaning remains irreducibly contested. This
 *   arrangement generates an interpretive industry that benefits the
 *   theological academy while extracting doctrinal clarity from the
 *   magisterium and imposing an ambiguous council on traditionalist
 *   communities. The claim is tangled_rope because the council produced
 *   genuine coordination, yet the structural ambiguity functions as
 *   asymmetric extraction.
 *
 * KEY AGENTS:
 *   - academic_theologians: Primary beneficiary (moderate/constrained) â collect interpretive authority and career sustenance from the ambiguity
 *   - magisterial_authority: Agenda setter and payer (institutional/identity_locked) â enforces the council while bearing the cost of its undermining univocal teaching
 *   - traditionalist_communities: Primary payer target (organized/trapped) â bear the cost of an ambiguous council they reject
 *   - secular_historians: Analytical observer (analytical/analytical) â see the overdetermination from outside the authority structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, 0.62).
domain_priors:suppression_score(vatican_ii_authority__composite_overdetermination_reading, 0.55).
domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__composite_overdetermination_reading, "Vatican II Authority as Irreducibly Overdetermined Composite").
narrative_ontology:topic_domain(vatican_ii_authority__composite_overdetermination_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__composite_overdetermination_reading, 'f3b4a18d-4adc-4b89-9e29-ff4cd3bab67b').
narrative_ontology:cs_kernel_codification('f3b4a18d-4adc-4b89-9e29-ff4cd3bab67b', formalized).
narrative_ontology:cs_authority_grounding('f3b4a18d-4adc-4b89-9e29-ff4cd3bab67b', lineage).
narrative_ontology:cs_interpretation_layer_present('f3b4a18d-4adc-4b89-9e29-ff4cd3bab67b').
narrative_ontology:cs_reading_relation('f3b4a18d-4adc-4b89-9e29-ff4cd3bab67b', vatican_ii_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('f3b4a18d-4adc-4b89-9e29-ff4cd3bab67b', vatican_ii_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('f3b4a18d-4adc-4b89-9e29-ff4cd3bab67b', foundational, irreducible_overdetermination).
narrative_ontology:cs_axiom_status(irreducible_overdetermination, holdable).
narrative_ontology:cs_axiom_grounding('f3b4a18d-4adc-4b89-9e29-ff4cd3bab67b', irreducible_overdetermination, empirically_contingent).
narrative_ontology:cs_axiom('f3b4a18d-4adc-4b89-9e29-ff4cd3bab67b', secondary, hermeneutic_priority_of_historical_criticism).
narrative_ontology:cs_axiom_status(hermeneutic_priority_of_historical_criticism, holdable).
narrative_ontology:cs_axiom_grounding('f3b4a18d-4adc-4b89-9e29-ff4cd3bab67b', hermeneutic_priority_of_historical_criticism, conventional).
narrative_ontology:cs_reference_frame('f3b4a18d-4adc-4b89-9e29-ff4cd3bab67b', conciliar_texts_as_factional_compromise_artifacts).
narrative_ontology:cs_drift_state('f3b4a18d-4adc-4b89-9e29-ff4cd3bab67b', post_conciliar_synthetic_hermeneutic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f3b4a18d-4adc-4b89-9e29-ff4cd3bab67b', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, academic_theologians).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, magisterial_authority).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, traditionalist_communities).
narrative_ontology:constraint_vindicates(vatican_ii_authority__composite_overdetermination_reading, historical_critical_hermeneutic).
narrative_ontology:constraint_vindicates(vatican_ii_authority__composite_overdetermination_reading, conciliar_compromise_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces Vatican II as a binding ecumenical council; claims capacity for univocal authoritative interpretation; bears the structural cost of the council's textual overdetermination because every synthetic reading triggers factional backlash and erodes teaching credibility.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, magisterial_authority, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__composite_overdetermination_reading, magisterial_authority, payer).

% Build careers and institutional positions interpreting the structural ambiguities of Vatican II; publish hermeneutic mediations between incompatible conciliar rationales; benefit from the inexhaustible demand for interpretive labor that the overdetermined texts generate.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, academic_theologians, beneficiary,
    moderate, biographical, constrained, global).

% Reject Vatican II as doctrinally compromised or ambiguous; forced to accept the council's liturgical and disciplinary consequences to remain in full communion; marginalized when they publicly repudiate the council; bear the cost of a council they experience as error enforced by authority.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, traditionalist_communities, payer,
    organized, generational, trapped, global).

% Analyze the conciliar documents and negotiations as historical phenomena; observe the factional compromises and rhetorical ambiguities without commitment to the council's religious authority; provide external corroboration of the overdetermination thesis.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, secular_historians, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__composite_overdetermination_reading, academic_theologians).
narrative_ontology:fixing_cost_class(vatican_ii_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the global Catholic Church around a shared set of liturgical, ecclesiological, and ecumenical reforms adopted at the council, providing a common institutional reference point for a worldwide communion.
% TRANSFER_FUNCTION: Moves interpretive authority and academic opportunity from the magisterium's claim to univocal teaching to the theological academy; moves doctrinal constraint from the conciliar texts onto traditionalist communities forced to accept an ambiguous council.
% ABSENT_VOICES: Secular historians of religion and non-Catholic ecumenical observers who read the council as a political compromise rather than a supernatural event are excluded from authoritative interpretation; sedevacantist and radical traditionalist critics are expelled from institutional discourse.
% DISAPPEARANCE_RATIONALE: If the authority of Vatican II as a binding composite vanished overnight, the post-conciliar Church would lose its primary legitimating reference for liturgical reform and ecumenical outreach; the theological academy built on interpreting it would collapse; traditionalists would lose their adversarial target; the magisterium would face a doctrinal vacuum and potential schismatic realignments.
% FOUNDING_PROBLEM: The mid-twentieth century Catholic Church confronted a crisis of pastoral engagement with modernity, ecumenical estrangement from Protestant and Orthodox communities, and liturgical stagnation; the council was convened to renew ecclesial life and open the Church to the contemporary world.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians and non-Catholic ecumenical observers corroborate the modernity-engagement crisis; however, these same external sources often contest whether the council's compromises solved the problem or merely institutionalized theological ambiguity. No corroboration exists from outside the beneficiary set for the claim that the overdetermined structure was the necessary solution.
narrative_ontology:disappearance_verdict(vatican_ii_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__composite_overdetermination_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the magisterium's capacity for univocal teaching is steadily extracted by the gap between claimed clarity and textual overdetermination, while scholars capture the interpretive authority that leaks from that gap. Suppression (0.55) reflects active enforcement against outright rejection (traditionalist marginalization, SSPX schism, curial discipline of theologians) rather than totalitarian control. Theater ratio approaches 0.5 because an increasing share of magisterial and scholarly activity is performance: asserting continuity while contradictions fester, or asserting complexity without resolution. Resistance (0.5) captures persistent traditionalist rejection and internal conservative pushback against progressive exploitation of ambiguity. Accessibility collapse (0.6) reflects that exit options narrow once the authority structure is accepted â rejection means schism or marginalization, while remaining inside means accepting the ambiguous text.
 *
 * PERSPECTIVAL GAP:
 *   The magisterial seat experiences the constraint as a burden on its teaching authority: every attempt at definitive interpretation is destabilized by the texts' internal contradictions. The academic seat experiences the same structure as a resource: the ambiguity sustains careers, conferences, and methodologies. The traditionalist seat experiences it as enforced error. The engine computes these divergences from the structural data â the same constraint reads as subsidy to scholars and extraction from institutional clarity and traditionalist integrity.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic theologians are declared beneficiaries with constrained exit (low d, near beneficiary pole), so their effective extraction is damped or inverted into subsidy. Magisterial authority is declared victim with identity_locked exit (high d, near target pole) despite its agenda-setting role; the victim declaration overrides the administrator position for directionality because the constraint extracts from its capacity for univocal teaching. Traditionalist communities are declared victims with trapped exit (very high d), amplifying their effective extraction. The structural derivation handles these without override.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists snare classification because Vatican II produced genuine coordination: ecumenical opening, liturgical reform, and doctrinal development that solved real problems. It resists rope classification because the ambiguity is not a minor friction but a structural feature that sustains an interpretive industry and undermines magisterial clarity. It resists piton classification because the interpretive function has not atrophied; the ambiguity is actively productive for scholars and actively costly for the magisterium. Tangled rope is the correct classification: genuine coordination plus asymmetric extraction maintained by active enforcement of the council's authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreducible_overdetermination_or_synthetic_horizon,
    'Is Vatican II''s textual ambiguity structurally irreducible, or does a higher-order synthetic hermeneutic remain possible?',
    'Comparative analysis of conciliar drafting histories and theological synthesis attempts: if every synthetic reading fails on close textual inspection, the overdetermination thesis is strengthened.',
    'If synthetic resolution is possible, the constraint''s extractiveness is lower than measured and the magisterium''s univocal claims are recoverable; if irreducible, the composite reading is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreducible_overdetermination_or_synthetic_horizon, empirical, 'Whether the council''s contradictions can be synthesized or are permanent').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of traditionalist rejection structural (canonical penalties, excommunication) or internalized (ecclesial identity fusion making exit psychologically impossible)?',
    'Post-exit trajectory analysis: if traditionalists who leave for SSPX or independent chapels cease feeling suppressed, the mechanism was primarily internalized; if structural sanctions follow them, it is structural.',
    'If internalized, effective suppression is higher than structural measures suggest; if structural alone, the constraint''s extraction depends on visible enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of traditionalist rejection').

omega_variable(
    committer_frame_sibling_displacement,
    'This constraint is the composite_overdetermination reading of kernel vatican_ii_authority. How would classification change if the continuity or rupture reading were adopted instead?',
    'Generate sibling constraint stories for continuity_reading and rupture_reading and compare their epsilon values, beneficiary/victim structures, and directionality profiles.',
    'Adopting continuity_reading would likely lower extractiveness and reclassify as rope or scaffold; adopting rupture_reading would likely raise suppression and reclassify as snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_sibling_displacement, conceptual, 'Committer frame location and sibling reading displacement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(v2comp_tr_t0, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(v2comp_tr_t12, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(v2comp_tr_t24, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(v2comp_tr_t36, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 36, 0.42).
narrative_ontology:measurement(v2comp_tr_t48, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 48, 0.45).
narrative_ontology:measurement(v2comp_tr_t60, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(v2comp_be_t0, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(v2comp_be_t12, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(v2comp_be_t24, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(v2comp_be_t36, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 36, 0.55).
narrative_ontology:measurement(v2comp_be_t48, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 48, 0.58).
narrative_ontology:measurement(v2comp_be_t60, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(v2comp_su_t0, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(v2comp_su_t12, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(v2comp_su_t24, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(v2comp_su_t36, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 36, 0.65).
narrative_ontology:measurement(v2comp_su_t48, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 48, 0.6).
narrative_ontology:measurement(v2comp_su_t60, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__composite_overdetermination_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
