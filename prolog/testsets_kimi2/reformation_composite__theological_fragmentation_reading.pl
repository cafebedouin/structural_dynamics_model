% ============================================================================
% CONSTRAINT STORY: reformation_composite__theological_fragmentation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__theological_fragmentation_reading, []).

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
 *   constraint_id: reformation_composite__theological_fragmentation_reading
 *   human_readable: Confessional Boundary Maintenance in the Reformation (Theological Fragmentation Reading)
 *   domain: historical/epistemological/religious
 *
 * SUMMARY:
 *   The Reformation as theological event: competing soteriological and
 *   ecclesiological commitments regarding justification, sacraments, and
 *   papal authority generate structurally incompatible denominations. The
 *   constraint is the system of confessional boundary maintenance â the
 *   Augsburg Confession, Tridentine decrees, and analogous documents â that
 *   organizes European Christianity into mutually exclusive camps. From this
 *   reading, fragmentation is not primarily driven by state-building or print
 *   capitalism but by genuine doctrinal incommensurability. Denominational
 *   leadership on all sides benefits from the clarity and consolidated
 *   authority that bounded confessions provide, while radical dissenters and
 *   non-conforming laity bear the costs of enforcement.
 *
 * KEY AGENTS:
 *   - Protestant confessional leadership: Primary agenda-setter and beneficiary (institutional, identity-locked)
 *   - Catholic hierarchy: Primary agenda-setter and beneficiary (institutional, identity-locked)
 *   - Radical dissenters: Primary target (powerless, trapped)
 *   - Non-conforming laity: Secondary target (powerless, trapped)
 *   - Ecumenical theologians: Excluded voice (moderate, constrained)
 *   - Historical analyst: Observer (analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.62).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.75).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Confessional Boundary Maintenance in the Reformation (Theological Fragmentation Reading)").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "historical/epistemological/religious").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, 'fe43f0f7-f30c-4cae-961d-9f8bf4231488').
narrative_ontology:cs_kernel_codification('fe43f0f7-f30c-4cae-961d-9f8bf4231488', formalized).
narrative_ontology:cs_authority_grounding('fe43f0f7-f30c-4cae-961d-9f8bf4231488', lineage).
narrative_ontology:cs_interpretation_layer_present('fe43f0f7-f30c-4cae-961d-9f8bf4231488').
narrative_ontology:cs_reading_relation('fe43f0f7-f30c-4cae-961d-9f8bf4231488', reformation_composite__political_realignment_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe43f0f7-f30c-4cae-961d-9f8bf4231488', reformation_composite__technological_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('fe43f0f7-f30c-4cae-961d-9f8bf4231488', foundational, doctrinal_irreconcilability_generative).
narrative_ontology:cs_axiom_status(doctrinal_irreconcilability_generative, holdable).
narrative_ontology:cs_axiom_grounding('fe43f0f7-f30c-4cae-961d-9f8bf4231488', doctrinal_irreconcilability_generative, empirically_contingent).
narrative_ontology:cs_axiom('fe43f0f7-f30c-4cae-961d-9f8bf4231488', foundational, confessional_exclusivity_as_ecclesial_integrity).
narrative_ontology:cs_axiom_status(confessional_exclusivity_as_ecclesial_integrity, holdable).
narrative_ontology:cs_axiom_grounding('fe43f0f7-f30c-4cae-961d-9f8bf4231488', confessional_exclusivity_as_ecclesial_integrity, theological).
narrative_ontology:cs_reference_frame('fe43f0f7-f30c-4cae-961d-9f8bf4231488', medieval_sacral_unity).
narrative_ontology:cs_drift_state('fe43f0f7-f30c-4cae-961d-9f8bf4231488', post_westphalian_europe, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('fe43f0f7-f30c-4cae-961d-9f8bf4231488', '').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, protestant_confessional_leadership).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, catholic_hierarchy).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, radical_dissenters).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, non_conforming_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Composes and enforces confessional documents such as the Augsburg Confession and Heidelberg Catechism to define orthodoxy for territorial churches. Derives consolidated religious authority from bounded doctrinal communities that are distinct from Rome. Cannot abandon the confessional framework without dissolving their own legitimacy, since their authority is constituted by these specific doctrinal boundaries.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, protestant_confessional_leadership, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, protestant_confessional_leadership, beneficiary).

% Defends and enforces Tridentine definitions, the Index of Prohibited Books, and papal jurisdiction against Protestant claims. Benefits from sharpened boundary maintenance and consolidated authority in territories that remain in communion with Rome. Bound to the defense of universal jurisdiction by the theological claim of apostolic succession.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, catholic_hierarchy, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, catholic_hierarchy, beneficiary).

% Anabaptists, spiritualists, and anti-Trinitarians who are excluded by all magisterial confessions. Bear the costs of persecution, execution, exile, and property confiscation. No major confessional community affords them protection; all established churches enforce their silence or removal.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, radical_dissenters, payer,
    powerless, immediate, trapped, regional).

% Ordinary believers residing in territories where the prince's confession differs from their own. Bear costs of exclusion from public office, worship, guild membership, and legal standing. Cannot easily emigrate due to economic ties, language barriers, and travel costs; must either conform outwardly or accept civil disability.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, non_conforming_laity, payer,
    powerless, biographical, trapped, local).

% Scholars and clergy attempting to mediate between confessions and restore theological unity through colloquies and irenic treatises. Structurally excluded from formal confessional negotiations because the agenda-setters' authority depends on maintaining irreconcilability. Their proposals are heard but systematically overridden.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, ecumenical_theologians, excluded,
    moderate, biographical, constrained, continental).

% Evaluates the causal weight of theological commitment versus political and technological factors in generating denominational fragmentation. Neither collects authority from nor pays costs into the confessional constraint.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, historical_analyst, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__theological_fragmentation_reading, diffuse).
narrative_ontology:fixing_cost_class(reformation_composite__theological_fragmentation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear doctrinal boundaries that organize religious communities into coherent, mutually recognizable bodies capable of collective worship, discipline, mutual aid, and political negotiation without recurring to a single universal ecclesiastical authority.
% TRANSFER_FUNCTION: Moves the authority to define orthodoxy from a universal center to territorial and denominational leadership; moves the costs of exclusion, persecution, and civil disability onto dissenters and non-conforming laity.
% ABSENT_VOICES: Ecumenical theologians seeking reunion, radical reformers claiming direct spiritual authority outside all confessions, and laity whose devotional practices naturally cross confessional lines are structurally excluded from the negotiation of confessional documents.
% DISAPPEARANCE_RATIONALE: If the confessional boundary system vanished overnight, the organizational structure of European Christianity would collapse back toward either universal jurisdiction or ungoverned pluralism. Denominational leadership would lose its bounded authority, territorial churches would lose their theological justification for religious monopoly, and the enforcement mechanisms of the Peace of Augsburg and Westphalia would have no doctrinal content to enforce.
% FOUNDING_PROBLEM: The late medieval Latin church faced a crisis of doctrinal authority, pastoral failure, and perceived corruption that undermined confidence in its soteriological and ecclesiological claims, generating pressure for reform that could not be accommodated within existing institutional structures.
% FOUNDING_PROBLEM_CORROBORATION: Protestant confessional leadership attests the problem was the Roman church's doctrinal error. Catholic counter-reform attests the problem was disobedience and heresy. Secular historians, political theorists, and economic historians outside both beneficiary sets attest that the problem was genuinely contested and that the confessional solution persisted and hardened beyond the resolution of the original crisis.
narrative_ontology:disappearance_verdict(reformation_composite__theological_fragmentation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__theological_fragmentation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__theological_fragmentation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_composite__theological_fragmentation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__theological_fragmentation_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__theological_fragmentation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__theological_fragmentation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial authority, material support, and social control that confessional leadership consolidated by bounding religious communities, set against the genuine coordination function of doctrinal clarity and communal organization. Suppression (0.75) is high because the maintenance of mutually exclusive confessions required active state-backed enforcement through banishment, execution, censorship, and cuius regio, eius religio. Theater ratio (0.40) captures the performative orthodoxy that developed as confessional identities became habitual and entrenched. Accessibility collapse (0.60) registers the closure of ecumenical and radical alternatives once the confessional map stabilized. Resistance (0.55) reflects persistent peasant dissent, irenicist movements, and crypto-religious practice that never fully accepted confessional boundaries. The measurement series share a single time grid so that every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the confessional leadership seat, the constraint appears as necessary doctrinal clarity â without firm boundaries, the community dissolves into error and chaos. From the radical dissenter and non-conforming laity seats, the same structure is experienced as violent exclusion dressed in theological language. The engine computes this divergence from the structural data: identical scope and power atoms, opposite beneficiary-victim roles, producing divergent directionality and per-seat classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Protestant and Catholic leadership are structural beneficiaries (low d) because the fragmentation grants each bounded authority over a defined flock and vindicates their respective doctrinal claims. Radical dissenters and non-conforming laity are structural targets (high d) because they bear the enforcement costs of confessional exclusivity. Ecumenical theologians are excluded rather than coordinated â their proposals for reunion threaten the boundary maintenance that sustains leadership authority, so their exclusion is the enforcement object itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â a crisis of doctrinal authority and pastoral failure in late medieval Latin Christendom â was contested and partially resolved by the confessional system. By the mid-seventeenth century, the original theological crisis had been superseded by permanent institutional fragmentation integrated into state sovereignty. The mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) signals potential mandatrophy: the constraint outlived the specific problem it was built to solve but persists because it coordinates political and social order. The tangled rope classification captures this: genuine coordination (religious community organization) combined with asymmetric extraction (leadership consolidation at dissenters' expense).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_primacy_vs_siblings,
    'Does the theological_fragmentation_reading capture the primary causal structure of the Reformation, or do the political_realignment_reading and technological_mediation_reading provide independently necessary complementary frames?',
    'Comparative historiographical meta-analysis weighing doctrinal, political, and technological evidence across the 1517-1648 interval, including archival study of princely correspondence and print runs alongside theological texts.',
    'If political or technological factors are independently sufficient, this constraint''s beneficiary set expands to include territorial princes or publishing networks, and its type may shift toward snare (political extraction) or rope (media coordination), dissolving the theological reading''s exclusivity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_primacy_vs_siblings, conceptual, 'Whether theological commitment is the primary or merely contributory cause of Reformation fragmentation.').

omega_variable(
    state_enforcement_theological_necessity,
    'Would the confessional fragmentation have persisted without the active enforcement of territorial princes, or was state violence merely instrumentalizing genuinely irreconcilable doctrine?',
    'Counterfactual analysis of Reformation trajectories in weak-state contexts versus strong-state contexts; examination of theological debate in places where princes remained neutral.',
    'If fragmentation is state-dependent, the constraint''s active enforcement is externally supplied rather than endogenous to theology, raising suppression and shifting directionality toward princely beneficiaries rather than confessional leadership alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_theological_necessity, empirical, 'Whether confessional boundaries are endogenous to doctrine or dependent on state enforcement.').

omega_variable(
    radical_dissenter_exclusion_inevitability,
    'Was the exclusion of radical dissenters (Anabaptists, spiritualists) a necessary consequence of magisterial soteriology, or a strategic choice by leadership to consolidate authority?',
    'Analysis of magisterial reformers'' theological writings on free will, ecclesiology, and tolerance; comparison with Radical Reformation theological alternatives that were structurally excluded.',
    'If exclusion was strategic rather than doctrinally necessary, the constraint''s extraction is higher than the coordination story admits, pushing classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(radical_dissenter_exclusion_inevitability, conceptual, 'Whether radical exclusion is doctrinally inherent or strategically extractive.').

omega_variable(
    mandatrophy_confessional_persistence,
    'Did the confessional system persist beyond the resolution of its founding theological crisis, and if so, by what mechanism?',
    'Examination of post-Westphalian religious politics to determine whether confessional boundaries maintained their theological justification or became inertia-driven social sorting.',
    'If persistence is inertial, the constraint degrades toward piton; if it continues to coordinate genuine religious community, it remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_confessional_persistence, empirical, 'Whether confessional fragmentation has outlived its founding problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reformation_theological_tr_t0, reformation_composite__theological_fragmentation_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(reformation_theological_tr_t15, reformation_composite__theological_fragmentation_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(reformation_theological_tr_t30, reformation_composite__theological_fragmentation_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(reformation_theological_tr_t50, reformation_composite__theological_fragmentation_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(reformation_theological_tr_t75, reformation_composite__theological_fragmentation_reading, theater_ratio, 75, 0.42).
narrative_ontology:measurement(reformation_theological_tr_t100, reformation_composite__theological_fragmentation_reading, theater_ratio, 100, 0.48).
narrative_ontology:measurement(reformation_theological_tr_t130, reformation_composite__theological_fragmentation_reading, theater_ratio, 130, 0.4).

% Extraction over time
narrative_ontology:measurement(reformation_theological_be_t0, reformation_composite__theological_fragmentation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(reformation_theological_be_t15, reformation_composite__theological_fragmentation_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(reformation_theological_be_t30, reformation_composite__theological_fragmentation_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(reformation_theological_be_t50, reformation_composite__theological_fragmentation_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(reformation_theological_be_t75, reformation_composite__theological_fragmentation_reading, base_extractiveness, 75, 0.72).
narrative_ontology:measurement(reformation_theological_be_t100, reformation_composite__theological_fragmentation_reading, base_extractiveness, 100, 0.68).
narrative_ontology:measurement(reformation_theological_be_t130, reformation_composite__theological_fragmentation_reading, base_extractiveness, 130, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(reformation_theological_su_t0, reformation_composite__theological_fragmentation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(reformation_theological_su_t15, reformation_composite__theological_fragmentation_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(reformation_theological_su_t30, reformation_composite__theological_fragmentation_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(reformation_theological_su_t50, reformation_composite__theological_fragmentation_reading, suppression_requirement, 50, 0.78).
narrative_ontology:measurement(reformation_theological_su_t75, reformation_composite__theological_fragmentation_reading, suppression_requirement, 75, 0.82).
narrative_ontology:measurement(reformation_theological_su_t100, reformation_composite__theological_fragmentation_reading, suppression_requirement, 100, 0.75).
narrative_ontology:measurement(reformation_theological_su_t130, reformation_composite__theological_fragmentation_reading, suppression_requirement, 130, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__theological_fragmentation_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one member of the reformation_composite kernel family, decomposed per the epsilon-invariance principle from the colloquial label 'the Reformation' into three structurally distinct readings: theological_fragmentation_reading, political_realignment_reading, and technological_mediation_reading. Each reading carries a different epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
