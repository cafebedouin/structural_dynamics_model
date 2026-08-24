% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__subordinationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__subordinationist_reading, []).

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
 *   constraint_id: homoousios_nicene__subordinationist_reading
 *   human_readable: Homoousios as Compatible with Subordination (Subordinationist Reading)
 *   domain: historical_theology/ecclesiastical_history/philosophy_of_religion
 *
 * SUMMARY:
 *   The subordinationist reading of homoousios emerges after Nicaea (325) as
 *   an imperial project to reconcile the Nicene term with a hierarchical
 *   ontology. Under Constantius II (337-361) and Valens (364-378), this
 *   reading is enforced through councils (Ariminum, Seleucia, Constantinople
 *   360) that depose Nicene bishops and impose creeds affirming the Son's
 *   subordination. The constraint coordinates a broad coalition of 'Arian'
 *   and 'Semi-Arian' groups while extracting compliance from the Nicene
 *   orthodox. The claimed type is tangled_rope: genuine coordination among
 *   subordinationists (shared terminology, imperial backing) fused with
 *   asymmetric extraction from the Nicene party (exile, property seizure).
 *   The metrics reflect rising extraction and suppression as imperial
 *   enforcement intensifies, then a slight decline after Valens' death.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, 0.68).
domain_priors:suppression_score(homoousios_nicene__subordinationist_reading, 0.75).
domain_priors:theater_ratio(homoousios_nicene__subordinationist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__subordinationist_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__subordinationist_reading, "Homoousios as Compatible with Subordination (Subordinationist Reading)").
narrative_ontology:topic_domain(homoousios_nicene__subordinationist_reading, "historical_theology/ecclesiastical_history/philosophy_of_religion").

domain_priors:requires_active_enforcement(homoousios_nicene__subordinationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__subordinationist_reading, '1fcda286-fa80-489d-b90d-bcff70b90eeb').
narrative_ontology:cs_kernel_codification('1fcda286-fa80-489d-b90d-bcff70b90eeb', fixed_text).
narrative_ontology:cs_authority_grounding('1fcda286-fa80-489d-b90d-bcff70b90eeb', lineage).
narrative_ontology:cs_interpretation_layer_present('1fcda286-fa80-489d-b90d-bcff70b90eeb').
narrative_ontology:cs_reading_relation('1fcda286-fa80-489d-b90d-bcff70b90eeb', homoousios_nicene__metaphysical_equality_reading, forecloses).
narrative_ontology:cs_reading_relation('1fcda286-fa80-489d-b90d-bcff70b90eeb', homoousios_nicene__honorific_similarity_reading, forecloses).
narrative_ontology:cs_axiom('1fcda286-fa80-489d-b90d-bcff70b90eeb', foundational, son_derives_being_from_father).
narrative_ontology:cs_axiom_status(son_derives_being_from_father, holdable).
narrative_ontology:cs_axiom_grounding('1fcda286-fa80-489d-b90d-bcff70b90eeb', son_derives_being_from_father, theological).
narrative_ontology:cs_axiom('1fcda286-fa80-489d-b90d-bcff70b90eeb', secondary, scripture_over_conciliar_tradition).
narrative_ontology:cs_axiom_status(scripture_over_conciliar_tradition, holdable).
narrative_ontology:cs_axiom_grounding('1fcda286-fa80-489d-b90d-bcff70b90eeb', scripture_over_conciliar_tradition, theological).
narrative_ontology:cs_reference_frame('1fcda286-fa80-489d-b90d-bcff70b90eeb', scriptural_subordinationism).
narrative_ontology:cs_drift_state('1fcda286-fa80-489d-b90d-bcff70b90eeb', post_nicene_controversy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1fcda286-fa80-489d-b90d-bcff70b90eeb', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__subordinationist_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, subordinationist_communities).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, arian_bishops).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, semi_arian_theologians).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, nicene_orthodox_bishops).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, metaphysical_equality_proponents).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, western_nicene_theologians).
narrative_ontology:constraint_vindicates(homoousios_nicene__subordinationist_reading, scriptural_authority_over_conciliar_tradition).
narrative_ontology:constraint_vindicates(homoousios_nicene__subordinationist_reading, monotheistic_hierarchy_preserved).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities (Arian, Semi-Arian) that gain theological legitimacy and imperial patronage when the subordinationist reading is enforced. They coordinate around a shared doctrinal formula that preserves hierarchy while using the Nicene term. Exit means losing imperial favor and facing orthodox persecution.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, subordinationist_communities, beneficiary,
    organized, generational, constrained, continental).

% Bishops who control sees and conciliar votes under pro-Arian emperors. They set the agenda for councils, define orthodoxy, and benefit from the extraction of ecclesiastical resources from deposed Nicene bishops. Their identity is fused with the subordinationist position; recantation means deposition and exile.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, arian_bishops, beneficiary,
    institutional, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, arian_bishops, agenda_setter).

% Theologians (e.g., Basil of Ancyra, George of Laodicea) who articulate the homoiousios/subordinationist synthesis. They gain intellectual authority and imperial stipends. Exit is constrained by their published works and institutional positions.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, semi_arian_theologians, beneficiary,
    moderate, biographical, constrained, regional).

% Bishops (e.g., Athanasius, Hilary, Ossius) who hold the metaphysical equality reading. They bear the costs of exile, confiscation, and exclusion from imperial communion. Their identity is fused with the Nicene homoousios; compromise is doctrinally impossible.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, nicene_orthodox_bishops, payer,
    organized, generational, identity_locked, continental).

% Clergy and laity who adhere to the equality reading in subordinationist-controlled regions. They face deposition, exile, and denial of sacraments. Exit is geographically and socially trapped.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, metaphysical_equality_proponents, payer,
    moderate, biographical, trapped, regional).

% Western bishops and theologians (e.g., Damasus, Ambrose) who resist subordinationist imperial policy. They bear the cost of maintaining a separate communion and lobbying for Nicene restoration. Exit is constrained by their institutional responsibility.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, western_nicene_theologians, payer,
    powerful, generational, constrained, continental).

% Emperors (Constantius II, Valens) and their advisors who enforce the subordinationist reading through councils, exiles, and legislation. They extract ecclesiastical unity for political stability. They can switch readings (as Valens did) with low personal cost.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, imperial_court, agenda_setter,
    institutional, biographical, arbitrage, continental).

% Ordinary believers who experience shifting liturgical languages, episcopal turnover, and sacramental uncertainty. They have no voice in conciliar definitions but bear the social cost of theological conflict.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, lay_christians, excluded,
    powerless, immediate, trapped, local).

% Modern scholars who analyze the constraint's structural operation across the 4th century. They neither collect nor pay but map the extraction-coordination dynamics.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, historical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a doctrinal formula that allows diverse subordinationist groups (Arian, Semi-Arian) to unite under the Nicene term 'homoousios' while preserving a hierarchical ontology (Father > Son). Solves the coordination problem of imperial ecclesiastical unity without conceding metaphysical equality.
% TRANSFER_FUNCTION: Moves episcopal authority, church property, and imperial patronage from Nicene orthodox bishops to subordinationist bishops. Moves theological legitimacy from the conciliar definition of Nicaea (325) to a scripturally grounded subordinationist interpretation. Moves the cost of theological dissent onto the Nicene party (exile, deposition).
% ABSENT_VOICES: The Homoiousian (honorific similarity) party is structurally excluded — they are too close to the subordinationists to be allies but too distinct to be included. The pneumatological dimension (Holy Spirit's status) is absent; the reading focuses exclusively on Father-Son relations. Laity are excluded from conciliar decision-making.
% DISAPPEARANCE_RATIONALE: If the subordinationist reading vanished overnight (e.g., imperial enforcement ceased), the Nicene orthodox would reclaim sees, the Homoiousians would negotiate a new synthesis, and the Arian churches would fragment. The theological landscape would reorganize around the metaphysical equality reading (as historically occurred at Constantinople 381).
% FOUNDING_PROBLEM: How to affirm the Son's full divinity (against pagan polytheism and Jewish monotheism) while preserving the Father's monarchy and the Son's subordination (against modalism/Sabellianism). The subordinationist reading was built to solve the 'monarchy of the Father' problem using the term homoousios.
% FOUNDING_PROBLEM_CORROBORATION: Subordinationist sources (Arius' Thalia, Asterius' Syntagmation, the Dedication Creed of 341) attest the founding problem as live. Nicene sources (Athanasius' Orations, the Tomus ad Antiochenos) attest it as a manufactured problem to evade Nicaea. No neutral third-party corroboration exists; the historical record is entirely partisan.
narrative_ontology:disappearance_verdict(homoousios_nicene__subordinationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__subordinationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__subordinationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_nicene__subordinationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__subordinationist_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__subordinationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__subordinationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint transfers ecclesiastical resources and authority from Nicene to subordinationist bishops. Suppression (0.75) is higher because persistence depends on active imperial enforcement (exile, conciliar manipulation). Theater ratio (0.42) is moderate: the conciliar process and scriptural argumentation are real coordination activities, but a growing share is performative enforcement of imperial will. Accessibility collapse (0.62) reflects the difficulty of articulating a third option once the homoousios/homoiousios/heterousios trichotomy solidifies. Resistance (0.58) is substantial: the Nicene party maintains a coherent alternative and eventually prevails.
 *
 * PERSPECTIVAL GAP:
 *   From the imperial seat, the constraint is a rope: it coordinates a fractured church. From the Nicene bishop seat, it is a snare: extraction enforced by exile. From the Semi-Arian seat, it is a tangled rope: they coordinate with Arians but resist full subordinationism. The engine computes this divergence from the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Imperial court (agenda_setter) sits near beneficiary end (d ~0.15): they extract unity and control. Subordinationist bishops (beneficiary/agenda_setter) sit near beneficiary end (d ~0.2): they gain sees and stipends. Nicene bishops (payer) sit near target end (d ~0.85): they bear exile and deposition. Lay Christians (excluded) are trapped (d ~0.9). The engine will compute per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving the Father's monarchy) was live in the 320s but became contested after Nicaea. By the 360s, the subordinationist reading persists not because the problem is live but because imperial power enforces it — a classic mandatrophy pattern where the mandate (theological coherence) has atrophied but the constraint (imperial enforcement) remains. The mandate is resolved (dead) for the Nicene party but contested for subordinationists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordinationist_coordination_vs_extraction,
    'Is the subordinationist reading a genuine coordination mechanism for theological unity, or is the coordination story cover for imperial extraction of ecclesiastical control?',
    'Compare the doctrinal content of subordinationist creeds (341, 351, 357, 359, 360) with imperial legislation: if creeds innovate theologically beyond what imperial unity requires, coordination is genuine; if they merely ratify imperial appointments, extraction dominates.',
    'If coordination is genuine, the constraint is tangled_rope; if extraction dominates with coordination as cover, it is snare. The claimed type (tangled_rope) assumes genuine coordination among subordinationists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordinationist_coordination_vs_extraction, conceptual, 'Whether the subordinationist reading''s coordination function is structural or theatrical.').

omega_variable(
    scriptural_authority_distribution,
    'Does the subordinationist appeal to scriptural authority genuinely distribute power away from conciliar tradition, or does it merely replace one authority (Nicaea) with another (imperial exegesis)?',
    'Analyze the citation patterns in subordinationist literature vs. imperial legislative prefaces: if scriptural arguments are diverse and debated, authority is distributed; if they are uniformly dictated by imperial theologians, authority is recentralized.',
    'If authority is recentralized, the beneficiary set narrows to imperial court alone; if distributed, semi-Arian theologians are genuine beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scriptural_authority_distribution, empirical, 'Whether scriptural authority functions as a decentralizing or recentralizing force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__subordinationist_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_tr_t0, homoousios_nicene__subordinationist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_tr_t14, homoousios_nicene__subordinationist_reading, theater_ratio, 14, 0.35).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_tr_t28, homoousios_nicene__subordinationist_reading, theater_ratio, 28, 0.42).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_tr_t42, homoousios_nicene__subordinationist_reading, theater_ratio, 42, 0.48).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_tr_t56, homoousios_nicene__subordinationist_reading, theater_ratio, 56, 0.42).

% Extraction over time
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_be_t0, homoousios_nicene__subordinationist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_be_t14, homoousios_nicene__subordinationist_reading, base_extractiveness, 14, 0.55).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_be_t28, homoousios_nicene__subordinationist_reading, base_extractiveness, 28, 0.68).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_be_t42, homoousios_nicene__subordinationist_reading, base_extractiveness, 42, 0.72).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_be_t56, homoousios_nicene__subordinationist_reading, base_extractiveness, 56, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_su_t0, homoousios_nicene__subordinationist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_su_t14, homoousios_nicene__subordinationist_reading, suppression_requirement, 14, 0.68).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_su_t28, homoousios_nicene__subordinationist_reading, suppression_requirement, 28, 0.75).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_su_t42, homoousios_nicene__subordinationist_reading, suppression_requirement, 42, 0.8).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_su_t56, homoousios_nicene__subordinationist_reading, suppression_requirement, 56, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__subordinationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_nicene__subordinationist_reading, 0.08).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__honorific_similarity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the homoousios_nicene kernel. The subordinationist reading forecloses the other two within any single theological framework. The three constraints form a constraint family linked by mutual foreclosure and historical succession.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_nicene__subordinationist_reading, institutional, 0.15).
constraint_indexing:directionality_override(homoousios_nicene__subordinationist_reading, organized, 0.2).
constraint_indexing:directionality_override(homoousios_nicene__subordinationist_reading, moderate, 0.4).
constraint_indexing:directionality_override(homoousios_nicene__subordinationist_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
