% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__political_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__political_swap_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: reformation_event_boundary__political_swap_reading
 *   human_readable: Reformation as Political Realignment (Swap Reading)
 *   domain: historical/political/religious
 *
 * SUMMARY:
 *   This constraint story instantiates the political_swap reading of the
 *   reformation_event_boundary kernel. It treats the Reformation not as a
 *   theological event with political consequences, but as a structural swap
 *   of authority and assets from the Catholic Church and papacy to secular
 *   territorial princes. Theology functions as a scaffold: Protestant
 *   doctrine provided the post-hoc rationalization and ceremonial machinery
 *   through which princes legitimized the break from Rome, seized church
 *   lands, and established state churches. The coordination problem solved is
 *   the collective-action challenge of extricating multiple territories from
 *   a universal jurisdiction without perpetual interstate war; the extraction
 *   is the massive transfer of wealth and obedience from ecclesiastical to
 *   secular coffers.
 *
 * KEY AGENTS:
 *   - territorial_princes: Primary beneficiary (powerful/mobile) â collect church assets and jurisdictional autonomy
 *   - catholic_church: Primary target/payer (institutional/constrained) â loses wealth and authority
 *   - papacy: Primary target/payer (institutional/constrained) â loses universal jurisdiction
 *   - protestant_theologians: Agenda-setter (moderate/constrained) â administer the theological scaffold
 *   - rural_peasantry: Secondary payer (powerless/trapped) â bear war costs and confessional coercion
 *   - excluded_catholic_communities: Excluded voice (powerless/trapped) â silenced in settlement negotiations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, 0.82).
domain_priors:suppression_score(reformation_event_boundary__political_swap_reading, 0.78).
domain_priors:theater_ratio(reformation_event_boundary__political_swap_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__political_swap_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "Reformation as Political Realignment (Swap Reading)").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical/political/religious").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, 'aad93500-b32b-48d2-b2f0-c808feabf82e').
narrative_ontology:cs_kernel_codification('aad93500-b32b-48d2-b2f0-c808feabf82e', fixed_text).
narrative_ontology:cs_authority_grounding('aad93500-b32b-48d2-b2f0-c808feabf82e', lineage).
narrative_ontology:cs_interpretation_layer_present('aad93500-b32b-48d2-b2f0-c808feabf82e').
narrative_ontology:cs_reading_relation('aad93500-b32b-48d2-b2f0-c808feabf82e', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('aad93500-b32b-48d2-b2f0-c808feabf82e', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('aad93500-b32b-48d2-b2f0-c808feabf82e', foundational, political_authority_primacy).
narrative_ontology:cs_axiom_status(political_authority_primacy, holdable).
narrative_ontology:cs_axiom_grounding('aad93500-b32b-48d2-b2f0-c808feabf82e', political_authority_primacy, empirically_contingent).
narrative_ontology:cs_axiom('aad93500-b32b-48d2-b2f0-c808feabf82e', foundational, theology_as_superstructure).
narrative_ontology:cs_axiom_status(theology_as_superstructure, holdable).
narrative_ontology:cs_axiom_grounding('aad93500-b32b-48d2-b2f0-c808feabf82e', theology_as_superstructure, empirically_contingent).
narrative_ontology:cs_reference_frame('aad93500-b32b-48d2-b2f0-c808feabf82e', princely_territorial_supremacy).
narrative_ontology:cs_drift_state('aad93500-b32b-48d2-b2f0-c808feabf82e', post_westphalian_settlement, gap(stable, minor, true)).
narrative_ontology:cs_created_at('aad93500-b32b-48d2-b2f0-c808feabf82e', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, territorial_princes).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, catholic_church).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, papacy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, rural_peasantry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exploit theological disputes to break papal authority, seize ecclesiastical lands and revenues, and establish state-controlled churches that consolidate dynastic power within their territories.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, territorial_princes, beneficiary,
    powerful, generational, mobile, continental).

% Loses vast landed wealth, tithe income, and jurisdictional authority across Northern and Central Europe; its institutional presence is either dissolved or subordinated to secular rulers.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, catholic_church, payer,
    institutional, civilizational, constrained, continental).

% Loses political leverage over monarchs and princes, sees its legal and fiscal exemptions revoked, and is treated as a foreign sovereign rather than a universal jurisdictional authority.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, papacy, payer,
    institutional, civilizational, constrained, continental).

% Formulate doctrinal justifications for princely break with Rome; provide the theological vocabulary that reframes asset seizure as spiritual renewal and state church formation as confessional duty; depend on princely protection for survival.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, protestant_theologians, agenda_setter,
    moderate, biographical, constrained, national).

% Bear the direct costs of religious warfare, taxation to fund new state churches, and displacement from former ecclesiastical estates; their confessional practice is dictated by princely fiat rather than choice.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, rural_peasantry, payer,
    powerless, biographical, trapped, local).

% Catholic populations in Protestant territories lose legal right to public worship and ecclesiastical infrastructure; their objections to confiscation and religious change are structurally absent from the political settlements.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, excluded_catholic_communities, excluded,
    powerless, generational, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates territorial rulers in jointly extricating their domains from supranational papal jurisdiction and establishing mutually recognized legal frameworks for territorial sovereignty over religious affairs, culminating in a stable state system.
% TRANSFER_FUNCTION: Moves ecclesiastical assets, legal jurisdiction, and coercive authority from the Catholic Church and papacy to secular territorial princes; moves doctrinal legitimacy from Rome to state-controlled theological faculties and consistories.
% ABSENT_VOICES: The papacy and Catholic populations under Protestant rule are excluded from the negotiation of new confessional settlements; peasants seeking local religious autonomy rather than princely determination are suppressed; Anabaptist and radical reformers who rejected state-church models are persecuted by both Catholic and Protestant authorities.
% DISAPPEARANCE_RATIONALE: Without the transfer of church wealth and authority to princes, the fiscal and legal basis of the early modern territorial state would not have formed in the same way; the European state system and the Westphalian balance of power would not have emerged as they did.
% FOUNDING_PROBLEM: The tension between papal universal jurisdiction and the consolidation of dynastic territorial sovereignty; the accumulation of ecclesiastical wealth and legal privilege that obstructed princely taxation and territorial control.
% FOUNDING_PROBLEM_CORROBORATION: Secular rulers and their chancelleries attested to the problem of papal interference. The papacy and Catholic historians attested that the problem was manufactured to justify seizure. Modern political historiography outside the immediate beneficiary tradition corroborates the realignment motive, whereas ecclesiastical historians emphasize independent doctrinal causation.
narrative_ontology:disappearance_verdict(reformation_event_boundary__political_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__political_swap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__political_swap_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_event_boundary__political_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__political_swap_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__political_swap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__political_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the transfer of ecclesiastical wealth and authority to princes was massive and decoupled from any equivalent service rendered to the Church. Suppression is high (0.78) because the constraint's persistence required active enforcement: princely armies, confessionalized legal systems, suppression of Catholic worship in Protestant territories, and the prosecution of dissenters. Theater_ratio is moderate-high (0.55) because the elaborate theological disputations and confessional documents served largely to dignify what was structurally a political and fiscal seizure. Accessibility_collapse (0.65) reflects the closure of alternatives to princely state-church monopoly; resistance (0.72) captures the substantial military and diplomatic opposition mounted by Catholic powers and internal dissidents.
 *
 * PERSPECTIVAL GAP:
 *   From the princely seat, the constraint is a necessary coordination mechanism to escape papal overreach and establish orderly territorial government; from the ecclesiastical seat, it is naked extraction dressed in theological language. The engine computes this divergence from the structural data: same events, opposite directionality depending on whether the agent is collecting land or losing it.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial_princes are the clear beneficiaries: they collect the extracted assets and authority, and their exit options are mobile because they shape the constraint. Catholic_church and papacy are the clear targets: they bear the loss of property and jurisdiction, with highly constrained exit because their institutional identity is fused with the universal jurisdiction being dismantled. Protestant_theologians sit near the agenda-setter position with moderate power and constrained exit: they depend on princes for protection, so their directionality is intermediate but closer to the beneficiary end because they gain status and safety from the arrangement. Rural_peasantry and excluded_catholic_communities are near full target: they suffer enforcement costs and have no exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than snare preserves the genuine coordination function: princes did face a real collective-action problem in mutually recognizing each other's break from Rome, and the state-church system did reduce some forms of interstate religious violence after Westphalia. If the coordination story were pure cover, the constraint would be a snare; the presence of a real coordination problem solved by the same structure that extracts from the Church places it in the tangled category. The theater_ratio captures the degree to which theology functioned as scaffold rather than engine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theology_causal_status,
    'Was Reformation theology purely post-hoc rationalization of princely interests, or did doctrinal innovation exert independent causal force on the political realignment?',
    'Archival analysis of the timing between princely political decisions and theological formulation; examination of cases where princes resisted theological innovations that would have served their material interests.',
    'If theology had independent causal force, the constraint''s extractiveness is overstated and its coordination component includes genuine doctrinal coordination; if purely post-hoc, the coordination story is largely cover and the constraint trends toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theology_causal_status, empirical, 'Ambiguity over theology''s causal role in the Reformation').

omega_variable(
    asset_transfer_necessity,
    'Did the seizure of church assets finance necessary state-building, or was it dispensable extraction that could have been avoided without destabilizing the political transition?',
    'Comparative fiscal history of Protestant and Catholic territories; analysis of state-capacity metrics relative to the scale of expropriation.',
    'If seizure was fiscally necessary for territorial administration, part of the measured extraction represents resource-allocation coordination rather than pure rent; if dispensable, extraction is pure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asset_transfer_necessity, empirical, 'Whether church asset seizure was necessary for state formation').

omega_variable(
    reading_committer_status,
    'Does the political_swap reading foreclose the theological_climb reading within a single historiographical framework, or do they remain coexisting live options?',
    'Survey of historiographical practice: whether scholars holding the political reading formally reject the theological reading''s core premise, or merely assign it different causal weight while treating both as legitimate research programs.',
    'If foreclosed, the kernel is a strict disjunction and composite readings are unstable; if coexisting, the kernel supports irreducibly multiple readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_status, conceptual, 'Structural relation between political and theological readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__political_swap_reading, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_event_boundary__political_swap_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(refo_tr_t25, reformation_event_boundary__political_swap_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement(refo_tr_t50, reformation_event_boundary__political_swap_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement(refo_tr_t75, reformation_event_boundary__political_swap_reading, theater_ratio, 75, 0.5).
narrative_ontology:measurement(refo_tr_t100, reformation_event_boundary__political_swap_reading, theater_ratio, 100, 0.53).
narrative_ontology:measurement(refo_tr_t130, reformation_event_boundary__political_swap_reading, theater_ratio, 130, 0.55).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_event_boundary__political_swap_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(refo_be_t25, reformation_event_boundary__political_swap_reading, base_extractiveness, 25, 0.5).
narrative_ontology:measurement(refo_be_t50, reformation_event_boundary__political_swap_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(refo_be_t75, reformation_event_boundary__political_swap_reading, base_extractiveness, 75, 0.75).
narrative_ontology:measurement(refo_be_t100, reformation_event_boundary__political_swap_reading, base_extractiveness, 100, 0.8).
narrative_ontology:measurement(refo_be_t130, reformation_event_boundary__political_swap_reading, base_extractiveness, 130, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t0, reformation_event_boundary__political_swap_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(refo_su_t25, reformation_event_boundary__political_swap_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement(refo_su_t50, reformation_event_boundary__political_swap_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(refo_su_t75, reformation_event_boundary__political_swap_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement(refo_su_t100, reformation_event_boundary__political_swap_reading, suppression_requirement, 100, 0.76).
narrative_ontology:measurement(refo_su_t130, reformation_event_boundary__political_swap_reading, suppression_requirement, 130, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is the political_swap reading of the reformation_event_boundary kernel. It decomposes the colloquial label 'the Reformation' into a specific structural claim: authority transfer from Rome to territorial princes driven by political exploitation of theological disputes. Sibling readings instantiate different structural claims from the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
