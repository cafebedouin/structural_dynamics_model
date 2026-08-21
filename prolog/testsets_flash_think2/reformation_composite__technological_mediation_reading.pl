% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__technological_mediation_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Printing Press as Catalyst for Reformation Mass Movement
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This constraint describes the Reformation as fundamentally a
 *   technological event, where the printing press transformed local
 *   theological dissent into a continental mass movement. The printing press
 *   itself, as a physical and operational reality, is treated as a Mountain,
 *   enabling and shaping all subsequent religious and political dynamics. Its
 *   inherent properties of rapid, cheap, and widespread information
 *   dissemination are the core of the constraint. This is the
 *   'technological_mediation_reading' of the 'reformation_composite' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.15).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.1).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, mountain).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Printing Press as Catalyst for Reformation Mass Movement").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, 'ae9fb23d-c5ee-40e1-8367-f669a4286bb6').
narrative_ontology:cs_kernel_codification('ae9fb23d-c5ee-40e1-8367-f669a4286bb6', formalized).
narrative_ontology:cs_authority_grounding('ae9fb23d-c5ee-40e1-8367-f669a4286bb6', self_enforcing).
narrative_ontology:cs_reading_relation('ae9fb23d-c5ee-40e1-8367-f669a4286bb6', reformation_composite__theological_fragmentation_reading, influences).
narrative_ontology:cs_reading_relation('ae9fb23d-c5ee-40e1-8367-f669a4286bb6', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_axiom('ae9fb23d-c5ee-40e1-8367-f669a4286bb6', foundational, information_dissemination_is_power).
narrative_ontology:cs_axiom_status(information_dissemination_is_power, holdable).
narrative_ontology:cs_axiom_grounding('ae9fb23d-c5ee-40e1-8367-f669a4286bb6', information_dissemination_is_power, empirically_contingent).
narrative_ontology:cs_axiom('ae9fb23d-c5ee-40e1-8367-f669a4286bb6', foundational, technological_change_drives_social_change).
narrative_ontology:cs_axiom_status(technological_change_drives_social_change, holdable).
narrative_ontology:cs_axiom_grounding('ae9fb23d-c5ee-40e1-8367-f669a4286bb6', technological_change_drives_social_change, empirically_contingent).
narrative_ontology:cs_reference_frame('ae9fb23d-c5ee-40e1-8367-f669a4286bb6', decentralized_information_flow).
narrative_ontology:cs_drift_state('ae9fb23d-c5ee-40e1-8367-f669a4286bb6', contemporary_digital_age, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('ae9fb23d-c5ee-40e1-8367-f669a4286bb6', '').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, protestant_reformers).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, literate_populace).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, printers_and_publishers).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, catholic_church_hierarchy).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, holy_roman_empire).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controlled the means of mass communication, deciding what was printed and distributed, and profiting immensely from the demand for new ideas and religious texts. They became key actors in the spread of the Reformation.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, printers_and_publishers, agenda_setter,
    organized, biographical, mobile, regional).

% Gained unprecedented reach for their theological arguments, transforming local grievances and academic debates into a widespread, continental mass movement. The printing press was essential to their success.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, protestant_reformers, beneficiary,
    organized, generational, mobile, continental).

% Gained direct access to religious texts, theological arguments, and political pamphlets, bypassing traditional intermediaries like priests. This fostered individual interpretation and contributed to the mass movement.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, literate_populace, beneficiary,
    moderate, biographical, mobile, local).

% Lost its monopoly on information dissemination and interpretation, facing direct and rapid challenges to its authority. Efforts to suppress printed materials were largely ineffective against the scale of production.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, catholic_church_hierarchy, payer,
    institutional, generational, constrained, continental).

% Faced severe political instability, wars, and challenges to its temporal authority as religious divisions, fueled by printed propaganda, led to widespread dissent and conflict across its territories.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, holy_roman_empire, payer,
    institutional, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__technological_mediation_reading, printers_and_publishers).
narrative_ontology:fixing_cost_class(reformation_composite__technological_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled the rapid and widespread coordination of theological dissent and new religious practices across vast geographical areas, allowing for the standardization and dissemination of new doctrines and critiques of existing authority.
% TRANSFER_FUNCTION: Transferred the power of theological interpretation and information dissemination from a centralized ecclesiastical authority to a decentralized network of printers, scholars, and readers, fundamentally altering the landscape of religious and political power.
% ABSENT_VOICES: Those who preferred the pre-printing press oral tradition or manuscript culture, or whose local dissent was effectively contained before mass publication, were effectively sidelined or overwhelmed by the new information regime. Their slower, localized forms of communication could not compete.
% DISAPPEARANCE_RATIONALE: If the printing press and its effects on information flow were to vanish, the very structure of mass movements, public discourse, and the challenge to centralized authority would fundamentally alter. The Reformation as a continental mass movement, rather than a series of localized heresies, would not have occurred in the same way.
% FOUNDING_PROBLEM: The slow, expensive, and centralized dissemination of information, which limited access to religious texts and theological debate, thereby maintaining the Catholic Church's interpretive monopoly and hindering the spread of new ideas.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and communication universally corroborate that the printing press fundamentally revolutionized information dissemination, effectively solving the problem of slow, centralized communication. The Catholic Church's later efforts to control printing (e.g., the Index Librorum Prohibitorum) implicitly acknowledge the problem's resolution and the new power of print. Independent scholars and historical analyses from outside the benefiting parties confirm this technological shift.
narrative_ontology:disappearance_verdict(reformation_composite__technological_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__technological_mediation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__technological_mediation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reformation_composite__technological_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__technological_mediation_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__technological_mediation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_composite__technological_mediation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_composite__technological_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The printing press, as a technology, is a structural feature of reality once invented and adopted, hence its classification as a Mountain. Its base extractiveness is low (0.15) because the technology itself doesn't directly extract rents, but rather enables new forms of social organization and power dynamics that can be extractive. Suppression (0.1) and resistance (0.1) are low because the technology itself was difficult to resist or suppress once established, though its *outputs* faced immense resistance. Accessibility collapse is low (0.1) because the press dramatically *expanded* access to information. Theater ratio is negligible (0.05) as its function was overwhelmingly practical. The declared beneficiaries and victims, despite the 'mountain' claim, are present because the *operation* of this technological mountain had profound, asymmetric impacts, triggering a False Summit Mountain evaluation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the beneficiaries, the printing press was a liberating force, enabling truth and progress. From the perspective of the victims, it was a destructive force, undermining established order and authority. The engine will compute these divergent classifications based on the structural roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Printers and publishers, along with Protestant reformers and the newly literate populace, are beneficiaries as they gained power, reach, and access from the printing press's operation. The Catholic Church hierarchy and the Holy Roman Empire are victims, as their authority and stability were undermined by the decentralized information flow. The directionality for beneficiaries would be low (subsidized by the technology's enabling function), while for victims it would be high (bearing the costs of the old order's disruption).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fsm_technological_mountain_or_snare,
    'Is the printing press, in its role as a catalyst for the Reformation, a genuine natural law (Mountain) or a constructed constraint whose operation benefits identifiable agents (False Summit Mountain, potentially a Snare of information control)?',
    'Analysis of the historical counterfactual: if the technology had been invented but systematically suppressed or controlled by a single entity, would its ''natural'' properties still have led to the same outcomes, or would it have become a tool of extraction? This would distinguish the inherent properties from the contingent social organization around it.',
    'If it''s a genuine Mountain, its classification remains stable. If it''s a False Summit, the engine''s FSM signature will reclassify it (likely to Tangled Rope or Snare), highlighting the extractive social structures built upon the ''natural'' technological base.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fsm_technological_mountain_or_snare, conceptual, 'Ambiguity between inherent technological property and its socially mediated, extractive operation.').

omega_variable(
    reading_delta_theological_fragmentation,
    'How would the ''theological_fragmentation_reading'' of the ''reformation_composite'' kernel alter the classification of the Reformation?',
    'By generating a separate constraint story for the ''theological_fragmentation_reading'', focusing on the specific doctrinal commitments and their enforcement mechanisms.',
    'The ''theological_fragmentation_reading'' would likely classify the Reformation as a Tangled Rope or Snare, emphasizing the coercive aspects of competing theological commitments and their enforcement by various denominations, rather than the enabling technology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_delta_theological_fragmentation, conceptual, 'Impact of focusing on doctrinal commitments vs. technological mediation.').

omega_variable(
    reading_delta_political_realignment,
    'How would the ''political_realignment_reading'' of the ''reformation_composite'' kernel alter the classification of the Reformation?',
    'By generating a separate constraint story for the ''political_realignment_reading'', focusing on the role of emerging nation-states and their assertion of sovereignty.',
    'The ''political_realignment_reading'' would likely classify the Reformation as a Tangled Rope or Snare, emphasizing the political extraction and suppression involved in state-building and the assertion of sovereignty against imperial/papal authority, rather than the technological enabler.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_delta_political_realignment, conceptual, 'Impact of focusing on political power dynamics vs. technological mediation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 1450, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1450, reformation_composite__technological_mediation_reading, theater_ratio, 1450, 0.01).
narrative_ontology:measurement(refo_tr_t1480, reformation_composite__technological_mediation_reading, theater_ratio, 1480, 0.01).
narrative_ontology:measurement(refo_tr_t1510, reformation_composite__technological_mediation_reading, theater_ratio, 1510, 0.02).
narrative_ontology:measurement(refo_tr_t1540, reformation_composite__technological_mediation_reading, theater_ratio, 1540, 0.03).
narrative_ontology:measurement(refo_tr_t1570, reformation_composite__technological_mediation_reading, theater_ratio, 1570, 0.04).
narrative_ontology:measurement(refo_tr_t1600, reformation_composite__technological_mediation_reading, theater_ratio, 1600, 0.05).
narrative_ontology:measurement(refo_tr_t1630, reformation_composite__technological_mediation_reading, theater_ratio, 1630, 0.05).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__technological_mediation_reading, theater_ratio, 1648, 0.05).

% Extraction over time
narrative_ontology:measurement(refo_be_t1450, reformation_composite__technological_mediation_reading, base_extractiveness, 1450, 0.05).
narrative_ontology:measurement(refo_be_t1480, reformation_composite__technological_mediation_reading, base_extractiveness, 1480, 0.08).
narrative_ontology:measurement(refo_be_t1510, reformation_composite__technological_mediation_reading, base_extractiveness, 1510, 0.11).
narrative_ontology:measurement(refo_be_t1540, reformation_composite__technological_mediation_reading, base_extractiveness, 1540, 0.13).
narrative_ontology:measurement(refo_be_t1570, reformation_composite__technological_mediation_reading, base_extractiveness, 1570, 0.14).
narrative_ontology:measurement(refo_be_t1600, reformation_composite__technological_mediation_reading, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement(refo_be_t1630, reformation_composite__technological_mediation_reading, base_extractiveness, 1630, 0.15).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__technological_mediation_reading, base_extractiveness, 1648, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1450, reformation_composite__technological_mediation_reading, suppression_requirement, 1450, 0.05).
narrative_ontology:measurement(refo_su_t1480, reformation_composite__technological_mediation_reading, suppression_requirement, 1480, 0.07).
narrative_ontology:measurement(refo_su_t1510, reformation_composite__technological_mediation_reading, suppression_requirement, 1510, 0.1).
narrative_ontology:measurement(refo_su_t1540, reformation_composite__technological_mediation_reading, suppression_requirement, 1540, 0.12).
narrative_ontology:measurement(refo_su_t1570, reformation_composite__technological_mediation_reading, suppression_requirement, 1570, 0.13).
narrative_ontology:measurement(refo_su_t1600, reformation_composite__technological_mediation_reading, suppression_requirement, 1600, 0.14).
narrative_ontology:measurement(refo_su_t1630, reformation_composite__technological_mediation_reading, suppression_requirement, 1630, 0.15).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__technological_mediation_reading, suppression_requirement, 1648, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__technological_mediation_reading, information_standard).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__political_realignment_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'reformation_composite' kernel. This 'technological_mediation_reading' focuses on the printing press as the primary enabler, while sibling readings emphasize theological and political dimensions. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
