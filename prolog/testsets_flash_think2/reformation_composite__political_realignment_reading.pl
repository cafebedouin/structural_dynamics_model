% ============================================================================
% CONSTRAINT STORY: reformation_composite__political_realignment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__political_realignment_reading, []).

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
 *   constraint_id: reformation_composite__political_realignment_reading
 *   human_readable: Reformation: Political Realignment Reading (Cuius Regio Eius Religio)
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'political realignment' reading of the
 *   Reformation, focusing on how emerging nation-states leveraged religious
 *   differentiation to assert sovereignty against the universalizing claims
 *   of imperial and papal authority. The principle of 'Cuius regio eius
 *   religio' (whose realm, his religion) is the primary observable,
 *   signifying the transfer of religious control to secular rulers. The
 *   constraint is claimed as a Rope by its beneficiaries (territorial rulers)
 *   who saw it as a necessary coordination for state-building, but its
 *   metrics reflect substantial extraction and suppression from the
 *   perspective of the Holy Roman Empire and Papacy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.85).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.9).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Reformation: Political Realignment Reading (Cuius Regio Eius Religio)").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, '084309c2-60cd-4e58-83ea-66b4d9d420d1').
narrative_ontology:cs_kernel_codification('084309c2-60cd-4e58-83ea-66b4d9d420d1', formalized).
narrative_ontology:cs_authority_grounding('084309c2-60cd-4e58-83ea-66b4d9d420d1', extraction).
narrative_ontology:cs_interpretation_layer_present('084309c2-60cd-4e58-83ea-66b4d9d420d1').
narrative_ontology:cs_reading_relation('084309c2-60cd-4e58-83ea-66b4d9d420d1', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('084309c2-60cd-4e58-83ea-66b4d9d420d1', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('084309c2-60cd-4e58-83ea-66b4d9d420d1', foundational, sovereignty_derives_from_territorial_control).
narrative_ontology:cs_axiom_status(sovereignty_derives_from_territorial_control, holdable).
narrative_ontology:cs_axiom_grounding('084309c2-60cd-4e58-83ea-66b4d9d420d1', sovereignty_derives_from_territorial_control, conventional).
narrative_ontology:cs_axiom('084309c2-60cd-4e58-83ea-66b4d9d420d1', secondary, religious_unity_as_political_tool).
narrative_ontology:cs_axiom_status(religious_unity_as_political_tool, holdable).
narrative_ontology:cs_axiom_grounding('084309c2-60cd-4e58-83ea-66b4d9d420d1', religious_unity_as_political_tool, instrumental).
narrative_ontology:cs_reference_frame('084309c2-60cd-4e58-83ea-66b4d9d420d1', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('084309c2-60cd-4e58-83ea-66b4d9d420d1', contemporary_globalization_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('084309c2-60cd-4e58-83ea-66b4d9d420d1', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, territorial_rulers).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, emerging_nation_states).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, holy_roman_empire).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, papal_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, common_people).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, theologians_reformers).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, common_people).
narrative_ontology:constraint_vindicates(reformation_composite__political_realignment_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(reformation_composite__political_realignment_reading, cuius_regio_eius_religio_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserted their right to determine the religion of their territories, thereby consolidating political power, gaining control over church lands and revenues, and reducing external interference from imperial or papal authority. They actively enforced religious conformity within their domains.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, territorial_rulers, agenda_setter,
    institutional, generational, mobile, regional).

% Benefited from the political fragmentation of the Holy Roman Empire and the weakening of papal temporal power, allowing them to develop distinct national identities and centralized administrative structures, often with a state-controlled church.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, emerging_nation_states, beneficiary,
    institutional, civilizational, mobile, national).

% Suffered a significant loss of political authority, territorial control, and tax revenue as constituent states asserted religious and political independence. Its attempts to enforce religious unity and imperial authority were met with armed resistance and ultimately failed.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, holy_roman_empire, payer,
    institutional, generational, constrained, continental).

% Experienced a drastic reduction in spiritual and temporal authority, loss of tithes and church property, and diminished political influence in many European territories. Its claims to universal religious and political supremacy were directly challenged and often rejected.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, papal_authority, payer,
    institutional, generational, constrained, global).

% Were often forced to adopt the religion of their ruler ('Cuius regio eius religio'), facing persecution or exile if they refused. While some gained access to vernacular scripture or local church control, the primary impact was a loss of religious choice and increased state control over their lives.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, common_people, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(reformation_composite__political_realignment_reading, common_people, beneficiary).

% Gained patronage, protection, and influence from territorial rulers who adopted their theological positions, allowing them to establish new church structures and disseminate their ideas, often at the cost of dependence on political power.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, theologians_reformers, beneficiary,
    moderate, biographical, constrained, regional).

% Those who remained loyal to the Catholic Church in Protestant territories often faced persecution, confiscation of property, or forced conversion/exile. Their voices and claims were systematically suppressed within the new political-religious order.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, catholic_loyalists, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled territorial rulers to coordinate their assertion of political autonomy and consolidate power within their domains, reducing external interference from imperial or papal powers, and establishing a more stable internal order based on religious uniformity.
% TRANSFER_FUNCTION: Transferred political authority, control over church property, tax revenues, and loyalty from the Holy Roman Empire and Papacy to local territorial rulers, effectively decentralizing power and resources.
% ABSENT_VOICES: Those who advocated for a unified Christendom under a single religious authority, or those who sought religious freedom independent of state control, were largely suppressed or excluded from the political settlements that defined the era.
% DISAPPEARANCE_RATIONALE: If the political realignment of the Reformation had not occurred, the modern nation-state system, the concept of state sovereignty, and the current religious landscape of Europe would be fundamentally different. The political and legal structures that emerged were direct consequences of this constraint.
% FOUNDING_PROBLEM: The perceived overreach of imperial and papal authority, the desire for greater political autonomy by local rulers, and the financial drain of supporting distant religious and political centers, coupled with internal desires for administrative and legal consolidation.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts from contemporary political theorists (e.g., Machiavelli, Bodin), diplomatic records, and modern historical analyses from outside the benefiting parties (e.g., by historians like Max Weber, Quentin Skinner) corroborate the political motivations of rulers, distinct from purely theological justifications.
narrative_ontology:disappearance_verdict(reformation_composite__political_realignment_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__political_realignment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__political_realignment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reformation_composite__political_realignment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__political_realignment_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__political_realignment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__political_realignment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the constraint fundamentally reallocated immense wealth, land, and political loyalty from the Church and Empire to local rulers. Suppression is very high, as the establishment of state-controlled religions involved active coercion, religious wars, and the suppression of dissent. The theater ratio is moderate-high, reflecting that while genuine theological disputes existed, the religious arguments often served as a legitimizing cover for political power grabs and territorial expansion. Accessibility collapse is high for imperial/papal authority to enforce universal religious conformity, and for subjects to choose religion independently. Resistance was high, leading to prolonged conflicts.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of territorial rulers, the Reformation was a necessary coordination mechanism for establishing stable, sovereign states. From the perspective of the Holy Roman Empire and Papacy, it was a destructive act of rebellion and extraction. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial rulers and emerging nation-states are clear beneficiaries (low d) as they gained sovereignty, resources, and internal stability. The Holy Roman Empire and Papal Authority are clear targets (high d) as they suffered significant losses of power, territory, and revenue. The common people are complex: they bore the cost of forced religious conformity (high d) but sometimes gained local autonomy or access to vernacular scripture (low d for specific benefits). Theologians and reformers benefited from patronage but were also constrained by political agendas.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    primary_driver_ambiguity,
    'Was the primary driver of the Reformation fundamentally theological, political, or technological?',
    'Comparative historical analysis weighing the causal primacy of theological debates, political power struggles, and the impact of the printing press on the overall trajectory of the Reformation.',
    'If theological or technological drivers are found to be primary, this ''political realignment'' reading would be reclassified as a secondary effect or an instrumental outcome, potentially altering its extractiveness and suppression metrics relative to the core driver.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(primary_driver_ambiguity, conceptual, 'Ambiguity regarding the Reformation''s core causal mechanism.').

omega_variable(
    religious_conviction_vs_opportunism,
    'What was the true balance between genuine religious conviction and political opportunism for territorial rulers in adopting Protestantism?',
    'Detailed biographical studies of individual rulers, analysis of their correspondence and policy decisions, and examination of the economic and social conditions of their territories.',
    'If political opportunism is found to be overwhelmingly dominant, the ''theater_ratio'' would increase, and the ''extractiveness'' would be more clearly attributed to rent-seeking rather than genuine coordination for religious belief. If genuine conviction was high, the coordination aspect would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_conviction_vs_opportunism, empirical, 'The extent to which political actors were driven by religious belief versus strategic advantage.').

omega_variable(
    printing_press_influence_on_political_realignment,
    'To what extent did the printing press (technological mediation) enable or accelerate the political realignment described in this reading?',
    'Quantitative analysis of pamphlet distribution, literacy rates, and the speed of information dissemination in relation to political decisions and popular uprisings during the period.',
    'If the printing press is found to be a critical enabler, the ''technological_mediation_reading'' would gain more causal weight, potentially influencing the ''suppression'' metric (as ideas became harder to suppress) and the ''accessibility_collapse'' (as alternatives became more visible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printing_press_influence_on_political_realignment, empirical, 'The role of technology in facilitating political change during the Reformation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__political_realignment_reading, theater_ratio, 1517, 0.3).
narrative_ontology:measurement(refo_tr_t1530, reformation_composite__political_realignment_reading, theater_ratio, 1530, 0.45).
narrative_ontology:measurement(refo_tr_t1555, reformation_composite__political_realignment_reading, theater_ratio, 1555, 0.55).
narrative_ontology:measurement(refo_tr_t1580, reformation_composite__political_realignment_reading, theater_ratio, 1580, 0.6).
narrative_ontology:measurement(refo_tr_t1618, reformation_composite__political_realignment_reading, theater_ratio, 1618, 0.62).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__political_realignment_reading, theater_ratio, 1648, 0.6).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__political_realignment_reading, base_extractiveness, 1517, 0.4).
narrative_ontology:measurement(refo_be_t1530, reformation_composite__political_realignment_reading, base_extractiveness, 1530, 0.55).
narrative_ontology:measurement(refo_be_t1555, reformation_composite__political_realignment_reading, base_extractiveness, 1555, 0.7).
narrative_ontology:measurement(refo_be_t1580, reformation_composite__political_realignment_reading, base_extractiveness, 1580, 0.78).
narrative_ontology:measurement(refo_be_t1618, reformation_composite__political_realignment_reading, base_extractiveness, 1618, 0.82).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__political_realignment_reading, base_extractiveness, 1648, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__political_realignment_reading, suppression_requirement, 1517, 0.5).
narrative_ontology:measurement(refo_su_t1530, reformation_composite__political_realignment_reading, suppression_requirement, 1530, 0.65).
narrative_ontology:measurement(refo_su_t1555, reformation_composite__political_realignment_reading, suppression_requirement, 1555, 0.75).
narrative_ontology:measurement(refo_su_t1580, reformation_composite__political_realignment_reading, suppression_requirement, 1580, 0.82).
narrative_ontology:measurement(refo_su_t1618, reformation_composite__political_realignment_reading, suppression_requirement, 1618, 0.88).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__political_realignment_reading, suppression_requirement, 1648, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__political_realignment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__technological_mediation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, rise_of_nation_state_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'reformation_composite' kernel. This 'political realignment' reading focuses on the assertion of state sovereignty, while the 'theological fragmentation' reading emphasizes doctrinal schism, and the 'technological mediation' reading highlights the role of the printing press. Each reading has a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
