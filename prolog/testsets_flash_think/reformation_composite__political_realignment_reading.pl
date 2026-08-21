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
 *   constraint_id: reformation_composite__political_realignment_reading
 *   human_readable: Reformation as Political Realignment
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This constraint models the Reformation as a political event where
 *   emerging nation-states leveraged religious differentiation to assert
 *   sovereignty against the universal claims of imperial and papal authority.
 *   The core mechanism is the principle of 'Cuius regio, eius religio' (whose
 *   realm, his religion), which allowed territorial rulers to dictate the
 *   religion of their subjects, thereby consolidating political power and
 *   resources. This reading emphasizes the transfer of authority and wealth
 *   from supranational entities to nascent state structures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.85).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.78).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Reformation as Political Realignment").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, '07e4ed04-4b5a-4964-9bd9-83044da87adc').
narrative_ontology:cs_kernel_codification('07e4ed04-4b5a-4964-9bd9-83044da87adc', formalized).
narrative_ontology:cs_authority_grounding('07e4ed04-4b5a-4964-9bd9-83044da87adc', extraction).
narrative_ontology:cs_interpretation_layer_present('07e4ed04-4b5a-4964-9bd9-83044da87adc').
narrative_ontology:cs_reading_relation('07e4ed04-4b5a-4964-9bd9-83044da87adc', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('07e4ed04-4b5a-4964-9bd9-83044da87adc', reformation_composite__technological_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('07e4ed04-4b5a-4964-9bd9-83044da87adc', foundational, cuius_regio_eius_religio).
narrative_ontology:cs_axiom_status(cuius_regio_eius_religio, holdable).
narrative_ontology:cs_axiom_grounding('07e4ed04-4b5a-4964-9bd9-83044da87adc', cuius_regio_eius_religio, conventional).
narrative_ontology:cs_axiom('07e4ed04-4b5a-4964-9bd9-83044da87adc', foundational, secular_sovereignty_over_religious_authority).
narrative_ontology:cs_axiom_status(secular_sovereignty_over_religious_authority, holdable).
narrative_ontology:cs_axiom_grounding('07e4ed04-4b5a-4964-9bd9-83044da87adc', secular_sovereignty_over_religious_authority, conventional).
narrative_ontology:cs_reference_frame('07e4ed04-4b5a-4964-9bd9-83044da87adc', universal_imperial_papal_hegemony).
narrative_ontology:cs_drift_state('07e4ed04-4b5a-4964-9bd9-83044da87adc', peace_of_westphalia_1648, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('07e4ed04-4b5a-4964-9bd9-83044da87adc', '').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, territorial_rulers).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, emerging_nation_states).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, holy_roman_emperor).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, papacy).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, local_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, theologians_and_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Princes, dukes, and kings who asserted religious autonomy within their territories, seizing church assets and establishing state-controlled churches. They benefited directly from increased political power and economic resources.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, territorial_rulers, agenda_setter,
    institutional, generational, mobile, regional).

% The nominal head of a vast, decentralized empire, whose authority was directly challenged by the religious and political fragmentation. He bore the cost of lost imperial unity and military conflicts.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, holy_roman_emperor, payer,
    institutional, generational, constrained, continental).

% The spiritual and temporal head of the Catholic Church, whose universal authority was undermined by the rise of state churches. It lost significant revenue, political influence, and spiritual allegiance in many regions.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, papacy, payer,
    institutional, civilizational, constrained, global).

% Political entities that used religious differentiation as a tool to consolidate internal power, define national identity, and assert independence from external imperial or papal interference. They gained sovereignty and control over national resources.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, emerging_nation_states, beneficiary,
    institutional, generational, mobile, national).

% The common people who were often forced to adopt the religion of their ruler, facing persecution, exile, or death if they refused. They bore the direct social and personal costs of religious conflict and state-imposed uniformity.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, local_populations, payer,
    powerless, biographical, trapped, local).

% Intellectuals and religious leaders who provided the doctrinal justifications for the new religious orders. While some were genuinely motivated by faith, many gained patronage, protection, and influence by aligning with powerful rulers, becoming instrumental in the political realignment.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, theologians_and_reformers, beneficiary,
    moderate, biographical, constrained, regional).

% Scholars who interpret the Reformation through a political lens, analyzing the interplay of power, sovereignty, and religious change. They seek to understand the structural shifts that led to the modern state system.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__political_realignment_reading, territorial_rulers).
narrative_ontology:fixing_cost_class(reformation_composite__political_realignment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the assertion of secular sovereignty by territorial rulers, providing a framework for state-building independent of universal religious or imperial claims, thereby reducing internal conflicts over religious authority within a given territory.
% TRANSFER_FUNCTION: Transfers political authority, economic resources (e.g., church lands, tithes), and the right to determine religious practice from imperial and papal institutions to emerging nation-states and their rulers.
% ABSENT_VOICES: Those who genuinely sought purely theological reform without political entanglement, or those who advocated for a unified Christendom under imperial/papal authority, were often marginalized or suppressed by the political imperatives of the era. Their voices are largely absent from the dominant narratives of state-building.
% DISAPPEARANCE_RATIONALE: If this political realignment had not occurred, the modern nation-state system, the principle of state sovereignty, and the subsequent development of international law (e.g., Westphalian sovereignty) would be profoundly different or non-existent. Europe's political and religious landscape would be unrecognizable.
% FOUNDING_PROBLEM: The conflict between universal imperial/papal claims to authority and the growing desire of local rulers for greater autonomy and control over their territories and populations, including religious affairs, exacerbated by the financial and political power of the Church.
% FOUNDING_PROBLEM_CORROBORATION: Historians specializing in early modern European political history, political scientists studying state formation, and scholars of international relations corroborate this political interpretation. Primary sources from secular rulers, diplomatic correspondence, and legal treatises of the era attest to the centrality of sovereignty disputes. Independent economic analyses of the redistribution of wealth from church to state also support this reading.
narrative_ontology:disappearance_verdict(reformation_composite__political_realignment_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__political_realignment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__political_realignment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high because territorial rulers actively seized church lands, tithes, and the right to appoint clergy, redirecting significant resources and authority from the Holy Roman Empire and the Papacy to their own nascent states. Suppression is high due to the need to enforce religious uniformity within territories and to suppress both imperial/papal resistance and internal religious dissent. The theater ratio is moderate, reflecting that while genuine theological debates occurred, they were often instrumentalized or overshadowed by political objectives. Accessibility collapse is high as the alternatives of a unified Christendom or universal imperial authority were actively dismantled. Resistance was also high, leading to decades of religious wars.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of territorial rulers, this was a necessary assertion of sovereignty and a rationalization of governance, leading to a more stable political order. From the perspective of the Holy Roman Emperor and the Papacy, it was a catastrophic fragmentation of Christendom and a usurpation of legitimate authority. Local populations experienced it as a coercive imposition of religious identity, often with severe consequences for non-compliance.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial rulers and emerging nation-states are clear beneficiaries, gaining political autonomy, economic resources, and control over their populations. The Holy Roman Emperor and the Papacy are victims, losing significant authority, land, and influence. Local populations are also victims, as their religious choices were often dictated by their rulers, leading to forced conversions, emigration, or persecution. Theologians and reformers, while providing the intellectual framework, often became beneficiaries by aligning with powerful rulers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_vs_theological_primacy,
    'Is the political realignment the primary driver of the Reformation, or a consequence of theological shifts?',
    'Comparative historical analysis focusing on the sequencing of political and theological decisions in key territories, and the stated motivations of actors.',
    'If theological shifts are primary, the constraint''s extractiveness might be lower, reflecting genuine religious conviction rather than pure power-seeking. If political drivers are primary, the extraction and suppression metrics are more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_vs_theological_primacy, conceptual, 'Ambiguity regarding the causal primacy of political versus theological factors in the Reformation.').

omega_variable(
    role_of_individual_agency,
    'To what extent did individual reformers'' theological convictions genuinely drive events, versus being instrumentalized by political actors for their own ends?',
    'Detailed biographical studies and analysis of correspondence, distinguishing between personal conviction and strategic political alliances.',
    'If individual agency was largely instrumentalized, the ''theater_ratio'' might be higher, indicating more performative religious adherence for political gain. If genuine, the ''theater_ratio'' might be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(role_of_individual_agency, empirical, 'The degree to which theological agency was independent of political instrumentalization.').

omega_variable(
    long_term_impact_on_secularization,
    'Did this political event inadvertently lay the groundwork for later secularization, or was it primarily about re-establishing religious authority at a different (state) level?',
    'Longitudinal historical analysis tracing the evolution of state-church relations and the concept of religious freedom from the 16th to 18th centuries.',
    'If it primarily re-established religious authority, the ''suppression'' metric might be seen as a feature of the new order. If it led to secularization, the constraint''s long-term ''extractiveness'' might be viewed as having a different, unintended outcome.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_impact_on_secularization, conceptual, 'The long-term consequences of the political realignment for secularization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__political_realignment_reading, theater_ratio, 1517, 0.55).
narrative_ontology:measurement(refo_tr_t1534, reformation_composite__political_realignment_reading, theater_ratio, 1534, 0.5).
narrative_ontology:measurement(refo_tr_t1555, reformation_composite__political_realignment_reading, theater_ratio, 1555, 0.48).
narrative_ontology:measurement(refo_tr_t1618, reformation_composite__political_realignment_reading, theater_ratio, 1618, 0.46).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__political_realignment_reading, theater_ratio, 1648, 0.45).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__political_realignment_reading, base_extractiveness, 1517, 0.6).
narrative_ontology:measurement(refo_be_t1534, reformation_composite__political_realignment_reading, base_extractiveness, 1534, 0.7).
narrative_ontology:measurement(refo_be_t1555, reformation_composite__political_realignment_reading, base_extractiveness, 1555, 0.78).
narrative_ontology:measurement(refo_be_t1618, reformation_composite__political_realignment_reading, base_extractiveness, 1618, 0.82).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__political_realignment_reading, base_extractiveness, 1648, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__political_realignment_reading, suppression_requirement, 1517, 0.65).
narrative_ontology:measurement(refo_su_t1534, reformation_composite__political_realignment_reading, suppression_requirement, 1534, 0.7).
narrative_ontology:measurement(refo_su_t1555, reformation_composite__political_realignment_reading, suppression_requirement, 1555, 0.75).
narrative_ontology:measurement(refo_su_t1618, reformation_composite__political_realignment_reading, suppression_requirement, 1618, 0.77).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__political_realignment_reading, suppression_requirement, 1648, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__political_realignment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, modern_nation_state_sovereignty).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, religious_pluralism_toleration).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, secularization_of_politics).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reformation_composite' kernel, focusing on political realignment. It is linked to sibling readings that emphasize theological and technological aspects, as these factors are interdependent in the broader historical event.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
