% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__husk_reading, []).

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
 *   constraint_id: preparedness_transmission__husk_reading
 *   human_readable: Preparedness Drills as Hollowed Memorial Ritual
 *   domain: disaster_risk_management/institutional_memory
 *
 * SUMMARY:
 *   A civil defense jurisdiction maintains a formal apparatus of drills,
 *   inspections, and after-action reports that document preparedness for
 *   flood scenarios. The administrative machinery persists: meetings are
 *   held, rosters are updated, exercises are scheduled and reported complete.
 *   However, interviews with field personnel reveal that the operational
 *   knowledge underlying these drills has partially atrophied. Staff turnover
 *   has left gaps in institutional memory; newer personnel execute protocols
 *   without understanding their adaptive rationale; inspection routines
 *   reliably detect only conformance to established templates, not novel
 *   vulnerabilities. The constraint under this reading is the continued
 *   performance of these rituals despite the hollowing of the operational
 *   knowledge they were designed to preserve and transmit. This is the
 *   husk_reading: organizational memory (the administrative apparatus)
 *   persists as ceremony, but functional knowledge (adaptive capacity under
 *   conditions the protocols did not anticipate) has decayed. The
 *   complementary readings—competence_reading (drills as live knowledge
 *   exercise) and hybrid_reading (infrastructure competence remains high
 *   while civilian coordination has atrophied)—offer structurally distinct
 *   accounts of the same kernel, the preparedness transmission commitment
 *   that the jurisdiction avows.
 *
 * KEY AGENTS:
 *   - civil_defense_administration: Maintains the drill schedule and inspection apparatus; benefits from continued organizational form even as functional capacity decays
 *   - field_personnel_and_coordinators: Execute drills and inspections; experience the protocol as increasingly ritual; encounter novel scenarios for which the protocols provide no adaptive guidance
 *   - at_risk_populations: Depend on the jurisdiction's actual preparedness; receive assurance from the visible apparatus; exposed to hidden capability gaps when novel flood scenarios occur
 *   - political_leadership: Funded the preparedness apparatus; points to continued drills and inspections as evidence of readiness; faces reputational cost if actual preparedness is exposed as hollow
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__husk_reading, 0.61).
domain_priors:suppression_score(preparedness_transmission__husk_reading, 0.48).
domain_priors:theater_ratio(preparedness_transmission__husk_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, accessibility_collapse, 0.39).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__husk_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__husk_reading, "Preparedness Drills as Hollowed Memorial Ritual").
narrative_ontology:topic_domain(preparedness_transmission__husk_reading, "disaster_risk_management/institutional_memory").

domain_priors:requires_active_enforcement(preparedness_transmission__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__husk_reading, 'ec181b1a-7d86-494d-953e-2d4a133ace57').
narrative_ontology:cs_kernel_codification('ec181b1a-7d86-494d-953e-2d4a133ace57', formalized).
narrative_ontology:cs_authority_grounding('ec181b1a-7d86-494d-953e-2d4a133ace57', extraction).
narrative_ontology:cs_interpretation_layer_present('ec181b1a-7d86-494d-953e-2d4a133ace57').
narrative_ontology:cs_reading_relation('ec181b1a-7d86-494d-953e-2d4a133ace57', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec181b1a-7d86-494d-953e-2d4a133ace57', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('ec181b1a-7d86-494d-953e-2d4a133ace57', foundational, preparedness_knowledge_transmission_incomplete).
narrative_ontology:cs_axiom_status(preparedness_knowledge_transmission_incomplete, holdable).
narrative_ontology:cs_axiom_grounding('ec181b1a-7d86-494d-953e-2d4a133ace57', preparedness_knowledge_transmission_incomplete, empirically_contingent).
narrative_ontology:cs_axiom('ec181b1a-7d86-494d-953e-2d4a133ace57', foundational, protocol_form_decoupled_from_adaptive_capacity).
narrative_ontology:cs_axiom_status(protocol_form_decoupled_from_adaptive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('ec181b1a-7d86-494d-953e-2d4a133ace57', protocol_form_decoupled_from_adaptive_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('ec181b1a-7d86-494d-953e-2d4a133ace57', administered_preparedness_with_transmitted_adaptive_knowledge).
narrative_ontology:cs_drift_state('ec181b1a-7d86-494d-953e-2d4a133ace57', contemporary_post_workforce_turnover, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ec181b1a-7d86-494d-953e-2d4a133ace57', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__husk_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, civil_defense_administration).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, at_risk_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, at_risk_populations).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, political_leadership).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, field_personnel_and_coordinators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and schedules the jurisdiction's drills and inspections. Drafts protocols, assigns personnel, publishes completion reports. Benefits from the continued existence of the apparatus (provides justification for budget allocation, demonstrates institutional competence, provides administrative structure that preserves the organization). Can adapt protocols or shift investment toward real-time adaptive training, but doing so would require acknowledging that current inspection routines do not detect novel vulnerabilities—a political cost the administration avoids by continuing to perform the established apparatus as designed.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, civil_defense_administration, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__husk_reading, civil_defense_administration, beneficiary).

% Execute the drills and inspections. Newer personnel (hired in last 10-15 years) follow established protocols without deep understanding of the adaptive rationale. Senior staff know what the protocols do not cover and increasingly express doubt about their sufficiency. They experience the constraint as a mismatch between formalized procedure and the variability of real disasters. Exit is difficult because professional identity and career advancement are embedded in the civil defense system, and transferable skills are limited to organizations with similar structures.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, field_personnel_and_coordinators, payer,
    moderate, biographical, identity_locked, regional).

% Reside in flood-prone areas and depend on the jurisdiction's preparedness for protection. Benefit from the visible apparatus (drills indicate someone is paying attention, protocols provide basic guidance). Pay the cost of false assurance: believe the apparatus is operationally competent when adaptive knowledge is hollow, direct personal preparation according to established protocols rather than novel threat assessments, face unplanned scenarios for which neither protocols nor adaptive knowledge prepare them. Cannot exit because residence is constrained by economic and social ties.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, at_risk_populations, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__husk_reading, at_risk_populations, payer).

% Authorized and funded the civil defense apparatus. Benefits from the continued demonstration that preparedness is being maintained (points to drills and inspections in public discourse about disaster risk management). Would face reputational and political liability if actual preparedness gaps were made visible during or after a major disaster. Can exit by shifting to alternative administration structures or by directing resources away from drills toward real-time adaptive capacity, but doing so would require publicly acknowledging that the current apparatus is insufficient.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, political_leadership, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__husk_reading, political_leadership, beneficiary).

% Conduct post-disaster autopsy studies and provide evidence that the jurisdiction's preparedness apparatus did not detect or adapt to the actual disaster scenario. Their analyses document the gap between protocol and outcome. They are excluded from the jurisdiction's agenda-setting and budget decisions; their findings are received as criticism or academic commentary rather than operational intelligence. They would recommend restructuring training toward adaptive capacity and transparency about capability gaps.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, disaster_analysts_and_evaluators, excluded,
    moderate, biographical, mobile, global).

% Academic and research perspective on institutional memory, knowledge transmission, and the piton/theater dynamics of administrative ritual. Measures the constraint's structural characteristics without stake in the jurisdiction's political or administrative outcomes.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, observer_research, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__husk_reading, civil_defense_administration).
narrative_ontology:fixing_cost_class(preparedness_transmission__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a jurisdiction-wide apparatus for disaster preparedness: coordinates drill scheduling across agencies, standardizes protocols and safety procedures, creates a common training ground where personnel from different departments exercise response procedures together, establishes a reporting structure that documents readiness status. Under the husk_reading, this coordination function is partially atrophied—it coordinates the form of preparedness but with decreasing accuracy about actual adaptive capacity.
% TRANSFER_FUNCTION: The constraint moves administrative authority from field personnel (who would otherwise design locally-adaptive responses) to the civil defense administration (which prescribes standardized protocols). It also transfers risk-mitigation responsibility from the administration and political leadership (who would otherwise face pressure to acknowledge capability gaps) to the at-risk populations (who assume the protocols are sufficient and prepare accordingly). The constraint transfers organizational legitimacy and budget justification from actual preparedness capacity to the appearance of preparedness through continued drill performance.
% ABSENT_VOICES: Disaster analysts and evaluators who study what the protocols failed to anticipate are excluded from the jurisdiction's preparedness planning and budget discussions. Their findings about novel failure modes are treated as external criticism rather than operational intelligence. Younger or newer field personnel who notice gaps in the protocols but lack seniority to change them are present but not heard at the agenda-setting level. The at-risk populations are consulted about their experience only after disasters occur, not in the design of preparation protocols.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared—if the civil defense administration ceased to maintain the drill and inspection apparatus—the jurisdiction would face immediate pressure to restructure preparedness efforts. Some would shift toward real-time adaptive training and scenario design; some would devolve preparedness to local communities and private actors; political leadership would face immediate questions about what replaces the apparatus. The at-risk populations would lose the (partially false) assurance the apparatus provides, forcing either increased private investment in preparedness or explicit acceptance of higher disaster risk. The constraint organizes a significant share of the jurisdiction's disaster management infrastructure and budget; its disappearance would force institutional rearrangement.
% FOUNDING_PROBLEM: Disasters are collective-action problems: individual actors and agencies lack incentive to prepare for low-probability high-consequence events; coordination across jurisdictions and agencies is needed to share information, ensure compatible procedures, and build redundancy. The founding problem was that without a formal preparedness apparatus, the jurisdiction would face disorganized, uncoordinated response and preventable failures during disasters.
% FOUNDING_PROBLEM_CORROBORATION: The civil defense administration and political leadership attest that the founding problem remains live: disasters still occur, coordination is still needed, and the apparatus prevents worse outcomes. Post-disaster analyses and disaster researchers attest that the founding problem has been partially solved (basic infrastructure and communication protocols exist) and partially transformed: the real problem is now adaptation to novel scenarios and transmission of judgment-based knowledge, not basic coordination. At-risk populations who have experienced recent disasters attest that the apparatus provided insufficient guidance for actual conditions and expressed surprise that trained personnel were executing protocols rather than adapting to what was actually unfolding. No major external authority (academic community, international disaster management standards bodies) explicitly corroborates the civil defense administration's claim that the current apparatus is adequate; international standards emphasize continuous adaptive learning, not protocol rigidity.
narrative_ontology:disappearance_verdict(preparedness_transmission__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__husk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_transmission__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__husk_reading, 0.61, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_transmission__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness of 0.61 reflects the constraint's structure: the continued performance of drills and inspections extracts benefit for the civil defense administration (justifies budget allocation, preserves institutional standing) and for political leadership (provides optics of preparedness), while the at_risk_populations bear the cost of false assurance—they believe the apparatus is operational when adaptive knowledge has atrophied. Suppression is moderate (0.48) because the decay is not actively hidden in some jurisdictions; some inspection reports note the gap between protocol and actual readiness. However, suppression rises modestly over time (0.35 to 0.48) as the gap widens and pressure to maintain the façade of competence increases. Theater_ratio is high and rising (0.42 to 0.72): an increasing fraction of effort is devoted to maintaining the ritual form—scheduling exercises to be reported complete, writing inspections that confirm adherence to established templates—rather than actually testing or developing adaptive capacity. The accessibility_collapse is low (0.39) because the operational knowledge gap is discoverable by those who work in the system; field staff can articulate what they don't know about novel scenarios. The measurement series tracks the lifecycle of the constraint: in early years (t=0), the apparatus retained some live knowledge; field personnel had been trained in adaptive principles and could improvise within protocol bounds. Over 25 years (t=25), the constraint becomes increasingly piton-like—the same drills are run because that is what the institution does, not because they reliably build or test adaptive capacity.
 *
 * PERSPECTIVAL GAP:
 *   The civil defense administration and political leadership occupy agenda-setter seats with institutional power and low exit pressure—they benefit from the constraint's persistence and can maintain it indefinitely through budget allocation and schedule enforcement. Field personnel have moderate power but face identity-lock (their professional identity is bound to the civil defense service) and biographical time horizons—they cannot simply leave, and their careers depend on the institutional machinery. At-risk populations have constrained to trapped exit (they cannot move away from flood risk) and identity-lock (residence and community ties bind them to location). The divergence arises because the constraint extracts benefit for the agenda-setter (organizational persistence, political legitimacy) while imposing cost on the payer (false assurance, preparation for wrong scenarios) and the beneficiary becomes payer-adjacent (the public believed it was coordinated but it became extractive as knowledge decayed).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality overrides are not warranted here. The structural derivation captures the constraint's true directionality: the civil defense administration benefits and controls the apparatus (near-beneficiary d), while the at-risk populations pay through false assurance and misdirected preparation (near-target d).
 *
 * MANDATROPHY ANALYSIS:
 *   The husk_reading explicitly addresses mandatrophy—the constraint's founding mandate (disaster preparedness) has partially atrophied, but the constraint persists through institutional inertia and the administrative machinery that maintains it. The founding problem (disaster risk is collective and requires coordinated preparation) is contested: the administration argues it remains live and that the continued apparatus proves readiness; field personnel and post-disaster analysis increasingly argue that the founding problem has been partially solved (some infrastructure and basic protocols exist) while the transmission of adaptive knowledge has failed. The constraint persists not because it solves the founding problem but because the administration benefits from its continued existence and the cost to fix it (would require acknowledging actual capacity gaps, restructuring training, and accepting political liability) exceeds the administration's immediate pain from the status quo. This is the piton signature: extracted benefit for the administrator, diffuse cost for the public, and no party hurt enough to overthrow the constraint because the victims (at-risk populations) lack organized power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_status,
    'Is preparedness transmission in this jurisdiction best understood as live exercised knowledge (competence_reading), as hollowed memorial ritual with decayed capacity (husk_reading), or as stratified competence with uneven decay (hybrid_reading)?',
    'Comparative stress-test: examine performance during actual flood events unplanned for — does the jurisdiction''s response capacity match pre-specified protocols, exceed them through adaptive knowledge, or fail on off-script scenarios while maintaining ritual form?',
    'If competence_reading is accurate, the constraint is genuine rope — coordination function is live and widely distributed. If husk_reading is accurate, the constraint is piton — organizational memory persists but operational knowledge has atrophied and ceremonies mask the decay. If hybrid_reading is accurate, the constraint is tangled_rope stratified by agent class — infrastructure specialists remain competent while civilian coordinators have lost adaptive capacity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_status, empirical, 'Which reading of the preparedness kernel captures this jurisdiction''s actual state: live competence, hollowed ritual, or stratified decay?').

omega_variable(
    inspection_validity_under_novelty,
    'Do the jurisdiction''s inspection routines successfully identify degradation of capacity for novel failure modes outside the pre-specified protocol set, or do they detect only conformity to historical templates?',
    'Post-event autopsy following a flood that violated the jurisdiction''s design assumptions: interviews with inspectors about what their routines detect vs. what was missed; analysis of inspection documentation to determine whether discovery of novel vulnerabilities is possible within the declared methodology.',
    'If inspections successfully flag novel vulnerabilities, they are live knowledge-generating processes and the constraint is less piton-like. If they detect only template conformity, the high theater_ratio and low adaptive capacity are structural, confirming husk_reading classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inspection_validity_under_novelty, empirical, 'Whether inspection routines have capacity to detect novel failure modes or only conformity to established templates.').

omega_variable(
    transmission_mechanism_retention,
    'Why does organizational memory persist (drills continue, infrastructure is maintained) while operational knowledge decays? Is the decay a function of aging workforce without generational transmission, institutional fragmentation between infrastructure and civilian coordination, or active suppression of information about capability gaps?',
    'Demographic analysis of the civil defense workforce (age, tenure, turnover); examination of training curricula and mentorship structures across three time periods (founding, mid-career, present); interviews with retiring vs. newly-recruited staff about what they were taught vs. what they learned by doing.',
    'If decay is demographic and unplanned, the constraint is inertial but remediable through deliberate knowledge transfer programs. If decay is institutional fragmentation (competence siloed in engineering, coordination knowledge lost), the hybrid_reading becomes operative. If decay is active suppression (higher-ups minimize reports of capability gaps to preserve political legitimacy), the piton becomes snare-adjacent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transmission_mechanism_retention, empirical, 'What mechanism sustains organizational memory while operational knowledge decays — demographic attrition, institutional compartmentalization, or suppression?').

omega_variable(
    ceremonial_vs_functional_cost,
    'What portion of the civil defense budget is devoted to maintaining the drill and inspection apparatus as ceremony (reports written, schedules kept, hierarchical sign-offs completed) vs. maintaining actual operational readiness (equipment maintenance, capability testing, adaptive scenario design)?',
    'Budget line-item analysis and time-study of staff activities across three fiscal years. Survey of field officers about fraction of their effort spent on ritual compliance vs. substantive preparation.',
    'High ceremonial fraction (>60%) would confirm the piton classification and justify the theater_ratio of 0.72. Lower fraction (30-40%) would suggest functional processes persist alongside ritual and argue for tangled_rope. Very low fraction (<20%) would argue the constraint is misclassified and is closer to snare — active performance of competence for political optics while investment in real readiness is suppressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremonial_vs_functional_cost, empirical, 'How much of the preparedness apparatus is devoted to ritual maintenance vs. functional capability building?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__husk_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_trans_husk_tr_t0, preparedness_transmission__husk_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(prep_trans_husk_tr_t5, preparedness_transmission__husk_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(prep_trans_husk_tr_t10, preparedness_transmission__husk_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(prep_trans_husk_tr_t15, preparedness_transmission__husk_reading, theater_ratio, 15, 0.62).
narrative_ontology:measurement(prep_trans_husk_tr_t20, preparedness_transmission__husk_reading, theater_ratio, 20, 0.68).
narrative_ontology:measurement(prep_trans_husk_tr_t25, preparedness_transmission__husk_reading, theater_ratio, 25, 0.72).

% Extraction over time
narrative_ontology:measurement(prep_trans_husk_be_t0, preparedness_transmission__husk_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(prep_trans_husk_be_t5, preparedness_transmission__husk_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(prep_trans_husk_be_t10, preparedness_transmission__husk_reading, base_extractiveness, 10, 0.49).
narrative_ontology:measurement(prep_trans_husk_be_t15, preparedness_transmission__husk_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(prep_trans_husk_be_t20, preparedness_transmission__husk_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(prep_trans_husk_be_t25, preparedness_transmission__husk_reading, base_extractiveness, 25, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(prep_trans_husk_su_t0, preparedness_transmission__husk_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(prep_trans_husk_su_t5, preparedness_transmission__husk_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(prep_trans_husk_su_t10, preparedness_transmission__husk_reading, suppression_requirement, 10, 0.41).
narrative_ontology:measurement(prep_trans_husk_su_t15, preparedness_transmission__husk_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(prep_trans_husk_su_t20, preparedness_transmission__husk_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(prep_trans_husk_su_t25, preparedness_transmission__husk_reading, suppression_requirement, 25, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the preparedness_transmission kernel. The kernel commitment is that preparedness is a transmissible capacity grounded in practiced knowledge. The competence_reading asserts that transmission is live and adaptive; the husk_reading asserts that organizational memory persists while operational knowledge decays; the hybrid_reading asserts that transmission is stratified by domain (infrastructure remains competent, civilian coordination has decayed). These are three structurally distinct constraints sharing the same kernel. The ε values diverge because the referent is the same (the standing preparedness apparatus) but the reading's assessment of that apparatus differs: competence_reading finds ε ≈ 0.2-0.3 (mostly coordination cost, minor extraction), husk_reading finds ε ≈ 0.6+ (substantial extraction via false assurance), hybrid_reading finds ε ≈ 0.4-0.5 (stratified). The network links represent the genealogical relationship: all three readings belong to the same contested kernel, and each reading's type classification differs based on the specific reading's epistemic commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
