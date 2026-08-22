% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__constitutional_hybrid_reading, []).

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
 *   constraint_id: sovereign_legitimacy__constitutional_hybrid_reading
 *   human_readable: Constitutional Hybrid Reading of Sovereign Legitimacy
 *   domain: political_philosophy/constitutional_theory/legitimacy_studies
 *
 * SUMMARY:
 *   This constraint describes the 'constitutional hybrid' reading of
 *   sovereign legitimacy, where authority is dual-sourced: inherited
 *   ceremonial/symbolic authority (e.g., a monarch) and delegated political
 *   authority (e.g., elected parliament). Constitutional law mediates the
 *   boundary between these two sources. This reading is a compromise, aiming
 *   for stability by incorporating elements of both monarchical and
 *   republican traditions. It is presented as a Tangled Rope because it
 *   genuinely coordinates a complex political arrangement but also extracts
 *   costs from those who prefer a 'purer' form of either monarchical or
 *   republican rule, and requires active enforcement (constitutional
 *   interpretation) to maintain its boundaries.
 *
 * KEY AGENTS:
 *   - hereditary_monarch: Primary beneficiary (institutional/identity_locked) — retains status and income.
 *   - elected_officials: Primary beneficiary (powerful/mobile) — wield delegated political power.
 *   - constitutional_judiciary: Agenda setter (institutional/constrained) — interprets and enforces the boundary.
 *   - the_citizenry: Payer (organized/constrained) — bears costs of maintaining dual institutions.
 *   - absolutist_monarchists: Payer (powerless/identity_locked) — constrained by limits on monarchical power.
 *   - pure_republicans: Payer (powerless/identity_locked) — constrained by perpetuation of inherited roles.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.35).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.45).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Constitutional Hybrid Reading of Sovereign Legitimacy").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political_philosophy/constitutional_theory/legitimacy_studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, 'f14c0883-390b-4414-9684-410c52502d16').
narrative_ontology:cs_kernel_codification('f14c0883-390b-4414-9684-410c52502d16', formalized).
narrative_ontology:cs_authority_grounding('f14c0883-390b-4414-9684-410c52502d16', lineage).
narrative_ontology:cs_interpretation_layer_present('f14c0883-390b-4414-9684-410c52502d16').
narrative_ontology:cs_reading_relation('f14c0883-390b-4414-9684-410c52502d16', sovereign_legitimacy__monarchical_reading, coexists_with).
narrative_ontology:cs_reading_relation('f14c0883-390b-4414-9684-410c52502d16', sovereign_legitimacy__republican_reading, coexists_with).
narrative_ontology:cs_axiom('f14c0883-390b-4414-9684-410c52502d16', foundational, dual_source_legitimacy).
narrative_ontology:cs_axiom_status(dual_source_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f14c0883-390b-4414-9684-410c52502d16', dual_source_legitimacy, conventional).
narrative_ontology:cs_axiom('f14c0883-390b-4414-9684-410c52502d16', foundational, constitutional_supremacy_over_tradition_and_will).
narrative_ontology:cs_axiom_status(constitutional_supremacy_over_tradition_and_will, holdable).
narrative_ontology:cs_axiom_grounding('f14c0883-390b-4414-9684-410c52502d16', constitutional_supremacy_over_tradition_and_will, conventional).
narrative_ontology:cs_reference_frame('f14c0883-390b-4414-9684-410c52502d16', constitutional_balance_of_powers).
narrative_ontology:cs_drift_state('f14c0883-390b-4414-9684-410c52502d16', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f14c0883-390b-4414-9684-410c52502d16', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_monarchists).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, pure_republicans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, the_citizenry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ceremonial and symbolic authority, often with associated income and status, without direct political power. Their legitimacy is inherited, but their function is defined and limited by constitutional law. Exit means abdication and loss of status.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, beneficiary,
    institutional, generational, identity_locked, national).

% Exercise delegated political authority, enacting policy and governing. Their legitimacy derives from popular consent, but their authority is constrained by the constitutional framework that also defines the monarch's role. Exit is through electoral defeat or term limits.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, beneficiary,
    powerful, biographical, mobile, national).

% Interprets and enforces the constitutional law that mediates the boundary between inherited and delegated authority. Their decisions define the practical limits of both the monarch's and elected officials' powers. Exit is through retirement or impeachment.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Provides the ultimate source of delegated authority and bears the costs of maintaining both the inherited and elected institutions. They benefit from the stability of the hybrid system but pay for its complexity and potential for boundary disputes. Exit is through emigration or revolution.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, the_citizenry, payer,
    organized, generational, constrained, national).

% Seek a return to pure monarchical rule where authority is solely inherited and unlimited. They are constrained by the constitutional framework that limits the monarch's power and legitimizes elected officials. Their 'cost' is the suppression of their preferred political order.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_monarchists, payer,
    powerless, generational, identity_locked, national).

% Advocate for a purely republican system where all authority is popularly delegated and inherited roles are abolished. They are constrained by the constitutional framework that preserves the monarch's symbolic role. Their 'cost' is the perpetuation of an institution they view as illegitimate.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, pure_republicans, payer,
    powerless, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the peaceful coexistence and functional separation of inherited symbolic authority and delegated political power within a single state, preventing conflicts over ultimate legitimacy.
% TRANSFER_FUNCTION: Transfers symbolic legitimacy and historical continuity from the monarch to the state, while transferring policy-making power and accountability from the people to elected officials. It also transfers the costs of maintaining both institutions to the citizenry.
% ABSENT_VOICES: Radical separatists or those advocating for entirely different forms of governance (e.g., anarchists, theocrats) are largely excluded from the constitutional discourse, as their proposals fundamentally challenge the dual-sourced legitimacy itself.
% DISAPPEARANCE_RATIONALE: If this constitutional hybrid vanished, the state would face an immediate legitimacy crisis. The roles of monarch and elected officials would become contested, likely leading to a power vacuum, civil unrest, or a rapid, potentially violent, transition to a purely monarchical or republican system.
% FOUNDING_PROBLEM: To resolve historical conflicts between monarchical claims of divine right and popular demands for self-governance, establishing a stable political order that incorporated elements of both traditions.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists widely corroborate the historical problem of reconciling inherited and delegated authority. Contemporary constitutional scholars and public discourse continue to debate the optimal balance and interpretation of this hybrid, indicating the problem remains live, albeit in a refined form.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__constitutional_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__constitutional_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sovereign_legitimacy__constitutional_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).
:- end_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate: while it reduces the potential for absolute extraction by either pure form, it imposes costs of complexity and compromise. Suppression (0.45) is also moderate, as it actively suppresses radical alternatives from both monarchist and republican extremes. Theater ratio (0.20) is low, as both the symbolic and political functions are genuinely active, though the symbolic role can sometimes appear performative. The historical measurements show a slight decrease in extractiveness and suppression over time, reflecting the gradual institutionalization and acceptance of the hybrid model, and a slight increase in theater as the symbolic role becomes more pronounced relative to direct political power.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the hereditary monarch and elected officials, this is a beneficial arrangement that grants them legitimate roles. From the perspective of absolutist monarchists and pure republicans, it is an extractive compromise that denies their preferred, 'pure' form of sovereignty. The constitutional judiciary, as the agenda-setter, experiences it as a complex but necessary coordination mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary monarch and elected officials are beneficiaries, as the constraint grants them their respective spheres of legitimate authority. The citizenry is a payer, bearing the costs of maintaining this dual system. Absolutist monarchists and pure republicans are also payers, as the constraint actively suppresses their preferred political orders. The constitutional judiciary is the agenda-setter, actively maintaining the constraint's boundaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the hybrid system as a pure Rope (ignoring the costs to those seeking purer forms) or a pure Snare (ignoring its genuine coordination function in resolving historical conflicts). The 'live' status of the founding problem, corroborated by ongoing scholarly debate, indicates that while the specific form of the problem has evolved, the underlying tension between inherited and delegated authority persists, preventing mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_dispute_resolution,
    'How effectively does constitutional law mediate boundary disputes between inherited and delegated authority, and what are the costs of its failures?',
    'Analysis of historical constitutional crises, judicial review outcomes, and public trust in the judiciary during periods of tension between the monarch and elected government.',
    'If mediation is consistently ineffective or costly, the constraint''s effective extractiveness and suppression would be higher, potentially reclassifying it closer to a Snare due to the instability and resource drain of unresolved conflicts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_dispute_resolution, empirical, 'Effectiveness of constitutional mediation in hybrid legitimacy systems.').

omega_variable(
    symbolic_authority_utility,
    'What is the actual utility or coordination benefit derived from maintaining inherited ceremonial/symbolic authority, beyond historical continuity?',
    'Sociological studies on national identity, public opinion surveys on the role of the monarchy, and comparative analysis with purely republican states regarding social cohesion or political stability.',
    'If the utility is negligible or negative, the ''beneficiary'' status of the monarch becomes more extractive, and the constraint''s theater_ratio would increase, pushing it towards a Piton or a more extractive Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_authority_utility, empirical, 'The functional value of inherited symbolic authority in a modern state.').

omega_variable(
    reading_framing_ambiguity,
    'Is this constraint a genuine constitutional hybrid, or is it a monarchical system with republican window-dressing (or vice-versa)?',
    'Comparative constitutional analysis across states with similar structures, focusing on the actual distribution of power during crises and the historical trajectory of constitutional amendments. The ''constitutional_hybrid_reading'' is the reading being instantiated here; the question is whether it is the most accurate framing.',
    'If it is primarily a monarchical system with republican elements, the ''monarchical_reading'' would be more accurate, implying higher extractiveness from the populace. If it is primarily republican with symbolic monarchy, the ''republican_reading'' would be more accurate, implying higher extractiveness from monarchical traditionalists. This would shift the primary beneficiaries and victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_ambiguity, conceptual, 'Ambiguity in the primary framing of the hybrid legitimacy system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 1700, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t1700, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(sove_tr_t1800, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(sove_tr_t1900, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(sove_tr_t2000, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(sove_tr_t2024, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(sove_be_t1700, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 1700, 0.45).
narrative_ontology:measurement(sove_be_t1800, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 1800, 0.4).
narrative_ontology:measurement(sove_be_t1900, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement(sove_be_t2000, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 2000, 0.36).
narrative_ontology:measurement(sove_be_t2024, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t1700, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 1700, 0.6).
narrative_ontology:measurement(sove_su_t1800, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 1800, 0.55).
narrative_ontology:measurement(sove_su_t1900, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(sove_su_t2000, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(sove_su_t2024, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy__republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'sovereign_legitimacy' kernel. This 'constitutional_hybrid_reading' mediates between the 'monarchical_reading' and 'republican_reading', incorporating elements of both while constraining their pure forms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
