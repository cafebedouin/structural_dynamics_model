% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__hybrid_reading, []).

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
 *   constraint_id: preparedness_commitment__hybrid_reading
 *   human_readable: Preparedness as Layered System (Memorial + Competence)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint models preparedness as a dual-layer system where memorial
 *   elements (ceremonies, monuments, declared policies, anniversary
 *   observances) stabilize long-term institutional commitment, while
 *   competence elements (exercised drills, maintained equipment, trained
 *   personnel, tested logistics) maintain actual operational function. The
 *   tension between layers creates a persistent maintenance cost: the
 *   memorial layer is politically cheap but functionally thin; the competence
 *   layer is functionally essential but politically expensive and invisible
 *   until failure. This reading (hybrid_reading) asserts both layers are
 *   structurally necessary and their tension is the system's central dynamic
 *   — not a bug but the mechanism by which preparedness persists across
 *   generational forgetting.
 *
 * KEY AGENTS:
 *   - political_officials: Primary agenda-setters and memorial-layer beneficiaries (institutional/arbitrage) — set mandates, claim credit, avoid competence costs
 *   - emergency_management_agencies: Dual-positioned agenda-setters and payers (institutional/constrained) — administer both layers, bear institutional strain
 *   - frontline_responders: Primary payers (organized/constrained) — operational competence falls on them, exit blocked by professional identity
 *   - affected_populations: Payers and excluded (powerless/trapped) — bear failure consequences, no voice in design
 *   - taxpayers: Payers (moderate/mobile) — fund both layers, limited visibility, costly exit
 *   - independent_auditors: Observers (analytical/analytical) — see the gap, no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, 0.65).
domain_priors:suppression_score(preparedness_commitment__hybrid_reading, 0.55).
domain_priors:theater_ratio(preparedness_commitment__hybrid_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_commitment__hybrid_reading, "Preparedness as Layered System (Memorial + Competence)").
narrative_ontology:topic_domain(preparedness_commitment__hybrid_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_commitment__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__hybrid_reading, '9ffe6507-4a1b-4415-a25d-a734c4aa0187').
narrative_ontology:cs_kernel_codification('9ffe6507-4a1b-4415-a25d-a734c4aa0187', formalized).
narrative_ontology:cs_authority_grounding('9ffe6507-4a1b-4415-a25d-a734c4aa0187', lineage).
narrative_ontology:cs_interpretation_layer_present('9ffe6507-4a1b-4415-a25d-a734c4aa0187').
narrative_ontology:cs_reading_relation('9ffe6507-4a1b-4415-a25d-a734c4aa0187', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ffe6507-4a1b-4415-a25d-a734c4aa0187', preparedness_commitment__husk_reading, influences).
narrative_ontology:cs_axiom('9ffe6507-4a1b-4415-a25d-a734c4aa0187', foundational, preparedness_requires_dual_layer).
narrative_ontology:cs_axiom_status(preparedness_requires_dual_layer, holdable).
narrative_ontology:cs_axiom_grounding('9ffe6507-4a1b-4415-a25d-a734c4aa0187', preparedness_requires_dual_layer, instrumental).
narrative_ontology:cs_axiom('9ffe6507-4a1b-4415-a25d-a734c4aa0187', secondary, memorial_layer_stabilizes_commitment).
narrative_ontology:cs_axiom_status(memorial_layer_stabilizes_commitment, holdable).
narrative_ontology:cs_axiom_grounding('9ffe6507-4a1b-4415-a25d-a734c4aa0187', memorial_layer_stabilizes_commitment, empirically_contingent).
narrative_ontology:cs_reference_frame('9ffe6507-4a1b-4415-a25d-a734c4aa0187', post_catastrophe_vow_institutionalized).
narrative_ontology:cs_drift_state('9ffe6507-4a1b-4415-a25d-a734c4aa0187', contemporary_all_hazards_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9ffe6507-4a1b-4415-a25d-a734c4aa0187', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__hybrid_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, political_officials).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, bureaucratic_institutions).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, affected_populations).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, emergency_management_agencies).
narrative_ontology:constraint_vindicates(preparedness_commitment__hybrid_reading, preparedness_requires_sustained_commitment).
narrative_ontology:constraint_vindicates(preparedness_commitment__hybrid_reading, operational_competence_must_be_exercised).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commission preparedness plans, fund memorial ceremonies, and claim credit for readiness. They benefit from the visible memorial layer (ceremonies, monuments, declared policies) which signals competence without requiring sustained investment in the competence layer (drills, equipment, personnel retention). Their electoral horizon makes the memorial layer politically optimal.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, political_officials, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, political_officials, beneficiary).

% Administer the preparedness system: write plans, run drills, maintain caches. They are caught between the memorial layer (which demands visible compliance) and the competence layer (which demands real capability). They pay in institutional strain — stretched budgets, staff burnout, mandate creep — while being the primary enforcers of both layers.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, emergency_management_agencies, payer).

% Bear the operational consequences when the competence layer is hollow. They train on paper plans that lack equipment, staff positions that exist only on org charts, and interagency protocols never exercised. Their exit is constrained by professional identity and public duty; they cannot easily leave the system that fails them.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, frontline_responders, payer,
    organized, biographical, constrained, regional).

% Experience the failure of preparedness directly when disaster strikes. They are excluded from the planning process (no seat at the table for memorial or competence decisions) and have no exit — geographic, economic, and social ties bind them to the jurisdiction. The memorial layer offers them symbolic reassurance; the competence layer determines survival.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, affected_populations, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, affected_populations, excluded).

% Fund the entire system through taxes. They pay for both layers but have limited visibility into which layer their money sustains. The memorial layer is highly visible (ceremonies, reports); the competence layer is invisible until failure. Their exit is mobile (can relocate) but costly, so most remain captive to the jurisdiction's choices.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, taxpayers, payer,
    moderate, biographical, mobile, national).

% Produce after-action reports, gap analyses, and congressional testimony. They see the structural gap between memorial claims and competence reality but have no enforcement power. Their exit is analytical — they can leave the conversation but the constraint persists regardless of their assessment.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, independent_auditors, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains societal readiness for low-probability, high-consequence events across generational timescales by stabilizing institutional commitment (memorial layer) and preserving operational knowledge (competence layer).
% TRANSFER_FUNCTION: Moves funding, personnel time, and organizational attention from taxpayers and frontline responders toward political visibility (memorial layer) and institutional maintenance (bureaucratic layer), while the competence layer extracts real operational capacity from responders to sustain exercise cycles.
% ABSENT_VOICES: Communities that have experienced disaster and learned informal preparedness practices are excluded from the formal system; their knowledge is treated as anecdote rather than data. Future generations who will inherit the competence debt have no voice in current allocation between memorial and competence investment.
% DISAPPEARANCE_RATIONALE: If the layered preparedness system vanished, political officials would lose a key legitimacy signal, agencies would lose their mandate structure, responders would improvise ad-hoc mutual aid (as they often do anyway), and populations would rely on community resilience. The world would rearrange toward decentralized, experience-based preparedness — likely with higher variance but less systemic fragility.
% FOUNDING_PROBLEM: After catastrophic events (e.g., major earthquakes, pandemics, wars), societies vow 'never again' but lack mechanisms to sustain readiness across the long intervals between events. The memorial layer was created to encode the vow into institutional DNA; the competence layer was created to keep the vow actionable.
% FOUNDING_PROBLEM_CORROBORATION: Historical analysis of civil defense programs (e.g., US Cold War civil defense, Japanese earthquake preparedness) shows the founding problem was real — societies do forget. But disaster sociologists (e.g., Tierney, Klinenberg) and after-action reports from Katrina, Fukushima, and COVID-19 attest that the memorial layer often displaces the competence layer, turning the solution into a new problem. No single beneficiary group corroborates the current arrangement as solving the founding problem.
narrative_ontology:disappearance_verdict(preparedness_commitment__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_commitment__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__hybrid_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_commitment__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is moderately high because the memorial layer extracts political capital and budget share without delivering proportional competence. Suppression (0.55) is moderate because alternative preparedness models (community-based, decentralized, insurance-driven) are marginalized by the formal system's monopoly on legitimacy. Theater ratio (0.70) is high because the memorial layer is predominantly performative — its function is signaling commitment, not operational readiness. Accessibility collapse (0.50) is moderate: community resilience exists but is structurally subordinated. Resistance (0.45) is moderate: responders and auditors push back but lack structural leverage. The measurement series shows rising extraction and theater over 40 years, with suppression creeping up as the system hardens.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (political officials, agencies) experience the constraint as coordination infrastructure they built and maintain — a rope/scaffold hybrid. The payer seats (responders, populations) experience it as extraction machinery that performs readiness while hollowing out capacity — a snare/tangled rope. The engine computes this divergence from the structural data; the hybrid_reading claim acknowledges both layers exist but does not adjudicate which seat's experience is 'truer.'
 *
 * DIRECTIONALITY LOGIC:
 *   Political officials sit near the beneficiary end (d ~ 0.15): they collect legitimacy rents from the memorial layer with near-arbitrage exit (can pivot to other issues). Emergency agencies sit near symmetric (d ~ 0.50): they both set the agenda and pay the institutional cost of maintaining the dual layer. Frontline responders sit near the target end (d ~ 0.85): they bear the competence deficit with constrained exit (professional identity lock). Affected populations are fully trapped targets (d ~ 1.0): no exit, no voice, full consequence. Taxpayers are mobile payers (d ~ 0.65): they pay but can relocate. Auditors are analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sustaining readiness across generational forgetting) remains live but contested. The memorial layer has partially displaced the competence layer — the mandate to 'remember' has outcompeted the mandate to 'be ready.' This is not pure mandatrophy (the competence layer still exists and is exercised episodically) but a structural drift where the memorial layer's lower cost and higher visibility create a persistent selection pressure toward performance over function. The constraint is not resolved; it is in active drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'Does the hybrid_reading accurately capture the structural relationship between memorial and competence layers, or does it reify a distinction that serves institutional self-justification?',
    'Comparative analysis of preparedness systems that have lost one layer (e.g., post-Soviet civil defense lost competence; post-9/11 US homeland security expanded memorial). If systems with only one layer fail predictably, the dual-layer claim is validated.',
    'If the dual-layer claim is validated, the constraint is a genuine tangled_rope with irreducible maintenance cost. If the distinction is institutional self-justification, the constraint is a snare where the memorial layer is pure extraction cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Whether the memorial/competence layer distinction is structurally real or a retrospective rationalization.').

omega_variable(
    memorial_layer_net_function,
    'Is the memorial layer net coordination (stabilizes commitment that would otherwise decay) or net extraction (consumes resources that could fund competence while providing political cover)?',
    'Counterfactual modeling: simulate preparedness decay curves with and without memorial investment, controlling for competence funding. Historical cases where memorial layer was removed (e.g., end of Cold War civil defense ceremonies) show whether competence persisted or collapsed.',
    'If net coordination, the hybrid reading''s claimed_type (tangled_rope) holds — genuine coordination + asymmetric extraction. If net extraction, the constraint is a snare where the memorial layer is the extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(memorial_layer_net_function, empirical, 'Whether the memorial layer''s coordination function justifies its resource consumption.').

omega_variable(
    tension_necessity,
    'Is the tension between memorial and competence layers structurally necessary (the system cannot function without both), or is it artificially maintained by beneficiaries who profit from the ambiguity?',
    'Examine preparedness systems that achieved high competence with minimal memorial layer (e.g., Swiss civil protection, Japanese neighborhood associations). If they sustain readiness without memorial theater, the tension is not necessary — it is imposed.',
    'If tension is unnecessary, the constraint is a snare with manufactured complexity. If necessary, the tangled_rope classification is structurally grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tension_necessity, empirical, 'Whether the memorial-competence tension is a functional requirement or an extracted rent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__hybrid_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__hybrid_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__hybrid_reading, theater_ratio, 20, 0.62).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__hybrid_reading, theater_ratio, 30, 0.67).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__hybrid_reading, theater_ratio, 40, 0.7).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__hybrid_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__hybrid_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__hybrid_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__hybrid_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__hybrid_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__hybrid_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__hybrid_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(prep_su_t20, preparedness_commitment__hybrid_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(prep_su_t30, preparedness_commitment__hybrid_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__hybrid_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__hybrid_reading, 0.1).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, disaster_response_coordination).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, infrastructure_resilience_mandates).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the preparedness_commitment kernel. The competence_reading (exercised knowledge only) and husk_reading (memorial performance only) are sibling constraints. The hybrid_reading asserts both layers exist and their tension is the central dynamic. All three share the same referent (the standing preparedness arrangement) but instantiate different constraints with different ε values and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_commitment__hybrid_reading, institutional, 0.15).
constraint_indexing:directionality_override(preparedness_commitment__hybrid_reading, organized, 0.85).
constraint_indexing:directionality_override(preparedness_commitment__hybrid_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
