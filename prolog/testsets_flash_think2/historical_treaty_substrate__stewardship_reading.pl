% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__stewardship_reading, []).

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
 *   constraint_id: historical_treaty_substrate__stewardship_reading
 *   human_readable: Historical Treaties as Shared Territorial Stewardship Pacts
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint instantiates the 'stewardship reading' of historical
 *   treaties, interpreting them as relational pacts for shared territorial
 *   stewardship rather than instruments of land cession. It emphasizes no
 *   cession of Indigenous sovereignty, mutual obligations for coexistence,
 *   and joint management of territorial resources. This reading is a direct
 *   counter-narrative to more extractive interpretations and is gaining
 *   traction in contemporary legal and political discourse.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, 0.15).
domain_priors:suppression_score(historical_treaty_substrate__stewardship_reading, 0.2).
domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__stewardship_reading, rope).
narrative_ontology:human_readable(historical_treaty_substrate__stewardship_reading, "Historical Treaties as Shared Territorial Stewardship Pacts").
narrative_ontology:topic_domain(historical_treaty_substrate__stewardship_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__stewardship_reading, '9372332f-1d5a-4afb-9c5e-13f54797254a').
narrative_ontology:cs_kernel_codification('9372332f-1d5a-4afb-9c5e-13f54797254a', fixed_text).
narrative_ontology:cs_authority_grounding('9372332f-1d5a-4afb-9c5e-13f54797254a', lineage).
narrative_ontology:cs_interpretation_layer_present('9372332f-1d5a-4afb-9c5e-13f54797254a').
narrative_ontology:cs_reading_relation('9372332f-1d5a-4afb-9c5e-13f54797254a', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('9372332f-1d5a-4afb-9c5e-13f54797254a', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('9372332f-1d5a-4afb-9c5e-13f54797254a', foundational, indigenous_inherent_sovereignty_uncoded).
narrative_ontology:cs_axiom_status(indigenous_inherent_sovereignty_uncoded, holdable).
narrative_ontology:cs_axiom_grounding('9372332f-1d5a-4afb-9c5e-13f54797254a', indigenous_inherent_sovereignty_uncoded, deontological).
narrative_ontology:cs_axiom('9372332f-1d5a-4afb-9c5e-13f54797254a', foundational, mutual_obligation_for_coexistence).
narrative_ontology:cs_axiom_status(mutual_obligation_for_coexistence, holdable).
narrative_ontology:cs_axiom_grounding('9372332f-1d5a-4afb-9c5e-13f54797254a', mutual_obligation_for_coexistence, conventional).
narrative_ontology:cs_reference_frame('9372332f-1d5a-4afb-9c5e-13f54797254a', relational_pact_framework).
narrative_ontology:cs_drift_state('9372332f-1d5a-4afb-9c5e-13f54797254a', contemporary_reconciliation_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9372332f-1d5a-4afb-9c5e-13f54797254a', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_state).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, environmental_advocates).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, resource_extraction_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain inherent jurisdiction over their traditional territories, participate in shared governance, and benefit from the long-term health and sustainability of the land and resources. Their identity is deeply tied to the land and treaty relationships.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, indigenous_nations, beneficiary,
    organized, generational, identity_locked, regional).

% Is obligated to seek consent, engage in shared governance, and uphold mutual obligations for coexistence. Benefits from stable, respectful relationships with Indigenous nations and enhanced environmental stewardship. Exit from these obligations is legally and politically constrained.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, settler_state, beneficiary).

% Bear the costs of engaging in consent processes, adhering to shared management plans, and potentially foregoing unilateral access to resources. Their operations are constrained by the requirements of shared stewardship, which they often resist.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, resource_extraction_industries, payer,
    powerful, biographical, mobile, national).

% Benefit from the enhanced environmental protection and sustainable resource management that results from shared territorial stewardship. They actively support and promote this reading of treaties.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, environmental_advocates, beneficiary,
    organized, generational, mobile, global).

% Analyze, interpret, and advocate for the recognition and implementation of treaties as relational pacts for shared stewardship. They provide critical intellectual support for this reading.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, legal_scholars_indigenous_law, observer,
    analytical, generational, analytical, universal).

% Advocate for treaties as completed property transactions that ceded Indigenous sovereignty. This reading structurally excludes their unilateral claims to territorial control and resource extraction, as it posits ongoing Indigenous jurisdiction and shared decision-making.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, proponents_of_extinguishment, excluded,
    powerful, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__stewardship_reading, diffuse).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate shared governance and sustainable resource management for territories historically subject to treaties, ensuring coexistence, mutual respect, and long-term ecological health.
% TRANSFER_FUNCTION: Transfers decision-making authority from unilateral settler state control to shared governance with Indigenous nations; reallocates resource benefits to jointly managed systems that prioritize sustainability and Indigenous well-being.
% ABSENT_VOICES: Proponents of extinguishment or unilateral settler state sovereignty are structurally excluded from the premise of shared stewardship; they would argue against any ongoing Indigenous jurisdiction or shared decision-making.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal and political landscape would revert to a more extractive, conflict-prone model, undermining reconciliation efforts, shared environmental goals, and the inherent rights of Indigenous nations. Resource management would become unilateral, and legal disputes would intensify.
% FOUNDING_PROBLEM: Historical treaties were often interpreted unilaterally by settler states, leading to dispossession, environmental degradation, and ongoing conflict, failing to establish genuine coexistence and mutual respect.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous legal traditions, international human rights bodies, and a growing body of comparative constitutional scholarship corroborate the persistence of unilateral interpretations and the need for a relational, stewardship-based approach. This is attested by Indigenous elders, legal experts, and UN declarations.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__stewardship_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(historical_treaty_substrate__stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__stewardship_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__stewardship_reading_tests).
:- end_tests(historical_treaty_substrate__stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.15) and suppression (0.2) are low because this reading, by its nature, aims to minimize extraction and coercion, focusing on mutual benefit and consent. The historical measurements show a slight decrease in extractiveness and suppression as this reading gains prominence, pushing back against unilateral settler state actions. However, the theater ratio (0.4) is relatively high because, while the ideal of shared stewardship is articulated, its full implementation often remains aspirational, with performative gestures sometimes preceding substantive change. Resistance (0.7) is high because this reading challenges deeply entrenched power structures and extractive practices.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Indigenous nations and environmental advocates, this reading represents a path towards justice and sustainability. From the perspective of resource extraction industries and proponents of extinguishment, it represents an imposition of new costs and a challenge to established property rights. The engine will compute these divergent classifications based on the structural roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are primary beneficiaries, retaining jurisdiction and participating in governance. The settler state is also a beneficiary, gaining stability and legitimacy through respectful relations, but also acts as an agenda-setter in the broader legal framework. Environmental advocates benefit from enhanced stewardship. Resource extraction industries are payers, as they must now operate under shared governance and consent, incurring costs they previously avoided. Proponents of extinguishment are excluded, as their claims are incompatible with this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementability_vs_aspiration,
    'Is this stewardship reading genuinely implementable within existing settler state legal and political structures, or does it remain largely an aspirational ideal?',
    'Empirical analysis of legal precedents, policy changes, and on-the-ground governance outcomes in jurisdictions where this reading is formally adopted or actively pursued. Track the gap between declared policy and actual practice.',
    'If largely aspirational, the effective extractiveness and suppression of the underlying historical treaty substrate remain higher than this reading suggests, as the ''stewardship'' framing acts as a form of theater. If genuinely implementable, it reduces effective extraction and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementability_vs_aspiration, empirical, 'The gap between the ideal of shared stewardship and its practical realization.').

omega_variable(
    power_imbalance_resolution,
    'Does this stewardship reading adequately address the historical and ongoing power imbalance between Indigenous nations and the settler state, or does it risk perpetuating it under a new guise?',
    'Analysis of decision-making authority in shared governance structures: do Indigenous nations hold genuine veto power and equal agency, or is their participation advisory and subject to settler state override?',
    'If power imbalances persist, the ''mutual obligations'' may function as a Tangled Rope, coordinating Indigenous participation while still extracting concessions. If genuine power-sharing is achieved, it functions as a true Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_imbalance_resolution, conceptual, 'Whether shared stewardship genuinely rebalances power or merely reframes existing imbalances.').

omega_variable(
    adjudication_of_competing_readings,
    'How does the legal system adjudicate between this stewardship reading and competing interpretations (e.g., extinguishment or nation-to-nation readings)?',
    'Analysis of judicial decisions, legislative reforms, and international legal developments that explicitly affirm or reject specific readings of treaty obligations. Examine which interpretive frameworks gain legal precedence.',
    'If the extinguishment reading gains precedence, this stewardship reading is foreclosed, and the constraint''s extractiveness increases. If the nation-to-nation reading gains precedence, it may reinforce or subtly alter the dynamics of shared stewardship.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adjudication_of_competing_readings, empirical, 'The legal and political contestation between different treaty interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__stewardship_reading, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1800, historical_treaty_substrate__stewardship_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(hist_tr_t1850, historical_treaty_substrate__stewardship_reading, theater_ratio, 1850, 0.2).
narrative_ontology:measurement(hist_tr_t1900, historical_treaty_substrate__stewardship_reading, theater_ratio, 1900, 0.3).
narrative_ontology:measurement(hist_tr_t1950, historical_treaty_substrate__stewardship_reading, theater_ratio, 1950, 0.35).
narrative_ontology:measurement(hist_tr_t2000, historical_treaty_substrate__stewardship_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(hist_tr_t2024, historical_treaty_substrate__stewardship_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(hist_be_t1800, historical_treaty_substrate__stewardship_reading, base_extractiveness, 1800, 0.25).
narrative_ontology:measurement(hist_be_t1850, historical_treaty_substrate__stewardship_reading, base_extractiveness, 1850, 0.2).
narrative_ontology:measurement(hist_be_t1900, historical_treaty_substrate__stewardship_reading, base_extractiveness, 1900, 0.18).
narrative_ontology:measurement(hist_be_t1950, historical_treaty_substrate__stewardship_reading, base_extractiveness, 1950, 0.16).
narrative_ontology:measurement(hist_be_t2000, historical_treaty_substrate__stewardship_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(hist_be_t2024, historical_treaty_substrate__stewardship_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1800, historical_treaty_substrate__stewardship_reading, suppression_requirement, 1800, 0.3).
narrative_ontology:measurement(hist_su_t1850, historical_treaty_substrate__stewardship_reading, suppression_requirement, 1850, 0.25).
narrative_ontology:measurement(hist_su_t1900, historical_treaty_substrate__stewardship_reading, suppression_requirement, 1900, 0.22).
narrative_ontology:measurement(hist_su_t1950, historical_treaty_substrate__stewardship_reading, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement(hist_su_t2000, historical_treaty_substrate__stewardship_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(hist_su_t2024, historical_treaty_substrate__stewardship_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__stewardship_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, resource_management_regulations).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, indigenous_rights_litigation).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, nation_to_nation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'historical_treaty_substrate' kernel. Each reading presents a distinct structural interpretation of historical treaties, leading to different classifications and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
