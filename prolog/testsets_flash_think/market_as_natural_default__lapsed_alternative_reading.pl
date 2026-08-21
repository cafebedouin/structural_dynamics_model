% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__lapsed_alternative_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: market_as_natural_default__lapsed_alternative_reading
 *   human_readable: Market as Natural Default: Lapsed Alternative Reading
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This constraint is the 'lapsed alternative' reading of the 'market as
 *   natural default' kernel. It posits that the perceived naturalness and
 *   dominance of current market structures result primarily from a historical
 *   forgetting of viable alternatives, rather than active, coercive
 *   suppression by beneficiaries. The constraint operates through
 *   institutional inertia and cognitive patterns that privilege the status
 *   quo, making it a Piton. Sibling readings include 'beneficiary maintained'
 *   (active defense by incumbents) and 'hybrid amnesia' (initial forgetting
 *   followed by capture).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.1).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.2).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, piton).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market as Natural Default: Lapsed Alternative Reading").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political_economy/ideology_studies/economic_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, '7ac9a866-0169-4633-b0f2-5a8e492b9d88').
narrative_ontology:cs_kernel_codification('7ac9a866-0169-4633-b0f2-5a8e492b9d88', implicit).
narrative_ontology:cs_authority_grounding('7ac9a866-0169-4633-b0f2-5a8e492b9d88', diffuse_epistemic).
narrative_ontology:cs_reading_relation('7ac9a866-0169-4633-b0f2-5a8e492b9d88', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ac9a866-0169-4633-b0f2-5a8e492b9d88', market_as_natural_default__hybrid_amnesia_reading, coexists_with).
narrative_ontology:cs_axiom('7ac9a866-0169-4633-b0f2-5a8e492b9d88', foundational, historical_contingency_of_markets).
narrative_ontology:cs_axiom_status(historical_contingency_of_markets, holdable).
narrative_ontology:cs_axiom_grounding('7ac9a866-0169-4633-b0f2-5a8e492b9d88', historical_contingency_of_markets, empirically_contingent).
narrative_ontology:cs_axiom('7ac9a866-0169-4633-b0f2-5a8e492b9d88', foundational, forgetting_as_structural_mechanism).
narrative_ontology:cs_axiom_status(forgetting_as_structural_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('7ac9a866-0169-4633-b0f2-5a8e492b9d88', forgetting_as_structural_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('7ac9a866-0169-4633-b0f2-5a8e492b9d88', market_as_historical_construct).
narrative_ontology:cs_drift_state('7ac9a866-0169-4633-b0f2-5a8e492b9d88', contemporary_discourse, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7ac9a866-0169-4633-b0f2-5a8e492b9d88', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, general_public).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, foregone_alternatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions (e.g., central banks, major corporations, influential think tanks) implicitly perpetuate the narrative of market inevitability by operating within its framework and rarely acknowledging or exploring historical alternatives. They do not actively suppress alternatives but benefit from their absence through inertia.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, dominant_economic_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Bears the diffuse costs of foregone social and economic innovation that might have arisen from alternative arrangements. Their choices are limited by the perceived 'naturalness' of the dominant market structure, making exit from its logic difficult.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, general_public, payer,
    powerless, generational, constrained, global).

% Actively investigate and document historical economic alternatives, seeking to recover the memory of different possible arrangements. They are outside the direct operation of the constraint but analyze its effects.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, historical_researchers, observer,
    analytical, biographical, analytical, global).

% Represents the range of economic and social arrangements that were historically possible but have been forgotten or marginalized. They have no voice in contemporary discourse, their potential benefits unrealized.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, foregone_alternatives, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(market_as_natural_default__lapsed_alternative_reading, foregone_alternatives).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_as_natural_default__lapsed_alternative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simplifies economic discourse and policy by presenting a single, dominant market structure as the natural or inevitable default, thereby reducing the cognitive load of considering alternatives.
% TRANSFER_FUNCTION: Transfers the potential for diverse social and economic outcomes (innovation, welfare gains, different distributions of power) from the general public and foregone alternatives to the perpetuation of the status quo.
% ABSENT_VOICES: Advocates for historical alternatives, whose arguments and practical examples have been forgotten or marginalized from mainstream economic and political discourse. Their absence is a consequence of the lapsed memory, not active suppression.
% DISAPPEARANCE_RATIONALE: If the historical amnesia were resolved and forgotten alternatives became salient in public consciousness, public discourse and policy choices would fundamentally shift. This would lead to the exploration and implementation of new economic models, reorganizing the global economic landscape.
% FOUNDING_PROBLEM: The need to simplify complex economic realities and legitimize existing power structures by presenting them as inevitable, thereby reducing social friction and contestation.
% FOUNDING_PROBLEM_CORROBORATION: Critical economic historians and social theorists, outside of those directly benefiting from the status quo, corroborate this interpretation through extensive archival research and genealogical analysis of economic thought and practice.
narrative_ontology:disappearance_verdict(market_as_natural_default__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__lapsed_alternative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__lapsed_alternative_reading, 0.1, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_alternative_reading_tests).
:- end_tests(market_as_natural_default__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.10) reflects that no single, identifiable agent actively captures rents from this specific mechanism of 'forgetting'; the 'extraction' is diffuse, representing foregone societal benefits. Suppression (0.20) is low because it's not about active coercion but the passive absence of alternatives from public memory. The high theater ratio (0.70) captures the performative aspect of 'naturalness' in economic discourse, where the current market is presented as inevitable or optimal without engaging with its historical contingency. Accessibility collapse (0.40) is moderate because alternatives are not structurally impossible, merely forgotten; resistance (0.15) is low because the mechanism is not one that invites direct conflict.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of mainstream economic discourse, the market's dominance is often seen as a natural outcome of efficiency or human nature. From the 'lapsed alternative' reading, this 'naturalness' is a historical artifact, a consequence of collective amnesia. The engine's classification as a Piton highlights this gap, showing a constraint that persists more by inertia and performance than by active function or concentrated benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'dominant economic institutions' act as agenda-setters by operating within the existing framework, implicitly reinforcing the 'natural default' narrative, but they are not direct beneficiaries of the 'forgetting' itself. The 'general public' and 'foregone alternatives' are victims, bearing the diffuse costs of unpursued paths. No direct beneficiaries are identified, aligning with the Piton classification and the reading's core premise.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a Snare or Tangled Rope, which would imply active, concentrated extraction and suppression. By identifying it as a Piton, the analysis correctly attributes its persistence to historical inertia and the theatrical maintenance of a 'natural' narrative, rather than ongoing, active rent-seeking from the specific mechanism of forgetting. The mandate (simplifying economic reality) has atrophied into a performance of inevitability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_vs_passive_suppression,
    'To what extent is the ''forgetting'' of alternatives a passive historical process versus an actively maintained narrative by specific agents?',
    'Detailed historical-sociological analysis of economic discourse and policy formation, identifying specific actors and institutions that actively promote the ''naturalness'' narrative and marginalize alternatives.',
    'If active maintenance is significant, the constraint''s suppression and extractiveness would be higher, potentially reclassifying it as a Tangled Rope or Snare, and identifying specific beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_vs_passive_suppression, empirical, 'Distinguishing between historical amnesia and active narrative control.').

omega_variable(
    lapsed_vs_beneficiary_maintained_reading,
    'Is the market''s ''natural default'' status primarily due to historical forgetting (lapsed alternative reading) or active, post-hoc defense by incumbent beneficiaries (beneficiary maintained reading)?',
    'Empirical investigation into the mechanisms of persistence: if active lobbying, legal defense, and narrative shaping by identifiable beneficiaries are dominant, the ''beneficiary maintained'' reading gains support.',
    'If the ''beneficiary maintained'' reading is more accurate, the constraint would have identifiable beneficiaries, higher extractiveness, and higher suppression, likely shifting its classification to a Snare or Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lapsed_vs_beneficiary_maintained_reading, conceptual, 'Distinguishing between the ''lapsed alternative'' and ''beneficiary maintained'' readings of market naturalization.').

omega_variable(
    lapsed_vs_hybrid_amnesia_reading,
    'Does the historical forgetting of alternatives remain a diffuse, inertial process (lapsed alternative reading), or has it created conditions for subsequent, active beneficiary capture (hybrid amnesia reading)?',
    'Longitudinal analysis tracing the evolution of market structures: if initial forgetting is followed by the emergence of concentrated beneficiaries who then actively defend the status quo, the ''hybrid amnesia'' reading is supported.',
    'If the ''hybrid amnesia'' reading is more accurate, the constraint''s extractiveness and suppression would likely increase over time, and identifiable beneficiaries would emerge, potentially leading to a reclassification as a Tangled Rope or Snare in later periods.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapsed_vs_hybrid_amnesia_reading, empirical, 'Distinguishing between pure historical amnesia and amnesia leading to capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 1920, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1920, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1920, 0.55).
narrative_ontology:measurement(mark_tr_t1940, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1940, 0.6).
narrative_ontology:measurement(mark_tr_t1960, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1960, 0.65).
narrative_ontology:measurement(mark_tr_t1980, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1980, 0.68).
narrative_ontology:measurement(mark_tr_t2000, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 2000, 0.7).
narrative_ontology:measurement(mark_tr_t2020, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 2020, 0.7).

% Extraction over time
narrative_ontology:measurement(mark_be_t1920, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1920, 0.08).
narrative_ontology:measurement(mark_be_t1940, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1940, 0.09).
narrative_ontology:measurement(mark_be_t1960, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1960, 0.09).
narrative_ontology:measurement(mark_be_t1980, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1980, 0.1).
narrative_ontology:measurement(mark_be_t2000, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 2000, 0.1).
narrative_ontology:measurement(mark_be_t2020, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 2020, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1920, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 1920, 0.22).
narrative_ontology:measurement(mark_su_t1940, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 1940, 0.21).
narrative_ontology:measurement(mark_su_t1960, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 1960, 0.2).
narrative_ontology:measurement(mark_su_t1980, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 1980, 0.19).
narrative_ontology:measurement(mark_su_t2000, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 2000, 0.19).
narrative_ontology:measurement(mark_su_t2020, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 2020, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
