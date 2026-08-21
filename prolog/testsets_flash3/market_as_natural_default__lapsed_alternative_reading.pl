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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: market_as_natural_default__lapsed_alternative_reading
 *   human_readable: Market as Natural Default (Lapsed Alternative Reading)
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This constraint describes the phenomenon where market dominance is
 *   perceived as a natural or inevitable state, not due to active enforcement
 *   or beneficiary defense, but because historical alternatives have been
 *   forgotten. This 'lapsed alternative' reading posits that the
 *   'naturalness' is a cognitive artifact of collective amnesia, making the
 *   market appear as a 'mountain' by default. This is one reading of the
 *   'market_as_natural_default' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.12).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.05).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, mountain).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market as Natural Default (Lapsed Alternative Reading)").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political_economy/ideology_studies/economic_history").

domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, '7dfee06b-233a-46f1-b404-a449120f1ac4').
narrative_ontology:cs_kernel_codification('7dfee06b-233a-46f1-b404-a449120f1ac4', implicit).
narrative_ontology:cs_authority_grounding('7dfee06b-233a-46f1-b404-a449120f1ac4', diffuse_epistemic).
narrative_ontology:cs_reading_relation('7dfee06b-233a-46f1-b404-a449120f1ac4', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('7dfee06b-233a-46f1-b404-a449120f1ac4', market_as_natural_default__hybrid_amnesia_reading, coexists_with).
narrative_ontology:cs_axiom('7dfee06b-233a-46f1-b404-a449120f1ac4', foundational, historical_amnesia_drives_naturalization).
narrative_ontology:cs_axiom_status(historical_amnesia_drives_naturalization, holdable).
narrative_ontology:cs_axiom_grounding('7dfee06b-233a-46f1-b404-a449120f1ac4', historical_amnesia_drives_naturalization, empirically_contingent).
narrative_ontology:cs_reference_frame('7dfee06b-233a-46f1-b404-a449120f1ac4', market_as_inevitable_default).
narrative_ontology:cs_drift_state('7dfee06b-233a-46f1-b404-a449120f1ac4', contemporary_historical_research_era, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('7dfee06b-233a-46f1-b404-a449120f1ac4', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research and document the historical contingency of market structures and the existence of forgotten alternatives. Their work can challenge the 'natural' perception of market dominance.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% Operates within market structures, perceiving them as natural or inevitable due to a lack of historical awareness of alternatives. Bears the diffuse costs of limited choice without recognizing them as imposed.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, general_public, payer,
    powerless, biographical, identity_locked, national).

% Formulate economic policy within the perceived 'natural' constraints of market dominance, often unaware of or disinclined to explore historically suppressed alternatives. Their actions reinforce the default.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading asserts no active coordination function for the 'natural default' itself; rather, it describes a cognitive state where the market appears to coordinate by default due to the absence of remembered alternatives.
% TRANSFER_FUNCTION: No direct transfer function is asserted by this reading. The 'transfer' is the unacknowledged opportunity cost of foregone alternatives, borne diffusely by the general public.
% ABSENT_VOICES: The voices of historical actors who advocated for or built alternative economic systems are absent, forgotten from collective memory. Their arguments for different forms of economic organization are not part of contemporary discourse.
% DISAPPEARANCE_RATIONALE: If the 'lapsed alternative' aspect of market dominance vanished overnight (i.e., historical memory of alternatives was instantly restored), the market structures themselves would not immediately disappear. Instead, the perception of their naturalness would be shattered, leading to new political and economic contestation over their legitimacy and the viability of alternatives.
% FOUNDING_PROBLEM: The 'problem' this constraint implicitly 'solves' is the cognitive burden of choice and the social friction of contestation, by presenting one economic arrangement as the only viable option.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians corroborate that the cognitive burden of choice and the desire for perceived stability are persistent human tendencies, which can lead to the naturalization of existing systems when alternatives are forgotten. No specific 'benefiting party' actively created this 'problem' in this reading.
narrative_ontology:disappearance_verdict(market_as_natural_default__lapsed_alternative_reading, world_unchanged).
narrative_ontology:founding_problem_status(market_as_natural_default__lapsed_alternative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__lapsed_alternative_reading, 0.12, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_alternative_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, ExtMetricName, E),
    domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(market_as_natural_default__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because no identifiable party actively extracts from this specific mechanism of 'forgetting'; the 'cost' is the diffuse opportunity cost of foregone alternatives. Suppression is negligible (0.05) as there's no active coercion to forget, only a passive lack of historical transmission. Theater ratio is low (0.08) because there's little performative maintenance of the 'forgetting' itself. Accessibility collapse is high (0.88) because the forgotten alternatives are, by definition, inaccessible to contemporary thought without active historical recovery. Resistance is low (0.03) because it's hard to resist something perceived as natural or non-existent.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the general public, the market appears as a natural, unchangeable 'mountain' due to the absence of remembered alternatives. From the analytical perspective of economic historians, it is a historically contingent construct whose 'naturalness' is a product of lapsed memory, not inherent structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'general_public' is the diffuse 'payer' of the opportunity cost, experiencing the constraint as an identity-locked default. 'Economic_historians' act as analytical observers, capable of recovering the forgotten alternatives. 'Policy_makers' are agenda-setters who operate within this default, reinforcing it through their actions, but not actively creating the 'forgetting' itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_forgetting_vs_passive_amnesia,
    'Is the forgetting of alternatives a passive process of historical decay, or is it actively (even if subtly) maintained by agents who benefit from the market''s perceived naturalness?',
    'Detailed historical and sociological research into the mechanisms of knowledge transmission and suppression, identifying specific actors or institutions involved in downplaying or omitting alternative histories.',
    'If active maintenance is found, the constraint''s extractiveness and suppression would be higher, and its classification would shift towards a ''snare'' or ''tangled_rope'' (e.g., the ''beneficiary_maintained_reading'' or ''hybrid_amnesia_reading''). If passive, the ''mountain'' classification for this reading holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(active_forgetting_vs_passive_amnesia, empirical, 'Distinguishing between passive historical amnesia and active suppression of alternatives.').

omega_variable(
    recoverability_of_alternatives,
    'To what extent are the ''lapsed alternatives'' genuinely recoverable and implementable in contemporary contexts, or are they merely historical curiosities?',
    'Feasibility studies and pilot programs for historically inspired alternative economic models, assessing their practical viability and scalability in modern conditions.',
    'If alternatives are highly recoverable, the ''accessibility_collapse'' metric would be lower, and the ''mountain'' classification would be challenged as the perceived inevitability of the market weakens. If not, the ''mountain'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recoverability_of_alternatives, empirical, 'Assessing the practical recoverability of forgotten economic alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1800, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1800, 0.08).
narrative_ontology:measurement(mark_tr_t1850, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1850, 0.08).
narrative_ontology:measurement(mark_tr_t1900, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(mark_tr_t1950, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(mark_tr_t2000, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(mark_tr_t2024, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 2024, 0.08).

% Extraction over time
narrative_ontology:measurement(mark_be_t1800, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1800, 0.1).
narrative_ontology:measurement(mark_be_t1850, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1850, 0.11).
narrative_ontology:measurement(mark_be_t1900, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1900, 0.12).
narrative_ontology:measurement(mark_be_t1950, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1950, 0.12).
narrative_ontology:measurement(mark_be_t2000, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 2000, 0.13).
narrative_ontology:measurement(mark_be_t2024, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 2024, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1800, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 1800, 0.05).
narrative_ontology:measurement(mark_su_t1850, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 1850, 0.05).
narrative_ontology:measurement(mark_su_t1900, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 1900, 0.05).
narrative_ontology:measurement(mark_su_t1950, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 1950, 0.05).
narrative_ontology:measurement(mark_su_t2000, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement(mark_su_t2024, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
