% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__international_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__international_regime, []).

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
    narrative_ontology:coordination_type/2,
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
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ost_article_ii_non_appropriation__international_regime
 *   human_readable: Outer Space Treaty Article II Non-Appropriation (International Regime Reading)
 *   domain: international_space_law/treaty_interpretation/commons_governance
 *
 * SUMMARY:
 *   This constraint represents the 'international regime' reading of Article
 *   II of the Outer Space Treaty (OST), which states that outer space is 'not
 *   subject to national appropriation by claim of sovereignty, by means of
 *   use or occupation, or by any other means.' This reading interprets
 *   Article II as deferring the question of resource appropriation to a
 *   future international regime, analogous to Article XI's call for an
 *   international regime for the Moon and other celestial bodies. In this
 *   reading, neither a purely extraction-permissive nor a strict
 *   conservationist interpretation is authoritative in the absence of such a
 *   multilateral framework. This creates a Scaffold-like situation where
 *   legal uncertainty persists, first-mover firms operate in a regulatory
 *   grey zone, and regime negotiation is stalled by zero-sum distributional
 *   conflict.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, 0.4).
domain_priors:suppression_score(ost_article_ii_non_appropriation__international_regime, 0.2).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__international_regime, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, extractiveness, 0.4).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__international_regime, scaffold).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__international_regime, "Outer Space Treaty Article II Non-Appropriation (International Regime Reading)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__international_regime, "international_space_law/treaty_interpretation/commons_governance").

narrative_ontology:has_sunset_clause(ost_article_ii_non_appropriation__international_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__international_regime, 'ac52d9f7-7f62-4d86-ade9-4aca51494803').
narrative_ontology:cs_kernel_codification('ac52d9f7-7f62-4d86-ade9-4aca51494803', fixed_text).
narrative_ontology:cs_authority_grounding('ac52d9f7-7f62-4d86-ade9-4aca51494803', lineage).
narrative_ontology:cs_interpretation_layer_present('ac52d9f7-7f62-4d86-ade9-4aca51494803').
narrative_ontology:cs_reading_relation('ac52d9f7-7f62-4d86-ade9-4aca51494803', ost_article_ii_non_appropriation__extraction_permissive, coexists_with).
narrative_ontology:cs_reading_relation('ac52d9f7-7f62-4d86-ade9-4aca51494803', ost_article_ii_non_appropriation__commons_conservation, coexists_with).
narrative_ontology:cs_axiom('ac52d9f7-7f62-4d86-ade9-4aca51494803', foundational, future_regime_primacy).
narrative_ontology:cs_axiom_status(future_regime_primacy, holdable).
narrative_ontology:cs_axiom_grounding('ac52d9f7-7f62-4d86-ade9-4aca51494803', future_regime_primacy, conventional).
narrative_ontology:cs_axiom('ac52d9f7-7f62-4d86-ade9-4aca51494803', foundational, current_legal_ambiguity).
narrative_ontology:cs_axiom_status(current_legal_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('ac52d9f7-7f62-4d86-ade9-4aca51494803', current_legal_ambiguity, empirically_contingent).
narrative_ontology:cs_reference_frame('ac52d9f7-7f62-4d86-ade9-4aca51494803', deferral_to_multilateralism).
narrative_ontology:cs_drift_state('ac52d9f7-7f62-4d86-ade9-4aca51494803', contemporary_unilateral_claims_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ac52d9f7-7f62-4d86-ade9-4aca51494803', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, future_international_regime).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, states_seeking_consensus).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, first_mover_space_resource_firms).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, states_seeking_unilateral_advantage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The conceptual entity that this reading defers to. Its potential existence and authority are preserved by the current legal ambiguity, allowing for a future, more comprehensive governance framework.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, future_international_regime, beneficiary,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__international_regime, future_international_regime).

% These states benefit from the deferral as it keeps open the possibility of a multilateral, equitable solution to space resource governance, preventing unilateral claims from becoming entrenched without international agreement.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, states_seeking_consensus, beneficiary,
    institutional, generational, constrained, global).

% These firms operate in a legal grey zone, investing heavily in resource extraction technologies without clear legal title to extracted resources. They bear the cost of legal uncertainty and potential future regulation, but also benefit from the lack of immediate prohibition.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, first_mover_space_resource_firms, payer,
    organized, biographical, constrained, global).

% These states are constrained by the lack of clear legal permission for appropriation, preventing them from fully legitimizing unilateral claims or supporting their national firms with full legal backing. They pay the cost of stalled negotiations and legal ambiguity.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, states_seeking_unilateral_advantage, payer,
    institutional, generational, constrained, global).

% Advocate for a strict interpretation of non-appropriation to protect outer space as a global commons. They are excluded from achieving their desired outcome by the current legal ambiguity, which allows de facto extraction to proceed.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, conservation_advocates, excluded,
    moderate, generational, identity_locked, global).

% Analyze the various interpretations of Article II, the implications of legal uncertainty, and the prospects for a future international regime. They do not directly benefit or pay, but their analysis influences policy debates.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, international_legal_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__international_regime, diffuse).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__international_regime, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent immediate, unilateral appropriation of celestial bodies and their resources by states or private entities, thereby preserving the possibility of a future, collectively agreed-upon international governance framework.
% TRANSFER_FUNCTION: The constraint transfers the 'right to appropriate' from any single actor to a future, as-yet-unformed international regime. It also transfers the cost of legal uncertainty to first-mover firms and states seeking unilateral advantage, while preserving the 'option value' of a global commons for all states.
% ABSENT_VOICES: Strict conservation advocates and those pushing for immediate, clear legal frameworks for private property rights in space are both marginalized by this reading. Conservationists would argue for stronger prohibitions, while property rights advocates would push for clear permissive rules.
% DISAPPEARANCE_RATIONALE: If this reading (and the underlying legal ambiguity) disappeared, the world would quickly rearrange. Either unilateral appropriation would be fully legitimized (extraction-permissive reading), leading to a 'gold rush' and potential conflict, or strict conservation would be enforced (commons-conservation reading), halting current commercial activities. The current holding pattern would collapse.
% FOUNDING_PROBLEM: The original problem was to prevent a 'flag planting' race for celestial bodies and to ensure outer space remained accessible to all, without foreclosing future resource utilization or governance models.
% FOUNDING_PROBLEM_CORROBORATION: The problem of preventing unilateral appropriation and ensuring equitable access remains live, as attested by ongoing debates in UNCOPUOS and the proliferation of national space resource laws. International legal scholars and many non-spacefaring states corroborate that the core problem persists, even if the specific 'international regime' solution remains elusive.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__international_regime, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__international_regime, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__international_regime, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__international_regime, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__international_regime_tests).
:- end_tests(ost_article_ii_non_appropriation__international_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Scaffold because it represents a temporary legal holding pattern, explicitly deferring to a future, more comprehensive agreement. Its extractiveness is moderate (0.4) due to the legal uncertainty that allows some de facto extraction by first-movers, but it's not high because no state has full legal backing for appropriation. Suppression is low (0.2) as there's no active enforcement against resource extraction, only a lack of clear legal permission. Theater ratio is low (0.1) as the deferral is a genuine (if stalled) attempt at coordination, not mere performance. The 'has_sunset_clause' is true conceptually, as the 'sunset' is the establishment of the international regime itself.
 *
 * PERSPECTIVAL GAP:
 *   States seeking unilateral advantage might perceive this as a weak Rope or even a Piton, as it doesn't actively prevent their actions but also doesn't legitimize them. Conservationists might see it as a Snare, as it allows de facto appropriation to proceed. The 'international regime' reading attempts to hold a neutral ground, but its practical effect is to maintain legal ambiguity.
 *
 * DIRECTIONALITY LOGIC:
 *   Future international regime and states seeking consensus are beneficiaries, as this reading preserves the possibility of a multilateral solution. First-mover space resource firms and states seeking unilateral advantage are victims, as their activities lack full legal certainty and are subject to future regulation. The legal uncertainty itself extracts from those who desire clear rules for either extraction or conservation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_negotiation_viability,
    'Is a multilateral international regime for space resource appropriation politically viable, or is zero-sum distributional conflict intractable?',
    'Progress in UNCOPUOS or other multilateral fora towards a binding agreement; successful negotiation of a resource-sharing framework.',
    'If viable, the constraint functions as a temporary scaffold towards a stable solution. If intractable, the legal uncertainty persists indefinitely, potentially leading to unilateral claims and conflict, reclassifying the constraint as a piton or even a snare for those excluded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_negotiation_viability, empirical, 'Uncertainty about the political feasibility of establishing a future international regime for space resources.').

omega_variable(
    reading_authority_ambiguity,
    'Is this ''international regime'' reading of Article II genuinely authoritative, or is it merely a placeholder for unresolved disagreement?',
    'A clear consensus among states on the deferral interpretation, or a judicial ruling from an international court affirming this reading''s primacy.',
    'If genuinely authoritative, the constraint provides a stable (if temporary) framework. If merely a placeholder, the legal uncertainty is higher, and the constraint''s ''scaffold'' function is weaker, potentially collapsing into a piton of performative deferral.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_authority_ambiguity, conceptual, 'Ambiguity regarding the authoritative status of the ''international regime'' reading in the absence of a concrete regime.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''ost_article_ii_non_appropriation'' kernel. What would change if a sibling reading were adopted?',
    'Adoption of a different reading by a critical mass of states or an international court.',
    'If the ''extraction_permissive'' reading were adopted, the constraint would shift towards a Rope or even a Mountain for resource extractors, and a Snare for those advocating conservation. If the ''commons_conservation'' reading were adopted, it would become a Snare for extractors and a Mountain for conservationists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as the ''international_regime'' reading of OST Article II non-appropriation, noting the structural changes under alternative readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__international_regime, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t0, ost_article_ii_non_appropriation__international_regime, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ost__tr_t10, ost_article_ii_non_appropriation__international_regime, theater_ratio, 10, 0.1).
narrative_ontology:measurement(ost__tr_t20, ost_article_ii_non_appropriation__international_regime, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(ost__be_t0, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ost__be_t10, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(ost__be_t20, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 20, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t0, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(ost__su_t10, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(ost__su_t20, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__international_regime, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ost_article_ii_non_appropriation' kernel, alongside 'extraction_permissive' and 'commons_conservation'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
