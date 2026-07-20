% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__hybrid_amnesia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__hybrid_amnesia_reading, []).

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
 *   constraint_id: market_as_natural_default__hybrid_amnesia_reading
 *   human_readable: Market-as-Natural-Default: Hybrid Amnesia Reading
 *   domain: political_economy/ideology/economic_history
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid_amnesia reading of the
 *   market_as_natural_default kernel. It models a two-stage process: an
 *   initial period of genuine historical forgetting of non-market
 *   alternatives (c. 1930-1970) created epistemic conditions that incumbent
 *   beneficiaries later captured and weaponized through defensive
 *   rationalization (c. 1980-present). The market is treated as the natural
 *   default state of economic organization, with the result that non-market
 *   coordination must carry an extraordinary burden of proof. The reading
 *   claims this is neither a pure natural law (Mountain) nor a fully
 *   constructed snare from the outset, but a Tangled Rope: the market
 *   mechanism carries genuine coordination value, yet the naturalization
 *   narrative asymmetrically extracts by suppressing alternatives and
 *   concentrating policy returns on incumbent corporate actors.
 *
 * KEY AGENTS:
 *   - corporate_incumbents: Primary beneficiary (institutional/arbitrage) â inherits and profits from naturalized market order
 *   - policy_entrepreneurs: Agenda-setter (organized/constrained) â weaponizes pre-existing amnesia into defensive rationalization
 *   - public_sector_planners: Primary payer (moderate/constrained) â bears justification asymmetry for non-market allocation
 *   - social_democratic_constituencies: Secondary payer (powerless/constrained) â loses policy vocabulary and institutional memory
 *   - collective_ownership_advocates: Excluded voice (powerless/trapped) â structurally barred from policy discourse
 *   - heterodox_economic_historians: Analytical observer (analytical/analytical) â documents the contingent construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__hybrid_amnesia_reading, 0.45).
domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, 0.6).
domain_priors:theater_ratio(market_as_natural_default__hybrid_amnesia_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__hybrid_amnesia_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__hybrid_amnesia_reading, "Market-as-Natural-Default: Hybrid Amnesia Reading").
narrative_ontology:topic_domain(market_as_natural_default__hybrid_amnesia_reading, "political_economy/ideology/economic_history").

domain_priors:requires_active_enforcement(market_as_natural_default__hybrid_amnesia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, 'b3eea9f9-e174-44aa-92f0-c7fe726277f7').
narrative_ontology:cs_kernel_codification('b3eea9f9-e174-44aa-92f0-c7fe726277f7', implicit).
narrative_ontology:cs_authority_grounding('b3eea9f9-e174-44aa-92f0-c7fe726277f7', extraction).
narrative_ontology:cs_interpretation_layer_present('b3eea9f9-e174-44aa-92f0-c7fe726277f7').
narrative_ontology:cs_reading_relation('b3eea9f9-e174-44aa-92f0-c7fe726277f7', market_as_natural_default__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3eea9f9-e174-44aa-92f0-c7fe726277f7', market_as_natural_default__beneficiary_maintained_reading, influences).
narrative_ontology:cs_axiom('b3eea9f9-e174-44aa-92f0-c7fe726277f7', foundational, market_naturalness_is_constructed_amnesia).
narrative_ontology:cs_axiom_status(market_naturalness_is_constructed_amnesia, holdable).
narrative_ontology:cs_axiom_grounding('b3eea9f9-e174-44aa-92f0-c7fe726277f7', market_naturalness_is_constructed_amnesia, empirically_contingent).
narrative_ontology:cs_axiom('b3eea9f9-e174-44aa-92f0-c7fe726277f7', foundational, beneficiary_capture_requires_prior_lapse).
narrative_ontology:cs_axiom_status(beneficiary_capture_requires_prior_lapse, holdable).
narrative_ontology:cs_axiom_grounding('b3eea9f9-e174-44aa-92f0-c7fe726277f7', beneficiary_capture_requires_prior_lapse, empirically_contingent).
narrative_ontology:cs_reference_frame('b3eea9f9-e174-44aa-92f0-c7fe726277f7', lapsed_alternative_settlement).
narrative_ontology:cs_drift_state('b3eea9f9-e174-44aa-92f0-c7fe726277f7', defensive_rationalization_present, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b3eea9f9-e174-44aa-92f0-c7fe726277f7', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, corporate_incumbents).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, public_sector_planners).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, social_democratic_constituencies).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, collective_ownership_advocates).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, market_efficiency_doctrine).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, there_is_no_alternative_tina).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive policy outcomes that favor market-based allocation, benefit from reduced regulatory and planning competition, and fund ideological infrastructure that naturalizes the market order. Their cost of exit from the naturalized frame is low because they shape it.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, corporate_incumbents, beneficiary,
    institutional, generational, arbitrage, global).

% Design and disseminate the intellectual framework treating market allocation as the default state of nature. Funded by corporate beneficiaries to produce policy papers, academic programs, and media narratives. Their career trajectories depend on maintaining the naturalization thesis.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, policy_entrepreneurs, agenda_setter,
    organized, biographical, constrained, global).

% Operate within state agencies where planning capacity has been systematically dismantled or delegitimized. Must justify non-market interventions with heavy evidentiary burdens that market allocations never face. Exit to planning jurisdictions is institutionally blocked by trade agreements and domestic legal frameworks.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, public_sector_planners, payer,
    moderate, biographical, constrained, national).

% Comprise voters and workers who historically supported mixed economies but have lost the institutional memory and policy vocabulary for non-market coordination. Experience stagnating wages and reduced public services as market naturalization narrows the policy window.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, social_democratic_constituencies, payer,
    powerless, generational, constrained, national).

% Hold positions advocating cooperative or state ownership models. Excluded from mainstream policy discourse, treated as outside the Overton window, and denied access to policy-making forums where market naturalization is treated as axiomatic.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, collective_ownership_advocates, excluded,
    powerless, generational, trapped, national).

% Document the contingent construction of market dominance and the historical existence of planning alternatives. Operate in marginal academic departments; their findings rarely penetrate policy discourse.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, heterodox_economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__hybrid_amnesia_reading, corporate_incumbents).
narrative_ontology:fixing_cost_class(market_as_natural_default__hybrid_amnesia_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared default economic framework that reduces deliberation costs by treating market allocation as the unremarked baseline for organizing production and distribution.
% TRANSFER_FUNCTION: Moves policy attention, institutional legitimacy, and material resources from public-sector planning and collective-ownership alternatives to incumbent market actors, by treating the market order as a spontaneous natural default that requires no justification.
% ABSENT_VOICES: Advocates of indicative planning, social democratic industrial strategists, and economic historians emphasizing the constructed nature of the post-war settlement were gradually excluded from policy discourse as amnesia deepened and was weaponized.
% DISAPPEARANCE_RATIONALE: If the naturalization constraint vanished, the policy space would reopen to alternatives previously treated as unnatural; institutional memory of planning, public provision, and mixed-economy coordination would resurface; the burden of proof would shift from defenders of non-market allocation to its opponents.
% FOUNDING_PROBLEM: The collapse of early-20th-century planning experiments and the administrative complexity of wartime/post-war coordination created an epistemic vacuum in which market mechanisms became a pragmatic default; the subsequent forgetting of this contingency created the kernel.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and heterodox scholars attest the founding pragmatic problem is solved and the arrangement persists as ideological capture; incumbent beneficiaries assert the naturalness is timeless. Corroboration from outside the benefiting parties includes archival historiography of the Mont Pelerin Society and critical political economy.
narrative_ontology:disappearance_verdict(market_as_natural_default__hybrid_amnesia_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__hybrid_amnesia_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_as_natural_default__hybrid_amnesia_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__hybrid_amnesia_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__hybrid_amnesia_reading_tests).
:- end_tests(market_as_natural_default__hybrid_amnesia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate but rising: the constraint extracts by foreclosing planning alternatives and loading justification costs onto non-market actors. Suppression (0.60) reflects active enforcement through think-tank discourse, media naturalization, and legal-institutional barriers to public provision. Theater ratio (0.40) captures the performative dimension: much contemporary defense of 'free markets' is post-hoc rationalization of inherited amnesia rather than empirical argument. Accessibility collapse (0.75) is high because decades of ideological dominance have made alternatives nearly unthinkable within mainstream policy discourse. Resistance (0.55) reflects renewed heterodox and social-democratic challenge. The temporal series show a ratchet: low extraction during the genuine amnesia phase, sharp inflection when beneficiaries began active weaponization in the 1980s.
 *
 * PERSPECTIVAL GAP:
 *   The corporate incumbent and policy entrepreneur seats experience the constraint as benign coordination (markets work, everyone benefits) or as necessary intellectual defense. The public-sector planner and social-democratic constituency seats experience it as an invisible cage: the same structure that provides familiar market transactions also prevents them from articulating or accessing alternative coordination modes. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Corporate incumbents are the primary beneficiaries (d near 0.0); they are subsidized by the constraint's suppression of planning alternatives. Policy entrepreneurs are secondary beneficiaries/agenda-setters (d low-moderate). Public-sector planners, social-democratic constituencies, and collective-ownership advocates are targets (d near 1.0): they bear the costs of justification asymmetry and exclusion. Heterodox historians are analytical observers (d near 0.5, symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The two-stage structure prevents mislabeling. If we treated the entire history as a Snare, we would miss the genuine coordination function of market mechanisms and the initial contingent pragmatism of the post-war settlement. If we treated it as a Rope, we would miss the active beneficiary capture and weaponization that ratcheted extraction upward after 1980. Tangled Rope is the only category that accommodates both the real coordination value and the asymmetric extraction through narrative naturalization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lapse_to_capture_transition,
    'At what point in the 1970s-1980s did passive epistemic amnesia transition to active defensive rationalization by identifiable beneficiaries?',
    'Granular historiographic analysis of think-tank funding flows, policy discourse archives, and legislative rhetoric to pinpoint the inflection from forgetting to weaponization.',
    'A sharp transition point would validate the two-stage hybrid reading; a gradual or undetectable transition would collapse the reading toward either pure lapse or pure maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapse_to_capture_transition, empirical, 'Timing of the transition from passive amnesia to active beneficiary weaponization').

omega_variable(
    reading_decomposition,
    'Can the hybrid two-stage process be empirically distinguished from a single continuous process of beneficiary maintenance?',
    'Comparative historiography of the 1930-1960 period: if archival evidence shows active beneficiary suppression of alternatives during the apparent ''lapse'' phase, the hybrid reading is falsified toward beneficiary_maintained_reading.',
    'If the two stages are indistinguishable, the hybrid reading is epistemically unstable and the kernel collapses to a simpler reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_decomposition, conceptual, 'Empirical stability of the hybrid two-stage reading against simpler alternatives').

omega_variable(
    coordination_extraction_separability,
    'Is the genuine coordination function of market mechanisms structurally separable from the naturalization narrative that suppresses alternatives?',
    'Comparative policy analysis of jurisdictions where market mechanisms coexist with robust public planning: if coordination holds without naturalization, the functions are separable.',
    'If separable, the naturalization narrative is extractive excess riding on real coordination; if inseparable, part of the measured extraction is the necessary cost of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether market coordination and naturalization ideology are structurally separable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 0, 94).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_amnesia_tr_t0, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hybrid_amnesia_tr_t20, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(hybrid_amnesia_tr_t40, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(hybrid_amnesia_tr_t60, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement(hybrid_amnesia_tr_t80, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(hybrid_amnesia_tr_t94, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 94, 0.4).

% Extraction over time
narrative_ontology:measurement(hybrid_amnesia_be_t0, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(hybrid_amnesia_be_t20, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(hybrid_amnesia_be_t40, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(hybrid_amnesia_be_t60, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement(hybrid_amnesia_be_t80, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 80, 0.42).
narrative_ontology:measurement(hybrid_amnesia_be_t94, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 94, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_amnesia_su_t0, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(hybrid_amnesia_su_t20, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(hybrid_amnesia_su_t40, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(hybrid_amnesia_su_t60, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 60, 0.45).
narrative_ontology:measurement(hybrid_amnesia_su_t80, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(hybrid_amnesia_su_t94, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 94, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__hybrid_amnesia_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the market_as_natural_default kernel. It is decomposed from the colloquial label 'market as natural default' because the kernel conflates three structurally distinct claims: pure historical lapse, beneficiary maintenance from the outset, and the hybrid two-stage process authored here. Each reading carries a distinct epsilon, stakeholder structure, and temporal trajectory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
