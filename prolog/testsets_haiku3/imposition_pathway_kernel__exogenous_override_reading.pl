% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__exogenous_override_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: imposition_pathway_kernel__exogenous_override_reading
 *   human_readable: State Exogenous Override: Meiji Commitment Displacement
 *   domain: historical_sociology/state_formation/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the exogenous_override reading of the
 *   imposition_pathway kernel: state capacity enables commitment displacement
 *   through top-down coercive decrees WITHOUT prior fringe adoption.
 *   Historical case: Meiji-era Japan mandated calendar change (Gregorian
 *   adoption), eliminated samurai dress/status codes, enforced Western
 *   grooming norms, and rewrote naming conventions through administrative
 *   decree backed by enforcement machinery, not voluntary gradual uptake. The
 *   state apparatus itself became the enforcer and initial adopter;
 *   tradition-adherent populations bore the costs. This reading asserts that
 *   the mechanism is DISTINCT from endogenous climb (where fringe adoption
 *   precedes and climbs toward majority) and requires separate theoretical
 *   modeling in the M-set commitment framework. Extractiveness is high (0.68
 *   stabilized) because the displaced populations bear the costs without
 *   consent; suppression is high (0.79) because enforcement against
 *   traditional practices is active and pervasive; theater is low-moderate
 *   (0.22) because the modernization justification is real but the
 *   enforcement priority is commitment displacement, not improvement of the
 *   services the commitment provides.
 *
 * KEY AGENTS:
 *   - State Modernization Apparatus: enforces decrees, becomes first adopter, justifies impositions as modernization
 *   - Tradition-Adherent Populations: bear compliance costs, experience displacement as coercion, trapped exit
 *   - Local Authorities: lose normative authority over their domains, become state enforcement agents
 *   - Military and Bureaucratic Elites: benefit from unified commitment framework, gain organizational coherence
 *   - Fringe Adoption Communities: structurally excluded by decree preemption, organic climb pathway eliminated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, 0.68).
domain_priors:suppression_score(imposition_pathway_kernel__exogenous_override_reading, 0.79).
domain_priors:theater_ratio(imposition_pathway_kernel__exogenous_override_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__exogenous_override_reading, "State Exogenous Override: Meiji Commitment Displacement").
narrative_ontology:topic_domain(imposition_pathway_kernel__exogenous_override_reading, "historical_sociology/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__exogenous_override_reading, '2a33363d-63aa-4557-9192-3237a94d2b3d').
narrative_ontology:cs_kernel_codification('2a33363d-63aa-4557-9192-3237a94d2b3d', formalized).
narrative_ontology:cs_authority_grounding('2a33363d-63aa-4557-9192-3237a94d2b3d', extraction).
narrative_ontology:cs_interpretation_layer_present('2a33363d-63aa-4557-9192-3237a94d2b3d').
narrative_ontology:cs_reading_relation('2a33363d-63aa-4557-9192-3237a94d2b3d', imposition_pathway_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('2a33363d-63aa-4557-9192-3237a94d2b3d', imposition_pathway_kernel__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('2a33363d-63aa-4557-9192-3237a94d2b3d', foundational, state_capacity_enables_nonfringe_displacement).
narrative_ontology:cs_axiom_status(state_capacity_enables_nonfringe_displacement, holdable).
narrative_ontology:cs_axiom_grounding('2a33363d-63aa-4557-9192-3237a94d2b3d', state_capacity_enables_nonfringe_displacement, empirically_contingent).
narrative_ontology:cs_axiom('2a33363d-63aa-4557-9192-3237a94d2b3d', foundational, exogenous_override_distinct_from_endogenous_climb).
narrative_ontology:cs_axiom_status(exogenous_override_distinct_from_endogenous_climb, holdable).
narrative_ontology:cs_axiom_grounding('2a33363d-63aa-4557-9192-3237a94d2b3d', exogenous_override_distinct_from_endogenous_climb, deontological).
narrative_ontology:cs_reference_frame('2a33363d-63aa-4557-9192-3237a94d2b3d', fringe_adoption_mechanism_universal).
narrative_ontology:cs_drift_state('2a33363d-63aa-4557-9192-3237a94d2b3d', meiji_decree_enforcement, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2a33363d-63aa-4557-9192-3237a94d2b3d', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, state_modernization_apparatus).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, tradition_adherent_populations).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, local_authority_structures).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, local_authority_structures).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, military_and_bureaucratic_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues binding decrees mandating commitment displacement (Gregorian calendar adoption, Western dress codes, hair cutting for men, elimination of samurai status markers). Enforces compliance through administrative inspection, school mandates, military service requirements, and social sanctions. Justifies impositions as necessary for state modernization and international legitimacy. Does not wait for voluntary fringe adoption; the decree precedes any meaningful grassroots uptake and the state apparatus becomes the initial enforcer of the new commitment.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, state_modernization_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Subject to decrees that displace long-established commitments (calendar, dress, grooming, naming conventions) without prior fringe adoption pathway. Face administrative penalties, social shame, employment restrictions if they maintain traditional practices. No meaningful opt-out exists short of exile or total exit from the state system. The commitment displacement is experienced as coercive imposition, not emergent adoption.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, tradition_adherent_populations, payer,
    powerless, biographical, trapped, national).

% Required to enforce the state's commitment displacement decrees within their jurisdictions (villages, prefectures, domains). They lose authority over what were once their own normative spaces (how people dress, what calendar is followed, who holds status markers). They also become instruments of modernization and gain legitimacy through alignment with the state apparatus. Their enforcement machinery becomes state machinery.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, local_authority_structures, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__exogenous_override_reading, local_authority_structures, beneficiary).

% Are the first adopters of the new commitments (Western military dress, Gregorian calendar, new naming conventions) not through grassroots preference but through institutional mandate. Their adoption sets the template that becomes binding for the entire population. They experience the displacement as legitimate modernization and their early-adopter status grants them organizational coherence and competitive advantage against traditional authorities.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, military_and_bureaucratic_elites, beneficiary,
    powerful, civilizational, mobile, national).

% Would-be organic adopters of new commitments (scholars interested in Western learning, merchants adopting foreign practices for trade, intellectuals experimenting with new norms) are structurally preempted by the state decree. Their potential gradual-climb pathway is eliminated; the state's exogenous override forecloses the mechanism of emergent adoption and replaces it with coercion.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, fringe_adoption_communities, excluded,
    moderate, biographical, constrained, local).

% Western powers and international law treat the state's commitment displacement as evidence of modernization and state capacity. A state that can impose new commitments (calendar, dress, administrative norms) is read as modernized and competent. The international recognition creates incentive structure for the state apparatus to pursue exogenous override rather than waiting for endogenous adoption to accumulate.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, international_powers, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__exogenous_override_reading, state_modernization_apparatus).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified commitment framework across a diverse population for state integration: a single calendar system enables synchronized administrative action; standardized dress and naming conventions eliminate status-marker ambiguity; common temporal and symbolic reference frames reduce coordination friction in military, bureaucracy, and commerce.
% TRANSFER_FUNCTION: Moves authority over local norms and identity practices from distributed community structures (families, villages, guilds, local authorities) to the centralized state apparatus. Moves the labor of conformity enforcement from organic fringe adoption to coercive administrative machinery. Moves legitimacy from traditional authorities to the state modernization apparatus.
% ABSENT_VOICES: Fringe adoption communities who would have climbed gradually toward new commitments are structurally excluded — the state decree preempts their potential adoption pathway. Tradition-bearer communities have no seat in deciding the displacement; their voice enters only as resistance, which is suppressed.
% DISAPPEARANCE_RATIONALE: If the state's enforcement of commitment displacement decrees evaporated overnight, populations would partially revert to traditional practices; administrative coordination would fragment; military and bureaucratic operations would lose synchronization; state capacity for uniform action across regions would collapse. The constraints enable the centralized coordination that the modern state depends on.
% FOUNDING_PROBLEM: Diverse local commitments (calendars, dress codes, status-marker systems, naming conventions) created coordination friction for centralized state administration; international legitimacy required visible 'modernization' markers; military and administrative systems required unified temporal and symbolic reference frames; local authorities maintained competing normative authority that limited state reach.
% FOUNDING_PROBLEM_CORROBORATION: State apparatus documentation and modernization theory corroborate the founding problem's persistence — coordination failure across regions remained an active challenge. However, historians and sociology scholars outside the state apparatus document that fringe adoption pathways existed and could have climbed organically; the state's choice to use exogenous override was preference, not necessity. The founding problem was real; the displacement mechanism was not its only solution.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__exogenous_override_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steeply in the first 15 years (decree issuance and enforcement intensification) then stabilizes at 0.68 as the new commitment becomes normalized through generational replacement. Suppression requirement follows the same trajectory: highest during active resistance (years 5–15), then moderates but remains elevated because the enforcement infrastructure persists even after compliance becomes habituated. Theater ratio is low relative to suppression (0.22 vs 0.79) because the constraint's function is genuinely to create unified state coordination — the modernization justification is not pure theater — but the enforcement machinery is oversized relative to the actual coordination need, indicating extraction. The time grid is shared across all three metrics; every metric is authored at each time point. Measurements are observed (not projected) for the Meiji historical record; the interval spans 1868–1908, capturing decree, enforcement, and stabilization.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus and military/bureaucratic elites compute the constraint as coordination with legitimate modernization extraction (beneficial overall from their seat). Tradition-adherent populations and local authorities compute it as pure extraction backed by enforcement (no coordination benefit reaching them). The engine computes per-seat directionality from the structural data: beneficiaries near d=0.0, victims near d=1.0, creating markedly different type classifications. The agenda-setter seat (state apparatus) experiences genuine coordination function; the payer seats (tradition-adherent populations) experience coercion without reciprocal benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   State modernization apparatus: d ≈ 0.1 (full beneficiary — collects coordination gains, controls the decrees, faces no suppression). Tradition-adherent populations: d ≈ 0.92 (near-full target — bear compliance costs, trapped exit, no upside participation in coordination benefit). Local authorities: d ≈ 0.65 (moderate target — lose autonomous authority, constrained by decree, but gain state legitimacy and bureaucratic position). Military elites: d ≈ 0.25 (moderate beneficiary — early adopters, gain organizational coherence, but subordinate to state apparatus). The directionality derivation is driven by beneficiary/victim declarations and exit options: powerless agents with trapped exit (tradition-adherent populations) sit at the target end; institutional actors with arbitrage options (state apparatus) sit at the beneficiary end; constrained-exit moderate-power agents (local authorities) sit in the middle-to-target range.
 *
 * MANDATROPHY ANALYSIS:
 *   This is NOT mandatrophy. The founding problem (coordination friction from diverse local commitments, military/administrative synchronization need) remains live for the entire interval. The constraint persists because it solves a real coordination problem that the state apparatus depends on. The divergence between the state's claim (genuine coordination) and the measurements (high extraction, high suppression) indicates that the constraint is a tangled_rope, not a snare or piton: real coordination function married to asymmetric extraction. The state-benefiting reading and the tradition-bearer reading are structurally opposed, not a case of atrophied function maintained by theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fringe_adoption_counterfactual,
    'Would the Gregorian calendar, Western dress, and new naming conventions have climbed organically toward majority adoption without state decree, following an endogenous fringe pathway?',
    'Historical counterfactual analysis of fringe adoption trajectories in comparable domains (e.g., adoption of new agricultural technologies, merchant dress codes, intellectual practices) that were NOT subject to state decree in the same period. Comparison with regions where the state did NOT enforce the same commitments but similar adoption occurred.',
    'If organic climb was evident in non-decreed domains and absent only where the state overrode, the exogenous_override reading''s core claim is vindicated. If fringe adoption trajectories are absent across the board, the endogenous_climb reading''s core claim (that all climbs have fringe stages) is falsified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_adoption_counterfactual, empirical, 'Whether the state decree was necessary or merely accelerated a trajectory that would have climbed organically.').

omega_variable(
    coordination_necessity_vs_extraction_preference,
    'Was the state apparatus genuinely unable to achieve sufficient military/administrative coordination without exogenous override, or did it choose exogenous override because it extracted maximum legitimacy and control, even though a slower endogenous climb would have also worked?',
    'Archival analysis of state planners'' deliberation: did they model endogenous climb as an option and reject it, or was override treated as the only viable mechanism? Comparison with other modernizing states that relied on slower endogenous adoption and achieved comparable coordination outcomes.',
    'If the state had viable alternatives and chose override for extraction/control reasons, the constraint is snare-inflected (pure coercive extraction with a coordination cover story). If override was genuinely necessary to achieve state coordination in the required timeframe, the constraint remains tangled_rope (real coordination with asymmetric extraction as the price).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_vs_extraction_preference, conceptual, 'Whether exogenous override was necessary for coordination or chosen for maximum extraction.').

omega_variable(
    alternative_mechanisms_foreclosure,
    'By issuing the decree, did the state foreclose the organic fringe adoption pathway, or would both mechanisms have coexisted with decree-driven adoption accelerating alongside organic climb?',
    'Detailed genealogy of fringe adoption communities pre-decree and their trajectory post-decree: were they absorbed into the official pathway, suppressed, or did they persist as parallel adoption? Archival evidence of whether the state treated fringe adopters as allies or competitors.',
    'If fringe adoption was suppressed or redirected by the decree, the override mechanism actively FORECLOSES endogenous climb (supporting exogenous_override reading). If fringe and official pathways coexisted, the constraint is better modeled as hybrid_cascade.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_mechanisms_foreclosure, empirical, 'Whether the state decree foreclosed organic adoption or merely accelerated/redirected it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(impo_tr_t5, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(impo_tr_t10, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(impo_tr_t15, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(impo_tr_t25, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(impo_tr_t40, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(impo_be_t5, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(impo_be_t10, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(impo_be_t15, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(impo_be_t25, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(impo_be_t40, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(impo_su_t5, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(impo_su_t10, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(impo_su_t15, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 15, 0.81).
narrative_ontology:measurement(impo_su_t25, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 25, 0.79).
narrative_ontology:measurement(impo_su_t40, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% Part of the imposition_pathway constraint family. The family decomposes a contested kernel about commitment displacement mechanisms: endogenous_climb_reading asserts all displacement occurs through fringe adoption climbs; exogenous_override_reading (this constraint) asserts state capacity enables top-down imposition without fringe pathway; hybrid_cascade_reading asserts decree creates artificial fringe (state employees, military) that climbs organically. Each reading has distinct ε and victim structure. The readings are linked by network.affects_constraints to model the theoretical dispute: exogenous_override constrains which of the other readings are coherent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_pathway_kernel__exogenous_override_reading, institutional, 0.08).
constraint_indexing:directionality_override(imposition_pathway_kernel__exogenous_override_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
