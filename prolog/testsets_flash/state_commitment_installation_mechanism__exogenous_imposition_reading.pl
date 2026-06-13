% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__exogenous_imposition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__exogenous_imposition_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_commitment_installation_mechanism__exogenous_imposition_reading
 *   human_readable: State Commitment Installation: Exogenous Imposition Reading
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint describes the process by which new commitments (e.g.,
 *   legal systems, national ideologies, economic policies) are installed
 *   top-down by a central authority holding a mandate for transformation,
 *   rather than emerging from grassroots movements or incremental adoption.
 *   It is a 'snare' because it primarily functions to extract legitimacy and
 *   control from local populations and traditional institutions for the
 *   benefit of the state and its aligned elites, relying heavily on coercion
 *   and suppression of alternatives. This is one reading of the
 *   'state_commitment_installation_mechanism' kernel.
 *
 * KEY AGENTS:
 *   - state_authority: Primary beneficiary/agenda-setter (institutional/arbitrage)
 *   - mandate_holding_elites: Secondary beneficiary (powerful/mobile)
 *   - local_communities: Primary target/victim (powerless/trapped)
 *   - traditional_institutions: Secondary target/victim (organized/constrained)
 *   - displaced_elites: Secondary target/victim (moderate/constrained)
 *   - historical_sociologists: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.65).
domain_priors:suppression_score(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.75).
domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__exogenous_imposition_reading, snare).
narrative_ontology:human_readable(state_commitment_installation_mechanism__exogenous_imposition_reading, "State Commitment Installation: Exogenous Imposition Reading").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__exogenous_imposition_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__exogenous_imposition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__exogenous_imposition_reading, '5fa345ab-680e-45a7-9e4b-01c02740f3a9').
narrative_ontology:cs_kernel_codification('5fa345ab-680e-45a7-9e4b-01c02740f3a9', formalized).
narrative_ontology:cs_authority_grounding('5fa345ab-680e-45a7-9e4b-01c02740f3a9', extraction).
narrative_ontology:cs_reading_relation('5fa345ab-680e-45a7-9e4b-01c02740f3a9', state_commitment_installation_mechanism__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('5fa345ab-680e-45a7-9e4b-01c02740f3a9', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('5fa345ab-680e-45a7-9e4b-01c02740f3a9', foundational, state_authority_as_sole_legitimator).
narrative_ontology:cs_axiom_status(state_authority_as_sole_legitimator, holdable).
narrative_ontology:cs_axiom_grounding('5fa345ab-680e-45a7-9e4b-01c02740f3a9', state_authority_as_sole_legitimator, conventional).
narrative_ontology:cs_axiom('5fa345ab-680e-45a7-9e4b-01c02740f3a9', foundational, legitimacy_flows_top_down).
narrative_ontology:cs_axiom_status(legitimacy_flows_top_down, holdable).
narrative_ontology:cs_axiom_grounding('5fa345ab-680e-45a7-9e4b-01c02740f3a9', legitimacy_flows_top_down, deontological).
narrative_ontology:cs_reference_frame('5fa345ab-680e-45a7-9e4b-01c02740f3a9', centralized_decree_and_compliance).
narrative_ontology:cs_drift_state('5fa345ab-680e-45a7-9e4b-01c02740f3a9', post_colonial_critique_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5fa345ab-680e-45a7-9e4b-01c02740f3a9', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, state_authority).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, mandate_holding_elites).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, local_communities).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, traditional_institutions).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, displaced_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central government or ruling elite that decrees new commitments (e.g., legal codes, national ideologies, economic systems) and uses its coercive apparatus to enforce their adoption. Benefits from consolidating power and legitimizing its transformative agenda.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Bureaucrats, intellectuals, or military leaders who are aligned with the state authority's transformative vision and gain status, resources, and influence by implementing the new commitments. They are the primary agents of top-down installation.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, mandate_holding_elites, beneficiary,
    powerful, biographical, mobile, national).

% Bear the direct costs of adopting new commitments that often conflict with existing local norms, practices, and social structures. They experience disruption, loss of autonomy, and may face coercion or violence for non-compliance. Their resistance is often localized and fragmented.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, local_communities, payer,
    powerless, generational, trapped, local).

% Religious bodies, tribal councils, or customary legal systems whose authority and legitimacy are directly challenged and undermined by the state's new commitments. They are forced to either adapt, resist, or face suppression, losing their traditional roles and influence.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, traditional_institutions, payer,
    organized, generational, constrained, regional).

% Former power holders (e.g., regional aristocrats, religious leaders) whose status and influence derived from the old order. They are marginalized or suppressed by the new commitments and may attempt to organize resistance or subversion.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, displaced_elites, payer,
    moderate, biographical, constrained, national).

% Analyze the long-term processes of state formation and cultural change, observing the mechanisms by which new commitments are installed and legitimized. They seek to understand the structural forces at play and the differential impacts on various social groups.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to establish a unified legal, administrative, or ideological framework across a diverse territory, replacing fragmented local systems with a single, centrally managed order. This reduces internal transaction costs for the state and enables large-scale collective action (e.g., taxation, conscription).
% TRANSFER_FUNCTION: Transfers legitimacy, authority, and resources from traditional, localized institutions and practices to the central state authority and its aligned elites. It also transfers the costs of adaptation and compliance to local populations.
% ABSENT_VOICES: The voices of local communities and traditional institutions, whose established ways of life are being dismantled, are systematically excluded from the decision-making process. Their dissent is treated as resistance to be overcome, not as legitimate input.
% DISAPPEARANCE_RATIONALE: If the state's capacity for top-down imposition vanished, the new commitments would likely unravel, leading to a resurgence of local autonomy, traditional practices, and potentially fragmented authority structures. The state's legitimacy would collapse, and the social order would revert to a more decentralized form.
% FOUNDING_PROBLEM: The state perceives a problem of fragmentation, disunity, or inefficiency stemming from diverse local commitments, hindering its ability to govern, extract resources, or project power effectively.
% FOUNDING_PROBLEM_CORROBORATION: The state authority consistently frames its actions as necessary to overcome fragmentation and build a modern, unified nation. Historical sociologists corroborate that states often face such challenges, but also note that the 'problem' is often constructed to justify power expansion, with the solution serving the state's interests more than the general welfare.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__exogenous_imposition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__exogenous_imposition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_commitment_installation_mechanism__exogenous_imposition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the new commitments are designed to centralize power and resources, often at the expense of local autonomy and traditional rights. Suppression is also high (0.75) as the state actively enforces adoption through administrative, legal, and sometimes military means, crushing resistance and eliminating alternative commitment structures. Theater ratio is low (0.20) because the state's claims of 'progress' or 'unity' are often genuine justifications for its own power consolidation, not merely performative; the enforcement is real and directly serves the state's agenda. The historical measurements show a steady increase in both extractiveness and suppression as the state consolidates its power over the period.
 *
 * PERSPECTIVAL GAP:
 *   The state authority and mandate-holding elites perceive this as a necessary and beneficial process of modernization and nation-building, a 'rope' or 'scaffold' for progress. Local communities and traditional institutions, however, experience it as a 'snare' that dismantles their way of life and extracts their autonomy. The engine's classification as 'snare' reflects the structural reality from the perspective of the victims, which is amplified by their limited exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_authority and mandate_holding_elites are clear beneficiaries, as the constraint directly enhances their power, legitimacy, and control. Local_communities, traditional_institutions, and displaced_elites are victims, bearing the costs of forced change, loss of autonomy, and suppression of their existing commitments. Their trapped or constrained exit options amplify their directionality towards being targets.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a snare because its primary function is extraction and suppression, not genuine coordination for the benefit of all parties. The 'founding problem' of fragmentation is used as a justification for state expansion, but the solution disproportionately benefits the state while actively harming other groups. The persistence of the constraint is due to the state's coercive power, not its continued utility to all participants. The 'contested' status of the founding problem highlights this divergence: the state claims the problem is live, while victims experience the solution as a new form of oppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exogenous_vs_endogenous_legitimacy,
    'Is the legitimacy of new commitments primarily derived from top-down imposition (exogenous) or from bottom-up adoption and demonstrated utility (endogenous)?',
    'Comparative historical analysis of state-building processes, examining cases where top-down decrees failed without grassroots buy-in versus cases where they succeeded. Longitudinal studies tracking the long-term stability of imposed commitments versus organically adopted ones.',
    'If legitimacy is found to be primarily endogenous, this ''exogenous_imposition_reading'' would be reclassified as a less stable, more fragile ''snare'' or ''piton'' that requires constant, unsustainable suppression. If exogenous imposition is sufficient, the ''snare'' classification would be reinforced, but its long-term viability would be less contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_vs_endogenous_legitimacy, empirical, 'Distinguishing the primary source of legitimacy for new state commitments.').

omega_variable(
    imposition_vs_hybrid_cascade,
    'Does top-down imposition truly bypass grassroots validation, or does it merely initiate a cascade that still requires some form of local acceptance or adaptation to stabilize?',
    'Detailed ethnographic and historical studies of how imposed commitments are received, resisted, and eventually (or never) integrated at the local level. This would test the ''hybrid_cascade_reading'' against the ''exogenous_imposition_reading''.',
    'If a significant ''cascade'' effect requiring local validation is found, this reading''s high suppression and low theater_ratio might be re-evaluated, suggesting a more complex ''tangled_rope'' dynamic where local actors eventually become co-opted or find some benefit, or a ''piton'' if the imposed commitments become purely performative without deep integration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imposition_vs_hybrid_cascade, empirical, 'Assessing the degree to which top-down imposition is truly ''pure'' versus requiring subsequent local integration.').

omega_variable(
    kernel_reading_framing,
    'Is this constraint best understood as a pure ''exogenous imposition'' (this reading), or as a more nuanced ''endogenous climb'' or ''hybrid cascade'' (sibling readings)?',
    'This is a conceptual omega. Resolution depends on the analytical frame chosen by the historical sociologist. The choice of frame emphasizes different causal mechanisms and power dynamics. The ''exogenous_imposition_reading'' emphasizes state power and coercion, while the ''endogenous_climb_reading'' emphasizes bottom-up agency, and the ''hybrid_cascade_reading'' seeks to integrate both.',
    'Adopting a different reading would shift the focus of analysis, potentially reclassifying the constraint from a ''snare'' (this reading) to a ''rope'' or ''tangled_rope'' if more coordination or mutual benefit is emphasized, or a ''piton'' if the commitments are seen as failing to take root.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'The choice of analytical frame for state commitment installation mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__exogenous_imposition_reading, 1800, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1800, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(stat_tr_t1830, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 1830, 0.15).
narrative_ontology:measurement(stat_tr_t1860, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 1860, 0.18).
narrative_ontology:measurement(stat_tr_t1890, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 1890, 0.2).
narrative_ontology:measurement(stat_tr_t1920, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 1920, 0.19).
narrative_ontology:measurement(stat_tr_t1950, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 1950, 0.2).

% Extraction over time
narrative_ontology:measurement(stat_be_t1800, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 1800, 0.55).
narrative_ontology:measurement(stat_be_t1830, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 1830, 0.6).
narrative_ontology:measurement(stat_be_t1860, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 1860, 0.63).
narrative_ontology:measurement(stat_be_t1890, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 1890, 0.65).
narrative_ontology:measurement(stat_be_t1920, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 1920, 0.64).
narrative_ontology:measurement(stat_be_t1950, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 1950, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1800, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 1800, 0.65).
narrative_ontology:measurement(stat_su_t1830, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 1830, 0.7).
narrative_ontology:measurement(stat_su_t1860, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 1860, 0.73).
narrative_ontology:measurement(stat_su_t1890, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 1890, 0.75).
narrative_ontology:measurement(stat_su_t1920, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 1920, 0.74).
narrative_ontology:measurement(stat_su_t1950, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 1950, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
