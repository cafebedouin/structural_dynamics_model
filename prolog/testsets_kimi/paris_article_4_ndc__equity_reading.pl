% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__equity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__equity_reading, []).

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
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: Paris Article 4 NDC Equity Reading (CBDR Structural Distinctions)
 *   domain: international_climate_governance/treaty_law
 *
 * SUMMARY:
 *   This constraint story captures the equity reading of Paris Agreement
 *   Article 4 NDCs, which interprets Nationally Determined Contributions
 *   through the prism of Common But Differentiated Responsibilities and
 *   Respective Capabilities (CBDR-RC). Under this reading, developed states
 *   assume binding constraints and transfer obligations while developing
 *   states retain policy space and receive support; equity coalitions acquire
 *   veto authority over supranational enforcement design. The kernel is
 *   contested: sovereigntist readings emphasize voluntary national
 *   sovereignty, while supranational readings press for uniform binding
 *   trajectories. This story isolates the equity reading as a structurally
 *   distinct constraint with moderate extractiveness asymmetrically
 *   distributed between North and South.
 *
 * KEY AGENTS:
 *   - developed_states: Primary target (institutional/constrained) â bears binding constraints and transfer obligations
 *   - developing_states: Primary beneficiary (institutional/constrained) â retains policy space and receives finance
 *   - equity_coalitions: Agenda setter (organized/mobile) â administers the CBDR veto over supranational enforcement
 *   - supranational_enforcement_bodies: Secondary institutional actor (institutional/constrained) â enforcement authority is vetoed by equity coalitions
 *   - independent_climate_governance_scholars: Analytical observer (analytical/analytical) â sees the structural asymmetry and regime drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, 0.6).
domain_priors:suppression_score(paris_article_4_ndc__equity_reading, 0.72).
domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__equity_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "Paris Article 4 NDC Equity Reading (CBDR Structural Distinctions)").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "international_climate_governance/treaty_law").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, '0ff0cd51-e60a-47c9-ad35-3316409d8c5e').
narrative_ontology:cs_kernel_codification('0ff0cd51-e60a-47c9-ad35-3316409d8c5e', formalized).
narrative_ontology:cs_authority_grounding('0ff0cd51-e60a-47c9-ad35-3316409d8c5e', lineage).
narrative_ontology:cs_interpretation_layer_present('0ff0cd51-e60a-47c9-ad35-3316409d8c5e').
narrative_ontology:cs_reading_relation('0ff0cd51-e60a-47c9-ad35-3316409d8c5e', paris_article_4_ndc__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ff0cd51-e60a-47c9-ad35-3316409d8c5e', paris_article_4_ndc__supranational_reading, influences).
narrative_ontology:cs_axiom('0ff0cd51-e60a-47c9-ad35-3316409d8c5e', foundational, historical_emissions_create_differential_duty).
narrative_ontology:cs_axiom_status(historical_emissions_create_differential_duty, holdable).
narrative_ontology:cs_axiom_grounding('0ff0cd51-e60a-47c9-ad35-3316409d8c5e', historical_emissions_create_differential_duty, conventional).
narrative_ontology:cs_axiom('0ff0cd51-e60a-47c9-ad35-3316409d8c5e', foundational, development_priority_preserves_policy_space).
narrative_ontology:cs_axiom_status(development_priority_preserves_policy_space, holdable).
narrative_ontology:cs_axiom_grounding('0ff0cd51-e60a-47c9-ad35-3316409d8c5e', development_priority_preserves_policy_space, conventional).
narrative_ontology:cs_reference_frame('0ff0cd51-e60a-47c9-ad35-3316409d8c5e', rio_cbdr_historical_responsibility).
narrative_ontology:cs_drift_state('0ff0cd51-e60a-47c9-ad35-3316409d8c5e', contemporary_climate_emergency, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0ff0cd51-e60a-47c9-ad35-3316409d8c5e', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__equity_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, developing_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, equity_coalitions).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the primary obligation to finance mitigation and adaptation in developing states and to adopt binding emissions reductions. Face institutional pressure to increase ambition while major developing emitters retain flexibility. Exit is constrained by treaty ratification, reputational costs, and the collapse of alternative multilateral forums.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_states, payer,
    institutional, generational, constrained, global).

% Retain policy space under the CBDR framework, with NDCs framed as self-determined contributions rather than binding quotas. Receive financial and technological transfers through the Paris architecture. Their continued participation is contingent on the recognition of the development-rights distinction.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developing_states, beneficiary,
    institutional, generational, constrained, global).

% Operate as veto players within UNFCCC negotiations, ensuring that any interpretation of NDCs respects CBDR and blocking supranational enforcement mechanisms that would impose symmetric obligations. Set the terms of what counts as legitimate climate justice and differentiate acceptable from unacceptable ambition.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, equity_coalitions, agenda_setter,
    organized, generational, mobile, global).

% Administer the Paris Agreement transparency and compliance framework but lack authority to impose sanctions or enforce uniform targets because equity coalitions veto any enforcement design that ignores differentiated responsibilities. Their institutional purpose is partially captured by the veto dynamic.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, supranational_enforcement_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Bear the atmospheric consequences of slower collective mitigation when differentiation allows major developing emitters to defer peak emissions. Structurally underrepresented in the veto politics that determine enforcement design, despite rhetorical inclusion in loss-and-damage debates.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, climate_vulnerable_communities, excluded,
    powerless, civilizational, trapped, global).

% Analyze the tension between participation and stringency, documenting how the equity reading preserves treaty regime stability at the cost of asymmetric ambition and delayed peaks in major developing economies.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, independent_climate_governance_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__equity_reading, developing_states).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__equity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a differentiated framework for global climate mitigation that secures broad developing-country participation by accounting for historical emissions disparities and divergent national capacities, avoiding the Kyoto-era collapse of major-emitter coverage.
% TRANSFER_FUNCTION: Moves financial and technological support from developed to developing states; moves the burden of binding near-term constraint and transparency onto developed states while preserving developing state policy autonomy over emission trajectories.
% ABSENT_VOICES: Future generations and populations in highly vulnerable regions who would prioritize maximum uniform binding ambition over procedural equity; fossil-fuel exporters who reject any binding mitigation architecture regardless of differentiation.
% DISAPPEARANCE_RATIONALE: If the CBDR equity reading vanished overnight, the North-South bargain sustaining the Paris Agreement would fracture. Developing states would likely withdraw or default to sovereigntist non-compliance; developed states would abandon transfer obligations; the treaty would revert to either a hollow voluntarist shell or a blocked supranational impasse.
% FOUNDING_PROBLEM: The Kyoto Protocol's symmetric binding targets failed to secure participation from major developing emitters and collapsed. The Paris regime required an interpretation of NDCs that could enroll the global South without demanding immediate symmetric sacrifice.
% FOUNDING_PROBLEM_CORROBORATION: Developing state parties and G77+China equity coalitions attest the problem remains live and justify ongoing differentiation. Independent climate governance scholars, the IPCC, and the IEA provide external corroboration that the emissions geography has shifted dramatically toward major developing economies, contesting the continued applicability of the original North-South binary; this external empirical testimony supports the claim that the founding problem's factual premises have drifted.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__equity_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__equity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__equity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(paris_article_4_ndc__equity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__equity_reading, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__equity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__equity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.60) because the constraint genuinely coordinates global mitigation participation but imposes asymmetric obligations that decouple from current emissions geography. Suppression is high (0.72) because the equity frame actively blocks uniform binding targets and supranational accountability mechanisms through institutional veto. Theater ratio is moderate-high (0.55): COP proceedings perform solidarity and ambition while actual emissions trajectories diverge from NDCs, and the CBDR distinction is ritually invoked to defer harder questions about major developing emitters. Accessibility collapse is 0.50 because the supranational alternative remains visible in discourse but is institutionally blocked. Resistance is 0.65 because developed states actively contest transfer scaling and accountability asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (developed_states) and beneficiary seats (developing_states, equity_coalitions) should compute differently: from the North the arrangement reads as extractive redistribution that delays global peak emissions; from the South the identical structure reads as corrective justice that secures necessary policy space. The agenda-setter seat (equity_coalitions) experiences the constraint as a source of institutional leverage, while the supranational enforcement bodies experience it as an authority cap. The engine captures this divergence from the structural role and exit data rather than from any reconciled narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed_states are declared victims (binding constraints, transfer obligations, constrained exit) and therefore derive high directionality toward the target pole. Developing_states and equity_coalitions are declared beneficiaries (policy space, finance access, veto power, mobile or constrained-but-benefiting exit) and therefore derive low directionality toward the beneficiary pole. The asymmetry is the engine's primary input: the same treaty text produces opposite structural relationships depending on the seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The equity reading prevents mislabeling by preserving the genuine coordination function â without CBDR differentiation, major developing emitters would not have joined Paris, and the Kyoto collapse would repeat. At the same time, it prevents mislabeling as pure coordination by naming the identifiable victim seat (developed_states bearing asymmetric obligations) and the active enforcement mechanism (the equity veto). If the founding emissions geography were uncontested, the reading might approach rope; given the empirical shift of emissions to major developing economies, the asymmetric obligation structure generates extraction that the tangled_rope classification captures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equity_frame_as_justice_or_extraction,
    'Is the CBDR structural distinction in Article 4 a genuine corrective-justice mechanism, or does it function as institutional cover for delaying mitigation in major developing emitters?',
    'Comparative emissions trajectory analysis: if major developing states peak and decline faster under the equity frame than they would under a uniform frame, the distinction is functional justice; if the frame delays peaks beyond cost-optimal pathways, it operates as extraction.',
    'A justice resolution would lower extractiveness and push the constraint toward rope; an extraction resolution would raise extractiveness and push toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_frame_as_justice_or_extraction, empirical, 'Whether CBDR differentiation serves justice or extraction').

omega_variable(
    supranational_veto_as_coordination_or_suppression,
    'Does the equity coalition veto over supranational enforcement preserve legitimate participatory coordination, or does it suppress the compliance architecture necessary for atmospheric stabilization?',
    'Counterfactual institutional design comparison: measure the compliance rate and ambition level of differentiated versus uniform enforcement designs in simulated or historical treaty regimes.',
    'If the veto suppresses effective enforcement, the suppression metric is higher than the structural measure suggests and the constraint leans toward snare; if it preserves essential participation, the coordination function is stronger and the constraint leans toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supranational_veto_as_coordination_or_suppression, conceptual, 'Whether the CBDR veto is coordination or suppression').

omega_variable(
    major_emitter_developing_state_status,
    'Do contemporary emissions and capacity data still support classifying major economies like China as ''developing states'' under the CBDR framework?',
    'Empirical audit of cumulative and current emissions, per-capita income, and technological capacity against the 1992 UNFCCC categories.',
    'If the empirical premise has broken down, the foundational axiom of differential duty is overridden by evidence, increasing the theater ratio and extractiveness as the distinction becomes performative rather than functional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(major_emitter_developing_state_status, empirical, 'Empirical contingency of the developing-state category').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 2015, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paris_equity_tr_t2015, paris_article_4_ndc__equity_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(paris_equity_tr_t2017, paris_article_4_ndc__equity_reading, theater_ratio, 2017, 0.4).
narrative_ontology:measurement(paris_equity_tr_t2019, paris_article_4_ndc__equity_reading, theater_ratio, 2019, 0.45).
narrative_ontology:measurement(paris_equity_tr_t2021, paris_article_4_ndc__equity_reading, theater_ratio, 2021, 0.5).
narrative_ontology:measurement(paris_equity_tr_t2023, paris_article_4_ndc__equity_reading, theater_ratio, 2023, 0.52).
narrative_ontology:measurement(paris_equity_tr_t2024, paris_article_4_ndc__equity_reading, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(paris_equity_be_t2015, paris_article_4_ndc__equity_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(paris_equity_be_t2017, paris_article_4_ndc__equity_reading, base_extractiveness, 2017, 0.48).
narrative_ontology:measurement(paris_equity_be_t2019, paris_article_4_ndc__equity_reading, base_extractiveness, 2019, 0.52).
narrative_ontology:measurement(paris_equity_be_t2021, paris_article_4_ndc__equity_reading, base_extractiveness, 2021, 0.56).
narrative_ontology:measurement(paris_equity_be_t2023, paris_article_4_ndc__equity_reading, base_extractiveness, 2023, 0.58).
narrative_ontology:measurement(paris_equity_be_t2024, paris_article_4_ndc__equity_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(paris_equity_su_t2015, paris_article_4_ndc__equity_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(paris_equity_su_t2017, paris_article_4_ndc__equity_reading, suppression_requirement, 2017, 0.62).
narrative_ontology:measurement(paris_equity_su_t2019, paris_article_4_ndc__equity_reading, suppression_requirement, 2019, 0.65).
narrative_ontology:measurement(paris_equity_su_t2021, paris_article_4_ndc__equity_reading, suppression_requirement, 2021, 0.68).
narrative_ontology:measurement(paris_equity_su_t2023, paris_article_4_ndc__equity_reading, suppression_requirement, 2023, 0.7).
narrative_ontology:measurement(paris_equity_su_t2024, paris_article_4_ndc__equity_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
