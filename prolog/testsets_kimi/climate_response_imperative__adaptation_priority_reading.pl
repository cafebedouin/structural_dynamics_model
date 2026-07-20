% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__adaptation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__adaptation_priority_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: climate_response_imperative__adaptation_priority_reading
 *   human_readable: Adaptation-Priority Climate Response Reading
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This constraint instantiates the adaptation_priority_reading of the
 *   contested climate_response_imperative kernel. It frames international
 *   climate action as primarily resilience-building and damage reduction in
 *   exposed regions, treating mitigation as aspirational and deferred. Under
 *   this reading, the legitimate climate response is organized through
 *   existing multilateral financial architectureâloans, insurance
 *   mechanisms, and resilience bondsâchanneled to developing nations. The
 *   structural delta is that present-day developing nations enter the victim
 *   set: they face immediate capital requirements for adaptation
 *   infrastructure that they cannot meet without incurring unsustainable
 *   debt, creating a vicious circle in which the populations least
 *   responsible for historical emissions bear the highest costs. Wealthy
 *   nations and private financial actors benefit from deferred mitigation
 *   obligations and investment opportunities, while future generations and
 *   mitigation advocates are structurally excluded. The constraint is
 *   authored as a tangled_rope: it carries a genuine coordination function
 *   (near-term harm reduction) but operates through asymmetric extraction
 *   (debt-financed adaptation that reproduces global inequalities).
 *
 * KEY AGENTS:
 *   - wealthy_nations: Primary agenda-setter and beneficiary (institutional/arbitrage/global) â defines finance rules, avoids mitigation liability
 *   - multilateral_development_banks: Agenda-setter (institutional/constrained/global) â administers loans and conditionalities
 *   - private_climate_finance: Beneficiary (powerful/mobile/global) â captures returns on adaptation investment vehicles
 *   - developing_nations: Primary payer/victim (powerless/constrained/national) â bears adaptation costs via debt
 *   - future_generations: Excluded non-agent (powerless/trapped/universal) â bears deferred mitigation costs
 *   - mitigation_advocacy_coalition: Excluded voice (organized/constrained/global) â marginalized by priority frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, 0.72).
domain_priors:suppression_score(climate_response_imperative__adaptation_priority_reading, 0.68).
domain_priors:theater_ratio(climate_response_imperative__adaptation_priority_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__adaptation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__adaptation_priority_reading, "Adaptation-Priority Climate Response Reading").
narrative_ontology:topic_domain(climate_response_imperative__adaptation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__adaptation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__adaptation_priority_reading, '3cfcad65-5958-44d8-b86d-a5db74268d9e').
narrative_ontology:cs_kernel_codification('3cfcad65-5958-44d8-b86d-a5db74268d9e', distributed).
narrative_ontology:cs_authority_grounding('3cfcad65-5958-44d8-b86d-a5db74268d9e', distributed).
narrative_ontology:cs_reading_relation('3cfcad65-5958-44d8-b86d-a5db74268d9e', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('3cfcad65-5958-44d8-b86d-a5db74268d9e', climate_response_imperative__degrowth_reading, influences).
narrative_ontology:cs_axiom('3cfcad65-5958-44d8-b86d-a5db74268d9e', foundational, adaptation_immediacy_priority).
narrative_ontology:cs_axiom_status(adaptation_immediacy_priority, holdable).
narrative_ontology:cs_axiom_grounding('3cfcad65-5958-44d8-b86d-a5db74268d9e', adaptation_immediacy_priority, instrumental).
narrative_ontology:cs_axiom('3cfcad65-5958-44d8-b86d-a5db74268d9e', foundational, climate_finance_loan_architecture).
narrative_ontology:cs_axiom_status(climate_finance_loan_architecture, holdable).
narrative_ontology:cs_axiom_grounding('3cfcad65-5958-44d8-b86d-a5db74268d9e', climate_finance_loan_architecture, conventional).
narrative_ontology:cs_reference_frame('3cfcad65-5958-44d8-b86d-a5db74268d9e', adaptation_first_capital_flows).
narrative_ontology:cs_drift_state('3cfcad65-5958-44d8-b86d-a5db74268d9e', contemporary_climate_finance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3cfcad65-5958-44d8-b86d-a5db74268d9e', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__adaptation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, wealthy_nations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, private_climate_finance).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, developing_nations).
narrative_ontology:constraint_vindicates(climate_response_imperative__adaptation_priority_reading, adaptation_priority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the international climate finance architecture through UNFCCC negotiations and bilateral agreements, set the terms of what counts as climate finance, and benefit from deferred mitigation obligations and continued emissions-intensive economic activity. They enforce the framing that adaptation is urgent while mitigation remains aspirational.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, wealthy_nations, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__adaptation_priority_reading, wealthy_nations, beneficiary).

% Administer climate finance flows to developing nations, originate adaptation loans, enforce policy conditionalities, and set technical eligibility standards for resilience projects. Their institutional mandate and survival depend on continued capital intermediation.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, multilateral_development_banks, agenda_setter,
    institutional, generational, constrained, global).

% Invest in adaptation bonds, resilience infrastructure funds, and climate insurance instruments targeting developing regions. They collect returns on capital deployed under the climate finance umbrella and benefit from public guarantees that de-risk their investments.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, private_climate_finance, beneficiary,
    powerful, biographical, mobile, global).

% Face immediate climate damages requiring urgent capital investment. Must accept loan-based adaptation finance on terms set by MDBs and wealthy nations, incurring debt service and policy conditionalities that constrain their development pathways. Alternatives such as grant-based reparations are structurally unavailable.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, developing_nations, payer,
    powerless, immediate, constrained, national).

% Bear the deferred costs of unmitigated emissions and the amplified climate impacts resulting from the deprioritization of mitigation. They are unrepresented in contemporary financial and political frameworks and cannot exit the temporal arrangement.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, future_generations, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_imperative__adaptation_priority_reading, future_generations).

% Argue that mitigation cannot be treated as aspirational without catastrophic outcomes, and that adaptation finance without parallel emissions reduction is a temporary palliative. Their policy preferences are formally acknowledged in UNFCCC text but structurally marginalized in budget allocations, loan structures, and negotiating agendas.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, mitigation_advocacy_coalition, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__adaptation_priority_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_response_imperative__adaptation_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Climate change is already causing unavoidable damages in exposed regions; the arrangement coordinates international financial flows to build immediate resilience (sea walls, early warning systems, drought-resistant agriculture) since mitigation benefits are too delayed to protect current populations.
% TRANSFER_FUNCTION: Moves capital from wealthy nations and international financial institutions to developing nations in the form of loans, insurance, and resilience bonds, while simultaneously transferring the deferred cost of unmitigated emissions to future generations and extracting debt service back to creditors.
% ABSENT_VOICES: Future generations who inherit the deferred mitigation gap; developing nation civil society groups demanding grant-based reparations rather than loans; degrowth and mitigation-priority advocates who argue that the adaptation-first framing perpetuates the underlying growth-and-emission trajectory.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority imperative vanished, developing nations would no longer be locked into debt-financed resilience pathways; capital would reorient toward mitigation or reparative grants; wealthy nations would lose the legitimizing frame that permits continued emissions; and the current architecture of multilateral climate finance would lose its primary justification.
% FOUNDING_PROBLEM: Developing nations face immediate, life-threatening climate damages that require urgent capital investment in resilience infrastructure, and the international community needed a mechanism to channel finance to these exposures before mitigation effects could materialize.
% FOUNDING_PROBLEM_CORROBORATION: Developing nations corroborate the immediate damages but dispute that loan-based finance is the appropriate solution, pointing to historical responsibility. Independent climate justice movements and critical development economists outside the benefiting parties attest that the founding problem is genuine but the chosen mechanism reproduces colonial extraction; climate scientists corroborate the urgency of both adaptation and mitigation but note that deferring mitigation amplifies future damages.
narrative_ontology:disappearance_verdict(climate_response_imperative__adaptation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__adaptation_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__adaptation_priority_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_imperative__adaptation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__adaptation_priority_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__adaptation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__adaptation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the finance mechanism extracts debt service and policy conditionalities from developing nations while deferring the mitigation burden. Suppression (0.68) reflects the active suppression of reparative justice frames and grant-based finance alternatives through the institutionalization of loan-based climate finance. Theater_ratio (0.45) captures the growing performative dimension: pledges are repeatedly unmet, and 'adaptation' is used to legitimate continued emissions. Accessibility_collapse (0.55) is moderate because alternative framings (climate reparations, mitigation priority) are still intellectually available but politically marginalized. Resistance (0.48) reflects organized but structurally weak opposition from developing nation blocs and civil society.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (wealthy nations, MDBs) experience the constraint as necessary coordination to channel scarce capital to urgent needs. The payer seat (developing nations) experiences it as an extractive debt trap that locks in subordinate development pathways. The excluded seats (future generations, mitigation advocates) experience it as a deferred catastrophe. The engine should compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy nations and private climate finance are declared beneficiaries: they collect deferred mitigation space and financial returns, placing their derived directionality near the subsidy end. Developing nations are declared victims/payers: they bear capital costs and debt service, placing their directionality near the full-target end. Future generations, though not agents, are structurally targeted by the deferred mitigation failure. The exit asymmetry is severe: wealthy nations can arbitrage between frames; developing nations are constrained by immediate need and lack of alternative capital.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by preserving the genuine coordination function: adaptation finance does reduce near-term mortality and damage. Without acknowledging that function, the constraint would read as a pure snare. However, the coupling of that function to loan-based, conditional finance that falls on least-responsible parties establishes the asymmetric extraction that makes it a tangled_rope rather than a rope. The founding problemâimmediate climate damagesâis live, but the arrangement's persistence is increasingly justified by institutional inertia and beneficiary interest rather than by optimal problem-solving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the adaptation_priority_reading structurally dominant because it solves the most urgent coordination problem, or because it serves the interests of the parties best positioned to define the kernel?',
    'Comparative policy-trace analysis: examine budget allocations, debt flows, and emissions trajectories under regimes where each reading holds formal dominance.',
    'If dominance tracks creditor benefit rather than damage reduction, the constraint''s classification as tangled_rope shifts toward snare; if dominance tracks genuine harm reduction, the coordination function is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contested kernel dominance between adaptation, mitigation, and degrowth readings.').

omega_variable(
    finance_extraction_boundary,
    'Does the capital flowing to developing nations under this reading produce net resilience value that exceeds the debt burden and conditionalities imposed?',
    'Independent debt-sustainability and adaptation-outcome audits comparing grant-equivalent value of projects against service payments and policy constraints.',
    'If debt service exceeds adaptation benefit, the extraction component dominates and the constraint computes closer to snare; if resilience value exceeds cost, the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finance_extraction_boundary, empirical, 'Whether adaptation finance is net-extractive or net-coordinating for recipient nations.').

omega_variable(
    mitigation_deferral_legitimacy,
    'Does the ''aspirational'' framing of mitigation function as a genuine acknowledgment of political limits, or as a legitimizing narrative for continued emissions by wealthy nations?',
    'Correlate emissions trajectories of wealthy nations with rhetorical emphasis on adaptation priority in NDC revisions and international pledges.',
    'If emissions rise while adaptation is prioritized, the aspirational frame operates as cover for extraction, increasing theater_ratio and effective suppression of mitigation alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_deferral_legitimacy, conceptual, 'Whether mitigation deferral is political realism or legitimizing extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__adaptation_priority_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__adaptation_priority_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t7, climate_response_imperative__adaptation_priority_reading, theater_ratio, 7, 0.28).
narrative_ontology:measurement(clim_tr_t14, climate_response_imperative__adaptation_priority_reading, theater_ratio, 14, 0.35).
narrative_ontology:measurement(clim_tr_t21, climate_response_imperative__adaptation_priority_reading, theater_ratio, 21, 0.4).
narrative_ontology:measurement(clim_tr_t28, climate_response_imperative__adaptation_priority_reading, theater_ratio, 28, 0.43).
narrative_ontology:measurement(clim_tr_t35, climate_response_imperative__adaptation_priority_reading, theater_ratio, 35, 0.45).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_be_t7, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 7, 0.45).
narrative_ontology:measurement(clim_be_t14, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 14, 0.55).
narrative_ontology:measurement(clim_be_t21, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 21, 0.62).
narrative_ontology:measurement(clim_be_t28, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 28, 0.68).
narrative_ontology:measurement(clim_be_t35, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 35, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t7, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 7, 0.42).
narrative_ontology:measurement(clim_su_t14, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 14, 0.52).
narrative_ontology:measurement(clim_su_t21, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 21, 0.6).
narrative_ontology:measurement(clim_su_t28, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 28, 0.65).
narrative_ontology:measurement(clim_su_t35, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 35, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__adaptation_priority_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_response_imperative kernel, decomposed from the colloquial label 'climate response' into structurally distinct commitments per the Îµ-invariance principle. Each reading carries a different Îµ, stakeholder structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
