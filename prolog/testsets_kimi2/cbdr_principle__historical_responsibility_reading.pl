% ============================================================================
% CONSTRAINT STORY: cbdr_principle__historical_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cbdr_principle__historical_responsibility_reading, []).

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
 *   constraint_id: cbdr_principle__historical_responsibility_reading
 *   human_readable: CBDR Historical Responsibility Binding Obligations
 *   domain: international_climate_governance/treaty_law
 *
 * SUMMARY:
 *   This constraint instantiates the historical_responsibility_reading of the
 *   contested CBDR kernel. Under this reading, Common But Differentiated
 *   Responsibilities requires developed nations to accept binding emissions
 *   reductions proportional to their cumulative historical emissions and to
 *   provide loss-and-damage financing to developing and vulnerable nations.
 *   The constraint is structurally asymmetric: developed nations enter the
 *   victim set as obligated payers, while developing nations exit the victim
 *   set for adaptation finance and instead become beneficiaries. This is one
 *   of two sibling readings; the voluntary_commitment_reading holds that CBDR
 *   requires only voluntary nationally determined contributions with
 *   technology transfer. The metrics are authored independently of the claim:
 *   the constraint is claimed as tangled_rope because it combines a genuine
 *   global coordination function with asymmetric extraction, and because its
 *   persistence requires active enforcement through treaty compliance
 *   machinery.
 *
 * KEY AGENTS:
 *   - Developed nations: Primary payer/victim (institutional/constrained) â bear binding obligations and financial transfers.
 *   - Developing nations: Primary beneficiary (organized/constrained) â receive differentiated treatment and finance.
 *   - Climate-vulnerable nations: Secondary beneficiary (powerless/trapped) â high exposure, negligible power, dependent on principle for resource access.
 *   - UNFCCC/COP process: Agenda setter (institutional/analytical) â administers framework without independent enforcement.
 *   - Fossil fuel exporting nations: Excluded voice (moderate/constrained) â marginalized in the developed/developing framing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, 0.72).
domain_priors:suppression_score(cbdr_principle__historical_responsibility_reading, 0.55).
domain_priors:theater_ratio(cbdr_principle__historical_responsibility_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__historical_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__historical_responsibility_reading, "CBDR Historical Responsibility Binding Obligations").
narrative_ontology:topic_domain(cbdr_principle__historical_responsibility_reading, "international_climate_governance/treaty_law").

domain_priors:requires_active_enforcement(cbdr_principle__historical_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__historical_responsibility_reading, 'f4957b3a-6bfb-4460-8773-08ce748f1d6f').
narrative_ontology:cs_kernel_codification('f4957b3a-6bfb-4460-8773-08ce748f1d6f', formalized).
narrative_ontology:cs_authority_grounding('f4957b3a-6bfb-4460-8773-08ce748f1d6f', lineage).
narrative_ontology:cs_interpretation_layer_present('f4957b3a-6bfb-4460-8773-08ce748f1d6f').
narrative_ontology:cs_reading_relation('f4957b3a-6bfb-4460-8773-08ce748f1d6f', cbdr_principle__voluntary_commitment_reading, influences).
narrative_ontology:cs_axiom('f4957b3a-6bfb-4460-8773-08ce748f1d6f', foundational, historical_emissions_generate_binding_obligation).
narrative_ontology:cs_axiom_status(historical_emissions_generate_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('f4957b3a-6bfb-4460-8773-08ce748f1d6f', historical_emissions_generate_binding_obligation, conventional).
narrative_ontology:cs_axiom('f4957b3a-6bfb-4460-8773-08ce748f1d6f', foundational, loss_and_damage_as_rectification).
narrative_ontology:cs_axiom_status(loss_and_damage_as_rectification, holdable).
narrative_ontology:cs_axiom_grounding('f4957b3a-6bfb-4460-8773-08ce748f1d6f', loss_and_damage_as_rectification, deontological).
narrative_ontology:cs_reference_frame('f4957b3a-6bfb-4460-8773-08ce748f1d6f', rio_historical_asymmetry_framework).
narrative_ontology:cs_drift_state('f4957b3a-6bfb-4460-8773-08ce748f1d6f', post_paris_agreement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f4957b3a-6bfb-4460-8773-08ce748f1d6f', '').
narrative_ontology:cs_kernel_id(cbdr_principle__historical_responsibility_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, developing_nations).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, climate_vulnerable_nations).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear binding emissions reduction obligations indexed to cumulative historical emissions and provide loss and damage financing to vulnerable nations. Treaty withdrawal is legally possible but diplomatically costly and rare; domestic political resistance to transfer obligations remains high.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developed_nations, payer,
    institutional, generational, constrained, global).

% Receive loss and damage and adaptation financing flows justified by developed nations' historical emissions. Accept less stringent mitigation obligations under the principle. Depend on continued finance flows and cannot easily exit the UNFCCC framework without losing access to climate funds and negotiated differentiation.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developing_nations, beneficiary,
    organized, generational, constrained, global).

% Receive loss and damage funds as beneficiaries of the historical responsibility logic. Have negligible historical emissions and high climate exposure. Cannot exit atmospheric impacts; diplomatically dependent on the principle for resource access but lack institutional power to enforce compliance by developed nations.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, climate_vulnerable_nations, beneficiary,
    powerless, generational, trapped, global).

% Administers the treaty framework through which CBDR is interpreted and operationalized. Sets negotiation agendas, compliance reporting frameworks, and operationalizes the loss and damage fund but lacks independent enforcement capacity; authority derives from state consent and consensus decision-making.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, unfccc_cop_process, agenda_setter,
    institutional, generational, analytical, global).

% Would object to binding emissions reductions and climate finance obligations that threaten fossil fuel revenues but are marginalized in the historical responsibility framing, which focuses on developed versus developing responsibility rather than producer versus consumer accountability.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, fossil_fuel_exporting_nations, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cbdr_principle__historical_responsibility_reading, developing_nations).
narrative_ontology:fixing_cost_class(cbdr_principle__historical_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global greenhouse gas mitigation by assigning binding emissions budgets to developed nations in proportion to cumulative historical emissions, while simultaneously coordinating loss and damage finance flows to developing and vulnerable nations.
% TRANSFER_FUNCTION: Moves financial resources and emissions reduction obligations from developed nations to developing and climate-vulnerable nations based on cumulative historical emissions accounting.
% ABSENT_VOICES: Fossil fuel producer interests and developed nation domestic constituencies opposed to binding transfers are structurally sidelined in the historical responsibility framing; future generations are unrepresented in the negotiation architecture.
% DISAPPEARANCE_RATIONALE: If the binding historical-responsibility reading vanished overnight, the financial and mitigation architecture of the UNFCCC would lose its primary equity logic; NDCs would likely shift toward purely voluntary contributions, and loss and damage funding mechanisms would collapse for lack of obligated contributors.
% FOUNDING_PROBLEM: Atmospheric greenhouse gas concentrations driven disproportionately by industrialized country historical emissions, creating both a collective-action need for mitigation and an asymmetry in responsibility for climate harms and adaptation costs borne by vulnerable nations.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports corroborate the historical emissions asymmetry from a scientific seat outside the direct beneficiary set; however, no external non-state party independently attests that the binding-proportionality formula is the necessary or just response, and developed nations contest the framing.
narrative_ontology:disappearance_verdict(cbdr_principle__historical_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__historical_responsibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__historical_responsibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cbdr_principle__historical_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__historical_responsibility_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cbdr_principle__historical_responsibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cbdr_principle__historical_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the reading imposes binding, proportionally indexed obligations and financial transfers on a defined victim set. Suppression (0.55) is moderate: international treaty law lacks a supranational enforcer, but binding targets, reporting, and compliance mechanisms exert real constraining force. Theater_ratio (0.42) reflects the performative dimension of climate diplomacy where pledges often exceed deliveries. Accessibility_collapse (0.50) captures that alternatives (pure voluntarism, market-only) remain visible but are normatively crowded out by the justice framing. Resistance (0.65) reflects sustained developed-nation pushback against binding obligations and loss/damage liability.
 *
 * PERSPECTIVAL GAP:
 *   From the developed-nation seat, the constraint reads as extraction: forced deindustrialization and unidirectional wealth transfer justified by historical accounting. From the developing-nation and vulnerable-nation seats, it reads as coordination: the only fair mechanism for solving a global commons problem caused by others. The engine computes this divergence from the structural data without adjudicating the normative question.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed nations are declared victims/payers with constrained exit options (treaty withdrawal is possible but diplomatically costly), placing their directionality near the target end. Developing and vulnerable nations are declared beneficiaries with constrained or trapped exit, placing their directionality near the beneficiary end. The effective extraction chi is therefore amplified for developed nations and damped for developing nations.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the arrangement as pure extraction (snare) by requiring the genuine coordination function â solving global emissions asymmetry â to be present, which it is. It also prevents mislabeling as pure coordination (rope) by requiring identifiable victims and active enforcement, which the binding-obligation reading supplies. If the climate problem were fully solved or historical emissions fully compensated, the constraint would face mandatrophy; currently the founding problem remains contested but physically live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_responsibility_naturalness,
    'Is the historical cumulative emissions baseline a natural-law foundation for obligation, or a constructed political frame deployed within treaty bargaining?',
    'Comparative legal analysis of whether non-climate treaty regimes use historical cumulative baselines for binding obligations.',
    'If purely constructed, the constraint''s natural-law framing collapses and extraction is reclassified as conventional bargaining; if naturalized, the high extractiveness is read as enforcement of law rather than politics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_responsibility_naturalness, conceptual, 'Whether historical emissions generate natural or constructed obligation.').

omega_variable(
    binding_without_supranational,
    'Can binding obligations in this reading be effective without supranational enforcement, or does the constraint''s actual force depend on voluntary developed-nation compliance?',
    'Empirical tracking of compliance rates with binding targets versus NDCs.',
    'If binding obligations lack enforcement, the suppression metric overstates actual coercive force and effective extraction may be lower than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_without_supranational, empirical, 'Enforcement gap in international treaty obligations.').

omega_variable(
    kernel_reading_sibling_divergence,
    'How would classification change if the voluntary_commitment_reading were adopted as the operative interpretation of the CBDR kernel?',
    'Comparison of the two compiled stories in the constraint family.',
    'The sibling reading removes developed nations from the victim set and eliminates loss/damage extraction entirely, likely computing as rope or low-extraction coordination rather than tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_divergence, conceptual, 'Sibling reading structural delta and classification impact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__historical_responsibility_reading, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_tr_t0, cbdr_principle__historical_responsibility_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cbdr_tr_t7, cbdr_principle__historical_responsibility_reading, theater_ratio, 7, 0.3).
narrative_ontology:measurement(cbdr_tr_t15, cbdr_principle__historical_responsibility_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(cbdr_tr_t25, cbdr_principle__historical_responsibility_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(cbdr_tr_t30, cbdr_principle__historical_responsibility_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(cbdr_tr_t34, cbdr_principle__historical_responsibility_reading, theater_ratio, 34, 0.42).

% Extraction over time
narrative_ontology:measurement(cbdr_be_t0, cbdr_principle__historical_responsibility_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cbdr_be_t7, cbdr_principle__historical_responsibility_reading, base_extractiveness, 7, 0.55).
narrative_ontology:measurement(cbdr_be_t15, cbdr_principle__historical_responsibility_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(cbdr_be_t25, cbdr_principle__historical_responsibility_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(cbdr_be_t30, cbdr_principle__historical_responsibility_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(cbdr_be_t34, cbdr_principle__historical_responsibility_reading, base_extractiveness, 34, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_su_t0, cbdr_principle__historical_responsibility_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cbdr_su_t7, cbdr_principle__historical_responsibility_reading, suppression_requirement, 7, 0.5).
narrative_ontology:measurement(cbdr_su_t15, cbdr_principle__historical_responsibility_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(cbdr_su_t25, cbdr_principle__historical_responsibility_reading, suppression_requirement, 25, 0.35).
narrative_ontology:measurement(cbdr_su_t30, cbdr_principle__historical_responsibility_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(cbdr_su_t34, cbdr_principle__historical_responsibility_reading, suppression_requirement, 34, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__historical_responsibility_reading, resource_allocation).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, cbdr_principle__voluntary_commitment_reading).

% DUAL FORMULATION NOTE:
% The CBDR kernel decomposes into two constraint stories because the same treaty text supports two structurally distinct obligation regimes: binding proportional obligations (this story) versus voluntary contributions (sibling). Their epsilon values differ significantly and they have different victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
