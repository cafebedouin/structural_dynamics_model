% ============================================================================
% CONSTRAINT STORY: precautionary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_precautionary_reading, []).

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
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: precautionary_reading
 *   human_readable: Precautionary Reading: Uncertainty-Bounded Risk Assessment in Energy Policy
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   The precautionary reading frames acceptable risk decisions as
 *   fundamentally constrained by irreducible uncertainty in probability
 *   estimation itself. When the true distribution of failure modes is unknown
 *   — not merely unmeasured but in principle unmeasurable without deploying
 *   the technology and observing outcomes — the reading holds that governance
 *   must require burden-of-proof on technology proponents and maintain social
 *   veto mechanisms beyond technical risk assessment. This reading privileges
 *   protection of exposed populations over deployment speed and treats
 *   uncertainty accounting as a matter of democratic accountability rather
 *   than pure statistical inference. The constraint exhibits genuine
 *   coordination (regulatory agencies, advocacy coalitions, affected
 *   populations) alongside extraction (technology developers bear proof
 *   burden and capital cost; cost-bearing energy consumers experience slower
 *   deployment or higher energy prices). The theater ratio has increased over
 *   the measurement interval (0.28 → 0.45) as precautionary procedures have
 *   proliferated without corresponding epistemic advance, raising the piton
 *   risk: institutional theater substituting for genuine uncertainty
 *   reduction. The constraint is one reading of the kernel
 *   'acceptable_risk_for_energy,' competing with expected-value and
 *   catastrophic-tail readings that distribute burden and victims
 *   differently.
 *
 * KEY AGENTS:
 *   - Populations Exposed to Unknown Failure Modes (powerless/trapped): Victims bearing full cost of unforeseeable harms; cannot exit proximity; cannot demand proof because proof-impossibility is the constraint's core claim.
 *   - Technology Developers and Financiers (powerful/mobile): Primary targets of the burden-shifting; experience increased approval time, capital cost, and regulatory arbitrage closure; possess exit options (jurisdictional arbitrage, technology switching).
 *   - Regulatory Agencies (moderate/constrained): Coordinate public-interest review while constrained by mandates, political pressure, and resource limitations. Asymmetric legal liability: sued for harms they fail to prevent, not for harms caused by excessive precaution.
 *   - Environmental Advocacy Coalition (organized/constrained): Beneficiary of the constraint; pure-coordination function enables aggregation of otherwise-powerless populations into organized voice.
 *   - Energy Consumers (moderate/constrained): Secondary victims; bear cost of delayed deployment or premium pricing during transition; benefit from precautionary risk reduction.
 *   - Transitional Governance Institutions (institutional/arbitrage): Coordinate the 15-30 year learning period; possess sunset clause and arbitrage options; low effective extraction.
 *   - Analytical Observer (analytical/analytical): Risks naturalizing contingent institutional reading as epistemically inevitable.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(precautionary_reading, 0.52).
domain_priors:suppression_score(precautionary_reading, 0.68).
domain_priors:theater_ratio(precautionary_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(precautionary_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(precautionary_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(precautionary_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(precautionary_reading, tangled_rope).
narrative_ontology:human_readable(precautionary_reading, "Precautionary Reading: Uncertainty-Bounded Risk Assessment in Energy Policy").
narrative_ontology:topic_domain(precautionary_reading, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(precautionary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(precautionary_reading, formalized).
narrative_ontology:cs_authority_grounding(precautionary_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(precautionary_reading).
narrative_ontology:cs_kernel_id(precautionary_reading, acceptable_risk_for_energy).
narrative_ontology:cs_reading_relation(precautionary_reading, expected_value_reading, coexists_with).
narrative_ontology:cs_reading_relation(precautionary_reading, catastrophic_tail_reading, coexists_with).
narrative_ontology:cs_axiom(precautionary_reading, foundational, irreducible_uncertainty_demands_burden_shift).
narrative_ontology:cs_axiom_status(irreducible_uncertainty_demands_burden_shift, holdable).
narrative_ontology:cs_axiom_grounding(precautionary_reading, irreducible_uncertainty_demands_burden_shift, empirically_contingent).
narrative_ontology:cs_axiom(precautionary_reading, foundational, affected_populations_require_democratic_veto).
narrative_ontology:cs_axiom_status(affected_populations_require_democratic_veto, holdable).
narrative_ontology:cs_axiom_grounding(precautionary_reading, affected_populations_require_democratic_veto, deontological).
narrative_ontology:cs_reference_frame(precautionary_reading, uncertainty_bounded_risk_governance).
narrative_ontology:cs_drift_state(precautionary_reading, contemporary_climate_energy_transition, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(precautionary_reading, populations_with_veto_rights).
narrative_ontology:constraint_beneficiary(precautionary_reading, regulatory_agencies).
narrative_ontology:constraint_beneficiary(precautionary_reading, environmental_advocacy_groups).
narrative_ontology:constraint_victim(precautionary_reading, technology_developers).
narrative_ontology:constraint_victim(precautionary_reading, rapid_deployment_actors).
narrative_ontology:constraint_victim(precautionary_reading, energy_consumers_bearing_implementation_costs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPOSED POPULATIONS (SNARE) — Cannot exit proximity to energy infrastructure; bear full cost of tail-risk failures. Unknown failure modes mean consent is structurally impossible. Maximum suppression: inability to relocate, inability to demand proof of safety, no compensation structure for unforeseeable harms. Generational scope because the harms (radiation, aquifer contamination, ecosystem collapse) persist across lifetimes.
constraint_indexing:constraint_classification(precautionary_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGULATORY AGENCIES (TANGLED ROPE) — Coordinate public-interest review of technology safety (genuine coordination function) while constrained by political pressure to approve development, jurisdictional fragmentation, and resource limitations. Extraction occurs through: (a) mandate expansion without corresponding funding, (b) legal liability for harms they fail to prevent, (c) asymmetric timing (must ban before evidence of harm, absorbs cost of false positives). Beneficiaries exist: environmental groups and exposed populations benefit from precautionary review. Victims exist: same agencies bear enforcement cost.
constraint_indexing:constraint_classification(precautionary_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENVIRONMENTAL ADVOCACY COALITION (ROPE) — Pure coordination: the precautionary reading enables collective action by populations that would otherwise face isolation and information asymmetry. The constraint functions to aggregate otherwise-powerless actors into an organized counterweight to developer interests. Low extraction from coalition's perspective — the mechanism solves the problem it was designed for (coordinating distributed populations into a voice). Suppression remains high for underlying populations, but the coalition itself experiences low suppression (access to networks, legal standing, expertise).
constraint_indexing:constraint_classification(precautionary_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: TECHNOLOGY DEVELOPERS (SNARE) — Experience the precautionary constraint as extraction: proof burden shifts to the developer; approval timeline extends; capital cost rises; regulatory arbitrage closes (cannot exit to jurisdictions without precaution norms). The constraint suppresses alternatives (fast-track approval, self-regulation, limited liability structures). However: high power + mobile exit options = developers can exit to sympathetic jurisdictions or switch technology types. Snare classification reflects that the precautionary constraint is *designed* to suppress and extract from this agent, even though that agent's structural power prevents it from being a maximum-suppression snare.
constraint_indexing:constraint_classification(precautionary_reading, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TRANSITIONAL GOVERNANCE FRAMEWORK (SCAFFOLD) — The precautionary reading functions as temporary coordination for the 15-30 year transition period while monitoring infrastructure matures, alternative technologies develop, and risk models improve. Sunset clause: as epistemic uncertainty bounds narrow (via empirical monitoring, model improvement, alternative energy maturation), the burden of proof can shift back toward expected-value reasoning. Effective extraction is low (χ ≈ 0.25) because the beneficiary (institutional governance) has arbitrage options — can transition to different risk frameworks — and sunset is built in.
constraint_indexing:constraint_classification(precautionary_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: PRECAUTION AS INSTITUTIONAL THEATER (PITON) — At civilizational scale, precautionary review can become purely performative: procedures multiply without improving epistemic rigor; uncertainty estimates are guessed rather than derived; public trust in assessment erodes while approval timelines extend without corresponding knowledge gains. Theater ratio (0.45 for this constraint) reflects genuine functional content — precautionary review does produce information — but the institutional tendency is toward decay: 'We follow precautionary procedure' substitutes for 'We have reduced irreducible uncertainty.' The degradation happens when precaution operates in isolation from the expected-value reading (cannot compare risk-benefit), creating bureaucratic boundary maintenance rather than genuine safety improvement.
constraint_indexing:constraint_classification(precautionary_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal perspective, irreducible uncertainty in probability estimation is a limit of epistemic access itself — cannot be 'solved,' only acknowledged and bounded. Any risk governance system must account for this, making precautionary accounting a structural feature of rational policy, not a contingent institutional choice. However, the structural data contradicts this classification: identifiable beneficiaries (advocacy groups, regulatory agencies) and victims (developers, cost-bearing populations) exist, revealing that 'irreducible uncertainty' is being used to justify particular institutional arrangements. The false summit detector will fire.
constraint_indexing:constraint_classification(precautionary_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(precautionary_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(precautionary_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(precautionary_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(precautionary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(precautionary_reading, TR),
    TR >= 0.70.

:- end_tests(precautionary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The precautionary reading extracts from technology developers through burden-shifting and increased capital cost, and from cost-bearing consumers through deployment delays. But the extraction is not maximal because (a) the coordination function is genuine (affected populations DO need governance mechanisms), (b) burden-shifting reflects a defensible epistemic position (unknown unknowns are real), and (c) the developers retain exit options (jurisdictional arbitrage, technology switching). The reading genuinely solves a coordination problem — powerless populations need veto rights — alongside its extraction mechanism. Suppression (0.68): High. Significant barriers to technology deployment include: mandatory precautionary assessment delays (3-7 years standard), shifted proof burden, legal liability structures favoring caution, social license requirements, and regulatory fragmentation. But suppression is not total: arbitrage jurisdictions exist, some technologies achieve precautionary approval, and cost-benefit analyses can sometimes override precaution. Theater ratio (0.45): Moderate-low. Precautionary assessment produces real information: monitoring data, uncertainty bounds, identification of previously-unknown failure modes. But the institutional tendency toward theater is growing (0.28 → 0.45): procedures multiply without improving epistemic rigor, 'following precautionary procedure' substitutes for 'reducing uncertainty,' and public trust erodes as approval timelines extend.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. The exposed populations see an essential veto mechanism (snare classification reflects their powerlessness). The developers see pure extraction (snare with their framing). The advocacy coalition sees coordination enabling otherwise-impossible collective voice (rope). The regulatory agencies see a mixed system with asymmetric liability (tangled rope). The institutional governance view sees a temporary framework with a sunset (scaffold). The civilizational analytical view risks naturalizing a contingent institutional choice as epistemically inevitable (false summit mountain). The piton classification reveals institutional theater risk. The perspectival gap traces to different answers to a foundational question: Does uncertainty in probability estimation demand precautionary governance structures, or does it demand different uncertainty quantification methods within expected-value frameworks? The precautionary reading says the former; the expected-value reading says the latter; the catastrophic-tail reading says uncertainty matters most in tail events, not mean estimates.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from the agent's structural relationship to the extraction flow. Technology developers (d ≈ 0.85) experience maximum extraction despite high power because the precautionary constraint is *designed* to suppress their interests; they experience the constraint as snare. Regulatory agencies (d ≈ 0.55) split the difference: they implement precaution (beneficiary side) while absorbing enforcement liability (victim side); tangled rope. Environmental advocacy coalitions (d ≈ 0.20) are beneficiaries with constrained exit — the constraint exists to amplify their voice; rope. Exposed populations (d ≈ 0.95) are maximally targeted by the harm risk itself, though the precautionary reading attempts to protect them; snare. The constraint's claimed beneficiaries (advocacy groups, regulatory agencies) have moderate to low d values; the claimed victims (developers) have high d values, consistent with the tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the precautionary reading is a *reading* of the acceptable-risk kernel, not a natural law. The classification (tangled rope) reflects the specific institutional arrangement this reading instantiates, not a claim that precaution is epistemically required or that the classification is universal across contexts. The false summit risk (analytical observer naturalizing contingent institutional choice as inevitable) is documented in the mountain perspective and addressed by the kernel_framing_choice omega. The constraint resolves mandatrophy by showing that apparent universality of 'irreducible uncertainty accounting' actually depends on reading choice: the expected-value reading acknowledges uncertainty but does not shift burden of proof; the catastrophic-tail reading focuses uncertainty on tail events, not mean estimates. The precautionary reading's special contribution is the social veto mechanism (burden-shifting + democratic governance), which is justified by specific value commitments about whose harm matters most, not by epistemics alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    uncertainty_irreducibility_threshold,
    'At what confidence level does uncertainty become ''irreducible'' (unmeasurable in principle) versus ''merely difficult'' (measurable with more data/time)?',
    'Bayesian calibration studies: compare precautionary assessments that claimed irreducible uncertainty against post-deployment actual outcomes; measure entropy reduction as monitoring data accumulates',
    'If threshold ≤ 70% confidence: precautionary approach is justified; most risk assessments contain genuine unknowns. If threshold ≥ 95% confidence: ''irreducible uncertainty'' becomes a cover story; empirical data typically resolves claimed unknowns. Classification shifts from Tangled Rope to Snare if threshold effect is above 90%.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uncertainty_irreducibility_threshold, empirical, 'Threshold distinguishing irreducible from merely-difficult uncertainty').

omega_variable(
    burden_of_proof_asymmetry,
    'Does shifting burden of proof to technology proponents genuinely reflect epistemic rationality, or does it embed a normative preference for status quo that is masked as uncertainty accounting?',
    'Comparative policy analysis: measure approval rates and deployment timelines under precautionary vs expected-value frameworks; track whether burden-shifting produces better outcomes (fewer harms, faster identification of safe technologies) or worse outcomes (beneficial technologies delayed, regulatory capture of incumbent technologies)',
    'If precautionary burden produces measurably better long-term outcomes: embedded normative preference is justified. If burden produces equivalent or worse outcomes while simply delaying decisions: extraction mechanism dominates epistemic function. Victim set may expand to include populations harmed by non-deployment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_of_proof_asymmetry, empirical, 'Whether burden-of-proof asymmetry improves policy outcomes').

omega_variable(
    interdependence_with_expected_value_reading,
    'Can the precautionary reading function as a standalone risk governance framework, or does it necessarily depend on the expected-value reading to provide comparison benchmarks?',
    'Institutional analysis: can regulatory agencies apply precautionary criteria without implicitly evaluating expected values (costs of non-deployment, benefits of alternatives)? Do isolation attempts lead to decision paralysis or arbitrary threshold-setting?',
    'If precaution can function standalone: it is a complete alternative framework. If precaution requires implicit comparison with expected-value: it is a modification to expected-value reasoning, and the two readings coexist rather than coexist_with-ing (i.e., classification should shift toward influenced rather than coexists_with).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interdependence_with_expected_value_reading, conceptual, 'Whether precautionary framework is epistemically self-standing').

omega_variable(
    kernel_reading_framing_choice,
    'Does this constraint instantiate the precautionary reading because the precautionary principle is epistemically sound, or because different stakeholders have fused different value commitments with what appears to be an uncertainty claim?',
    'Discourse analysis: trace the history of ''irreducible uncertainty'' framing in policy debates; identify when uncertainty language masks disagreement about acceptable risk distribution; compare to cases where uncertainty is claimed but burden of proof is not shifted (e.g., medicine, aviation)',
    'If precautionary framing is epistemically justified: this constraint''s classification is correct as written. If precautionary framing is value-laden: the constraint is actually a boundary-maintenance mechanism for a particular reading of ''acceptable risk,'' and the omega documents that this constraint is contingent on that reading''s legitimacy claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_choice, conceptual, 'Whether precautionary reading reflects epistemics or value commitments').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(precautionary_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(precaut_tr_t0, precautionary_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(precaut_tr_t5, precautionary_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(precaut_tr_t10, precautionary_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(precaut_tr_t15, precautionary_reading, theater_ratio, 15, 0.45).

% Extraction over time
narrative_ontology:measurement(precaut_be_t0, precautionary_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(precaut_be_t5, precautionary_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(precaut_be_t10, precautionary_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(precaut_be_t15, precautionary_reading, base_extractiveness, 15, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(precautionary_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(precautionary_reading, expected_value_reading).
narrative_ontology:affects_constraint(precautionary_reading, catastrophic_tail_reading).

% DUAL FORMULATION NOTE:
% The precautionary reading is one of three structurally distinct readings of the 'acceptable_risk_for_energy' kernel. Each reading instantiates a different constraint story with different ε, different beneficiary/victim structures, and different institutional mechanisms. The readings are linked not by causal dependence but by mutual contestation: they are alternative governance frameworks applied to the same domain. All three must be present in the constraint corpus for the kernel's structural ambiguity to be visible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
