% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__precautionary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__precautionary_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: acceptable_risk_for_energy__precautionary_reading
 *   human_readable: Acceptable Risk for Energy (Precautionary Reading): Uncertainty-Driven Policy
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   Collective decisions about acceptable risk for energy technologies
 *   (nuclear, geothermal, offshore wind, carbon capture storage) require
 *   judgments about failure probabilities and potential harms. The
 *   precautionary reading holds that irreducible uncertainty in probability
 *   estimation itself must be central to policy: we cannot know the true
 *   probability of rare failure modes (reactor meltdown, CO2 leakage from
 *   geological storage, subsurface induced seismicity) because such events
 *   are rare and models are necessarily incomplete. This reading produces a
 *   tangled-rope constraint structure: it coordinates legitimate governance
 *   concerns (ensuring public voice, making uncertainty visible, preventing
 *   false confidence in technical assessment) alongside extraction (delayed
 *   technology deployment, asymmetric burden on proponents, suppression of
 *   voices favoring technology). The constraint exhibits theater-ratio drift:
 *   formal quantitative risk assessment (probability trees, fault trees,
 *   probabilistic safety analysis) becomes increasingly performative as its
 *   underlying uncertainty bounds widen, yet regulatory systems persist in
 *   requiring numerical risk estimates because alternatives lack comparable
 *   rigor. Extractiveness increases over time (0.38 → 0.52) as the tension
 *   between technical assessment and precautionary governance deepens:
 *   proponents invest more in proving safety, governance bodies invest more
 *   in demonstrating due diligence, but neither fully resolves the
 *   irreducible uncertainty that grounds the precautionary reading.
 *   Suppression remains stable and high: exposed populations have limited
 *   capacity to commission independent risk studies or contest expert
 *   assessments, regardless of which reading dominates.
 *
 * KEY AGENTS:
 *   - Exposed Populations (Unknown Failure Modes): Primary victims (powerless/trapped) — cannot exit deployment zones; bear cost of failure modes outside probability estimates. No agency in adoption decisions.
 *   - Local Communities Near Infrastructure: Secondary victims (moderate/constrained) — experience both protection (precautionary framework enables local voice) and constraint (delayed benefits, suppressed pro-technology positions). Organized community responses available.
 *   - Governance Bodies (Precautionary Authority): Primary beneficiary (institutional/arbitrage) — precautionary framework delegates uncertainty management, legitimizes extended deliberation, shifts burden to proponents. Experience it as coordination mechanism.
 *   - Precautionary Coalitions (Environmental NGOs, Public Health Networks): Organized beneficiaries (organized/mobile) — see precautionary framework as temporary scaffold enabling alternative governance capacity. Exit options available; sunset logic applies.
 *   - Energy Technology Proponents: Mixed agent (powerful/constrained) — bear extraction through burden-shifting and deliberation delays; benefit from public legitimacy mechanisms that enable deployment if due diligence succeeds. Constrained but not trapped.
 *   - Technical Risk Assessment Profession: Mixed institutional (institutional/arbitrage) — expanded role for uncertainty quantification (benefit); subordination to social governance (extraction). Arbitrage options available.
 *   - Formal Risk Quantification Rituals: Degraded institutional (institutional/arbitrage) — numerical probability estimates lose authority under precautionary reading because uncertainty bounds undermine point estimates. Persist through regulatory inertia (piton).
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing precautionary governance as immutable law of rationality rather than contingent institutional choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__precautionary_reading, 0.52).
domain_priors:suppression_score(acceptable_risk_for_energy__precautionary_reading, 0.58).
domain_priors:theater_ratio(acceptable_risk_for_energy__precautionary_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__precautionary_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__precautionary_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__precautionary_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__precautionary_reading, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__precautionary_reading, "Acceptable Risk for Energy (Precautionary Reading): Uncertainty-Driven Policy").
narrative_ontology:topic_domain(acceptable_risk_for_energy__precautionary_reading, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__precautionary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__precautionary_reading, '55b97a69-ac13-4093-adf7-7a3dfab869cf').
narrative_ontology:cs_kernel_codification('55b97a69-ac13-4093-adf7-7a3dfab869cf', distributed).
narrative_ontology:cs_authority_grounding('55b97a69-ac13-4093-adf7-7a3dfab869cf', distributed).
narrative_ontology:cs_reading_relation('55b97a69-ac13-4093-adf7-7a3dfab869cf', acceptable_risk_for_energy__expected_value_reading, coexists_with).
narrative_ontology:cs_reading_relation('55b97a69-ac13-4093-adf7-7a3dfab869cf', acceptable_risk_for_energy__catastrophic_tail_reading, influences).
narrative_ontology:cs_axiom('55b97a69-ac13-4093-adf7-7a3dfab869cf', foundational, irreducible_uncertainty_in_probability_space).
narrative_ontology:cs_axiom_status(irreducible_uncertainty_in_probability_space, holdable).
narrative_ontology:cs_axiom_grounding('55b97a69-ac13-4093-adf7-7a3dfab869cf', irreducible_uncertainty_in_probability_space, empirically_contingent).
narrative_ontology:cs_axiom('55b97a69-ac13-4093-adf7-7a3dfab869cf', foundational, burden_reversal_for_safety_proof).
narrative_ontology:cs_axiom_status(burden_reversal_for_safety_proof, holdable).
narrative_ontology:cs_axiom_grounding('55b97a69-ac13-4093-adf7-7a3dfab869cf', burden_reversal_for_safety_proof, deontological).
narrative_ontology:cs_reference_frame('55b97a69-ac13-4093-adf7-7a3dfab869cf', epistemic_humility_governance).
narrative_ontology:cs_drift_state('55b97a69-ac13-4093-adf7-7a3dfab869cf', contemporary_climate_energy_transition, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('55b97a69-ac13-4093-adf7-7a3dfab869cf', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__precautionary_reading, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__precautionary_reading, technology_governance_bodies).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__precautionary_reading, precautionary_coalitions).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__precautionary_reading, exposed_populations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__precautionary_reading, unknown_failure_mode_targets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPOSED POPULATIONS (SNARE) — Cannot exit deployment zones; bear full cost of failure modes that fall outside probability estimates. Irreducible uncertainty in risk assessment translates directly to uncompensated exposure. No exit option except relocation; no voice in technology adoption decisions. Maximum extraction.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__precautionary_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOCAL COMMUNITIES (TANGLED ROPE) — Experience mixed coordination and extraction. The precautionary frame provides some protection (extended deliberation, burden on proponents) but also constrains local benefits (delayed economic development, access to technology). High suppression cost: limited capacity to commission independent risk studies or contest expert assessments. Partial agency through organized community responses.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__precautionary_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GOVERNANCE BODIES / PRECAUTIONARY AUTHORITY (ROPE) — Experiences precautionary framework as coordination mechanism: bundles uncertainty management, social legitimacy, and political risk mitigation into a single decision protocol. Benefits from precautionary burden-shifting (proponents must prove safety, not prove harm). Pure coordination from this perspective — no net extraction, substantial co-benefits.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__precautionary_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PRECAUTIONARY COALITIONS (SCAFFOLD) — Organized actors (environmental NGOs, public health networks, indigenous peoples' organizations) see precautionary framework as temporary support structure for building alternative governance capacity. Low effective extraction because coalitions have agency and mobility. Sunset clause: as alternative risk-assessment mechanisms (local deliberation, Indigenous knowledge integration, community science) mature, reliance on formal precautionary burden-shifting decreases.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__precautionary_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ENERGY TECHNOLOGY PROPONENTS (TANGLED_ROPE) — Experience extraction through burden-of-proof requirement and deliberation delays, but also coordination benefit: precautionary framework enables public participation mechanisms that create legitimacy path. Constrained exit: cannot deploy without addressing uncertainty, but precautionary due diligence, if satisfied, provides social license. Mixed extraction and coordination.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__precautionary_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TECHNICAL RISK ASSESSMENT PROFESSION (TANGLED_ROPE) — Mixed experience. Precautionary reading validates expanded role for uncertainty quantification, scenario analysis, and failure-mode identification (coordination function). But also constrains role by subordinating technical assessment to social governance (extraction function). Arbitrage options available: credentialed analysts can exit to private consulting or other domains.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__precautionary_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: FORMAL RISK QUANTIFICATION (PITON) — Probability estimation itself becomes increasingly performative under precautionary reading. Precise numerical risk estimates (e.g., 10^-6 annual fatality probability) lose authority because irreducible uncertainty in probability space undermines the premise of point estimates. Formal quantification persists through institutional inertia — required by regulation, expected by engineers — despite erosion of its epistemic legitimacy. Theater ratio ≥ 0.70: the ritual of numerical risk assessment continues but signal-to-noise ratio has collapsed.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__precautionary_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FUNDAMENTAL UNCERTAINTY LIMIT (MOUNTAIN) — From a civilizational/universal perspective, irreducible uncertainty in probability estimation is a fundamental feature of knowledge about complex systems: estimates of rare-event probabilities are always underdetermined by available data; tail risks depend on unknown unknowns; model-space exploration is infinite. From this view, the precautionary constraint appears as an immutable law of rationality itself — no decision procedure can overcome epistemic limits. However, the structural data contradicts this: precautionary governance is not inherent to risk; it is a chosen institutional arrangement with identifiable beneficiaries and victims. Engine false-summit detector will reveal this as naturalization.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__precautionary_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__precautionary_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(acceptable_risk_for_energy__precautionary_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(acceptable_risk_for_energy__precautionary_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__precautionary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(acceptable_risk_for_energy__precautionary_reading, TR),
    TR >= 0.70.

:- end_tests(acceptable_risk_for_energy__precautionary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-to-high. The precautionary reading imposes genuine costs on technology proponents (extended deliberation, burden of proof, deployment delays) while benefiting governance institutions (political risk mitigation, legitimacy mechanisms). The extraction is not maximal because (1) coordination benefits are substantial — deliberation, participation, uncertainty transparency genuinely improve governance quality — and (2) some proponents succeed in deployment when precautionary requirements are met. The upward trajectory (0.38 → 0.52) reflects growing tension as formal quantification becomes less credible but regulatory systems depend on it. Suppression (0.58): Moderate-high. Exposed populations face significant barriers to voice (technical expertise asymmetry, limited resources for independent assessment, regulatory access barriers) regardless of which reading dominates, but precautionary reading *institutionalizes* this suppression by elevating governance-body and precautionary-coalition voice above community voice. Theater ratio (0.64): Moderate-high. Formal quantitative risk assessment (fault trees, probabilistic safety analysis, Bayesian updating) becomes increasingly performative as uncertainty bounds widen — precise numerical estimates (e.g., 10^-6 annual fatality probability) lack credibility when the true distribution is unknown. Yet regulatory systems require these estimates, and both proponents and governance bodies invest heavily in numerical risk communication. The theater-ratio drift (0.45 → 0.64) marks the growing gap between ritual performance and epistemic substance.
 *
 * PERSPECTIVAL GAP:
 *   The precautionary reading produces a seven-way perspectival gap. Exposed populations see snare (extraction without coordination benefit or exit option). Local communities see tangled rope (mixed extraction and protection). Governance bodies see rope (pure coordination without self-perceived extraction). Precautionary coalitions see scaffold (temporary support with sunset). Proponents see tangled rope (extraction through burden-shifting, coordination through legitimacy pathways). Technical profession sees tangled rope (expanded role with constrained authority). Formal quantification sees piton (performative persistence despite eroded function). The analytical observer at civilizational scale risks seeing mountain (naturalizing precautionary governance as immutable epistemic law). This perspectival divergence is not noise — it is the constraint's defining feature. Each perspective is structurally coherent from its own position; the gap is not measurement error but genuine difference in structural relationship to the irreducible uncertainty that grounds the precautionary reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The precautionary reading's directionality structure is determined by how each agent's position relates to the burden-of-proof shift and uncertainty-handling mechanisms. Exposed populations have high directionality toward victimhood (d ≈ 0.95): structurally trapped, bearing uncompensated uncertainty exposure, no exit options, no voice in decisions. Governance bodies have low directionality (d ≈ 0.15): institutional position with arbitrage options, benefits from coordination function (legitimacy, risk mitigation), no net extraction. Precautionary coalitions have moderate directionality (d ≈ 0.35): organized actors with mobile options, supportive of precautionary frame but not permanently locked into it. Proponents have moderate-to-high directionality (d ≈ 0.65): bear burden-shifting costs but retain options if requirements are met; constrained but not trapped. Technical profession has moderate directionality (d ≈ 0.55): expanded role and constrained authority create symmetric mixed experience. The derived directionality values feed into the sigmoid f(d) function and scope modifier to produce effective extractiveness chi from the base extractiveness ε.
 *
 * MANDATROPHY ANALYSIS:
 *   The precautionary reading resolves mandatrophy by showing that tangled-rope classification is appropriate: the constraint genuinely coordinates (enables social participation, makes uncertainty visible, prevents false confidence) while genuinely extracting (delays technology deployment, burdens proponents, suppresses technology-favorable voices). Neither coordination nor extraction is theatrical or secondary. The mandatrophy is NOT 'is this really coordination or really extraction?' but 'how do we design governance that maintains coordination benefits while minimizing extraction from powerless agents?' The false summit (analytical observer seeing mountain) is revealing: if precautionary governance were truly an immutable epistemic law, it would appear as mountain to all perspectives, not just the civilizational analytical view. The structural data (identifiable beneficiaries, measurable extraction from specific victims, institutional choice points) shows that precautionary governance is a contingent institutional arrangement, not a law of nature. The engine's false-summit detector will reclassify this perspective to tangled_rope when beneficiary declarations are processed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    uncertainty_quantification_limits,
    'What constitutes ''irreducible'' uncertainty in probability estimation? Where is the boundary between reducible (more data/better models) and irreducible (model-space ambiguity, unknown unknowns)?',
    'Epistemological analysis: comparison of historical risk estimates with observed outcomes; identification of systematic blind spots in model coverage; meta-analysis of how often ''unknown unknowns'' materialize as recognized failure modes post-hoc',
    'If ''irreducible'' is narrow (only unknowable-in-principle): precautionary burden is light, closer to rope. If ''irreducible'' is broad (includes all unobserved tail scenarios): precautionary burden is heavy, closer to snare for proponents. Classification shifts toward snare or scaffold depending on boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uncertainty_quantification_limits, conceptual, 'Definition and scope of irreducible uncertainty in probability space').

omega_variable(
    burden_of_proof_incidence,
    'Who bears the burden of proof: proponents must prove safety, or opponents must prove danger? Does precautionary reading reverse this burden, and is reversal sustainable across pluralistic governance?',
    'Comparative institutional analysis: examination of how precautionary jurisdictions (EU, some Scandinavian countries) implement burden reversal; tracking of litigation and regulatory appeal outcomes; measurement of technology deployment timelines under precautionary vs. permissive regimes',
    'If burden reversal is stable: precautionary reading is durable, governance bodies have genuine coordination benefit. If burden reversal is contested or erodes under pressure: extraction hidden in burden-shifting becomes visible, classification drifts toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(burden_of_proof_incidence, empirical, 'Institutional stability of burden-of-proof reversal under precautionary governance').

omega_variable(
    alternative_risk_assessment_maturation,
    'Do alternative risk-assessment mechanisms (participatory deliberation, Indigenous knowledge systems, community science) actually provide complementary information to technical assessment, or do they primarily add political legitimacy without reducing uncertainty?',
    'Case studies: comparison of technical risk assessments vs. community-identified failure modes for completed technology deployments; measurement of whether alternative mechanisms improved actual safety outcomes vs. only improved perceived legitimacy',
    'If alternative mechanisms improve actual risk knowledge: scaffold sunset is real, precautionary framework transitions to hybrid governance. If alternative mechanisms are primarily legitimacy theater: scaffold is aspirational, extraction mechanism persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_risk_assessment_maturation, empirical, 'Efficacy of alternative risk-assessment mechanisms for reducing actual uncertainty').

omega_variable(
    kernel_reading_boundary,
    'This constraint is one reading of the ''acceptable_risk_for_energy'' kernel. Which reading — precautionary, expected_value, or catastrophic_tail — is grounded in defensible decision theory, and which are grounded in institutional politics?',
    'Axiomatic analysis: each reading rest on different assumptions about decision under deep uncertainty (Maximin, Expected Utility, Robust Satisficing). Examine which axiomatic foundations are logically coherent vs. which embed normative political choices. Historical/comparative analysis: track which readings dominate in which institutional contexts (democratic vs. authoritarian, wealthy vs. resource-constrained, technical vs. populist) to identify political drivers.',
    'If precautionary reading is fully grounded in defensible axiomatics: it coexists as one legitimate approach among three. If precautionary reading embeds specific normative political choices: it influences (but does not foreclose) sibling readings. Classification outcome unaffected, but axiom grounding_type changes from ''instrumental'' to ''deontological''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Axiomatic and political grounding of precautionary reading vs. sibling readings').

omega_variable(
    distributional_justice_and_extraction,
    'Is extraction (from exposed populations) a necessary feature of any collective risk governance, or is it specific to precautionary reading''s burden-shifting architecture?',
    'Comparative institutional analysis: measurement of burden distribution (who bears deliberation costs, who bears delay costs, who bears uncertainty exposure) across precautionary vs. expected_value vs. catastrophic_tail governance regimes. Identify which regime minimizes total extraction vs. merely redistributes it.',
    'If extraction is regime-independent: all three readings produce snare for exposed populations; precautionary reading is only less catastrophic. If extraction is regime-specific: precautionary reading''s burden-shifting creates distributional properties distinct from siblings (e.g., benefits organized governance over individual populations).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_justice_and_extraction, empirical, 'Whether extraction from exposed populations is inherent to collective risk governance or specific to precautionary architecture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__precautionary_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(precautionary_risk_tr_t0, acceptable_risk_for_energy__precautionary_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(precautionary_risk_tr_t10, acceptable_risk_for_energy__precautionary_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(precautionary_risk_tr_t20, acceptable_risk_for_energy__precautionary_reading, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(precautionary_risk_be_t0, acceptable_risk_for_energy__precautionary_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(precautionary_risk_be_t10, acceptable_risk_for_energy__precautionary_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(precautionary_risk_be_t20, acceptable_risk_for_energy__precautionary_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(precautionary_risk_su_t0, acceptable_risk_for_energy__precautionary_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(precautionary_risk_su_t10, acceptable_risk_for_energy__precautionary_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(precautionary_risk_su_t20, acceptable_risk_for_energy__precautionary_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__precautionary_reading, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__precautionary_reading, acceptable_risk_for_energy__expected_value_reading).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__precautionary_reading, acceptable_risk_for_energy__catastrophic_tail_reading).

% DUAL FORMULATION NOTE:
% The 'acceptable_risk_for_energy' kernel generates three structurally distinct constraints corresponding to three competing readings. This file instantiates the precautionary reading (ε=0.52, tangled_rope). The expected-value reading and catastrophic-tail reading are separate constraint stories with different ε values, different victim/beneficiary structures, and different classification types. All three stories are linked via network.affects_constraints to represent the kernel contest. The precautionary reading influences but does not foreclose the siblings: precautionary governance can coexist with expected-value analysis (as complementary decision-making tools) and can coexist with catastrophic-tail protection (as compatible risk governance objectives). The readings differ in their core axioms about which decision principle should dominate policy, not in their empirical claims about risk.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__precautionary_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
