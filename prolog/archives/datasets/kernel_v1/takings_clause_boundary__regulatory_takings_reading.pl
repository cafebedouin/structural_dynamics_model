% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__regulatory_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Doctrine: The Fifth Amendment's Compensation Requirement for Regulations Diminishing Property Value
 *   domain: constitutional_law/property_rights/regulatory_takings
 *
 * SUMMARY:
 *   The regulatory takings reading of the Fifth Amendment's Takings Clause is
 *   ONE interpretation of a contested constitutional kernel: what counts as a
 *   'taking' requiring compensation to the property owner? This reading
 *   asserts that regulations that significantly diminish property value
 *   without physically appropriating the property nevertheless constitute
 *   compensable takings. The reading emerges from cases like Pennsylvania
 *   Coal Co. v. Mahon (1922), which first recognized that regulation can 'go
 *   too far' and become a taking, and is elaborated through the Penn Central
 *   three-factor test (character of government action, extent of interference
 *   with investment-backed expectations, economic impact). This reading
 *   creates a hybrid constraint: it coordinates the boundary between
 *   legitimate regulation and compensable takings (tangled rope coordination
 *   function) while simultaneously imposing extraction on property owners
 *   whose land is subjected to regulations that courts may or may not
 *   recognize as compensable. The measurement trajectory shows increasing
 *   extractiveness (0.35 → 0.52) and theater ratio (0.48 → 0.61) over a
 *   20-year period, indicating that regulatory expansion has outpaced the
 *   compensability doctrine's clarity, creating growing uncertainty and
 *   uncompensated value loss.
 *
 * KEY AGENTS:
 *   - Property Owners with Diminished Value (moderate/constrained): Primary victims — face 60-80% value loss from environmental or land-use regulations; constrained exit (cannot leave jurisdiction without abandoning asset, expensive litigation); victim status drives high d → high chi
 *   - Development Industry (powerful/arbitrage): Secondary beneficiary-victim hybrid — benefits from property rights protection but constrained by regulatory restrictions; arbitrage exit (can litigate, relocate, lobby); mixed extraction experience
 *   - Environmental Protection Beneficiaries & Regulatory Agencies (institutional/arbitrage): Primary beneficiaries — regulations protect ecosystems, public health, environmental goods that markets underprice; arbitrage exit (can adjust regulations, reallocate resources); low or negative chi because they are structurally benefiting from constraint
 *   - Regulatory Reform Coalition (organized/mobile): Organized agents pushing clarification and compensation mechanisms; mobile exit (can litigate, legislate, lobby); see constraint as temporary (scaffold); sunset logic applies as jurisprudence clarifies
 *   - Courts (institutional/arbitrage): Institutional actors enforcing the takings doctrine through Penn Central test; arbitrage exit (can revise doctrine interpretation); perform theater of distinguishing 'regulation' from 'taking'
 *   - Analytical Observer (analytical/analytical): Civilizational-scope observer recognizing the constraint as institutional hybrid with both genuine coordination (boundary-setting) and extractive components (uncompensated value loss)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.52).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.48).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Doctrine: The Fifth Amendment's Compensation Requirement for Regulations Diminishing Property Value").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional_law/property_rights/regulatory_takings").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, '5763f284-820a-40c7-b61c-4900820c9733').
narrative_ontology:cs_kernel_codification('5763f284-820a-40c7-b61c-4900820c9733', fixed_text).
narrative_ontology:cs_authority_grounding('5763f284-820a-40c7-b61c-4900820c9733', lineage).
narrative_ontology:cs_interpretation_layer_present('5763f284-820a-40c7-b61c-4900820c9733').
narrative_ontology:cs_reading_relation('5763f284-820a-40c7-b61c-4900820c9733', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('5763f284-820a-40c7-b61c-4900820c9733', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('5763f284-820a-40c7-b61c-4900820c9733', foundational, severe_value_diminution_triggers_compensation).
narrative_ontology:cs_axiom_status(severe_value_diminution_triggers_compensation, holdable).
narrative_ontology:cs_axiom_grounding('5763f284-820a-40c7-b61c-4900820c9733', severe_value_diminution_triggers_compensation, deontological).
narrative_ontology:cs_axiom('5763f284-820a-40c7-b61c-4900820c9733', foundational, penn_central_ad_hoc_balancing_applicable).
narrative_ontology:cs_axiom_status(penn_central_ad_hoc_balancing_applicable, holdable).
narrative_ontology:cs_axiom_grounding('5763f284-820a-40c7-b61c-4900820c9733', penn_central_ad_hoc_balancing_applicable, instrumental).
narrative_ontology:cs_reference_frame('5763f284-820a-40c7-b61c-4900820c9733', property_right_economic_value_protection).
narrative_ontology:cs_drift_state('5763f284-820a-40c7-b61c-4900820c9733', contemporary_environmental_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5763f284-820a-40c7-b61c-4900820c9733', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, regulatory_agencies).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, environmental_protection_beneficiaries).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, public_health_constituencies).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, property_owners_with_diminished_value).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, land_development_industry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROPERTY OWNER WITH DIMINISHED VALUE (SNARE) — A landowner subject to environmental regulations that reduce their land's market value by 60-80% (e.g., wetland protection, habitat conservation easements) faces extraction without physical takings language to compel compensation. Exit options are highly constrained: cannot exit the jurisdiction without abandoning the asset, cannot easily develop the land, cannot access the nominal compensation mechanism. Experiences maximum effective extraction — the regulation imposes a cost without providing the legal remedy the takings clause theoretically guarantees.
constraint_indexing:constraint_classification(takings_clause_boundary__regulatory_takings_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: DEVELOPMENT INDUSTRY (TANGLED ROPE) — Large-scale development actors benefit from coordination via permitting systems (clarity on what uses are allowed) but face extraction through regulatory restrictions that eliminate profitable projects. Exit options include arbitrage: relocate projects to less-regulated jurisdictions, litigate takings claims (expensive but viable), lobby for variances. Some benefits from the regulatory framework (predictability, property rights protection against squatters); significant costs from restrictions. Mixed extraction and coordination.
constraint_indexing:constraint_classification(takings_clause_boundary__regulatory_takings_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY AGENCIES & ENVIRONMENTAL BENEFICIARIES (ROPE) — Agencies coordinate collective action on environmental protection (wetland preservation, habitat conservation, pollution prevention). Beneficiaries include ecosystems, future generations, public health constituencies. From this perspective, the regulatory takings doctrine is a coordination mechanism: it clarifies which uses are prohibited and provides a uniform baseline for environmental standards. Effective extraction is minimal — these actors experience the regulations as solving coordination problems, not as mechanisms of coercive extraction. Their benefit comes from constraint enforcement, not from capturing property value.
constraint_indexing:constraint_classification(takings_clause_boundary__regulatory_takings_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY REFORM COALITION (SCAFFOLD) — Organized efforts to clarify and strengthen the takings doctrine (courts determining what constitutes a compensable taking, legislative proposals for mitigation banking, property tax adjustments for restricted land, conservation easement compensation) see the current regulatory takings boundary as a temporary problem with a sunset: as jurisprudence clarifies the doctrine, as administrative compensation mechanisms mature (mitigation banking, transferable development rights), the extraction period resolves. Sunset logic: as regulatory takings law clarifies, property owners gain predictability and access to remedies; the current ambiguity phase is temporary.
constraint_indexing:constraint_classification(takings_clause_boundary__regulatory_takings_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: TRADITIONAL POLICE POWERS DOCTRINE (PITON) — The pre-regulatory-takings framework treated regulation as a legitimate exercise of police powers requiring no compensation. This doctrine persists institutionally despite functional degradation: modern courts acknowledge takings doctrine applies, but the police powers framing creates performative debate about whether regulation is 'really' a taking. The doctrine is maintained through inertia and rhetorical appeal to settled law, not because the underlying logic survives scrutiny. Theater ratio high: significant institutional energy spent on distinguishing 'regulation' (no compensation) from 'takings' (compensation required), a distinction that lacks clear operational grounding.
constraint_indexing:constraint_classification(takings_clause_boundary__regulatory_takings_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational analytical position, the regulatory takings doctrine is a structural hybrid: it coordinates the boundary between legitimate regulation and compensable taking (genuine coordination function), while simultaneously extracting from property owners by imposing costs that fall outside the compensation mechanism. The doctrine's extraction is institutional — the state exercises power over property use without full compensation, justified by characterizing the exercise as regulation rather than taking. The constraint is analytically robust (tangled rope), not a natural law, because alternative institutional arrangements (full compensation, transferable development rights, mitigation banking) would resolve the extraction while preserving the coordination function.
constraint_indexing:constraint_classification(takings_clause_boundary__regulatory_takings_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__regulatory_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, TR),
    TR >= 0.70.

:- end_tests(takings_clause_boundary__regulatory_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The regulatory takings reading creates moderate-high extraction through value diminution without guaranteed compensation. The base extraction is not as high as pure expropriation (which would be 0.75+) because the reading acknowledges that some compensation is theoretically available (property owners can litigate, the doctrine recognizes takings claims), but the practical barrier to accessing compensation is severe. The measurements show extraction increasing from 0.35 to 0.52 over 20 years, indicating regulatory expansion outpacing the compensability doctrine's ability to provide clear remedies. Suppression (0.48): Moderate. Property owners face significant barriers to exit (relocation costs, loss of sunk investment, attachment to land, litigation expense) but are not totally trapped. They have constrained exit options (can litigate, can advocate for variance, can sell at depressed value). Suppression increased slightly over the interval (0.40 → 0.48), indicating courts have become more willing to uphold regulations even when value loss is substantial. Theater ratio (0.61): Moderate-high. Significant institutional energy is spent on the Penn Central test (character of action, reasonable expectations, economic impact) but the test is notoriously ad hoc and produces unpredictable results. Courts perform the legitimacy theater of distinguishing 'regulation' (no compensation) from 'taking' (compensation required), but the distinction often turns on framing rather than structural facts. Theater increased from 0.48 to 0.61, indicating the doctrine's performative character has grown as regulations have become more aggressive and courts have struggled to maintain the regulation/taking boundary.
 *
 * PERSPECTIVAL GAP:
 *   The regulatory takings reading produces wide perspectival disagreement. Property owners constrained by regulations that courts do not classify as compensable takings experience snare-like extraction: they bear costs without accessing remedies. The development industry, having greater resources and exit options (litigation, arbitrage), experiences the constraint as tangled rope: some regulations are legitimate coordination (clarity on what uses are allowed), others are uncompensated extraction (restrictions that eliminate profitable projects). Environmental beneficiaries and agencies experience rope (pure coordination with minimal extraction): the regulations solve collective action problems on environmental protection without imposing costs on them. The analytical observer recognizes the constraint as tangled rope at a civilizational scale: the regulatory takings doctrine coordinates the boundary between regulation and compensation while simultaneously failing to provide compensation for many severe value losses. The gap reflects different structural relationships: those with high exit costs experience snare; those with institutional resources experience tangled rope or rope; the observer sees the hybrid nature of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from structural position: property owners with constrained exit and victim status receive high d → high chi (experience maximum effective extraction relative to their power level). Development industry with arbitrage exit and mixed victim-beneficiary status receives moderate d → moderate chi (can exit through litigation, relocation, lobbying). Environmental beneficiaries and agencies with arbitrage exit and beneficiary status receive low d → low or negative chi (structurally benefit from the constraint, do not experience extraction). Analytical observer with analytical power and analytical exit receives d ≈ 0.72 → chi follows the base metrics without modification. The perspectival gap is substantial: property owners experience snare (extraction without compensation mechanism), industry experiences tangled rope (mixed benefit and cost), beneficiaries experience rope (coordination with minimal extraction), and the analytical observer correctly identifies the constraint as tangled rope (genuine coordination plus extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The regulatory takings reading resolves mandatrophy by clarifying that the constraint is genuinely a tangled rope, not a pure rope or pure snare. The coordination function is real: the takings doctrine provides a mechanism for distinguishing legitimate regulation (which protects public goods) from illegitimate taking (which imposes uncompensated costs). The extraction is real: property owners face value loss and uncertain access to remedies. The hybrid character is essential — the constraint would collapse if it were purely coordination (all regulations would be compensable, gridlocking environmental protection) or purely extraction (all regulations would require compensation, eliminating public goods protection). The mandatrophy is resolved by recognizing that both functions must coexist: the doctrine must protect environmental regulation AND protect property owners from severe uncompensated takings. The measurement trajectory shows this tension increasing: as regulations become more aggressive, the extraction component grows (0.35 → 0.52) and the theater ratio rises (0.48 → 0.61), but the coordination function persists. Tangled rope classification is correct and stable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    value_diminution_threshold_ambiguity,
    'At what percentage of value diminution does a regulation constitute a ''taking'' requiring compensation under the Fifth Amendment?',
    'Jurisprudential analysis: Penn Central test application across cases; correlation between percentage value loss and compensation awards; examination of whether courts use a threshold or ad hoc balancing',
    'If threshold exists and is clear (e.g., 70% triggers compensation): property owners gain predictability, snare classification confirmed. If threshold is ad hoc and opaque: extraction is high (snare) because owners cannot predict remedies. Current doctrine produces ad hoc balancing, creating irreducible uncertainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(value_diminution_threshold_ambiguity, empirical, 'Threshold for regulatory takings compensation').

omega_variable(
    regulatory_takings_vs_physical_takings_reading_foreclosure,
    'Does the regulatory takings reading logically foreclose the physical appropriation reading, or can both coexist as valid constitutional interpretations?',
    'Constitutional hermeneutics: examine whether accepting regulatory takings as compensable inherently requires rejecting physical takings doctrine, or whether the two can be held simultaneously in a unified framework',
    'If forecloses: the two readings occupy incompatible interpretive positions. If coexists: different factions can hold different readings without logical contradiction. Current jurisprudence: coexists (some takings claims rest on physical appropriation, others on regulatory diminution, courts recognize both).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_takings_vs_physical_takings_reading_foreclosure, conceptual, 'Whether regulatory and physical takings readings foreclose each other').

omega_variable(
    compensation_mechanism_feasibility,
    'Are administratively feasible compensation mechanisms (mitigation banking, TDRs, property tax adjustments) available to remediate regulatory value diminution without fiscal collapse or regulatory gridlock?',
    'Comparative institutional analysis: examine mitigation banking programs (Clean Water Act), transferable development rights markets (wetlands, coastal zones), property tax abatement programs; measure compensation costs against regulatory benefits',
    'If feasible and scaled: scaffold perspective confirmed, sunset is real. If infeasible (administrative cost too high, market mechanisms fail): tangled rope extraction persists because compensation mechanism is theoretically available but practically unworkable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compensation_mechanism_feasibility, empirical, 'Feasibility of compensation mechanisms for regulatory takings').

omega_variable(
    property_vs_entitlement_framing_ambiguity,
    'Is the regulatory takings doctrine best understood as protecting a property right in existing use, or an entitlement to maximize economic return on land ownership?',
    'Philosophical analysis of property theory: compare rules-based (use right) vs. entitlement-based (income maximization) framings; examine which framing justifies compensation; review alternative property regimes (covenants, easements, residual common-pool rights)',
    'If use-right framing: narrower victim set, regulations affecting marginal uses require no compensation. If entitlement framing: broader victim set, any income reduction from regulation is a taking. This reading instantiates entitlement framing; alternative readings instantiate narrower property framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_vs_entitlement_framing_ambiguity, conceptual, 'Property right vs. economic entitlement framing in takings doctrine').

omega_variable(
    public_trust_exemption_interaction,
    'Does the public trust doctrine (the state holds certain resources in trust for all citizens and can restrict private use without compensation) logically foreclose the regulatory takings doctrine, or can both operate simultaneously?',
    'Constitutional jurisprudence: examine whether public trust claims disable takings claims; analyze cases where public trust and takings doctrines conflict; determine whether a unified framework exists',
    'If forecloses: public trust assertion eliminates takings claims. If coexists: both doctrines can apply to the same regulation, creating contradictory remedies. Current doctrine: tension without resolution (coexists uneasily).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_trust_exemption_interaction, conceptual, 'Interaction between public trust and regulatory takings doctrines').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(takings_reg_theater_t0, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(takings_reg_theater_t10, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(takings_reg_theater_t20, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 20, 0.61).

% Extraction over time
narrative_ontology:measurement(takings_reg_extract_t0, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(takings_reg_extract_t10, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(takings_reg_extract_t20, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(takings_reg_suppress_t0, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(takings_reg_suppress_t10, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(takings_reg_suppress_t20, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__categorical_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, regulatory_capture__environmental_agencies).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, mitigation_banking__ecosystem_services_commodification).

% DUAL FORMULATION NOTE:
% The regulatory takings reading is one of three structurally distinct interpretations of the Fifth Amendment's takings clause, each with different ε values and victim/beneficiary structures. The physical appropriation reading (ε ≈ 0.15, closer to rope) restricts takings to physical occupations. The categorical takings reading (ε ≈ 0.25, closer to mountain) identifies automatic takings categories. The regulatory takings reading (ε ≈ 0.52, tangled rope) introduces ad hoc balancing and intermediate cases. All three are linked through the contested kernel; decomposition reflects different constitutional readings of the same constitutional text, not measurement variability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(takings_clause_boundary__regulatory_takings_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
