% ============================================================================
% CONSTRAINT STORY: regulatory_uncertainty_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_uncertainty_extraction, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: regulatory_uncertainty_extraction
 *   human_readable: Regulatory Uncertainty Extraction
 *   domain: political_economy/regulatory_systems
 *
 * SUMMARY:
 *   Regulatory uncertainty extraction emerges when regulatory frameworks
 *   maintain structural ambiguity in how rules are interpreted and enforced,
 *   creating competitive advantage for incumbents who have learned to
 *   navigate the uncertainty while imposing compliance burdens on entrants.
 *   This constraint operates across jurisdictions and sectors — from
 *   financial regulation to environmental compliance to occupational
 *   licensing — making it a systematic extraction mechanism. The constraint
 *   exhibits properties of both coordination (all agents benefit from
 *   regulatory clarity) and extraction (uncertainty creates asymmetric
 *   advantages). The theater ratio increasing from 0.48 to 0.72 over the
 *   interval reflects regulatory apparatus evolution toward more performative
 *   legitimacy maintenance (compliance reporting, regulatory narratives)
 *   rather than substantive clarity, indicating piton-level degradation. The
 *   extractiveness rising from 0.38 to 0.62 shows that the constraint's
 *   extractive function is intensifying over time as regulatory systems
 *   become more complex while ambiguity is selectively maintained. The
 *   perspectival gap spans from snare (powerless entrants) to rope
 *   (incumbents and compliance consultants) to piton (institutional
 *   apparatus), illustrating how the same regulatory structure enables wealth
 *   capture for some while creating exit barriers for others.
 *
 * KEY AGENTS:
 *   - Market Entrants: Primary victim (powerless/trapped) — face opaque enforcement rules with no exit option short of market exit
 *   - Small Businesses: Secondary victim (moderate/constrained) — bear disproportionate compliance costs relative to resources and can exit only at high cost
 *   - Incumbent Firms: Primary beneficiary (institutional/arbitrage) — navigate regulatory landscape with learned expertise; uncertainty creates moat against competition
 *   - Compliance Consulting Industry: Derivative beneficiary (institutional/arbitrage) — supply expertise services that monetize regulatory ambiguity
 *   - Regulatory Agencies: Complex actor (powerful/mobile) — maintain uncertainty through discretionary enforcement; extract through selective application while claiming coordination function
 *   - Consumers: Indirect victim (powerless/trapped) — bear costs through reduced competition, innovation suppression, and price elevation
 *   - Regulatory Clarity: Collective good victim (analytical/trapped) — abstract public interest that cannot organize or exit; contaminated by strategic ambiguity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_uncertainty_extraction, 0.58).
domain_priors:suppression_score(regulatory_uncertainty_extraction, 0.62).
domain_priors:theater_ratio(regulatory_uncertainty_extraction, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_uncertainty_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_uncertainty_extraction, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(regulatory_uncertainty_extraction, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_uncertainty_extraction, tangled_rope).
narrative_ontology:human_readable(regulatory_uncertainty_extraction, "Regulatory Uncertainty Extraction").
narrative_ontology:topic_domain(regulatory_uncertainty_extraction, "political_economy/regulatory_systems").

domain_priors:requires_active_enforcement(regulatory_uncertainty_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_uncertainty_extraction, incumbent_firms).
narrative_ontology:constraint_beneficiary(regulatory_uncertainty_extraction, regulatory_agencies).
narrative_ontology:constraint_beneficiary(regulatory_uncertainty_extraction, compliance_consulting_industry).
narrative_ontology:constraint_victim(regulatory_uncertainty_extraction, market_entrants).
narrative_ontology:constraint_victim(regulatory_uncertainty_extraction, small_businesses).
narrative_ontology:constraint_victim(regulatory_uncertainty_extraction, consumers).
narrative_ontology:constraint_victim(regulatory_uncertainty_extraction, regulatory_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARKET ENTRANT (SNARE) — New firms face regulatory requirements that are genuinely ambiguous, with enforcement selective and retroactive. Cannot operate without complying with unstated or emerging standards. No exit option: either comply with uncertain rules or exit the market entirely. Trapped bears maximum extraction burden.
constraint_indexing:constraint_classification(regulatory_uncertainty_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS (TANGLED ROPE) — Faces genuine coordination problem (regulatory clarity benefits all) but also constrained by high relative compliance costs. Can exit by exiting market or moving jurisdiction, but at significant cost. Partial beneficiary (clear rules help them too) but primary target of extraction through burden concentration.
constraint_indexing:constraint_classification(regulatory_uncertainty_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT FIRM (ROPE) — Already operates within the regulatory environment. Experiences constraint as coordination: establishing rules (even opaque ones they navigate) prevents worse alternatives. Can arbitrage between jurisdictions. Net beneficiary — regulatory uncertainty provides competitive moat against new entrants without explicitly excluding them.
constraint_indexing:constraint_classification(regulatory_uncertainty_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMPLIANCE CONSULTING INDUSTRY (ROPE) — Direct beneficiary of regulatory uncertainty. Uncertainty creates demand for expertise in interpreting and navigating rules. Experiences constraint as pure coordination: their service IS the coordination mechanism that solves ambiguity. Can arbitrage between jurisdictions and industries. Net beneficiary with organizational agency.
constraint_indexing:constraint_classification(regulatory_uncertainty_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AGENCY (TANGLED ROPE) — Maintains ambiguity partly through genuine capacity constraints (understaffing, complex domain knowledge requirements) and partly through active enforcement discretion that sustains uncertainty. Coordinates legitimate public interest (consumer protection, market fairness) but also extracts through selective enforcement patterns. Powerful agents can shift to clarity but choose not to — extraction is functional to agency mission and budget justification.
constraint_indexing:constraint_classification(regulatory_uncertainty_extraction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: INSTITUTIONAL REGULATORY APPARATUS (PITON) — From civilizational view, the complex institutional system of regulations, agencies, and compliance infrastructure is substantially performative theater maintaining the appearance of rational coherent governance while functionally optimizing for incumbent stability. The apparatus is maintained through inertia and legitimacy theater despite lower actual effectiveness than simpler direct standards would provide. Theater ratio high indicates degradation.
constraint_indexing:constraint_classification(regulatory_uncertainty_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From epistemically rigorous universal view, regulatory uncertainty may appear as natural law: regulators cannot create perfectly clear rules ex ante because they cannot know all contingencies and future conditions. Any rule system leaves interpretive gaps. Information asymmetry between regulator and regulated is structural. However, the base properties show that much measured uncertainty is NOT inherent but actively maintained through selective enforcement and discretionary interpretation, revealing this mountain classification as a false summit that naturalizes contingent institutional choices.
constraint_indexing:constraint_classification(regulatory_uncertainty_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_uncertainty_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_uncertainty_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_uncertainty_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_uncertainty_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_uncertainty_extraction, TR),
    TR >= 0.70.

:- end_tests(regulatory_uncertainty_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant value from entrants through compliance burden, market access delays, and selective enforcement risk. However, extraction is not maximal (snare-level ≥0.66) because the underlying regulatory function is partly genuine — rules do serve public coordination functions, and incumbents themselves benefit from some degree of clarity. The extraction is layered on top of legitimate coordination. Suppression (0.62): High. Market entrants face substantial barriers to understanding and meeting requirements. Selective enforcement means rules are not uniformly applied, so compliance is uncertain. Exit options are limited (comply or exit market). Small businesses face proportionally higher suppression through resource constraints. Theater ratio (0.65): High and rising. Regulatory systems invest substantially in legitimacy narratives (public comments, environmental impact statements, compliance guidance) that maintain appearance of rational coherence while actual enforcement patterns are discretionary. The apparatus sustains theater to justify its complexity and jurisdiction.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives reveals the constraint operates as an asymmetric advantage mechanism. Incumbents see the regulatory framework as legitimate coordination enabling business operation — their experience of the constraint is rope (beneficial coordination with low personal extraction). Entrants see the same framework as an extraction barrier preventing market access — their experience is snare (high extraction with no viable exit). The gap is not a measurement error but a structural feature: the constraint's function is to coordinate legitimate rules while maintaining ambiguity that creates advantages for those who know how to navigate it. The gap would close only if regulation achieved genuine clarity — at which point the constraint would transition to rope across all perspectives, and the extraction mechanism would disappear. The fact that the gap persists and institutions work to maintain it (through selective enforcement, guidance delays, complexity expansion) indicates active extraction alongside legitimate coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural relationship each agent holds to regulatory uncertainty. Market entrants are trapped victims with no exit options (d ≈ 0.95, f(d) ≈ 1.42): maximum experienced extraction. Small businesses are constrained victims (d ≈ 0.75, f(d) ≈ 1.05): high extraction with some mitigation through relative organizational capacity. Incumbents are beneficiaries with arbitrage capacity (d ≈ 0.20, f(d) ≈ 0.02): negative or minimal experienced extraction — the constraint subsidizes them. Compliance consultants are beneficiaries with arbitrage capacity (d ≈ 0.15, f(d) ≈ -0.01): negative effective extraction. Regulatory agencies hold both beneficiary and enforcer positions depending on analytical frame; the powerful/mobile designation reflects their capacity to shift enforcement patterns, suggesting moderate extraction (d ≈ 0.55). The directionality chain shows that the same structural ambiguity produces high extraction for powerless agents and negative extraction for institutional beneficiaries — the asymmetry is the core feature of the tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy (coordination vs. extraction paradox) is resolved by recognizing that regulatory uncertainty genuinely solves a coordination problem — all agents benefit from having a regulatory framework, even if ambiguous — while simultaneously enabling extraction through selective enforcement and asymmetric burden distribution. The constraint is NOT a false coordinator (rope mislabeled as extraction) because extraction mechanisms are real and structurally significant. It is NOT pure extraction (snare) because coordination function is genuine and necessary. It is authentically tangled rope: the regulatory framework coordinates legitimate public interests while the uncertainty mechanism extracts from powerless entrants. The beneficiary/victim structure confirms this: beneficial coordination (compliance consulting, incumbent navigation) coexists with victimization (entry barrier elevation, market suppression). The false summit detection catches the analytical observer's temptation to naturalize uncertainty as inherent when much is actively maintained. The actual classification stands: tangled_rope with high extractiveness, significant suppression, and institutional coordination functions that justify moderate enforcement costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_source_decomposition,
    'What portion of regulatory uncertainty is inherent to the domain complexity versus actively maintained through discretionary enforcement?',
    'Comparative analysis of jurisdiction clarity outcomes; empirical measurement of enforcement consistency; documentation of rule interpretation guidance issuance patterns',
    'If inherent > 70%: mountain classification strengthens. If maintained > 70%: tangled_rope extraction mechanism is primary driver, not coordination bottleneck.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ambiguity_source_decomposition, empirical, 'Source decomposition of regulatory uncertainty').

omega_variable(
    selective_enforcement_intentionality,
    'Is selective enforcement a deliberate extraction mechanism or an artifact of resource constraints and bureaucratic variation?',
    'Analysis of enforcement audit trails; comparison between agencies with equivalent resources; review of enforcement guidance documents and internal policy communications',
    'If deliberate: snare classification for powerless agents confirmed, beneficiary coordination is strategic. If artifact: constraint reclassifies toward rope, suggesting policy solutions (automation, standardization) that bypass intentionality question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_intentionality, empirical, 'Whether selective enforcement is deliberate or artifact').

omega_variable(
    compliance_barrier_height,
    'What percentage of market entrants exit due to regulatory barrier relative to other factors (capital, market demand, technology)?',
    'Longitudinal survey of failed market entrants; attribution analysis of stated exit reasons; comparison with barriers in deregulated jurisdictions',
    'If regulatory barriers account for >40% of exits: snare classification strengthened. If <20%: constraint may be mislabeled (other factors dominant), requiring decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_barrier_height, empirical, 'Magnitude of regulatory compliance barriers to market entry').

omega_variable(
    jurisdictional_arbitrage_viability,
    'Can agents realistically arbitrage between regulatory jurisdictions, or are they trapped by network effects and incumbent lock-in despite formal jurisdiction optionality?',
    'Study of cross-jurisdiction migration patterns; cost analysis of jurisdiction switching; network dependency assessment',
    'If arbitrage available: exit_options for moderate and institutional agents are higher than current classification suggests. If locked-in: identity_locked or trapped reclassification warranted despite structural mobility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_arbitrage_viability, empirical, 'Viability of jurisdictional arbitrage as exit option').

omega_variable(
    regulatory_clarity_sunset_plausibility,
    'Is there an institutional pathway to regulatory clarity with a realistically achievable sunset, or does the extractive structure self-reproduce through bureaucratic incentive alignment?',
    'Historical case studies of regulatory clarity initiatives; analysis of success/failure patterns; examination of incentive structures that sustain uncertainty in agencies',
    'If achievable sunset: scaffold classification becomes viable alternative. If self-reproducing: constraint is structurally snare/tangled_rope without escape pathway.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_clarity_sunset_plausibility, conceptual, 'Whether regulatory clarity has achievable sunset pathway').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_uncertainty_extraction, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regunc_tr_t0, regulatory_uncertainty_extraction, theater_ratio, 0, 0.48).
narrative_ontology:measurement(regunc_tr_t2, regulatory_uncertainty_extraction, theater_ratio, 2, 0.55).
narrative_ontology:measurement(regunc_tr_t5, regulatory_uncertainty_extraction, theater_ratio, 5, 0.65).
narrative_ontology:measurement(regunc_tr_t8, regulatory_uncertainty_extraction, theater_ratio, 8, 0.72).

% Extraction over time
narrative_ontology:measurement(regunc_be_t0, regulatory_uncertainty_extraction, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(regunc_be_t2, regulatory_uncertainty_extraction, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(regunc_be_t5, regulatory_uncertainty_extraction, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(regunc_be_t8, regulatory_uncertainty_extraction, base_extractiveness, 8, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_uncertainty_extraction, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(regulatory_uncertainty_extraction, 0.18).
narrative_ontology:affects_constraint(regulatory_uncertainty_extraction, barriers_to_market_entry).
narrative_ontology:affects_constraint(regulatory_uncertainty_extraction, incumbent_competitive_moat).
narrative_ontology:affects_constraint(regulatory_uncertainty_extraction, regulatory_capture).

% DUAL FORMULATION NOTE:
% Regulatory uncertainty extraction is downstream of institutional regulatory architecture but structurally distinct from specific regulatory capture at particular agencies. The constraint represents the system-level extraction mechanism that emerges from ambiguity maintenance. Decomposition: barrier_to_entry story (ε ≈0.72, snare) focuses on entry-specific impediments; incumbent_moat story (ε ≈0.35, rope) focuses on navigation advantage; regulatory_capture story (ε ≈0.65, snare) focuses on specific agency-incumbent relationships. This story integrates across these domains to show the unified extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_uncertainty_extraction, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
