% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__hegemonic_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__hegemonic_extraction_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__hegemonic_extraction_reading
 *   human_readable: RBIO Practice Norm Complex as Hegemonic Extraction (Frozen Institutional Path-Dependency Reading)
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the HEGEMONIC EXTRACTION READING of the RBIO
 *   practice norm complex — a contested kernel that different parties
 *   interpret as either a neutral institutional framework (liberal
 *   institutional reading), a violation of state sovereignty (sovereignty
 *   maximalist reading), or a deliberately constructed mechanism for
 *   extracting wealth and geopolitical advantage from the Global South (this
 *   reading). The hegemonic extraction reading holds that the rules-based
 *   international order (RBIO) — comprising IMF conditionality, ISDS clauses,
 *   IP enforcement, debt-service priority, and P5 veto structures — is
 *   formally revisable but practically un-amendable due to structural
 *   path-dependency and the blocking power of beneficiary states. The
 *   constraint is enforced selectively: stringent conditionality for weaker
 *   states, light enforcement for geopolitical allies. Enforcement
 *   selectivity reveals extractive intent rather than neutral coordination
 *   logic. The core claim: what appears as inevitable institutional structure
 *   is actually a frozen hegemonic project maintained through institutional
 *   design choices (P5 veto, voting allocation, conditionality architecture)
 *   that benefit U.S. and European capital at the expense of Global South
 *   sovereignty and popular welfare.
 *
 * KEY AGENTS:
 *   - Global South Sovereign States: Primary victims (powerless/trapped) — subjected to debt-conditionality nexus with no genuine alternative institutional pathway. Structural adjustment programs (privatization, labor deregulation, austerity) mandated as loan conditions.
 *   - Indigenous and Local Populations: Primary victims (powerless/trapped) — dispossessed through investor-state dispute settlement (ISDS) clauses enforced by private corporate tribunals. No legal standing; suppression is structural (military, police, corporate security backing capital).
 *   - U.S. Capital Interests & European Multinationals: Primary beneficiaries (institutional/arbitrage) — benefit from institutional framework that enforces capital mobility, protects investments, and prevents regulatory defection. Experience the system as coordination mechanism with favorable asymmetry.
 *   - IMF/World Bank Technocracy: Secondary institutional actor (organized/constrained) — formally autonomous but embedded in U.S.-led order (U.S. retains veto power, voting structure reflects 1945). Administer extraction mechanism while believing in neoliberal development logic.
 *   - Formal RBIO Legitimacy Apparatus: Institutional performance layer (institutional/arbitrage) — multilateral institutions, treaty frameworks, rules-based order rhetoric maintain inertia despite functional degradation. High theater: reform proposals without structural change.
 *   - Analytical Observer: Cross-position perspective (analytical/analytical) — risks naturalizing contingent institutional choices (P5 veto, conditionality clauses) as immutable structural features of international anarchy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.68).
domain_priors:suppression_score(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.72).
domain_priors:theater_ratio(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__hegemonic_extraction_reading, snare).
narrative_ontology:human_readable(rbio_practice_norm_complex__hegemonic_extraction_reading, "RBIO Practice Norm Complex as Hegemonic Extraction (Frozen Institutional Path-Dependency Reading)").
narrative_ontology:topic_domain(rbio_practice_norm_complex__hegemonic_extraction_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__hegemonic_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__hegemonic_extraction_reading, '2608bca8-a51d-460c-9241-2754ee0a4153').
narrative_ontology:cs_kernel_codification('2608bca8-a51d-460c-9241-2754ee0a4153', formalized).
narrative_ontology:cs_authority_grounding('2608bca8-a51d-460c-9241-2754ee0a4153', extraction).
narrative_ontology:cs_interpretation_layer_present('2608bca8-a51d-460c-9241-2754ee0a4153').
narrative_ontology:cs_reading_relation('2608bca8-a51d-460c-9241-2754ee0a4153', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('2608bca8-a51d-460c-9241-2754ee0a4153', rbio_practice_norm_complex__sovereignty_maximalist_reading, influences).
narrative_ontology:cs_axiom('2608bca8-a51d-460c-9241-2754ee0a4153', foundational, institutional_design_benefits_identifiable_agents).
narrative_ontology:cs_axiom_status(institutional_design_benefits_identifiable_agents, holdable).
narrative_ontology:cs_axiom_grounding('2608bca8-a51d-460c-9241-2754ee0a4153', institutional_design_benefits_identifiable_agents, empirically_contingent).
narrative_ontology:cs_axiom('2608bca8-a51d-460c-9241-2754ee0a4153', foundational, practical_un_amenability_locks_extraction).
narrative_ontology:cs_axiom_status(practical_un_amenability_locks_extraction, holdable).
narrative_ontology:cs_axiom_grounding('2608bca8-a51d-460c-9241-2754ee0a4153', practical_un_amenability_locks_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('2608bca8-a51d-460c-9241-2754ee0a4153', hegemonic_order_institutional_codification).
narrative_ontology:cs_drift_state('2608bca8-a51d-460c-9241-2754ee0a4153', contemporary_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2608bca8-a51d-460c-9241-2754ee0a4153', '2026-02-26T14:32:15Z').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, united_states_capital_interests).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, european_multinational_capital).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, imf_world_bank_technocracy).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_sovereign_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, indigenous_and_local_populations).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, peasant_and_laboring_classes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL SOUTH STATE (SNARE) — Trapped by debt-conditionality nexus. Structural adjustment programs require privatization, labor deregulation, and austerity; non-compliance triggers capital flight, loan denial, rating downgrades. State cannot exit without catastrophic economic collapse. No alternative legitimacy frame available within the international financial system. Full extraction, zero agency.
constraint_indexing:constraint_classification(rbio_practice_norm_complex__hegemonic_extraction_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIGENOUS/LOCAL POPULATIONS (SNARE) — Trapped by sovereignty violation + capital-backed extraction. Land dispossession, labor coercion, environmental destruction authorized by 'investor-state dispute settlement' (ISDS) clauses. No legal standing in international arbitration (private corporate tribunals, not state-to-state). Suppression is structural (military, police, corporate security) and normalized as development. Maximum extraction, zero voice.
constraint_indexing:constraint_classification(rbio_practice_norm_complex__hegemonic_extraction_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: BENEFICIARY INSTITUTIONAL ACTORS (ROPE) — Experience the norm complex as coordination: the RBIO system (IMF conditionality, ISDS, IP enforcement, debt-service priority) aligns Global South policy with capital-friendly regimes. Rules-based order = rules that benefit capital mobility and protect investments. Exit option: arbitrage to competing institutional arrangements (EU, China-led AIIB) is available but costly. Net beneficiary — experienced as pure coordination with favorable payoff asymmetry.
constraint_indexing:constraint_classification(rbio_practice_norm_complex__hegemonic_extraction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: IMF/WORLD BANK TECHNOCRACY (TANGLED ROPE) — Organized actors with constrained exit. Formally autonomous institution; actually embedded in U.S.-led geopolitical order (U.S. retains veto, voting structure reflects 1945 power distribution). See mission as development coordination; actual function is enforcing capital-friendly policy. Career path locked into institution; reform proposals consistently blocked by P5. Genuine belief in neoliberal coordination logic coexists with structural extraction mechanism they administer.
constraint_indexing:constraint_classification(rbio_practice_norm_complex__hegemonic_extraction_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FORMAL RBIO LEGITIMACY APPARATUS (PITON) — The multilateral institutions, treaty frameworks, and 'rules-based order' rhetoric maintain institutional inertia despite their functional degradation as neutral coordinators. Theater ratio is high: frequent reform proposals, governance updates, inclusive development language, but core extraction mechanisms (conditionality, ISDS, debt-service priority) remain unchanged across decades. Performance of reform without structural change. Maintained through path-dependency, not because the legitimacy claims actually hold.
constraint_indexing:constraint_classification(rbio_practice_norm_complex__hegemonic_extraction_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a civilizational/universal view, power asymmetry in the international system produces differential institutional power; the hegemonic state will use institutions to project advantage; this is inherent to anarchy without world government. The RBIO appears as a natural outcome of structural realism — immutable unless replaced by an equally coercive counter-hegemonic system. However, the presence of identifiable beneficiaries and the selective enforcement patterns reveal this as a false summit: the arrangement is contingent on specific institutional choices (P5 veto, voting structure, conditionality clauses), not natural law.
constraint_indexing:constraint_classification(rbio_practice_norm_complex__hegemonic_extraction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rbio_practice_norm_complex__hegemonic_extraction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rbio_practice_norm_complex__hegemonic_extraction_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rbio_practice_norm_complex__hegemonic_extraction_reading, TR),
    TR >= 0.70.

:- end_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Base extraction captures the asymmetric institutional design: Global South states must implement privatization, labor deregulation, and austerity to service debt; capital gains unfettered mobility and investment protection. The extraction is not total (0.90+) because alternative pathways exist in principle (AIIB, bilateral credit, capital controls), even if they are blocked by geopolitical interdependence and capital flight threats. The value reflects high but not complete structural coercion. Suppression (0.72): High and rising. Structural suppression includes debt-service priority (capital claims override human welfare spending), ISDS mechanisms (corporate tribunals override domestic courts), and geopolitical coercion (capital flight, rating downgrades, trade sanctions if defection attempted). Suppression increased sharply after 1982 debt crisis and has remained elevated through 2020 pandemic accumulation. The trajectory shows institutional hardening, not softening. Theater ratio (0.65): Moderate-high and rising. In the 1982 debt crisis, extraction was explicit — IMF conditionality openly required privatization and austerity. By 1995, development language and inclusive governance rhetoric had risen substantially. By 2008-2020, the theater includes SDG alignment, inclusive development framing, and 'debt sustainability' discourse. Yet core extraction mechanisms (conditionality, ISDS, debt-service priority, P5 veto) remained unchanged across the interval. Rising theater masks structural stability of extraction.
 *
 * PERSPECTIVAL GAP:
 *   The hegemonic extraction reading produces maximum perspectival divergence. The beneficiary (institutional capital) experiences rope: the system coordinates financial flows, protects investments, ensures debt repayment. The powerless Global South state experiences snare: no exit, full cost of conditionality, extraction through debt servicing. The IMF technocracy experiences tangled_rope: genuine coordination belief in neoliberal development coexists with structural role administering extraction. The formal legitimacy apparatus experiences piton: the multilateral institutions and treaty frameworks persist through inertia despite functional degradation as neutral coordinators. The analytical observer risks experiencing mountain: structural realism naturalizes hegemonic institutional design as inevitable outcome of international anarchy. The false summit detector reveals the mountain as constructed: the RBIO design choices (P5 veto, voting structure, conditionality architecture, ISDS mechanism) are contingent institutional choices benefiting identifiable agents, not immutable structural features.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d reflects each agent's structural position in the extraction flow. Global South states as trapped victims: d ≈ 0.95 (near-maximal target status). Beneficiary capital interests: d ≈ 0.05 (near-maximal beneficiary status, even higher when arbitrage exit available). IMF technocracy: d ≈ 0.60 (organized agents with constrained exit, embedded in hegemonic order but with residual autonomy). The sigmoid function f(d) maps these to experienced extractiveness values: trapped victims experience high chi, beneficiaries experience low/negative chi, constrained institutional actors experience moderate chi. The perspectival gap reflects these differential d values: the same structural arrangement produces snare for powerless agents, rope for beneficiaries, and tangled_rope for organized administrators.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION RESOLVED: The hegemonic extraction reading resolves the mandatrophy by locating extraction within institutional design rather than naturalizing power asymmetry. The constraint is NOT 'unequal power in international system' (which would be natural law). It is 'specific institutional mechanisms (conditionality, ISDS, P5 veto, debt-service priority) that encode beneficiary preferences as binding rules and prevent amendment.' The snare classification follows from: (1) high base extractiveness (0.68) reflecting asymmetric institutional design; (2) high suppression (0.72) reflecting debt coercion and geopolitical blocking; (3) powerless agents with no exit; (4) enforcement selectivity revealing beneficiary bias. The mandatrophy resolves by distinguishing between structural international inequality (which the liberal institutional reading treats as benign, the sovereignty maximalist reading treats as violative) and the specific RBIO mechanisms that institutionalize and amplify that inequality. This reading names the mechanisms as extractive while acknowledging alternatives exist in principle (hence snare, not mountain).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditionality_coercion_boundary,
    'At what point does IMF conditionality cross from coordination (incentive alignment) to coercion (no genuine alternative)?',
    'Comparative institutional analysis: loan disbursement rates for compliant vs non-compliant states; availability of alternative financing (China AIIB, ALBA, bilateral) at equivalent terms; historical cases of successful policy defection without sanctions',
    'If coercion threshold met: snare classification confirmed. If coordination logic holds: tangled_rope or rope more accurate. Directly affects whether the constraint is extractive or mixed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_coercion_boundary, empirical, 'Boundary between incentive alignment (coordination) and coercion (snare)').

omega_variable(
    alternative_pathway_sufficiency,
    'Do alternative institutional pathways (AIIB, ALBA, bilateral state credit, capital controls) genuinely offer exit from RBIO extraction, or are they blocked by geopolitical interdependence?',
    'Longitudinal case studies of states attempting institution-switching (Indonesia 1997, Argentina 2001, Venezuela sanctions regime); measurement of relative autonomy and policy space under alternative arrangements; quantification of switching costs (capital flight, diplomatic isolation, trade disruption)',
    'If alternatives viable: exit is constrained (not trapped) — reclassify perspectives downward from snare to tangled_rope. If alternatives illusory: exit remains structurally blocked — snare classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_pathway_sufficiency, empirical, 'Whether alternative institutional pathways offer genuine exit').

omega_variable(
    p5_veto_amendment_impossibility,
    'Is P5 veto + structural path-dependency producing genuine immutability of core RBIO clauses, or is reform theoretically possible but politically unmotivated?',
    'Historical audit of amendment attempts (ISDS reform, IMF governance, SDR allocation); game-theoretic analysis of P5 veto incentives under different geopolitical scenarios; evaluation of whether reform coalition could be assembled if hegemon interest shifted',
    'If genuinely impossible: frozen institutional path-dependency is structural (fits snare narrative). If politically contingent: the appearance of immutability masks unwillingness to reform — reframe as hegemonic choice rather than institutional law. Affects the ''practically un-amendable'' claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(p5_veto_amendment_impossibility, empirical, 'Whether RBIO amendment is structurally impossible or politically unmotivated').

omega_variable(
    reading_contest_naturalization,
    'Is this ''hegemonic extraction'' reading a valid structural claim, or does it naturalize political struggle as institutional inevitability?',
    'Engagement with liberal_institutional_reading and sovereignty_maximalist_reading through committer-frame analysis: which reading''s axioms have been empirically challenged? Which remain holdable? Are the readings foreclosing or coexisting?',
    'This omega is the reading''s own self-examination. If the hegemonic reading forecloses liberal institutionalism by empirical evidence (beneficiary structure, enforcement selectivity), the reading is justified. If coexistence is maintained through different evaluative frames, the readings compete without resolution. Directly affects whether cs_structure.reading_relations uses ''forecloses'' or ''coexists_with''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_naturalization, conceptual, 'Validity of hegemonic extraction reading vs. alternative readings of RBIO').

omega_variable(
    enforcement_selectivity_mechanism,
    'Does enforcement selectivity (strict conditionality for weaker states, light enforcement for allies) reflect deliberate hegemonic strategy or structural capacity constraints?',
    'Comparative analysis of IMF enforcement patterns: are enforcement ratios correlated with geopolitical alignment (U.S. allies receive lighter scrutiny)? Do IMF staff documents reveal explicit dual-standard logic? Do historical instances exist of IMF enforcing hard against allies or relaxing enforcement on adversaries?',
    'If deliberate strategy: extraction reading is confirmed (institutional actors knowingly administer hegemonic project). If capacity-driven: perhaps tangled_rope is more accurate (institution genuinely trying but unevenly empowered). Directly affects the ''extractive intent'' claim in the constraint name.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_mechanism, empirical, 'Whether enforcement selectivity reflects deliberate hegemonic strategy or capacity constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__hegemonic_extraction_reading, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_hegemonic_theater_1982_debt_crisis_explicit, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1982, 0.35).
narrative_ontology:measurement(rbio_hegemonic_theater_1995_development_language_rise, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1995, 0.5).
narrative_ontology:measurement(rbio_hegemonic_theater_2008_inclusive_governance_rhetoric, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2008, 0.65).
narrative_ontology:measurement(rbio_hegemonic_theater_2020_sdg_alignment_claims, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2020, 0.65).

% Extraction over time
narrative_ontology:measurement(rbio_hegemonic_extractiveness_1971_bretton_woods_collapse, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1971, 0.42).
narrative_ontology:measurement(rbio_hegemonic_extractiveness_1982_debt_crisis_onset, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1982, 0.58).
narrative_ontology:measurement(rbio_hegemonic_extractiveness_1995_structural_adjustment_peak, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1995, 0.64).
narrative_ontology:measurement(rbio_hegemonic_extractiveness_2008_financial_crisis, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2008, 0.68).
narrative_ontology:measurement(rbio_hegemonic_extractiveness_2020_pandemic_debt_accumulation, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(rbio_hegemonic_suppression_1971_oilcris_geopolitics, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1971, 0.45).
narrative_ontology:measurement(rbio_hegemonic_suppression_1982_debt_enforcement, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1982, 0.62).
narrative_ontology:measurement(rbio_hegemonic_suppression_1995_conditionality_hardening, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1995, 0.72).
narrative_ontology:measurement(rbio_hegemonic_suppression_2008_austerity_imposition, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2008, 0.72).
narrative_ontology:measurement(rbio_hegemonic_suppression_2020_pandemic_debt_service, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2020, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__hegemonic_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.18).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, debt_conditionality_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, isds_investor_protection_regime).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, us_dollar_hegemony_monetary_system).

% DUAL FORMULATION NOTE:
% The RBIO practice norm complex decomposes into three constraint stories reflecting the three contesting readings: this story (hegemonic extraction reading), the liberal institutional reading, and the sovereignty maximalist reading. Each has distinct base extractiveness, beneficiary/victim structures, and perspectives. They are linked via network.affects_constraints because each reading's legitimacy depends on engaging with the siblings' claims. Downstream constraints (debt conditionality, ISDS, dollar hegemony) are affected by which reading of the RBIO is adopted: the extraction mechanisms operate differently under each reading's interpretation of legitimacy and enforcement selectivity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__hegemonic_extraction_reading, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
