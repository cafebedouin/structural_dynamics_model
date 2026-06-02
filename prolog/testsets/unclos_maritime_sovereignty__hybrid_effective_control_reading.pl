% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__hybrid_effective_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__hybrid_effective_control_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__hybrid_effective_control_reading
 *   human_readable: UNCLOS Maritime Sovereignty: Hybrid Effective Control Reading
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   The hybrid effective control reading of UNCLOS maritime sovereignty
 *   instantiates a graduated sovereignty framework where natural geographic
 *   features (islands, rocks) generate full territorial seas and EEZ (12nm +
 *   200nm) while artificial features (constructed islands, platforms,
 *   installations) generate only 500m safety zones unless the constructing
 *   state can demonstrate prolonged effective control absent challenge. This
 *   reading sits between the strict geographic interpretation (natural
 *   features determine all sovereignty; artificial features generate minimal
 *   zones regardless of presence) and the expansive construction
 *   interpretation (states can construct sovereignty through engineering and
 *   occupation, converting artificial features to full territorial claims).
 *   The hybrid effective control reading is the dominant operative reading in
 *   contemporary international law following the 2016 Philippines v. China
 *   tribunal ruling, which established that effective control absent
 *   unopposed occupancy does not convert artificial features into state
 *   territory. However, this reading is unstable: hegemons with sustained
 *   military and administrative presence continue to claim effective control
 *   status for artificial features, weak states cannot muster
 *   counter-presence to contest claims, and the distinction between natural
 *   and artificial features is eroding as construction technology advances.
 *   The constraint exhibits high extractiveness (0.52) and suppression (0.68)
 *   because the rule provides benefits to states with construction capacity
 *   and military reach while constraining weaker maritime claimants to their
 *   natural feature baseline. Theater is moderate (0.58) — international
 *   legal argumentation provides a façade of objectivity, but outcomes track
 *   power asymmetries and presence capacity rather than text-based rules.
 *
 * KEY AGENTS:
 *   - Regional Maritime Hegemons (China, Vietnam, Philippines, Indonesia): Institutional beneficiaries with construction capacity and naval presence — benefit from effective control doctrine that enables artificial feature conversion to sovereignty claims. Power to maintain presence and absorb international criticism.
 *   - Militarily Weaker Claimants (small island states, non-militarized communities, indigenous fishing populations): Powerless victims — cannot construct features, cannot field sustained presence, cannot contest hegemon claims. Trapped by military inferiority and resource scarcity. Experienced extraction: loss of disputed EEZ, blocked maritime access, forced boundary agreements.
 *   - UNCLOS Dispute Resolution Bodies (international tribunals, Permanent Court of Arbitration): Organized institutional actors with constrained enforcement capacity — can rule on claims but cannot compel compliance from hegemons. Benefit from dispute resolution function (legitimacy through litigation) and constrained by lack of enforcement power.
 *   - International Maritime Commons Coalition (environmental NGOs, open-ocean resource advocates, smaller maritime states forming coalitions): Organized agents with moderate power — generate legal arguments and tribunal support; constrained by inability to prevent construction or enforce boundaries. Experience mixed coordination (tribunal precedents preserve some commons) and extraction (effective control expands state claims).
 *   - UNCLOS Treaty Framework (Articles 60-62, 121-133): Institutional authority grounded in lineage ( 1982 treaty text) with degraded function — written rules specify geographic criteria but lack enforcement mechanism; effective control has become the operative principle despite text's aspirations.
 *   - Analytical Observer: Views the constraint from civilizational perspective — risks naturalizing effective control as inherent principle rather than contingent power-based rule.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.52).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.68).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "UNCLOS Maritime Sovereignty: Hybrid Effective Control Reading").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, '29951ffe-806b-4a8e-b4e0-97815fc782fa').
narrative_ontology:cs_kernel_codification('29951ffe-806b-4a8e-b4e0-97815fc782fa', fixed_text).
narrative_ontology:cs_authority_grounding('29951ffe-806b-4a8e-b4e0-97815fc782fa', extraction).
narrative_ontology:cs_interpretation_layer_present('29951ffe-806b-4a8e-b4e0-97815fc782fa').
narrative_ontology:cs_reading_relation('29951ffe-806b-4a8e-b4e0-97815fc782fa', unclos_maritime_sovereignty__strict_geographic_reading, influences).
narrative_ontology:cs_reading_relation('29951ffe-806b-4a8e-b4e0-97815fc782fa', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_axiom('29951ffe-806b-4a8e-b4e0-97815fc782fa', foundational, effective_control_requires_sustained_presence).
narrative_ontology:cs_axiom_status(effective_control_requires_sustained_presence, holdable).
narrative_ontology:cs_axiom_grounding('29951ffe-806b-4a8e-b4e0-97815fc782fa', effective_control_requires_sustained_presence, conventional).
narrative_ontology:cs_axiom('29951ffe-806b-4a8e-b4e0-97815fc782fa', foundational, artificial_features_privilege_limited_over_natural_baselines).
narrative_ontology:cs_axiom_status(artificial_features_privilege_limited_over_natural_baselines, holdable).
narrative_ontology:cs_axiom_grounding('29951ffe-806b-4a8e-b4e0-97815fc782fa', artificial_features_privilege_limited_over_natural_baselines, conventional).
narrative_ontology:cs_reference_frame('29951ffe-806b-4a8e-b4e0-97815fc782fa', text_based_maritime_zones_with_effective_control_recognition).
narrative_ontology:cs_drift_state('29951ffe-806b-4a8e-b4e0-97815fc782fa', post_2016_tribunal_stabilization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('29951ffe-806b-4a8e-b4e0-97815fc782fa', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_maritime_hegemons).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, coastal_powers_with_naval_capability).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, smaller_island_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, non_militarized_coastal_communities).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_maritime_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MILITARILY WEAKER CLAIMANT (SNARE) — Small island states or economically marginal coastal powers cannot construct features or effectively contest regional hegemons' control. Trapped by military inferiority and resource scarcity. Experiences maximal extraction: can claim only 12nm territorial sea from natural features, while hegemons construct artificial islands that mature into full EEZ claims through unopposed effective control. No exit option — cannot build, cannot defend, cannot organize sufficient counter-presence.
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__hybrid_effective_control_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MID-TIER MARITIME STATE (TANGLED ROPE) — Has some naval capacity and economic resources but cannot match hegemons' construction pace or sustained presence. Benefits from the graduated sovereignty framework (can claim legitimate EEZ from natural features; can construct small facilities and claim safety zones). Simultaneously constrained by cost of presence maintenance, risk of escalation, and asymmetric power. Mixed coordination (rule-based maritime zones) and extraction (hegemon claims expand unchallenged).
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGIONAL MARITIME HEGEMON (ROPE) — Primary beneficiary. Experiences the constraint as coordination of legitimate maritime expansion: the hybrid effective control reading enables feature construction as a recognized path to sovereignty. No suppression felt — can build freely, exercise continuous control, face minimal contestation. The 500m safety zone rule is a minor limitation; effective control over years converts artificial features into de facto territorial seas. Benefits from rule clarity and international recognition of effective control as sovereignty-generating mechanism.
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__hybrid_effective_control_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL COORDINATION BODY (SCAFFOLD) — Arbitration tribunals and UNCLOS institutions see the hybrid effective control reading as a temporary coordination mechanism with a sunset clause embedded in dispute resolution. Tribunal rulings (Philippines v. China, Mauritius v. UK) establish that effective control absent unopposed long-term occupation does NOT automatically convert artificial features to full sovereignty. The coordination function is the dispute resolution process itself; the sunset is the tribunal's authority to invalidate claims not backed by genuine state capacity. Theater is moderate (legal procedures appear technical but often reflect power asymmetries).
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__hybrid_effective_control_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: UNCLOS FORMAL TREATY TEXT (PITON) — The written UNCLOS framework (Articles 60-62 on artificial islands, Articles 121-133 on island classification) is substantially degraded as a constraint mechanism. The text claims to establish clear jurisdictional lines but provides no enforcing mechanism; effective control (a term not formally defined in UNCLOS) has become the operative rule despite the text's aspiration to objective geographic criteria. The treaty persists through institutional inertia — states invoke UNCLOS articles, tribunals cite them, diplomatic language references them — but the actual jurisdictional outcomes are determined by power and presence, not treaty language. Theater ratio is high because legal argumentation substitutes for mechanical application of the text.
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__hybrid_effective_control_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL MARITIME COMMONS COALITION (TANGLED ROPE) — Coalition of states and NGOs advocating for high-seas open access and commons governance experiences this constraint as both coordination (the 500m safety zone rule protects some open-sea area; the distinction between natural and artificial features preserves some geographic limits) and extraction (effective control doctrine expands state claims beyond text-based limits). Constrained by power — can generate legal arguments and tribunal support but cannot prevent construction or enforce commons. Benefits from dispute resolution mechanisms that establish precedent for natural feature precedence. Mixed experience: the reading provides tools (court victories) but lacks enforcement capacity against hegemons.
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, effective control is presented as an immutable principle: occupation and presence generate sovereignty by nature of geography and physics. Control establishes legitimate jurisdiction as naturally as natural features do. This perspective risks naturalizing what is actually a contingent institutional arrangement (power asymmetries, construction capacity, military strength). The engine's false summit detector will identify this as misclassification — the 'natural law' framing obscures that effective control is a rule that benefits those with capacity to establish and maintain presence.
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__hybrid_effective_control_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unclos_maritime_sovereignty__hybrid_effective_control_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unclos_maritime_sovereignty__hybrid_effective_control_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, TR),
    TR >= 0.70.

:- end_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The hybrid reading enables artificial feature construction as a path to sovereignty, which benefits hegemons substantially but not as much as the expansive_construction_reading would. Weak states retain baseline EEZ from natural features (textual minimum), but hegemons can claim additional territory through construction + presence. The 500m safety zone limitation is minimal constraint on states with naval capacity. Extractiveness rises from 0.30 (pre-2016, when effective control was more contested) to 0.52 (post-tribunal stabilization) as hegemons accept the rule and implement it systematically. Suppression (0.68): High. Weak states cannot construct features (capital and technical barriers), cannot afford sustained maritime presence (navy costs), cannot contest hegemon presence (military inferiority). The barriers to exit are structural (economic and military) and institutional (tribunal mechanisms are slow, compliance is voluntary). Suppression rises from 0.55 to 0.68 as hegemons consolidate presence and weak states recognize the contest is unwinnable at current power asymmetries. Theater ratio (0.58): Moderate-high. UNCLOS legal argumentation provides a façade of objectivity — states cite treaty text, tribunals issue rulings with elaborate reasoning, international community treats disputes as legal questions. In practice, outcomes correlate with military capacity and presence duration, not text interpretation. Theater increases from 0.42 (when effective control was genuinely ambiguous) to 0.58 (when legal process becomes formalized ritual around foregone power-based outcomes). The rise reflects increasing theater relative to functional rule-following.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same rule (graduated sovereignty based on feature type and effective control duration) produces opposite classifications from different structural positions. The hegemon sees coordination (Rope) — the rule is clear and enables legitimate expansion. The weak state sees pure extraction (Snare) — the rule is enforced only against them; hegemons build unchallenged. The mid-tier state sees mixed coordination-extraction (Tangled Rope) — the rule provides some protection (natural feature baseline, 500m safety zone) but enables hegemon expansion beyond text. The tribunal sees temporary coordination (Scaffold) — dispute resolution mechanisms are supposed to provide sunset logic (contestation deadline, tribunal finality) but enforcement depends on state compliance. The UNCLOS text itself appears degraded (Piton) — written rules exist but lack enforcement; legal process substitutes for mechanical rule application. The analytical observer risks seeing natural law (Mountain) — effective control presented as inherent principle — but the structural data reveals power asymmetries as the operative mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The hybrid effective control reading's directionality derives from beneficiary/victim declarations and state capacity asymmetries. Regional maritime hegemons are beneficiaries with institutional power and arbitrage options (can build, maintain presence, challenge weak-state contestations). They experience low directionality values (d ≈ 0.10-0.20) and low effective extraction (χ). Militarily weaker claimants are victims with powerless/trapped status (cannot build, cannot defend, cannot contest). They experience high directionality values (d ≈ 0.90-0.95) and high effective extraction (χ). The sigmoid f(d) amplifies the asymmetry: beneficiaries' f(d) ≈ -0.12 to 0.02 (nearly zero or negative); victims' f(d) ≈ 1.28-1.42 (maximum extraction-sensing). Scope modifier σ(S) ≈ 0.9 (regional context) slightly dampens the chi values, but the power asymmetry dominates. The resulting perspectival gap is extreme: hegemons experience Rope (low extraction, clear rules, legitimate expansion); weak states experience Snare (maximal extraction, insurmountable barriers, forced exclusion).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The hybrid effective control reading resolves the coordination-vs-extraction mandate by explicitly distinguishing the coordination function (graduated sovereignty framework that provides clear maritime zone definitions) from the extraction mechanism (effective control doctrine that benefits high-power states). This reading accepts that BOTH are present in the same constraint: (1) Coordination function: the natural/artificial feature distinction and the 500m safety zone rule provide genuine maritime coordination — states can identify which features generate which zones, reducing territorial ambiguity below the level of raw power contest. (2) Extraction mechanism: effective control doctrine enables hegemons to claim beyond the text's graduated limits through sustained presence, which weak states cannot match. The reading does NOT try to minimize extraction (the expansive_construction_reading does that, claiming the extraction is justified by legitimate sovereignty); it explicitly models both functions simultaneously. This is why the claimed type is Tangled Rope (mixed coordination + extraction with asymmetric gains) rather than Rope (pure coordination). The mandatrophy is resolved by acknowledging that maritime governance requires both principles (coordination needs clear rules; actual sovereignty allocation follows power) and that pretending one principle dominates the other (either the strict_geographic_reading's emphasis on text, or the expansive_construction_reading's emphasis on power) would misclassify the constraint's structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effective_control_definition_ambiguity,
    'What constitutes ''effective control'' sufficient to generate sovereignty claims? Is it (a) continuous military/administrative presence, (b) demonstrated resource exploitation, (c) lack of opposing claims, or (d) some combination weighted by hegemon preference?',
    'Comparative analysis of tribunal rulings (Philippines v. China 2016, Mauritius v. UK 2019) identifying which factors were decisive. Examination of failed claims (e.g., Vietnam''s Spratly garrisons) to identify why presence alone was insufficient.',
    'If definition depends on military capacity: the reading legitimates power-based sovereignty (snare accelerator). If definition emphasizes resource exploitation: EEZ claims are restricted to economically viable zones (constraints extraction). If definition emphasizes unopposed long-term presence: the 10+ year maturation period becomes the operative gate (enables exit options for weak states to contest).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effective_control_definition_ambiguity, conceptual, 'Definitional ambiguity in ''effective control'' as sovereignty-generating mechanism').

omega_variable(
    maturation_period_contestability,
    'Does a weak state''s delayed contestation of a hegemon''s feature construction count as acceptance of the effective control claim, or does the claim require continuous, uninterrupted control?',
    'Longitudinal case analysis: tracking which claims faced delayed challenges and whether tribunals permitted challenge based on later evidence of insufficiency. Examining whether ''continuous'' control means 12-month, 5-year, or indefinite duration.',
    'If contestation deadlines exist: weak states have a fixed window to organize counter-presence (Rope transformation potential). If contestation can occur at any time: effective control remains uncertain until claim is ancient (extended Tangled Rope). If continuous presence is required: any interruption invalidates the claim (opens exit option for weaker states).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maturation_period_contestability, empirical, 'Whether effective control claims can be contested after maturation period').

omega_variable(
    natural_vs_artificial_feature_enforcement,
    'Are tribunal rulings distinguishing natural from artificial features robust to future pressure from states that have invested heavily in artificial island construction?',
    'Monitoring tribunal composition changes, state compliance with unfavorable rulings, and whether new cases show willingness to challenge the natural/artificial distinction or reframe artificial features as effectively ''occupied natural'' formations.',
    'If distinction erodes: the hybrid reading collapses toward expansive_construction_reading (higher extractiveness, snare acceleration). If distinction hardens: the graduated sovereignty framework becomes more stable (maintains Tangled Rope with some Rope components).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_artificial_feature_enforcement, empirical, 'Robustness of natural vs. artificial feature distinction under political pressure').

omega_variable(
    reading_vs_strict_geographic_boundary,
    'Does the hybrid effective control reading represent a genuine departure from strict geographic interpretation (UNCLOS text), or merely an unavoidable compromise that the strict reading would also permit under effective control doctrine?',
    'Close textual analysis of UNCLOS Articles 60-62, 121-133 examining whether effective control language exists in the text or is purely interpretive. Comparison with strict geographic reading''s treatment of the same texts.',
    'If readings are indistinguishable on this point: the boundary between hybrid_effective_control_reading and strict_geographic_reading is not a meaningful dispute (false kernel decomposition, should be consolidated). If readings genuinely diverge: the decomposition is correct and the omega indicates a genuine structural difference.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_strict_geographic_boundary, conceptual, 'Whether hybrid and strict readings represent genuine structural difference or false decomposition').

omega_variable(
    beneficiary_vs_victim_power_asymmetry,
    'Is the extractiveness value (0.52) accurately capturing the asymmetry between states with construction capacity and those without, or is the extraction substantially higher (0.65+) among militarily weaker claimants?',
    'Case-by-case analysis of small island states'' realized maritime loss (EEZ reductions, blocked resource access, forced maritime delimitation agreements) compared to their initial UNCLOS-text-based claims. Quantify actual extraction magnitude.',
    'If extraction is 0.65+: reclassify as Snare at the target perspective (currently classified as Snare but with lower extractiveness; higher value confirms snare fully). If extraction remains 0.52: tangled rope classification holds; mixed coordination-extraction model is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_vs_victim_power_asymmetry, empirical, 'Magnitude of extraction experienced by militarily weaker maritime claimants').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_hybrid_tr_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(unclos_hybrid_tr_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(unclos_hybrid_tr_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(unclos_hybrid_be_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(unclos_hybrid_be_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(unclos_hybrid_be_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(unclos_hybrid_su_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(unclos_hybrid_su_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(unclos_hybrid_su_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.18).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, south_china_sea_hegemonic_presence).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, small_island_state_maritime_dependency).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, treaty_enforcement_mechanism_degradation).

% DUAL FORMULATION NOTE:
% The hybrid effective control reading is one of three constraint stories decomposing the contested UNCLOS maritime sovereignty kernel. The strict_geographic_reading models the text-based interpretation (higher ε for beneficiary compliance burden, lower ε for weaker states' extraction). The expansive_construction_reading models the power-based interpretation (higher extractiveness, fewer constraints on hegemons). All three share beneficiary/victim structure but differ in ε values, enforcement mechanisms, and theater ratios. Network edges represent both logical relations (how readings relate to each other) and causal dependencies (how effective control doctrine affects outcomes in specific regional disputes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
