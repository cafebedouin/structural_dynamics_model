% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__expansive_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__expansive_construction_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: unclos_maritime_sovereignty__expansive_construction_reading
 *   human_readable: Artificial Island Construction as Territorial Sea Generator
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint captures the reading that artificial island construction
 *   on submerged features (reefs, shoals, low-tide elevations) generates de
 *   facto territorial waters through effective occupation and administrative
 *   control. The reading asserts that the physical act of construction plus
 *   sustained administration creates sovereign entitlements equivalent to
 *   natural islands under UNCLOS Article 121. Beneficiaries are the
 *   constructing states and their commercial/military partners who gain
 *   exclusive resource rights and strategic depth. Victims are neighboring
 *   claimants whose overlapping entitlements are foreclosed,
 *   freedom-of-navigation states whose transit rights are compressed, and
 *   local fishing communities displaced by militarized exclusion zones. The
 *   constraint operates through a hybrid coordination-extraction mechanism:
 *   it coordinates dispute settlement by creating facts on the water, but
 *   extracts sovereignty and resource wealth from adjacent states and the
 *   global commons. Active enforcement is required — coast guard patrols,
 *   administrative decrees, and military presence maintain the claim against
 *   challenges.
 *
 * KEY AGENTS:
 *   - island_constructing_states: Primary agenda_setter (institutional/arbitrage) — builds, administers, enforces, and extracts
 *   - neighboring_claimant_states: Primary victim (powerful/constrained) — loses EEZ/territorial sea, faces coercive exclusion
 *   - freedom_of_navigation_states: Primary victim (institutional/mobile) — loses high-seas transit corridors, faces interference
 *   - displaced_fishing_communities: Victim (powerless/trapped) — loses traditional grounds, no exit
 *   - resource_extraction_concessionaires: Beneficiary (organized/arbitrage) — gains exclusive concessions from constructing state
 *   - maritime_militarization_contractors: Beneficiary (organized/arbitrage) — profits from base construction and sustainment
 *   - international_tribunals: Observer (analytical/analytical) — adjudicates competing readings but lacks enforcement
 *   - small_island_developing_states: Victim (moderate/constrained) — existential threat from sea-level rise compounded by artificial feature claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.82).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.78).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "Artificial Island Construction as Territorial Sea Generator").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, 'b0b620ec-52a5-4b73-8987-c1135a190ca1').
narrative_ontology:cs_kernel_codification('b0b620ec-52a5-4b73-8987-c1135a190ca1', formalized).
narrative_ontology:cs_authority_grounding('b0b620ec-52a5-4b73-8987-c1135a190ca1', lineage).
narrative_ontology:cs_interpretation_layer_present('b0b620ec-52a5-4b73-8987-c1135a190ca1').
narrative_ontology:cs_reading_relation('b0b620ec-52a5-4b73-8987-c1135a190ca1', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('b0b620ec-52a5-4b73-8987-c1135a190ca1', unclos_maritime_sovereignty__hybrid_effective_control_reading, influences).
narrative_ontology:cs_axiom('b0b620ec-52a5-4b73-8987-c1135a190ca1', foundational, effective_occupation_generates_maritime_entitlements).
narrative_ontology:cs_axiom_status(effective_occupation_generates_maritime_entitlements, holdable).
narrative_ontology:cs_axiom_grounding('b0b620ec-52a5-4b73-8987-c1135a190ca1', effective_occupation_generates_maritime_entitlements, conventional).
narrative_ontology:cs_axiom('b0b620ec-52a5-4b73-8987-c1135a190ca1', foundational, constructive_geography_equals_natural_geography).
narrative_ontology:cs_axiom_status(constructive_geography_equals_natural_geography, holdable).
narrative_ontology:cs_axiom_grounding('b0b620ec-52a5-4b73-8987-c1135a190ca1', constructive_geography_equals_natural_geography, instrumental).
narrative_ontology:cs_reference_frame('b0b620ec-52a5-4b73-8987-c1135a190ca1', unclos_1982_baseline_framework).
narrative_ontology:cs_drift_state('b0b620ec-52a5-4b73-8987-c1135a190ca1', post_2016_pca_award, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b0b620ec-52a5-4b73-8987-c1135a190ca1', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, resource_extraction_concessionaires).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, maritime_militarization_contractors).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, displaced_fishing_communities).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, small_island_developing_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiate and sustain large-scale dredging, construction, and garrisoning of features in disputed waters. Issue domestic laws declaring administrative districts, grant resource concessions, deploy coast guard and military assets. Control the rule-making and enforcement apparatus. Can modulate investment or abandon features if strategic calculus shifts.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Lose traditional fishing grounds, hydrocarbon blocks, and EEZ/territorial sea entitlements to constructed features. Respond with diplomatic protests, arbitral proceedings, their own construction programs, and coast guard confrontations. Cannot undo established facts; exit means accepting permanent loss or escalating to conflict.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states, payer,
    powerful, generational, constrained, regional).

% Conduct FONOPs through claimed territorial seas, maintain transit corridors for commercial and military shipping, rally coalition statements. Bear operational costs and diplomatic friction. Can reroute (mobile exit) but at strategic and economic cost; the constraint directly compresses their operational space.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states, payer,
    institutional, biographical, mobile, global).

% Traditional fishing grounds enclosed by constructed features and enforced exclusion zones. Livelihood and cultural identity fused to specific maritime spaces. No alternative grounds; no political voice in the constructing state's calculus. Exit means abandonment of identity, not just income.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, displaced_fishing_communities, payer,
    powerless, biographical, identity_locked, local).

% Receive exclusive exploration and exploitation rights from constructing states for hydrocarbon, mineral, and fisheries resources in claimed zones. Capture rents with mobile capital — can shift concession portfolios globally. Their presence legitimizes the constructing state's administrative claims.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, resource_extraction_concessionaires, beneficiary,
    organized, biographical, arbitrage, regional).

% Design, build, and sustain the infrastructure of occupation: runways, ports, barracks, radar, missile batteries, power generation. Profit from long-term sustainment contracts. Mobile across conflict zones. Their industry lobby reinforces the constructing state's commitment.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, maritime_militarization_contractors, beneficiary,
    organized, biographical, arbitrage, global).

% Adjudicate competing readings (e.g., PCA Philippines v. China, ITLOS cases). Issue awards clarifying that artificial features do not generate EEZ/TS. Lack enforcement; their rulings are ignored or selectively implemented by constructing states. Provide the analytical baseline for classification.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, international_tribunals, observer,
    analytical, generational, analytical, universal).

% Face existential threat from sea-level rise; their maritime entitlements are the economic basis for survival. Artificial feature claims by powerful states compress their zones and set precedent that constructed features generate rights — undermining the natural-feature basis of their own entitlements. Constrained exit: can litigate, form coalitions, seek adaptation finance, but cannot match construction capacity.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, small_island_developing_states, payer,
    moderate, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts indeterminate or contested maritime space into administered zones with clear (if contested) sovereignty claims, reducing the transaction costs of perpetual dispute by creating facts that demand recognition.
% TRANSFER_FUNCTION: Moves exclusive rights to 12nm territorial sea + up to 200nm EEZ per feature (fisheries, hydrocarbons, seabed minerals, strategic positioning) from the global commons and neighboring claimants to the constructing state and its commercial partners.
% ABSENT_VOICES: Indigenous Pacific communities whose traditional navigation and resource use span the affected waters; future generations who inherit a partitioned ocean; marine ecosystems that cannot litigate. They are excluded by the state-centric framework of UNCLOS and the reading's sovereignty logic.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, constructed features would revert to their legal status as artificial installations with 500m safety zones only. Claimed territorial seas and EEZs would collapse, reopening vast areas to high-seas freedoms and neighborly entitlements. Resource concessions would be voided. Coast guard and military deployments would lose legal basis. The South China Sea, East China Sea, and other disputed zones would reorganize around natural-feature baselines.
% FOUNDING_PROBLEM: UNCLOS III (1973-1982) left the status of artificial features on submerged features ambiguous. Article 60 (artificial islands in EEZ) and Article 80 (artificial islands on continental shelf) grant coastal states exclusive construction rights but are silent on whether such constructions generate their own maritime zones. Article 121(1) defines islands as 'naturally formed areas of land' — but 'naturally formed' was not defined. The founding problem: how to allocate sovereignty and resource rights over features that humans can build but nature did not make.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the UNCLOS negotiating record (Virginia Commentary, Vol. II, Art. 121 drafting history) and the ICJ's 2012 Nicaragua v. Colombia judgment (paras. 88-91) which noted the Convention's silence on artificial features. The constructing states' claim that the problem is solved by 'effective occupation' is a unilateral reading; the 2016 PCA award in Philippines v. China (paras. 277-279) corroborates that the problem remains contested and that Article 121's 'naturally formed' language excludes artificial features.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__expansive_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__expansive_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__expansive_construction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(unclos_maritime_sovereignty__expansive_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the reading transfers vast maritime zones (up to 200nm EEZ + 12nm TS per feature) from the global commons and neighboring states to the constructor. The theater ratio (0.42) reflects that administrative infrastructure (lighthouses, weather stations, coast guard stations) serves dual use — genuine safety coordination AND sovereignty performance. Suppression (0.78) is high because maintaining the claim requires continuous coercive presence: coast guard harassment of neighboring fishers, radar lock-ons to transiting warships, administrative penalties on foreign vessels. Accessibility collapse (0.71) is substantial — once a feature is built and garrisoned, legal and physical reversal is nearly impossible. Resistance (0.68) is significant: arbitral rulings (Philippines v. China), FONOPs, diplomatic protests, and coalition-building all contest the reading, but have not reversed established facts.
 *
 * PERSPECTIVAL GAP:
 *   The constructing state seat experiences this as rope (coordination of disputed space into administered order). Neighboring claimants experience it as snare (pure extraction of their entitlements). Freedom-of-navigation states experience it as tangled_rope (coordination of traffic management mixed with extraction of transit rights). Displaced fishers experience it as piton (inertial deprivation — the constraint persists after the coordinating function is gone). The engine computes these per-seat types from power/exit/spatial_scope; the authored claim (tangled_rope) reflects the structural hybridity visible from the analytical seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Island-constructing states are structural beneficiaries (d ≈ 0.15): they write the rules, collect the resources, control enforcement, and hold arbitrage-grade exit (can abandon features if costs exceed gains). Resource concessionaires and militarization contractors are beneficiaries (d ≈ 0.20): they capture rents with mobile exit. Neighboring claimants are targets (d ≈ 0.85): powerful but constrained exit — they can protest, litigate, or build their own features, but cannot undo the constructor's facts. Freedom-of-navigation states are symmetric-to-target (d ≈ 0.60): institutional power with mobile exit, but the constraint directly compresses their operational space. Displaced fishing communities are identity-locked targets (d ≈ 0.95): powerless, trapped, their livelihood identity fused to the now-excluded waters. Small island developing states are constrained targets (d ≈ 0.80): moderate power but existential stakes — exit means abandonment of sovereignty.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (dispute settlement through effective control) has not atrophied — it is actively expanding. The extraction function has intensified. This is not a piton (degraded coordination) but a tangled_rope whose extractive component grows while the coordinating cover (safety of navigation, scientific research) thins. The mandate (UNCLOS dispute resolution) is being read expansively to justify what the treaty text limits. Mandatrophy is unresolved — the reading's proponents claim the coordination function is live; opponents claim it is cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_artificial_boundary_stability,
    'Is the natural/artificial feature distinction a stable legal boundary or a gradient that state practice erodes?',
    'Track state practice and tribunal reasoning on features that are partially natural (e.g., reefs with minor augmentation) vs. fully artificial. If tribunals treat the distinction as a spectrum, the boundary collapses and extraction expands.',
    'If the boundary is a gradient, the expansive reading''s ε increases further — every enhancement becomes a sovereignty ratchet. If stable, the hybrid reading''s maturation pathway becomes the contested margin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_artificial_boundary_stability, conceptual, 'Whether the legal category boundary between natural and artificial features holds under state practice').

omega_variable(
    effective_control_threshold_ambiguity,
    'What quantum of ''effective occupation and administrative control'' suffices to generate territorial sea?',
    'Analyze tribunal findings on minimum administration: personnel, infrastructure, duration, continuity. The Philippines v. China award suggests ''civilian administration'' is insufficient; the hybrid reading requires ''prolonged effective control absent challenge''.',
    'A low threshold makes the reading a low-cost snare (build a hut, claim 12nm). A high threshold makes it a costly tangled_rope (permanent garrison required). The threshold determines whether extraction is marginal or infrastructural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(effective_control_threshold_ambiguity, empirical, 'The minimum administrative footprint needed to activate the reading''s sovereignty claim').

omega_variable(
    sea_level_rise_interaction,
    'How does this reading interact with the legal status of features that are naturally submerging due to sea-level rise?',
    'Track ILC work on sea-level rise and state practice on ''preserving'' maritime entitlements through artificial maintenance of disappearing features.',
    'If artificial maintenance of naturally drowning features is recognized, the reading becomes a climate adaptation tool AND an extraction multiplier — existing EEZs are preserved through construction, expanding the beneficiary set to all low-lying states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sea_level_rise_interaction, preference, 'Whether the reading expands to cover climate-driven artificial preservation of natural features').

omega_variable(
    committer_framing_kernel_reading,
    'Does this reading foreclose, coexist with, or influence the sibling readings of the unclos_maritime_sovereignty kernel?',
    'Structural analysis of whether a single legal framework can hold multiple readings simultaneously. The strict_geographic_reading''s premise (only natural features generate zones) directly contradicts this reading''s premise (constructed features generate zones) — they cannot coexist in one framework.',
    'If forecloses is correct, the kernel is a zero-sum interpretive contest. If coexists_with, the kernel is a pluralist dispute. If influences, this reading''s state practice creates pressure on the hybrid reading''s maturation threshold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framing_kernel_reading, conceptual, 'Structural relationship between this reading and its siblings in the commitment kernel').

omega_variable(
    suppression_mechanism_structural_vs_normative,
    'Is the measured suppression structural (coast guard patrols, military installations) or normative (legal uncertainty deterring challenge)?',
    'Decompose suppression events: physical interference incidents vs. self-restraint by third parties due to legal ambiguity. If normative suppression dominates, the constraint''s coercive footprint is smaller than the metric suggests but its chilling effect is wider.',
    'If primarily normative, the reading''s persistence depends on the hybrid reading''s ambiguity — resolving the legal uncertainty collapses the suppression. If structural, suppression persists regardless of legal clarification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_normative, empirical, 'Whether suppression operates through physical coercion or legal uncertainty''s chilling effect').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 1982, 0.15).
narrative_ontology:measurement(uncl_tr_t1995, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(uncl_tr_t2005, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(uncl_tr_t2012, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 2012, 0.35).
narrative_ontology:measurement(uncl_tr_t2016, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 2016, 0.41).
narrative_ontology:measurement(uncl_tr_t2020, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(uncl_tr_t2024, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 1982, 0.35).
narrative_ontology:measurement(uncl_be_t1995, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement(uncl_be_t2005, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(uncl_be_t2012, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 2012, 0.67).
narrative_ontology:measurement(uncl_be_t2016, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 2016, 0.75).
narrative_ontology:measurement(uncl_be_t2020, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 2020, 0.79).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 1982, 0.3).
narrative_ontology:measurement(uncl_su_t1995, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(uncl_su_t2005, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(uncl_su_t2012, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 2012, 0.68).
narrative_ontology:measurement(uncl_su_t2016, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 2016, 0.73).
narrative_ontology:measurement(uncl_su_t2020, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 2020, 0.76).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__expansive_construction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__expansive_construction_reading, 0.12).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, south_china_sea_nine_dash_line_enforcement).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, east_china_sea_senkaku_adminstration).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, arctic_seabed_claims_under_article_76).

% DUAL FORMULATION NOTE:
% This constraint is the expansive_construction_reading of the unclos_maritime_sovereignty kernel. It structurally differs from the strict_geographic_reading (ε ≈ 0.15, mountain) and hybrid_effective_control_reading (ε ≈ 0.45, tangled_rope with maturation pathway). The kernel decomposition follows the BGS pattern: the same treaty text (Articles 60, 80, 121) generates three constraints with different ε values, different beneficiary/victim structures, and different coordination/extraction balances.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__expansive_construction_reading, institutional, 0.15).
constraint_indexing:directionality_override(unclos_maritime_sovereignty__expansive_construction_reading, powerful, 0.85).
constraint_indexing:directionality_override(unclos_maritime_sovereignty__expansive_construction_reading, powerless, 0.95).
constraint_indexing:directionality_override(unclos_maritime_sovereignty__expansive_construction_reading, moderate, 0.8).
constraint_indexing:directionality_override(unclos_maritime_sovereignty__expansive_construction_reading, organized, 0.2).
constraint_indexing:directionality_override(unclos_maritime_sovereignty__expansive_construction_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
