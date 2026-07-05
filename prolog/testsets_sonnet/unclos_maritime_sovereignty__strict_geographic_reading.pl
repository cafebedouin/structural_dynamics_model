% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__strict_geographic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__strict_geographic_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__strict_geographic_reading
 *   human_readable: UNCLOS Article 121 Strict Geographic Reading of Island Status
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint instantiates the strict geographic reading of UNCLOS
 *   Article 121: only features naturally formed and above water at high tide
 *   generate territorial sea and EEZ; dredging, construction, or
 *   fortification of submerged reefs or low-tide elevations does not upgrade
 *   their legal status, no matter how permanent or defensible the resulting
 *   structure becomes. This reading was substantially vindicated by the 2016
 *   Permanent Court of Arbitration award in Philippines v. China, which held
 *   that none of the disputed Spratly features are naturally formed islands
 *   capable of generating an EEZ. The reading functions as a tangled rope: it
 *   genuinely coordinates a verifiable, survey-based standard for a
 *   chronically contested resource (maritime jurisdiction), but it also
 *   asymmetrically extracts legal recognition away from states that sank real
 *   capital into construction, handing the practical benefit of a capped map
 *   to naval powers whose interest is unconstrained transit rather than the
 *   doctrine's textual purity.
 *
 * KEY AGENTS:
 *   - naval_powers: Primary beneficiary (institutional/arbitrage) — gains predictable, capped maritime jurisdiction map favoring freedom of navigation
 *   - expansionist_coastal_states: Primary target (powerful/constrained) — construction investment stripped of legal sovereignty effect
 *   - arbitral_tribunals: Agenda-setter (institutional/analytical) — administers and articulates the doctrine without independent enforcement power
 *   - regional_fishing_communities: Excluded beneficiary (powerless/trapped) — benefits in principle, absent from the venues that decide enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.38).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.42).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "UNCLOS Article 121 Strict Geographic Reading of Island Status").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, '6a42dad9-e1b1-4356-873f-d21ea90d15d1').
narrative_ontology:cs_kernel_codification('6a42dad9-e1b1-4356-873f-d21ea90d15d1', fixed_text).
narrative_ontology:cs_authority_grounding('6a42dad9-e1b1-4356-873f-d21ea90d15d1', lineage).
narrative_ontology:cs_interpretation_layer_present('6a42dad9-e1b1-4356-873f-d21ea90d15d1').
narrative_ontology:cs_reading_relation('6a42dad9-e1b1-4356-873f-d21ea90d15d1', unclos_maritime_sovereignty__expansive_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('6a42dad9-e1b1-4356-873f-d21ea90d15d1', unclos_maritime_sovereignty__hybrid_effective_control_reading, influences).
narrative_ontology:cs_axiom('6a42dad9-e1b1-4356-873f-d21ea90d15d1', foundational, construction_cannot_upgrade_feature_status).
narrative_ontology:cs_axiom_status(construction_cannot_upgrade_feature_status, holdable).
narrative_ontology:cs_axiom_grounding('6a42dad9-e1b1-4356-873f-d21ea90d15d1', construction_cannot_upgrade_feature_status, conventional).
narrative_ontology:cs_axiom('6a42dad9-e1b1-4356-873f-d21ea90d15d1', foundational, high_tide_natural_formation_is_dispositive).
narrative_ontology:cs_axiom_status(high_tide_natural_formation_is_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('6a42dad9-e1b1-4356-873f-d21ea90d15d1', high_tide_natural_formation_is_dispositive, conventional).
narrative_ontology:cs_reference_frame('6a42dad9-e1b1-4356-873f-d21ea90d15d1', article_121_natural_formation_baseline).
narrative_ontology:cs_drift_state('6a42dad9-e1b1-4356-873f-d21ea90d15d1', post_2016_pca_award_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6a42dad9-e1b1-4356-873f-d21ea90d15d1', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, freedom_of_navigation_advocates).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, island_construction_workforce_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, regional_fishing_communities).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__strict_geographic_reading, natural_formation_doctrine).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__strict_geographic_reading, high_tide_baseline_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain global naval presence and freedom-of-navigation operations. The strict reading keeps disputed artificial features from generating territorial sea or EEZ claims that would otherwise constrain naval transit and force recognition of expanded coastal jurisdiction. Benefits without administering the doctrine directly; relies on tribunals and diplomatic pressure to enforce it.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers, beneficiary,
    institutional, generational, arbitrage, global).

% Smaller states with fishing fleets or shipping routes near disputed features benefit from the doctrine capping how much maritime space a rival claimant can generate through construction. They have no enforcement machinery of their own and depend on arbitral rulings and great-power backing to make the reading operative.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states, beneficiary,
    moderate, generational, mobile, regional).

% Bodies such as the Permanent Court of Arbitration adjudicate Article 121 disputes and have issued rulings (e.g. the 2016 South China Sea award) affirming that submerged reefs and low-tide elevations built into artificial islands do not generate territorial sea or EEZ. They administer the doctrine's content but cannot compel compliance; enforcement depends on state power, not the tribunal itself.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, arbitral_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% States that have invested heavily in dredging and building on submerged reefs and low-tide elevations to project maritime claims. The strict reading strips these constructed features of any capacity to generate territorial sea or EEZ, rendering the construction investment legally void for sovereignty purposes even though physical control persists. Exit means either abandoning the claims or continuing to assert them extra-legally against tribunal rulings.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states, payer,
    powerful, generational, constrained, regional).

% Contractors, dredging firms, and garrison personnel deployed to build and maintain artificial features. Their labor and infrastructure investment produces features that the doctrine declares legally inert regardless of physical permanence; they bear the sunk cost while gaining no corresponding legal status.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, island_construction_workforce_states, payer,
    moderate, biographical, trapped, regional).

% Communities whose traditional fishing grounds lie near disputed features benefit in principle when the strict reading prevents an expansionist state's constructed feature from converting shared waters into exclusive EEZ. In practice they have no seat in the tribunals or diplomatic negotiations that determine whether the doctrine is enforced against the state actually controlling the waters they fish.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, regional_fishing_communities, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__strict_geographic_reading, regional_fishing_communities, excluded).

% Administers seabed resource regimes that depend on stable baselines. Observes how the strict reading's outcomes affect resource allocation boundaries but does not itself adjudicate island status disputes.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_seabed_authority, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__strict_geographic_reading, diffuse).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__strict_geographic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, geographically verifiable rule — natural formation, above water at high tide — for determining which features generate territorial sea and EEZ, so competing claims can be resolved by survey and satellite record rather than by unilateral assertion of control.
% TRANSFER_FUNCTION: Moves potential maritime jurisdiction (and the fisheries, seabed resources, and strategic transit rights that come with it) away from states that have invested in artificial construction and toward the states and naval powers that benefit from a capped, predictable maritime map.
% ABSENT_VOICES: Local fishing and coastal communities near disputed features have no standing before the tribunals that decide whether their waters remain open or become an EEZ; their livelihoods are shaped by rulings issued in venues they never appear in.
% DISAPPEARANCE_RATIONALE: If the strict natural-formation standard vanished, states with the resources to dredge and build would gain a direct path to converting submerged reefs into full maritime zones, redrawing regional jurisdictional maps, shifting fishing and seabed rights, and removing the main legal check naval powers currently invoke against expansive claims.
% FOUNDING_PROBLEM: Coastal states were unilaterally declaring maritime zones around minor rocks, reefs, and eventually artificially built features, threatening to convert open or contested waters into exclusive national jurisdiction without a shared verification standard.
% FOUNDING_PROBLEM_CORROBORATION: International tribunals (the 2016 PCA award in Philippines v. China) and multiple non-claimant coastal states outside the beneficiary naval powers have affirmed the founding problem persists — construction-based claims continue to be asserted in the South China Sea and elsewhere, and the doctrine is invoked precisely because unilateral expansion attempts have not stopped.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__strict_geographic_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__strict_geographic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__strict_geographic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).
:- end_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and rising (0.18 to 0.38 over the interval) because the doctrine's practical bite grew as construction programs escalated after 2013 — the more capital an expansionist state sinks into a feature, the more the strict reading extracts by voiding that investment's legal effect. Suppression tracks similarly (0.20 to 0.42): the doctrine requires increasing diplomatic and naval pressure to hold against states that continue asserting jurisdiction over built features despite the 2016 ruling. Theater ratio stays comparatively low (0.28 at the endpoint) because the underlying survey-and-tribunal function remains substantively active, not merely performative — rulings are issued and cited even though compliance is uneven. Accessibility collapse is moderately high (0.62): once a tribunal has ruled a feature submerged-at-high-tide, the legal alternative (claiming it as a natural island) is largely foreclosed, though physical occupation persists as a non-legal fact. Resistance is high (0.71) because expansionist states actively contest the ruling's authority and continue building and garrisoning regardless of its legal conclusions.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers and non-claimant states are structural beneficiaries: they gain a predictable, geography-anchored cap on how much jurisdiction any single state can generate, without having to administer the doctrine themselves — arbitral tribunals do that. Expansionist coastal states and the workforce/infrastructure investment behind their constructed features are the targets: their capital and labor produce features the doctrine declares legally inert for sovereignty purposes, a direct transfer of potential jurisdiction away from the builder toward the world's shared or unclaimed maritime commons (which naval powers can then navigate freely). Regional fishing communities sit as an unusual case — nominal beneficiaries of a capped map, but structurally excluded from the fora where the cap's enforcement is actually decided, so their benefit is contingent on outcomes negotiated entirely without them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unverifiable, unilaterally asserted maritime claims — remains fully live; construction-based claims have if anything intensified since the doctrine's founding in 1982, and the 2016 tribunal ruling was issued precisely because the underlying dispute persists. This blocks a mandatrophy read: the strict reading is not a vestigial rule defended by inertia after its problem disappeared. It is an actively contested standard whose enforcement costs are rising because the practice it targets — building sovereignty out of concrete and dredged sand — has grown more aggressive, not less.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_versus_constructed_baseline_stability,
    'Is the natural-formation / high-tide threshold a stable, verifiable geographic fact, or is it itself subject to dispute given sea-level rise, erosion, and historical reef modification that predates the current construction era?',
    'Satellite and tidal-gauge historical record review establishing pre-construction baseline elevation of each disputed feature at multiple points before large-scale dredging began.',
    'If pre-construction baselines are themselves contested or poorly documented, the strict reading''s claimed geographic objectivity weakens, and its classification shifts closer to a policy instrument dressed as natural fact rather than a purely verifiable standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_versus_constructed_baseline_stability, empirical, 'Whether the natural/artificial baseline distinction is as verifiable in practice as the doctrine assumes.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that the UNCLOS text itself does not explicitly address artificial construction''s effect on Article 121 status, is the strict_geographic_reading the textually compelled interpretation, or is it one defensible construction among the three live readings (strict, hybrid_effective_control, expansive_construction) shaped by the interpreting body''s institutional interest in a capped maritime map?',
    'Comparative analysis of state practice, ICJ/PCA jurisprudential reasoning across multiple maritime delimitation cases, and drafting history of Article 121 to determine whether the strict reading reflects textual consensus or arbitral policy preference.',
    'If the strict reading is primarily policy-driven rather than textually compelled, its beneficiary structure (naval powers, non-claimant states) becomes evidence of an interpretive choice serving particular interests rather than a neutral geographic fact — supporting a tangled_rope rather than a more mountain-like classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether this reading is textually compelled or one interest-shaped construction among live alternatives.').

omega_variable(
    enforcement_capacity_asymmetry,
    'Does the strict reading''s persistence depend on continued naval power willingness to back tribunal rulings with military and diplomatic pressure, and what happens to the doctrine''s practical force if that willingness declines?',
    'Track compliance and enforcement-posture data (freedom of navigation operations, diplomatic statements, sanctions) over subsequent years relative to naval powers'' broader strategic priorities and resource constraints.',
    'If enforcement is contingent on naval power interest rather than the doctrine''s own legal force, the classification of the doctrine as coordination mechanism versus geopolitical instrument shifts toward the latter as naval power commitment fluctuates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_asymmetry, empirical, 'Whether the doctrine''s real-world force depends on contingent great-power enforcement willingness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 1982, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(uncl_tr_t1994, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1994, 0.12).
narrative_ontology:measurement(uncl_tr_t2005, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(uncl_tr_t2013, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2013, 0.2).
narrative_ontology:measurement(uncl_tr_t2016, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2016, 0.24).
narrative_ontology:measurement(uncl_tr_t2020, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2020, 0.26).
narrative_ontology:measurement(uncl_tr_t2026, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1982, 0.18).
narrative_ontology:measurement(uncl_be_t1994, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1994, 0.2).
narrative_ontology:measurement(uncl_be_t2005, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2005, 0.25).
narrative_ontology:measurement(uncl_be_t2013, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2013, 0.31).
narrative_ontology:measurement(uncl_be_t2016, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2016, 0.34).
narrative_ontology:measurement(uncl_be_t2020, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2020, 0.36).
narrative_ontology:measurement(uncl_be_t2026, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2026, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1982, 0.2).
narrative_ontology:measurement(uncl_su_t1994, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1994, 0.24).
narrative_ontology:measurement(uncl_su_t2005, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2005, 0.29).
narrative_ontology:measurement(uncl_su_t2013, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2013, 0.35).
narrative_ontology:measurement(uncl_su_t2016, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2016, 0.4).
narrative_ontology:measurement(uncl_su_t2020, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2020, 0.41).
narrative_ontology:measurement(uncl_su_t2026, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2026, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, south_china_sea_freedom_of_navigation_regime).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the unclos_maritime_sovereignty kernel, each instantiated as a separate constraint per the ε-invariance principle: strict_geographic_reading (this file, ε≈0.38, tangled_rope), expansive_construction_reading (higher ε, favors constructing states, likely snare or tangled_rope from the perspective of non-claimant states), and hybrid_effective_control_reading (intermediate ε, introduces a time-and-non-contestation maturation mechanism absent here). The three readings share the same underlying kernel text but produce structurally distinct beneficiary/victim sets and different ε values because they encode different normative premises about whether construction can generate sovereignty — this is not one constraint measured three ways, but three constraints sharing a textual ancestor.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
