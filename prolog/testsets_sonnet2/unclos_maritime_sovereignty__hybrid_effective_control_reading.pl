% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__hybrid_effective_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: unclos_maritime_sovereignty__hybrid_effective_control_reading
 *   human_readable: Hybrid Effective-Control Reading of UNCLOS Feature-Status Rules
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid effective-control reading of the
 *   UNCLOS maritime-feature kernel: natural features retain full
 *   EEZ/territorial-sea entitlements, but artificial features (built up from
 *   submerged shoals or low-tide elevations) generate only a 500-meter safety
 *   zone under Article 60 — UNLESS sustained, unchallenged administrative and
 *   military control over that installation persists long enough to
 *   functionally mature the claim toward something closer to territorial
 *   status, notwithstanding the formal text. This is a distinct constraint
 *   from the strict geographic reading (which holds artificial construction
 *   never alters legal status regardless of duration) and from the expansive
 *   construction reading (which treats effective occupation itself, not
 *   gradual maturation, as immediately generative of de facto waters). The
 *   hybrid reading's defining structural feature is TIME: it is the only
 *   reading in which duration of unchallenged control is doing independent
 *   legal work, distinct from either the pure-text position or the
 *   pure-occupation position.
 *
 * KEY AGENTS:
 *   - states_with_construction_and_dredging_capacity: Primary beneficiary (institutional/arbitrage) — builds and maintains presence that the reading rewards over time
 *   - regional_naval_powers: Beneficiary (institutional/arbitrage) — capacity to outlast challenge converts into legal advantage
 *   - militarily_weaker_claimant_states: Primary target (moderate/constrained) — inability to physically contest reads as acquiescence
 *   - small_island_states_without_projection_capacity: Target (powerless/trapped) — erosion of practical claims over time regardless of formal entitlement
 *   - artisanal_fishing_communities: Diffuse victim (powerless/trapped) — lose access as maturing claims expand patrolled perimeters
 *   - international_tribunals_and_arbitral_bodies: Analytical observer — can rule against the doctrine with no enforcement power
 *   - coastal_states_with_disputed_baselines: Excluded voice — prefer the strict reading but have no binding forum
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.58).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.52).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "Hybrid Effective-Control Reading of UNCLOS Feature-Status Rules").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'd60bb07b-ddaf-4903-a5a9-0391a60d248a').
narrative_ontology:cs_kernel_codification('d60bb07b-ddaf-4903-a5a9-0391a60d248a', fixed_text).
narrative_ontology:cs_authority_grounding('d60bb07b-ddaf-4903-a5a9-0391a60d248a', distributed).
narrative_ontology:cs_reading_relation('d60bb07b-ddaf-4903-a5a9-0391a60d248a', unclos_maritime_sovereignty__strict_geographic_reading, influences).
narrative_ontology:cs_reading_relation('d60bb07b-ddaf-4903-a5a9-0391a60d248a', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_axiom('d60bb07b-ddaf-4903-a5a9-0391a60d248a', foundational, duration_of_unchallenged_control_confers_incremental_title).
narrative_ontology:cs_axiom_status(duration_of_unchallenged_control_confers_incremental_title, holdable).
narrative_ontology:cs_axiom_grounding('d60bb07b-ddaf-4903-a5a9-0391a60d248a', duration_of_unchallenged_control_confers_incremental_title, conventional).
narrative_ontology:cs_axiom('d60bb07b-ddaf-4903-a5a9-0391a60d248a', secondary, artificial_feature_status_is_time_dependent_not_fixed).
narrative_ontology:cs_axiom_status(artificial_feature_status_is_time_dependent_not_fixed, holdable).
narrative_ontology:cs_axiom_grounding('d60bb07b-ddaf-4903-a5a9-0391a60d248a', artificial_feature_status_is_time_dependent_not_fixed, instrumental).
narrative_ontology:cs_reference_frame('d60bb07b-ddaf-4903-a5a9-0391a60d248a', id_1982_unclos_textual_settlement).
narrative_ontology:cs_drift_state('d60bb07b-ddaf-4903-a5a9-0391a60d248a', post_2016_pca_ruling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d60bb07b-ddaf-4903-a5a9-0391a60d248a', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_and_dredging_capacity).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_naval_powers).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, small_island_states_without_projection_capacity).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, artisanal_fishing_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess the engineering, dredging, and garrisoning capacity to convert submerged or low-tide features into artificial installations, then maintain uncontested administrative presence over years or decades. Under this reading, that sustained presence — absent formal challenge — can mature a mere 500m safety zone into a stronger territorial claim. They actively administer patrols, permits, and infrastructure to build the record of effective control the reading rewards.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_and_dredging_capacity, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_and_dredging_capacity, agenda_setter).

% Benefit from a rule that converts sustained presence and power projection into legal advantage over time, effectively rewarding capacity to persist rather than original entitlement. They face little cost from ambiguity because they can outlast diplomatic protest and out-patrol weaker claimants.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_naval_powers, beneficiary,
    institutional, generational, arbitrage, regional).

% Hold competing claims to the same features but lack the naval or diplomatic capacity to mount continuous physical challenge to an occupier's presence. Under the hybrid reading, their inability to contest in real time is read as acquiescence that helps mature the rival's claim, converting their weakness into a legal cost. Formal protest notes exist but do not, on this reading, stop the maturation clock as reliably as physical challenge would.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimant_states, payer,
    moderate, biographical, constrained, regional).

% Depend on EEZ resources (fisheries, seabed) for a large share of national revenue but cannot patrol or garrison disputed features against encroachment. The graduated-sovereignty rule means every year an outside power maintains unchallenged infrastructure on a nearby feature erodes their practical claim, regardless of what the map said at independence.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, small_island_states_without_projection_capacity, payer,
    powerless, generational, trapped, regional).

% Fish waters that fall inside contested zones. As artificial installations expand their de facto safety perimeters and patrol presence intensifies around maturing claims, traditional fishing grounds become inaccessible or dangerous to enter, with no mechanism to seek compensation since the zone's legal status is itself unsettled.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, artisanal_fishing_communities, payer,
    powerless, biographical, trapped, local).

% Adjudicate specific disputes (as in the 2016 South China Sea arbitration) but have no enforcement mechanism against a state that simply continues holding a feature. Their rulings can reject the maturation logic in principle while doing nothing to reverse it in practice, since compliance is voluntary.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_tribunals_and_arbitral_bodies, observer,
    institutional, generational, analytical, global).

% Would prefer the strict geographic reading be dispositive so that artificial construction never converts into title, but have no forum that can bind a non-compliant occupying power to that reading. Their preferred rule is legally arguable but practically unenforceable against a determined occupier.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, coastal_states_with_disputed_baselines, excluded,
    moderate, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a graduated, administrable rule distinguishing natural islands (full maritime entitlements) from artificial ones (limited safety zones), while giving states and tribunals a workable standard — effective, unchallenged control over time — for resolving otherwise intractable disputes over ambiguous or engineered features.
% TRANSFER_FUNCTION: Moves practical control over maritime resource zones, fishing grounds, and seabed rights from claimants who lack sustained physical presence to claimants who can build, garrison, and patrol continuously, converting capacity-to-persist into legal entitlement over time.
% ABSENT_VOICES: Coastal states favoring the strict geographic reading, and the artisanal communities who fish contested waters, have no seat in the interpretive process; the reading is elaborated primarily through state practice, naval doctrine, and academic commentary from powers capable of sustaining occupation, not through the claims of those who would lose under it.
% DISAPPEARANCE_RATIONALE: If the hybrid maturation doctrine vanished and only the strict geographic reading governed, occupying powers would lose the legal argument that persistence converts to title, though they might retain de facto control absent enforcement; weaker claimants would gain a cleaner (if still unenforced) legal position. Whether the world actually rearranges depends on whether the underlying power asymmetry — not just the legal theory — is what is doing the work.
% FOUNDING_PROBLEM: UNCLOS drafters needed a rule to prevent trivial or submerged features from generating disproportionate maritime zones, while still providing SOME legal mechanism for resolving long-standing, good-faith disputes over feature status where continuous state administration had already occurred before the convention's adoption.
% FOUNDING_PROBLEM_CORROBORATION: States with construction capacity attest the doctrine still serves its founding purpose of resolving genuine ambiguity through settled practice. The Permanent Court of Arbitration's 2016 South China Sea ruling and independent international law scholars attest the doctrine, as currently invoked, has been repurposed to legitimize post-hoc territorial expansion via artificial construction rather than resolve pre-existing ambiguity — corroboration from outside the beneficiary states exists but has no enforcement power to act on its own finding.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__hybrid_effective_control_reading, contested).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__hybrid_effective_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) sits deliberately between the strict geographic reading (near-zero extraction — text is dispositive, no reward for occupation) and the expansive construction reading (high extraction — occupation alone generates title immediately). The hybrid reading's intermediate ε reflects that SOME natural features are unaffected (full entitlement preserved) while artificial features are subject to a graduated, time-dependent erosion of weaker claimants' positions. Suppression (0.52) and theater_ratio (0.4) both rise steadily across the interval as more states adopt construction-and-patrol strategies explicitly calibrated to this doctrine's maturation logic post-2008 (coinciding with intensified island-building activity), turning what began as an interpretive nuance into an actively pursued strategy — the rising theater_ratio captures the increasing share of naval patrol and administrative activity that exists specifically to build the legal record rather than to serve any operational purpose.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a construction-capable regional power, the hybrid reading is a sensible, moderate compromise that avoids rewarding trivial construction while still resolving genuinely old, ambiguous disputes. From the seat of a militarily weak claimant, the identical rule reads as a slow-motion transfer mechanism: every year of non-confrontation — the only rational choice available to a weaker state facing a stronger one — is silently converted into evidence against their own claim. The engine should compute divergent seat classifications here precisely because the same durational-control criterion cuts oppositely depending on which side can afford to wait.
 *
 * DIRECTIONALITY LOGIC:
 *   States with construction and dredging capacity and regional naval powers sit near the beneficiary end: they collect strengthened territorial position over time at no formal cost beyond patrol and construction expenditure they would incur regardless. Militarily weaker claimants and small island states sit near the target end: their inability to mount continuous physical challenge — a function of relative power, not of the merits of their claim — is read by this doctrine as contributing to the maturation of a rival's title. Artisanal fishing communities are trapped, diffuse victims whose loss is a downstream effect of the same maturation dynamic rather than a direct legal target.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — resolving ambiguous, long-settled disputes over features whose status predated UNCLOS — was live in 1982 but has been substantially supplanted by its opposite use: a tool for LEGITIMIZING NEWLY CREATED ambiguity through deliberate, recent construction specifically undertaken to trigger the maturation clock. This is the tangled-rope signature: genuine coordination function (resolving old disputes) persists in principle, but the same structure now channels asymmetric extraction (rewarding recent unilateral construction) requiring active enforcement (naval patrol, administrative presence) to sustain. The classification as tangled_rope rather than snare acknowledges the doctrine still does real interpretive work in genuinely old cases even as its dominant contemporary application has become extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maturation_threshold_ambiguity,
    'How long must effective, unchallenged control persist before it is recognized (by tribunals, by state practice, by customary international law) as legally maturing a claim beyond the 500m safety zone — and is there any such threshold at all, or is this reading itself a post-hoc rationalization with no fixed content?',
    'Systematic review of state practice and arbitral precedent (including but not limited to the 2016 PCA ruling) to determine whether any tribunal has ever actually applied a durational maturation standard to uphold expanded entitlements from an artificial feature, versus rejecting the doctrine outright.',
    'If no tribunal has ever validated the maturation logic in practice, this reading is a strategic narrative advanced by construction-capable states rather than a legally operative doctrine — the story''s claimed_type would shift toward snare, since the ''coordination function'' of resolving old ambiguity would have no real instantiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maturation_threshold_ambiguity, conceptual, 'Whether the durational maturation standard has any actual legal content or is purely aspirational framing by beneficiary states.').

omega_variable(
    silence_as_acquiescence_validity,
    'Is a weaker claimant''s failure to mount continuous physical or naval challenge properly read as legal acquiescence, or is this reading illegitimately converting rational non-escalation (avoiding war with a stronger power) into a legal admission against interest?',
    'Comparative analysis of international law doctrines on acquiescence and estoppel outside the maritime context, to determine whether formal diplomatic protest (which weaker states DO lodge) is sufficient to preserve claims regardless of physical presence.',
    'If formal protest is legally sufficient and physical presence is not required, the maturation clock this reading describes would not run against protesting weaker states, substantially reducing the reading''s real-world extractive effect and its ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(silence_as_acquiescence_validity, conceptual, 'Whether formal protest without physical challenge should stop the maturation clock.').

omega_variable(
    kernel_reading_dominance_trajectory,
    'Among the three sibling readings of the UNCLOS feature-status kernel, which reading is actually gaining ground in state practice and tribunal deference over time — is the hybrid reading a stable equilibrium, or a way-station between the strict reading (in legal text) and the expansive reading (in practice)?',
    'Track adoption of each reading''s logic in state legal briefs, naval doctrine statements, and tribunal reasoning over the next decade; a drift toward invoking maturation with shorter and shorter claimed durations would indicate convergence toward the expansive reading in substance while retaining hybrid language.',
    'If the hybrid reading is drifting toward the expansive reading''s substance, this story''s ε (0.58) understates the doctrine''s true trajectory and a follow-up story with higher ε would be warranted rather than revising this one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_dominance_trajectory, empirical, 'Whether the hybrid reading is a stable middle position or a transitional label masking drift toward the expansive reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 1994, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1994, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 1994, 0.2).
narrative_ontology:measurement(uncl_tr_t2000, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(uncl_tr_t2008, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2008, 0.28).
narrative_ontology:measurement(uncl_tr_t2014, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2014, 0.34).
narrative_ontology:measurement(uncl_tr_t2020, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(uncl_tr_t2026, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2026, 0.4).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1994, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 1994, 0.32).
narrative_ontology:measurement(uncl_be_t2000, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2000, 0.36).
narrative_ontology:measurement(uncl_be_t2008, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2008, 0.42).
narrative_ontology:measurement(uncl_be_t2014, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2014, 0.53).
narrative_ontology:measurement(uncl_be_t2020, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2020, 0.56).
narrative_ontology:measurement(uncl_be_t2026, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1994, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 1994, 0.3).
narrative_ontology:measurement(uncl_su_t2000, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2000, 0.33).
narrative_ontology:measurement(uncl_su_t2008, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2008, 0.4).
narrative_ontology:measurement(uncl_su_t2014, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2014, 0.48).
narrative_ontology:measurement(uncl_su_t2020, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement(uncl_su_t2026, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2026, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.12).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, expansive_construction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the unclos_maritime_sovereignty kernel decomposed under the ε-invariance principle: strict_geographic_reading (near-zero ε, text-dispositive, Mountain-flavored), hybrid_effective_control_reading (this story, intermediate ε ~0.58, Tangled Rope), and expansive_construction_reading (higher ε, occupation-alone-suffices, more purely extractive). Each carries its own beneficiary/victim structure and its own stable ε; they are not measurement variants of one constraint but three structurally distinct claims that share a contested textual kernel (UNCLOS Article 121 and associated state practice).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
