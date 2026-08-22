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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Hybrid Effective-Control Reading of Maritime Feature Status (UNCLOS Art. 121)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This story instantiates the hybrid effective-control reading of the
 *   UNCLOS Article 121 feature-status kernel: natural features generate full
 *   maritime zones automatically, artificial features generate only a 500m
 *   safety zone at inception, but sustained, unchallenged administrative and
 *   military control over an artificial feature can mature that limited zone
 *   into something functionally indistinguishable from a territorial claim.
 *   This is a deliberately intermediate reading between the strict geographic
 *   test (artificial construction never changes legal status) and the
 *   expansive construction reading (building alone generates territorial
 *   waters). The intermediate ε (0.58) reflects that this reading does real
 *   coordination work — it gives states and tribunals a workable rule for
 *   feature disputes that would otherwise have no answer for prolonged
 *   occupation — while also creating a durable channel through which
 *   construction and naval capacity convert into territorial advantage over
 *   time, which the strict reading would foreclose entirely.
 *
 * KEY AGENTS:
 *   - states_with_construction_and_dredging_capacity: primary beneficiary, converts engineering capacity into graduated sovereignty
 *   - regional_naval_power_projectors: secondary beneficiary, converts military presence into legal weight via uncontested duration
 *   - militarily_weaker_claimant_states: primary target, penalized for inability to mount continuous real-time contestation
 *   - small_island_fishing_communities: diffuse victim, lose access without ever being party to the underlying dispute
 *   - international_maritime_tribunals: analytical observer, can declare status but cannot enforce removal
 *   - flag_state_shipping_and_overflight_interests: excluded party bearing operational ambiguity costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.58).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.62).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "Hybrid Effective-Control Reading of Maritime Feature Status (UNCLOS Art. 121)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, '209a6836-1276-41b4-8556-764a415e25b5').
narrative_ontology:cs_kernel_codification('209a6836-1276-41b4-8556-764a415e25b5', fixed_text).
narrative_ontology:cs_authority_grounding('209a6836-1276-41b4-8556-764a415e25b5', distributed).
narrative_ontology:cs_reading_relation('209a6836-1276-41b4-8556-764a415e25b5', unclos_maritime_sovereignty__strict_geographic_reading, influences).
narrative_ontology:cs_reading_relation('209a6836-1276-41b4-8556-764a415e25b5', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_axiom('209a6836-1276-41b4-8556-764a415e25b5', foundational, duration_of_uncontested_control_generates_legal_consequence).
narrative_ontology:cs_axiom_status(duration_of_uncontested_control_generates_legal_consequence, holdable).
narrative_ontology:cs_axiom_grounding('209a6836-1276-41b4-8556-764a415e25b5', duration_of_uncontested_control_generates_legal_consequence, conventional).
narrative_ontology:cs_axiom('209a6836-1276-41b4-8556-764a415e25b5', secondary, artificial_origin_caps_initial_zone_but_not_permanently).
narrative_ontology:cs_axiom_status(artificial_origin_caps_initial_zone_but_not_permanently, holdable).
narrative_ontology:cs_axiom_grounding('209a6836-1276-41b4-8556-764a415e25b5', artificial_origin_caps_initial_zone_but_not_permanently, instrumental).
narrative_ontology:cs_reference_frame('209a6836-1276-41b4-8556-764a415e25b5', article_121_drafting_era_natural_formation_baseline).
narrative_ontology:cs_drift_state('209a6836-1276-41b4-8556-764a415e25b5', post_south_china_sea_construction_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('209a6836-1276-41b4-8556-764a415e25b5', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_and_dredging_capacity).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_naval_power_projectors).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, small_island_fishing_communities).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__hybrid_effective_control_reading, effective_control_doctrine).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__hybrid_effective_control_reading, acquiescence_bars_later_challenge).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess the industrial capacity to dredge, build, and garrison artificial features on reefs and low-tide elevations. Under this reading, sustained administrative presence and lack of formal challenge over time can mature a mere 500m safety zone into a claim treated as territorial in practice, even though the feature began as artificial. They invest in permanent structures precisely because duration of uncontested control is what the reading rewards.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_and_dredging_capacity, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_and_dredging_capacity, agenda_setter).

% Use naval and coast guard presence to deter contestation of features they occupy. The hybrid reading gives their administrative continuity legal weight it would not have under a strict geographic test, converting military and logistical dominance into an argument for eventual sovereign status.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_naval_power_projectors, beneficiary,
    institutional, civilizational, arbitrage, regional).

% Hold overlapping claims to the same features or waters but lack the capability to build competing installations or to mount a continuous naval challenge. Under this reading, their inability to contest occupation in real time is read as acquiescence, converting their weakness into the sibling's legal victory. Legal recourse through arbitration is slow and non-self-enforcing against a state that simply stays.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimant_states, payer,
    moderate, generational, constrained, regional).

% Depend on traditional fishing grounds now enclosed within safety zones or contested EEZ boundaries generated by artificial features. As occupation is normalized under this reading, their access to fishing grounds shrinks without any formal transfer of rights ever having occurred against them directly.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, small_island_fishing_communities, payer,
    powerless, biographical, trapped, local).

% Adjudicate disputes under UNCLOS but have no independent enforcement power; their rulings (e.g. the 2016 South China Sea arbitration) can declare artificial features non-qualifying yet cannot compel removal of installations already built and defended, illustrating the gap this reading occupies between formal law and administered fact.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_maritime_tribunals, observer,
    institutional, civilizational, analytical, global).

% Global shipping and airline operators rely on freedom of navigation through contested waters. They are not party to the sovereignty dispute but bear the operational cost of ambiguous or expanding safety zones and are not consulted when a feature's status shifts from artificial-limited to effectively-controlled-durable.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, flag_state_shipping_and_overflight_interests, excluded,
    powerful, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_and_dredging_capacity).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__hybrid_effective_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a graduated, administrable test that lets states distinguish minor safety-zone assertions around engineering works from genuine long-term territorial administration, avoiding the need to relitigate feature status every time a structure is built.
% TRANSFER_FUNCTION: Moves de facto control over adjacent waters, fishing grounds, and resource rights from states lacking construction/naval capacity to states possessing it, mediated through the passage of uncontested time rather than through negotiated cession.
% ABSENT_VOICES: Small-scale fishing communities and non-claimant maritime users (shipping lines, regional fishers of third states) have no seat in the bilateral or multilateral disputes that settle feature status; their access losses are treated as a side effect, never as a party interest requiring compensation.
% DISAPPEARANCE_RATIONALE: If the effective-control maturation doctrine vanished and only the strict geographic test applied, dozens of artificial installations currently functioning as quasi-territorial outposts would revert to 500m safety zones only, collapsing claimed EEZ overlaps, altering naval basing calculus, and reopening fishing grounds currently treated as foreclosed.
% FOUNDING_PROBLEM: UNCLOS Article 121 needed a workable rule distinguishing islands from rocks and artificial structures, but drafters could not anticipate large-scale island-building and had no mechanism for resolving disputes where a state simply occupies and administers a feature for decades without formal annexation or war.
% FOUNDING_PROBLEM_CORROBORATION: The Permanent Court of Arbitration's 2016 Philippines v. China award attests, from outside the benefiting states, that the founding problem (defining feature status objectively) was intended to be geographic and factual, not a function of administrative duration — while the construction-capable states themselves assert the founding problem was always about workable governance of occupied space, a claim made from inside the beneficiary group and not independently corroborated.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__hybrid_effective_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__hybrid_effective_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises across the interval (0.28 to 0.58) tracking the accumulation of large-scale island-building programs from the mid-2000s onward — as more states discovered that duration-of-control functions as a legal asset, the extractive channel widened. Theater ratio rises and plateaus around 0.4: a genuine share of activity is legitimate safety-zone administration (navigation aids, environmental monitoring), but an increasing share is performative assertion of 'effective administration' staged for legal record rather than operational need (flag-raising ceremonies, permanent garrison rotations, symbolic infrastructure). Suppression climbs steadily (0.35 to 0.62) as the doctrine's core mechanism is precisely that non-contestation is read as acquiescence — weaker states are structurally pressured toward silence because contestation is costly and inaction is legally punished, which is itself a suppressive dynamic distinct from and layered on top of ordinary geopolitical power asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   From the construction-capable state's seat, the doctrine looks like a rope: a sensible, graduated rule that avoids treating every man-made structure as either fully sovereign or legally inert, and rewards the costly investment of sustained administration. From the weaker claimant's seat, the same rule is a tangled rope shading toward snare: a formally neutral 'duration and non-challenge' test that in practice can only be satisfied by states with the naval and financial capacity to defend a feature indefinitely, meaning the rule's neutral language conceals an asymmetric extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   States with construction and dredging capacity and regional naval powers are declared beneficiaries because the doctrine's core mechanism — maturation through unchallenged control — is a resource only they can reliably deploy; this pushes their directionality toward the beneficiary end. Militarily weaker claimants and fishing communities are declared victims because their structural incapacity to sustain real-time challenge is precisely what the doctrine converts into legal loss; their exit options are constrained or trapped, pushing directionality toward the target end. The tribunal observer seat sits analytically outside the extraction flow, declaring status without controlling outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Article 121's silence on artificial features and prolonged occupation absent war or treaty — remains genuinely unresolved rather than dead; this prevents a simple snare classification, since real ambiguity is being managed, not manufactured. But the doctrine has drifted from filling a gap in the law to becoming the primary mechanism by which capacity differentials translate into sovereignty, and enforcement mechanisms (naval presence, garrison maintenance, non-recognition diplomacy) have hardened over the interval — this is why tangled_rope, not scaffold or rope, is the structurally correct claim: a genuine coordination function persists, but it is bundled with an asymmetric extraction channel that requires active maintenance (naval patrols, diplomatic non-recognition campaigns, continuous administrative presence) to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maturation_threshold_ambiguity,
    'How much uncontested time and what kind of administrative acts are sufficient to mature a 500m safety zone into an effectively territorial claim? No treaty text or settled arbitral precedent specifies a duration or an evidentiary bar.',
    'Accumulation of arbitral awards or ICJ rulings that either specify a duration/evidentiary standard or reject the maturation doctrine outright as inconsistent with Article 121''s text.',
    'A specified, high threshold would narrow this reading''s extractive scope significantly; a rejection of maturation entirely would collapse this reading into the strict geographic reading. An unspecified, low, or purely political threshold (as currently) maximizes the extractive channel for capacity-rich states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maturation_threshold_ambiguity, conceptual, 'Absence of a settled maturation threshold or evidentiary standard for effective control.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the hybrid effective-control reading a genuine doctrinal middle ground being developed in good faith by tribunals and scholars, or is it a strategically convenient compromise position that construction-capable states promote precisely because it legitimizes their existing installations while the strict reading (which would delegitimize them) and the expansive reading (which would be too transparently self-serving) are both avoided?',
    'Track which states and legal scholars actively advocate for this reading versus the sibling readings, and whether advocacy correlates with a state''s existing construction footprint.',
    'If advocacy correlates strongly with existing construction footprint, the ''graduated, moderate'' framing of this reading is itself an extraction-legitimizing move rather than neutral doctrinal development — this would support classifying the reading''s own persistence as partly performative (theater) rather than purely a good-faith legal compromise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether reading selection tracks doctrinal merit or strategic self-interest of capacity-rich states.').

omega_variable(
    acquiescence_versus_incapacity,
    'When a weaker claimant fails to contest an occupied feature, is that failure legally meaningful acquiescence (a considered choice not to object) or simple incapacity (inability to mount naval patrols, expensive arbitration, or diplomatic pressure campaigns)? The doctrine as applied does not distinguish these.',
    'Case-by-case examination of whether non-contesting states issued formal diplomatic protests, filed notes verbales, or otherwise signaled objection despite lacking capacity to physically intervene — a distinguishable proxy for intent versus incapacity.',
    'If most ''non-contestation'' cases involve states that did protest formally but lacked capacity to enforce the protest, the doctrine is misreading incapacity as acquiescence, which would sharply raise the measured suppression and extraction of this reading and weaken its coordination justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acquiescence_versus_incapacity, empirical, 'Whether the doctrine''s acquiescence trigger conflates genuine assent with mere inability to contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1994, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 1994, 0.2).
narrative_ontology:measurement_basis(uncl_tr_t1994, observed).
narrative_ontology:measurement(uncl_tr_t2000, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement_basis(uncl_tr_t2000, observed).
narrative_ontology:measurement(uncl_tr_t2008, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement_basis(uncl_tr_t2008, observed).
narrative_ontology:measurement(uncl_tr_t2014, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2014, 0.38).
narrative_ontology:measurement_basis(uncl_tr_t2014, observed).
narrative_ontology:measurement(uncl_tr_t2019, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2019, 0.4).
narrative_ontology:measurement_basis(uncl_tr_t2019, observed).
narrative_ontology:measurement(uncl_tr_t2024, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2024, 0.4).
narrative_ontology:measurement_basis(uncl_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1994, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 1994, 0.28).
narrative_ontology:measurement_basis(uncl_be_t1994, observed).
narrative_ontology:measurement(uncl_be_t2000, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement_basis(uncl_be_t2000, observed).
narrative_ontology:measurement(uncl_be_t2008, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2008, 0.4).
narrative_ontology:measurement_basis(uncl_be_t2008, observed).
narrative_ontology:measurement(uncl_be_t2014, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2014, 0.52).
narrative_ontology:measurement_basis(uncl_be_t2014, observed).
narrative_ontology:measurement(uncl_be_t2019, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2019, 0.55).
narrative_ontology:measurement_basis(uncl_be_t2019, observed).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(uncl_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1994, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 1994, 0.35).
narrative_ontology:measurement_basis(uncl_su_t1994, observed).
narrative_ontology:measurement(uncl_su_t2000, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement_basis(uncl_su_t2000, observed).
narrative_ontology:measurement(uncl_su_t2008, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2008, 0.48).
narrative_ontology:measurement_basis(uncl_su_t2008, observed).
narrative_ontology:measurement(uncl_su_t2014, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2014, 0.58).
narrative_ontology:measurement_basis(uncl_su_t2014, observed).
narrative_ontology:measurement(uncl_su_t2019, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement_basis(uncl_su_t2019, observed).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2024, 0.62).
narrative_ontology:measurement_basis(uncl_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.12).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, expansive_construction_reading).

% DUAL FORMULATION NOTE:
% This story is the middle member of a three-story constraint family decomposing the colloquial 'UNCLOS artificial island rule' into structurally distinct kernel readings. strict_geographic_reading treats construction as legally inert (low ε, closer to rope/mountain-adjacent formalism). expansive_construction_reading treats construction as directly territory-generating (high ε, closer to snare). This hybrid_effective_control_reading occupies the intermediate position: intermediate ε, tangled_rope classification, graduated by duration of uncontested control. All three share the same underlying text (UNCLOS Art. 121) and the same contested feature-status disputes (South China Sea, East China Sea) but assign different legal consequences and different beneficiary/victim structures to the same facts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
