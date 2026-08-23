% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__strict_geographic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: unclos_maritime_sovereignty__strict_geographic_reading
 *   human_readable: Strict Geographic Reading of UNCLOS Article 121 Island Definition
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   UNCLOS Article 121 defines an island as 'a naturally formed area of land,
 *   surrounded by water, which is above water at high tide.' The strict
 *   geographic reading holds that this language categorically excludes
 *   artificial islands, installations, and structures from generating
 *   territorial seas or EEZs, regardless of the scale of construction or
 *   effective control exercised. This reading is championed by naval powers
 *   and non-claimant states to preserve navigation freedoms and prevent
 *   maritime enclosure. Expansionist coastal states (principally China in the
 *   South China Sea) advance competing readings that would allow artificial
 *   features to generate maritime zones through effective occupation or
 *   administrative control. The 2016 arbitral award in Philippines v. China
 *   authoritatively endorsed the strict reading, but the constraint persists
 *   only through active enforcement — naval presence, legal argumentation,
 *   and diplomatic pressure — because the losing parties reject the
 *   tribunal's jurisdiction and continue construction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.42).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.58).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "Strict Geographic Reading of UNCLOS Article 121 Island Definition").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, '6b4241a1-0fd8-4a91-8d25-64ddd61e676c').
narrative_ontology:cs_kernel_codification('6b4241a1-0fd8-4a91-8d25-64ddd61e676c', formalized).
narrative_ontology:cs_authority_grounding('6b4241a1-0fd8-4a91-8d25-64ddd61e676c', lineage).
narrative_ontology:cs_interpretation_layer_present('6b4241a1-0fd8-4a91-8d25-64ddd61e676c').
narrative_ontology:cs_reading_relation('6b4241a1-0fd8-4a91-8d25-64ddd61e676c', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b4241a1-0fd8-4a91-8d25-64ddd61e676c', unclos_maritime_sovereignty__hybrid_effective_control_reading, coexists_with).
narrative_ontology:cs_axiom('6b4241a1-0fd8-4a91-8d25-64ddd61e676c', foundational, natural_formation_requirement).
narrative_ontology:cs_axiom_status(natural_formation_requirement, holdable).
narrative_ontology:cs_axiom_grounding('6b4241a1-0fd8-4a91-8d25-64ddd61e676c', natural_formation_requirement, conventional).
narrative_ontology:cs_axiom('6b4241a1-0fd8-4a91-8d25-64ddd61e676c', foundational, artificial_construction_never_generates_zones).
narrative_ontology:cs_axiom_status(artificial_construction_never_generates_zones, holdable).
narrative_ontology:cs_axiom_grounding('6b4241a1-0fd8-4a91-8d25-64ddd61e676c', artificial_construction_never_generates_zones, conventional).
narrative_ontology:cs_reference_frame('6b4241a1-0fd8-4a91-8d25-64ddd61e676c', unclos_article_121_plain_text).
narrative_ontology:cs_drift_state('6b4241a1-0fd8-4a91-8d25-64ddd61e676c', post_south_china_sea_arbitration_2016, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6b4241a1-0fd8-4a91-8d25-64ddd61e676c', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, flag_states_commercial_fleets).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, artificial_island_constructing_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, coastal_states_moderate_claims).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, flag_states_commercial_fleets).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, coastal_states_moderate_claims).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__strict_geographic_reading, freedom_of_navigation_principle).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__strict_geographic_reading, common_heritage_of_mankind_seabed).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__strict_geographic_reading, strict_treaty_interpretation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major naval states (US, UK, France, Japan, Australia) benefit from maximal high seas and minimal coastal state jurisdiction. They operate globally and require unrestricted transit through what would otherwise be territorial seas around artificial features. Their naval mobility gives them practical exit from any single contested zone.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers, beneficiary,
    powerful, generational, mobile, global).

% States without maritime claims in disputed regions (e.g., many African, South American, European states) benefit from the strict reading because it preserves high seas freedoms and prevents creeping enclosure of international waters. They have no direct stake in specific features but collectively uphold the legal regime.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states, beneficiary,
    moderate, biographical, mobile, global).

% Commercial shipping registries and major fleet operators benefit from predictable, minimal territorial sea claims reducing transit costs and legal risk. They pay indirectly through higher insurance and routing costs when coastal states assert expansive claims, but their primary structural position is beneficiary of open seas.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, flag_states_commercial_fleets, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__strict_geographic_reading, flag_states_commercial_fleets, payer).

% States building artificial islands on submerged features or low-tide elevations (notably China in SCS, potentially others) to extend maritime zones. They bear the cost of the strict reading: their constructed features generate only 500m safety zones, not 12nm territorial seas or 200nm EEZ. Their exit is constrained — they cannot easily relocate claims and face high political cost of retreat.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states, payer,
    powerful, generational, constrained, regional).

% States investing in land reclamation and artificial island construction for resource access, strategic depth, or status. The strict reading renders their investments legally sterile for maritime zone generation. They are locked into their construction commitments with no legal payoff in maritime zones.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, artificial_island_constructing_states, payer,
    moderate, biographical, constrained, regional).

% ITLOS, ICJ, and UNCLOS Annex VII arbitral tribunals adjudicate disputes over Article 121 interpretation. Their rulings (e.g., South China Sea Arbitration 2016) authoritatively apply the strict reading. They do not collect rents but their interpretive authority shapes the constraint's enforcement.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% Coastal states with naturally formed islands that qualify under the strict reading benefit from clear, defensible maritime zones. They pay when the strict reading is applied against their own marginal features (rocks unable to sustain human habitation). Their exit is mobile — they can accept the rule for others' features while claiming it for their own.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, coastal_states_moderate_claims, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__strict_geographic_reading, coastal_states_moderate_claims, payer).

% Local fishing communities and small-scale residents on or near disputed features. Their livelihoods depend on access to waters whose legal status the constraint determines. They have no voice in treaty interpretation, cannot exit the zone, and bear enforcement consequences (arrest, confiscation) from competing state claims.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, disputed_feature_residents_fisherfolk, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line legal test (natural formation + above water at high tide) to distinguish islands from rocks and artificial installations, enabling stable maritime boundary delimitation and preserving high seas freedoms for the international community.
% TRANSFER_FUNCTION: Transfers potential maritime jurisdiction (territorial sea + EEZ) from expansionist coastal states investing in artificial construction to the global commons (high seas) and naval/flag states. The transfer is legal entitlement, not physical resources — but the resource value (fisheries, hydrocarbons, seabed minerals) follows the jurisdiction.
% ABSENT_VOICES: Local fishing communities, indigenous maritime peoples, and small island developing states whose exclusive economic zones might be affected by neighboring artificial island construction are structurally excluded from the interpretive community of states and tribunals. They would object to both expansive claims that enclose their traditional waters and strict readings that deny them protective zones around their own marginal features.
% DISAPPEARANCE_RATIONALE: If the strict geographic reading vanished overnight, expansionist states would immediately claim territorial seas and EEZs around all artificial features, triggering a cascade of competing enclosure claims, naval confrontations, and resource disputes. The South China Sea, East China Sea, Persian Gulf, and Arctic would see rapid legal reconfiguration. The global commons would shrink measurably.
% FOUNDING_PROBLEM: UNCLOS Article 121 was drafted to prevent states from generating maritime zones from insignificant features (rocks, reefs) while preserving zones for genuine islands. The strict reading addresses the founding problem: how to draw a legally administrable line between natural territory that generates zones and human construction that does not.
% FOUNDING_PROBLEM_CORROBORATION: The 2016 South China Sea Arbitration (Philippines v. China) tribunal, constituted under UNCLOS Annex VII and comprising jurists from multiple legal traditions, unanimously applied the strict reading. Legal scholarship outside claimant states (e.g., Rothwell, Stephens, Oude Elferink, Beckman) overwhelmingly corroborates that the natural formation requirement is the treaty's plain meaning. No non-claimant state has formally endorsed the expansive construction reading.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__strict_geographic_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__strict_geographic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__strict_geographic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.42) reflects the significant but not total transfer of potential maritime jurisdiction from constructing states to the commons. The value has risen from 0.25 (1994) as construction technology and resource incentives expanded the stakes. Suppression (0.58) is substantial: the constraint's persistence depends on active naval operations (FONOPs), legal proceedings, and diplomatic coordination — without these, artificial island claims would consolidate de facto. Theater ratio (0.18) is low: the legal rule has real operational bite (the 2016 award changed nothing on the ground but shaped all subsequent diplomatic and legal discourse). Accessibility collapse (0.78) is high: once a feature is classified as artificial, no legal argument can generate zones from it — the binary natural/artificial distinction forecloses alternatives. Resistance (0.52) is moderate: expansionist states resist through non-participation in proceedings, continued construction, and alternative legal narratives, but have not fractured the interpretive consensus among tribunals and non-claimant states.
 *
 * PERSPECTIVAL GAP:
 *   From the naval power seat, this is a Rope: a coordination mechanism solving the collective action problem of maritime enclosure, with minimal coercive overhead (FONOPs are routine, not extraordinary). From the expansionist coastal state seat, it is a Snare: the coordination story (bright-line rule) is cover for locking them out of resource zones they could physically control, enforced by superior naval power. The engine computes this divergence from the structural data — the authored claim (tangled_rope) acknowledges both the genuine coordination function (stable boundaries, navigational predictability) and the asymmetric extraction (concentrated cost on one identifiable group of states).
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers and non-claimant states are structural beneficiaries (d near 0.0): the constraint subsidizes their operational freedom and requires no sacrifice. Expansionist coastal states are full targets (d near 1.0): they invest heavily in construction and receive zero maritime zone return under this reading. Artificial island constructing states are similarly targeted. International tribunals sit at d=0.5 (analytical): they administer the constraint but collect no rents. Coastal states with moderate claims are near-symmetric: they benefit from the rule's clarity for their own natural features but pay when it denies zones to their marginal features. Disputed feature residents are trapped (identity_locked → high d): they bear enforcement costs from all sides with no exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing maritime zone generation from insignificant features) remains live — technology has made artificial construction easier, not obsolete. The constraint has not atrophied into a Piton; enforcement effort has increased, not decreased. However, the mandate shows tension: the strict reading was designed for rocks and reefs, not for billion-dollar artificial island complexes with military installations. The coordination function (legal certainty) and extraction function (denying zones to constructors) are increasingly at odds as construction scale grows. This tension is the tangled_rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the strict_geographic_reading a distinct constraint from the expansive_construction_reading and hybrid_effective_control_reading, or are they observables of a single constraint measured differently?',
    'Apply the ε-invariance test: if changing the observable (legal text vs. state practice vs. tribunal jurisprudence) changes ε, they are distinct constraints. The strict reading''s ε (0.42) derives from the standing arrangement of non-recognition of artificial island zones; the expansive reading''s ε would derive from the standing arrangement of de facto enclosure. These are different referents.',
    'If distinct, each reading gets its own constraint story with independent classification. If unified, the corpus must model observable-dependent classification (forbidden by DP-001). The kernel_id/unclos_maritime_sovereignty groups them for network analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three readings are structurally distinct constraints or one constraint with measurement variance.').

omega_variable(
    natural_vs_artificial_boundary,
    'Where exactly does the natural/artificial boundary lie for features that are partially natural (e.g., a rock augmented with concrete, a reef with structures built atop it)?',
    'Future tribunal rulings on specific augmented features, or state practice converging on a technical threshold (e.g., percentage of natural substrate, permanence of augmentation).',
    'A porous boundary increases extraction (states game the threshold) and suppression (more enforcement needed). A sharp boundary reduces both but may be legally arbitrary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_artificial_boundary, empirical, 'The precise legal threshold between natural formation and artificial construction for hybrid features.').

omega_variable(
    effective_control_maturation,
    'Can prolonged effective control of an artificial feature mature into a legal title generating maritime zones, despite the treaty text?',
    'State practice and opinio juris over decades; possible future tribunal case on a feature held exclusively for 50+ years without challenge.',
    'If yes, the hybrid_effective_control_reading gains ground, the strict reading''s extraction decreases (some constructed features eventually get zones), and the constraint drifts toward scaffold (transitional). If no, the strict reading''s extraction and suppression remain high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(effective_control_maturation, conceptual, 'Whether prescription/acquisitive prescription can override Article 121''s natural formation requirement.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.58) primarily structural (naval enforcement, legal proceedings) or internalized (constructing states self-restraining due to legal socialization)?',
    'Compare suppression levels in zones with vs. without active naval presence; track constructing states'' behavior when enforcement capacity is temporarily absent.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint persists even without active enforcement. If structural, suppression collapses when naval presence withdraws.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the maritime legal order.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_strict_geo_tr_t1994, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1994, 0.12).
narrative_ontology:measurement(unclos_strict_geo_tr_t2002, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2002, 0.13).
narrative_ontology:measurement(unclos_strict_geo_tr_t2009, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2009, 0.14).
narrative_ontology:measurement(unclos_strict_geo_tr_t2012, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2012, 0.15).
narrative_ontology:measurement(unclos_strict_geo_tr_t2016, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2016, 0.16).
narrative_ontology:measurement(unclos_strict_geo_tr_t2020, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2020, 0.17).
narrative_ontology:measurement(unclos_strict_geo_tr_t2024, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2024, 0.18).

% Extraction over time
narrative_ontology:measurement(unclos_strict_geo_be_t1994, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1994, 0.25).
narrative_ontology:measurement(unclos_strict_geo_be_t2002, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2002, 0.28).
narrative_ontology:measurement(unclos_strict_geo_be_t2009, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2009, 0.31).
narrative_ontology:measurement(unclos_strict_geo_be_t2012, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2012, 0.35).
narrative_ontology:measurement(unclos_strict_geo_be_t2016, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2016, 0.4).
narrative_ontology:measurement(unclos_strict_geo_be_t2020, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2020, 0.41).
narrative_ontology:measurement(unclos_strict_geo_be_t2024, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(unclos_strict_geo_su_t1994, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1994, 0.35).
narrative_ontology:measurement(unclos_strict_geo_su_t2002, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2002, 0.4).
narrative_ontology:measurement(unclos_strict_geo_su_t2009, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2009, 0.45).
narrative_ontology:measurement(unclos_strict_geo_su_t2012, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2012, 0.52).
narrative_ontology:measurement(unclos_strict_geo_su_t2016, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2016, 0.56).
narrative_ontology:measurement(unclos_strict_geo_su_t2020, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2020, 0.57).
narrative_ontology:measurement(unclos_strict_geo_su_t2024, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__strict_geographic_reading, 0.1).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, south_china_sea_nine_dash_line).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, freedom_of_navigation_operations).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, seabed_mining_regulation_isa).

% DUAL FORMULATION NOTE:
% This constraint is one member of the unclos_maritime_sovereignty kernel family. The expansive_construction_reading and hybrid_effective_control_reading are sibling constraints with different ε values, beneficiary/victim structures, and claimed types. The strict reading has the lowest extraction (0.42) because it denies zones to constructors; the expansive reading would have higher extraction (enclosure of commons); the hybrid reading sits between. All three share the same kernel (Article 121) but instantiate different constraints per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__strict_geographic_reading, institutional, 0.48).
constraint_indexing:directionality_override(unclos_maritime_sovereignty__strict_geographic_reading, powerful, 0.85).
constraint_indexing:directionality_override(unclos_maritime_sovereignty__strict_geographic_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
