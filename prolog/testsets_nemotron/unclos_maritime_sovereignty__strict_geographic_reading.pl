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
 *   human_readable: Strict Geographic Reading of UNCLOS Article 121 Maritime Sovereignty
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint story captures the strict geographic reading of UNCLOS
 *   Article 121(3): "Rocks which cannot sustain human habitation or economic
 *   life of their own shall have no exclusive economic zone or continental
 *   shelf" — read together with Article 121(1) defining an island as "a
 *   naturally formed area of land, surrounded by water, which is above water
 *   at high tide." The strict reading holds that artificial construction on
 *   submerged features, low-tide elevations, or rocks cannot generate
 *   territorial seas or EEZs. This reading is contested by expansionist
 *   coastal states who argue that effective occupation and administrative
 *   control of constructed features mature into territorial entitlements. The
 *   kernel (UNCLOS maritime sovereignty) admits multiple readings; this story
 *   instantiates only the strict geographic reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.72).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.68).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "Strict Geographic Reading of UNCLOS Article 121 Maritime Sovereignty").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, '3fd11fa3-f5b6-4160-a35d-f0237bdd25fd').
narrative_ontology:cs_kernel_codification('3fd11fa3-f5b6-4160-a35d-f0237bdd25fd', formalized).
narrative_ontology:cs_authority_grounding('3fd11fa3-f5b6-4160-a35d-f0237bdd25fd', lineage).
narrative_ontology:cs_interpretation_layer_present('3fd11fa3-f5b6-4160-a35d-f0237bdd25fd').
narrative_ontology:cs_reading_relation('3fd11fa3-f5b6-4160-a35d-f0237bdd25fd', unclos_maritime_sovereignty__expansive_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('3fd11fa3-f5b6-4160-a35d-f0237bdd25fd', unclos_maritime_sovereignty__hybrid_effective_control_reading, coexists_with).
narrative_ontology:cs_axiom('3fd11fa3-f5b6-4160-a35d-f0237bdd25fd', foundational, maritime_entitlement_anchored_exclusively_in_natural_geography).
narrative_ontology:cs_axiom_status(maritime_entitlement_anchored_exclusively_in_natural_geography, holdable).
narrative_ontology:cs_axiom_grounding('3fd11fa3-f5b6-4160-a35d-f0237bdd25fd', maritime_entitlement_anchored_exclusively_in_natural_geography, conventional).
narrative_ontology:cs_axiom('3fd11fa3-f5b6-4160-a35d-f0237bdd25fd', foundational, artificial_construction_cannot_alter_feature_classification_under_article_121).
narrative_ontology:cs_axiom_status(artificial_construction_cannot_alter_feature_classification_under_article_121, holdable).
narrative_ontology:cs_axiom_grounding('3fd11fa3-f5b6-4160-a35d-f0237bdd25fd', artificial_construction_cannot_alter_feature_classification_under_article_121, conventional).
narrative_ontology:cs_reference_frame('3fd11fa3-f5b6-4160-a35d-f0237bdd25fd', unclos_article_121_textual_primacy).
narrative_ontology:cs_drift_state('3fd11fa3-f5b6-4160-a35d-f0237bdd25fd', post_south_china_sea_arbitration_2016, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3fd11fa3-f5b6-4160-a35d-f0237bdd25fd', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, international_shipping_interests).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, artificial_island_builders).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__strict_geographic_reading, article_121_natural_formation_requirement).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__strict_geographic_reading, territorial_sea_generation_requires_natural_island).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__strict_geographic_reading, artificial_construction_does_not_alter_feature_classification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major naval states (US, UK, France, Japan, Australia, India) benefit from maximal freedom of navigation and overflight. They rely on the strict reading to preserve high-seas corridors through contested archipelagos and to deny expansive territorial sea claims that would constrain carrier group operations and intelligence gathering. Their institutional navies operate globally with minimal exit costs from any single maritime regime.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers, beneficiary,
    institutional, generational, arbitrage, global).

% Coastal and landlocked states without maritime claims in the relevant regions (e.g., European states in South China Sea, African states in Arctic). They benefit from the strict reading as a default rule that prevents creeping enclosure of global commons. Their exit option is diplomatic alignment with naval powers; they are mobile across normative coalitions.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states, observer).

% Global container lines, bulk carriers, tanker fleets, and their flag states. They benefit from predictable, narrow territorial seas that minimize transit costs, pilotage requirements, and legal risk. Their exit is constrained by physical geography — ships must transit chokepoints — but they have arbitrage-grade exit in registry choice and route optimization.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_shipping_interests, beneficiary,
    powerful, biographical, constrained, global).

% States building artificial islands on submerged features or low-tide elevations to extend territorial seas and EEZs (China in South China Sea, Turkey in Aegean/Eastern Mediterranean, Russia in Arctic). They bear the cost of the strict reading: their multi-billion-dollar construction programs, administrative apparatuses, and strategic narratives are legally nullified. Exit is identity-locked — their territorial identity, regime legitimacy, and strategic doctrine are fused to the claim that construction creates sovereignty.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states, payer,
    institutional, generational, identity_locked, regional).

% State-owned enterprises, military engineering corps, and parastatal construction firms executing the physical buildup. They are paid to build but trapped in the strategic dead-end: if the strict reading prevails, their installations generate no territorial sea, only 500m safety zones. They cannot exit the constraint without abandoning the institutional mission that defines them.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, artificial_island_builders, payer,
    organized, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__strict_geographic_reading, artificial_island_builders, agenda_setter).

% ITLOS, ICJ, PCA tribunals adjudicating maritime disputes. They set the authoritative interpretation of Article 121 through case law (South China Sea Arbitration, Nicaragua v. Colombia, etc.). Their power derives from states' consent to jurisdiction; their exit is analytical — they observe and rule but do not bear territorial consequences.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% Low-lying atoll nations (Kiribati, Marshall Islands, Tuvalu, Maldives) facing sea-level rise. They are excluded from the strict vs. expansive binary: their natural islands may disappear, and the strict reading denies them the legal tool (artificial elevation/maintenance) to preserve maritime entitlements. They have no exit — their territory and EEZ are existentially bound to features the strict reading treats as fixed by nature.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, small_island_developing_states, excluded,
    powerless, generational, trapped, regional).

% Academic commentators, ILC members, treaty body experts. They map the interpretive landscape, trace state practice, and advise tribunals. They have no material stake but frame the conceptual categories ("naturally formed," "above water at high tide," "installation") that all parties must inhabit.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, legal_scholars_international_law, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, nature-anchored rule for which land features generate maritime zones, preventing a race to construct facts on the water that would destabilize the global ocean commons and trigger endless sovereignty disputes over engineered features.
% TRANSFER_FUNCTION: Moves the legal capacity to generate territorial seas and EEZs from artificial construction activity back to natural geography. Expansionist coastal states lose the maritime zones their construction would claim; naval powers, shipping interests, and non-claimant states retain high-seas freedoms and navigational predictability.
% ABSENT_VOICES: Small island developing states facing existential sea-level rise are structurally excluded — their interest in artificial maintenance/elevation of natural features to preserve maritime entitlements is not represented in the strict vs. expansive contest. Indigenous coastal communities whose traditional marine tenure crosses the natural/artificial boundary are also absent.
% DISAPPEARANCE_RATIONALE: If the strict reading vanished, expansionist states would accelerate artificial island construction as a recognized sovereignty technique, maritime boundaries would become construction-dependent rather than geography-dependent, and the global commons would shrink as every submerged feature becomes a potential territorial sea generator. Naval powers would lose legal grounding for FONOPs; shipping would face proliferating territorial sea claims.
% FOUNDING_PROBLEM: UNCLOS III (1973-1982) needed to close the loophole where states could manufacture sovereignty by building on reefs, shoals, and submerged banks — a practice already emerging in the 1970s (e.g., Philippines in Spratlys, Japan in Okinotorishima). The strict formulation was the compromise that anchored maritime entitlements in physical geography rather than engineering capacity.
% FOUNDING_PROBLEM_CORROBORATION: The negotiating history of UNCLOS Article 121 (recorded in the Official Records of the Third UN Conference on the Law of the Sea) shows delegates explicitly rejecting proposals to grant maritime zones to artificial islands. The 2016 South China Sea Arbitration (PCA Case No. 2013-19) corroborates from outside the benefiting parties: the tribunal, composed of jurists from non-claimant states, unanimously held that artificial construction cannot change a feature's classification under Article 121(3).
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__strict_geographic_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__strict_geographic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(unclos_maritime_sovereignty__strict_geographic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the strict reading strips expansionist states of the maritime zones their construction programs were designed to capture — billions in infrastructure investment and strategic positioning are legally nullified. Suppression (0.68) is substantial because the constraint's persistence depends on active legal enforcement (tribunal rulings, FONOP programs, diplomatic protests) against states that reject it. Theater ratio (0.41) is elevated: legal processes (arbitration, ICJ proceedings, ILC studies) perform adjudication while the physical facts on water (constructed islands, militarized features) continue to accumulate. Accessibility collapse (0.58) is moderate — alternatives (expansive, hybrid readings) persist in state practice despite the strict reading's tribunal victories. Resistance (0.63) is high — expansionist states actively resist through construction, white papers, domestic legislation, and refusal to participate in arbitration.
 *
 * PERSPECTIVAL GAP:
 *   From the naval power/agenda_setter seat, this is a genuine coordination mechanism (rope-like) that anchors maritime order in immutable geography. From the expansionist coastal state/payer seat, it is an enforced extraction mechanism (snare-like) that freezes the maritime map to the advantage of established naval powers. The engine computes this divergence from the structural data: same constraint, different effective extraction per seat. The strict reading's claimed_type (tangled_rope) reflects this structural hybridity — it coordinates (prevents construction races) AND extracts (nullifies specific states' investments).
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers, non-claimant states, and shipping interests are structural beneficiaries (d ~ 0.1-0.2): the strict reading preserves their navigation freedoms and prevents enclosure. Expansionist coastal states and their builders are structural targets (d ~ 0.8-0.9): they bear the full cost of the constraint's enforcement. Small island developing states are excluded (d undefined in the binary) — their existential interest falls outside the coordination/extraction frame. International tribunals are agenda_setters with analytical exit — they administer the constraint but do not bear its territorial consequences. The identity_locked exit of expansionist states is critical: their regime legitimacy is fused to the claim that construction creates sovereignty, making exit politically impossible without regime transformation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing manufactured sovereignty) remains live — construction technology has only advanced, making artificial island building cheaper, faster, and more scalable. However, the constraint's coordination function has been partially captured: the strict reading now serves as a legal shield for established naval powers' operational freedom while disproportionately burdening late-coming coastal states. This is not pure mandatrophy (the original problem persists) but a structural drift where the coordination mechanism has become a tool of strategic containment. The hybrid reading emerges as a pragmatic adaptation — recognizing that some constructed features have acquired de facto administrative reality that pure strictness cannot erase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sea_level_rise_natural_feature_disappearance,
    'When natural islands submerge due to sea-level rise, does the strict reading require their maritime zones to extinguish, or does a ''once an island, always an island'' doctrine emerge to preserve entitlements?',
    'State practice and tribunal rulings on disappearing atoll baselines; ILC work on sea-level rise and international law; UNGA resolutions on protection of maritime zones of vulnerable states.',
    'If maritime zones extinguish with the natural feature, the strict reading becomes an existential threat to small island states — converting them from excluded to primary victims. If a persistence doctrine emerges, the strict reading''s natural-geography anchor is softened, creating a precedent that could be invoked by expansionist states for constructed features.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sea_level_rise_natural_feature_disappearance, conceptual, 'Whether the strict reading''s natural-geography anchor survives the physical disappearance of natural features.').

omega_variable(
    construction_vs_maintenance_boundary,
    'Where is the line between ''artificial construction'' (which does not alter status) and ''maintenance/preservation of a naturally formed feature'' (which might)?',
    'Technical criteria from coastal engineering, ILC guidance, tribunal fact-finding on whether works alter the feature''s fundamental character vs. arrest natural erosion.',
    'A permissive maintenance exception would allow small island states to preserve their maritime zones but would also give expansionist states a doctrinal opening to characterize their massive construction as ''maintenance'' of a natural feature (e.g., claiming a submerged reef was once an island). A strict prohibition on any artificial alteration preserves the bright line but sacrifices vulnerable states.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(construction_vs_maintenance_boundary, conceptual, 'The boundary between prohibited construction and permitted preservation of natural features.').

omega_variable(
    effective_control_maturation_threshold,
    'At what point does prolonged, unchallenged effective control of an artificial feature create a legal title that even the strict reading must recognize?',
    'Customary international law formation: state practice + opinio juris over time; tribunal recognition of acquisitive prescription or historic title applied to maritime features.',
    'If effective control matures into title, the strict reading has a temporal expiration — it applies only to new construction, not to consolidated situations. This would validate the hybrid reading''s maturation pathway. If no maturation occurs, the strict reading is temporally invariant but faces growing compliance gaps as constructed features become entrenched facts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_control_maturation_threshold, empirical, 'Whether prolonged effective control can overcome the strict reading''s geographic formalism.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the kernel ''UNCLOS maritime sovereignty'' a single commitment system with competing readings, or are the strict, expansive, and hybrid readings structurally distinct constraints that merely share a treaty text?',
    'Analyze whether the readings share a common authority_grounding and interpretation_layer (single CS) or have divergent authority structures (separate CSs). Test: does a party adopting one reading logically foreclose the others within its own framework?',
    'If a single CS, the readings are in_contention and the engine''s cs_axiom_contradiction mechanism applies. If separate CSs, they are distinct constraints linked only by network.affects_constraints. The current story assumes single-kernel framing per the manifest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the three readings share a commitment-system structure or are independent constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 1982, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_strict_geo_tr_t1982, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1982, 0.05).
narrative_ontology:measurement(unclos_strict_geo_tr_t1994, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1994, 0.08).
narrative_ontology:measurement(unclos_strict_geo_tr_t2002, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2002, 0.12).
narrative_ontology:measurement(unclos_strict_geo_tr_t2009, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2009, 0.18).
narrative_ontology:measurement(unclos_strict_geo_tr_t2013, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2013, 0.25).
narrative_ontology:measurement(unclos_strict_geo_tr_t2016, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2016, 0.31).
narrative_ontology:measurement(unclos_strict_geo_tr_t2020, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2020, 0.37).
narrative_ontology:measurement(unclos_strict_geo_tr_t2026, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(unclos_strict_geo_be_t1982, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1982, 0.15).
narrative_ontology:measurement(unclos_strict_geo_be_t1994, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1994, 0.18).
narrative_ontology:measurement(unclos_strict_geo_be_t2002, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2002, 0.25).
narrative_ontology:measurement(unclos_strict_geo_be_t2009, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2009, 0.32).
narrative_ontology:measurement(unclos_strict_geo_be_t2013, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2013, 0.48).
narrative_ontology:measurement(unclos_strict_geo_be_t2016, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2016, 0.58).
narrative_ontology:measurement(unclos_strict_geo_be_t2020, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(unclos_strict_geo_be_t2026, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2026, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(unclos_strict_geo_su_t1982, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1982, 0.2).
narrative_ontology:measurement(unclos_strict_geo_su_t1994, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1994, 0.25).
narrative_ontology:measurement(unclos_strict_geo_su_t2002, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2002, 0.35).
narrative_ontology:measurement(unclos_strict_geo_su_t2009, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2009, 0.45).
narrative_ontology:measurement(unclos_strict_geo_su_t2013, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2013, 0.55).
narrative_ontology:measurement(unclos_strict_geo_su_t2016, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2016, 0.62).
narrative_ontology:measurement(unclos_strict_geo_su_t2020, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(unclos_strict_geo_su_t2026, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2026, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__strict_geographic_reading, 0.12).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, south_china_sea_nine_dash_line_enforcement).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, arctic_continental_shelf_delimitation).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, mediterranean_eastern_med_maritime_claims).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the UNCLOS maritime sovereignty kernel. The expansive reading treats effective occupation of constructed features as sovereignty-generating; the hybrid reading grants natural features full EEZ but gives artificial features a maturation pathway through prolonged effective control. All three share Article 121 as kernel but differ in authority_grounding (strict = lineage/textual; expansive = practice/effective_control; hybrid = distributed) and in which axioms they hold as foundational.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__strict_geographic_reading, institutional, 0.15).
constraint_indexing:directionality_override(unclos_maritime_sovereignty__strict_geographic_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
