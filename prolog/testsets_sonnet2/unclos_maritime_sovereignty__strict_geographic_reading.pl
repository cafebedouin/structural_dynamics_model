% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__strict_geographic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: unclos_maritime_sovereignty__strict_geographic_reading
 *   human_readable: UNCLOS Article 121 Strict Natural-Formation Reading of Island Status
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This story instantiates the strict geographic reading of the UNCLOS
 *   Article 121 kernel: only features that are naturally formed and above
 *   water at high tide generate a territorial sea and EEZ; dredging,
 *   construction, and installation-building on reefs or submerged shoals do
 *   not upgrade their legal status regardless of how permanent or elaborate
 *   the construction becomes. This reading was substantially vindicated by
 *   the 2016 Permanent Court of Arbitration ruling in Philippines v. China,
 *   which held that none of the heavily developed Spratly features qualified
 *   as islands under Article 121(3). The reading functions as coordination —
 *   a single, geographically verifiable bright line that lets the great
 *   majority of coastal and maritime states predict each other's maritime
 *   zones without renegotiating every contested feature — but it also has an
 *   asymmetric distributive effect: it structurally favors naval powers and
 *   non-claimant maritime states (who retain open water and freedom of
 *   navigation) at the expense of states that have invested in
 *   artificial-feature construction specifically to expand jurisdiction. The
 *   extractive tension is genuine but comparatively modest (ε≈0.38) because
 *   the rule denies a *claimed* expansion rather than seizing an
 *   already-recognized entitlement — the constraining feature is what it
 *   withholds, not what it takes.
 *
 * KEY AGENTS:
 *   - naval_powers: institutional beneficiary — freedom of navigation preserved around reclaimed features
 *   - expansionist_coastal_states: institutional payer — sunk construction investment yields no territorial sea
 *   - permanent_court_of_arbitration_and_similar_tribunals: institutional agenda_setter — applies and legitimizes the strict test
 *   - littoral_populations_dependent_on_disputed_eez_claims: powerless payer — promised resource access does not materialize
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.38).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.42).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "UNCLOS Article 121 Strict Natural-Formation Reading of Island Status").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, '5e73e17f-344c-430b-98e3-8889d077e831').
narrative_ontology:cs_kernel_codification('5e73e17f-344c-430b-98e3-8889d077e831', fixed_text).
narrative_ontology:cs_authority_grounding('5e73e17f-344c-430b-98e3-8889d077e831', distributed).
narrative_ontology:cs_reading_relation('5e73e17f-344c-430b-98e3-8889d077e831', unclos_maritime_sovereignty__expansive_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('5e73e17f-344c-430b-98e3-8889d077e831', unclos_maritime_sovereignty__hybrid_effective_control_reading, influences).
narrative_ontology:cs_axiom('5e73e17f-344c-430b-98e3-8889d077e831', foundational, natural_formation_is_the_exclusive_sovereignty_predicate).
narrative_ontology:cs_axiom_status(natural_formation_is_the_exclusive_sovereignty_predicate, holdable).
narrative_ontology:cs_axiom_grounding('5e73e17f-344c-430b-98e3-8889d077e831', natural_formation_is_the_exclusive_sovereignty_predicate, conventional).
narrative_ontology:cs_axiom('5e73e17f-344c-430b-98e3-8889d077e831', secondary, construction_cannot_cure_geographic_deficiency).
narrative_ontology:cs_axiom_status(construction_cannot_cure_geographic_deficiency, holdable).
narrative_ontology:cs_axiom_grounding('5e73e17f-344c-430b-98e3-8889d077e831', construction_cannot_cure_geographic_deficiency, instrumental).
narrative_ontology:cs_reference_frame('5e73e17f-344c-430b-98e3-8889d077e831', id_1982_convention_textual_baseline).
narrative_ontology:cs_drift_state('5e73e17f-344c-430b-98e3-8889d077e831', post_2016_arbitration_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('5e73e17f-344c-430b-98e3-8889d077e831', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_flag_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, regional_fishing_fleets_of_non_claimants).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, littoral_populations_dependent_on_disputed_eez_claims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate blue-water fleets that depend on freedom of navigation through contested waters. A strict reading that denies artificial features territorial sea keeps sea lanes and airspace open around reclaimed reefs, letting these states conduct transits and overflight without needing coastal-state consent. They fund legal advocacy, freedom-of-navigation operations, and arbitral interventions that keep the strict reading institutionally live.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers, beneficiary,
    institutional, generational, arbitrage, global).

% Register commercial shipping that transits contested seas. They benefit passively from a narrow definition of what generates territorial sea, since it minimizes the area in which they must seek permission or pay transit-related fees. They have no direct stake in the underlying territorial dispute and simply prefer maximal open water.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_flag_states, beneficiary,
    moderate, generational, mobile, global).

% Fish waters that would fall inside an EEZ if artificial or submerged features were recognized as island-generating. The strict reading keeps these waters classified as high seas or contested rather than exclusive, preserving their access, though they remain exposed to coast-guard harassment from claimant states that reject the reading in practice.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, regional_fishing_fleets_of_non_claimants, beneficiary,
    organized, biographical, constrained, regional).

% Have invested heavily in land reclamation, dredging, and permanent installations atop reefs and low-tide elevations, asserting these generate territorial sea and EEZ. Under the strict reading their built features are legally installations, not islands — the investment produces no zone of sovereignty. They cannot relocate the geography; their only paths are continued construction, diplomatic pressure to reframe the law, or non-compliance with adverse rulings.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states, payer,
    institutional, civilizational, trapped, regional).

% Coastal fishing communities and local economies whose governments have promised expanded fishing and resource rights premised on the artificial features being recognized as islands. When the strict reading is applied by tribunals or third states, the promised EEZ expansion does not materialize, and access to disputed fishing grounds and seabed resources they were told would be theirs remains contested or lost to competing claimants and open-access fleets.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, littoral_populations_dependent_on_disputed_eez_claims, payer,
    powerless, biographical, trapped, regional).

% Adjudicates Article 121 disputes (as in the 2016 South China Sea arbitration) and applies the strict natural-formation test to disputed features, issuing rulings that determine whether a feature generates territorial sea, a 12nm zone only, or nothing. Its rulings lack independent enforcement power but shape which reading state practice and third parties treat as authoritative.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, permanent_court_of_arbitration_and_similar_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% States not party to the specific dispute who must decide, for their own shipping, fishing licensing, and diplomatic recognition, which reading of Article 121 they will follow. Their aggregate practice either reinforces or erodes the strict reading's authority over time.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, third_states_and_flag_state_registries, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__strict_geographic_reading, diffuse).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__strict_geographic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, bright-line, geographically verifiable test — natural formation, above water at high tide — for what generates a territorial sea and EEZ, so that hundreds of coastal and maritime states can predict each other's maritime zones without case-by-case negotiation over every rock, reef, and installation.
% TRANSFER_FUNCTION: Moves the practical value of contested maritime zones (fishing grounds, seabed hydrocarbon and mineral rights, strategic chokepoint control) away from states that have built or would build artificial features to claim those zones, and toward naval powers and non-claimant fishing/shipping interests who retain open-water access and freedom of navigation over the same areas.
% ABSENT_VOICES: The expansionist coastal states' domestic constituencies who were promised resource rights are not parties to the arbitral proceedings that settle the legal question against them; local fishing communities in the disputed zones have no standing before the tribunals whose rulings determine their economic future.
% DISAPPEARANCE_RATIONALE: If the strict natural-formation test vanished and no legal principle replaced it, several states currently deterred by the rule (or by rulings applying it) would have a freer hand to convert reclaimed reefs into EEZ-generating islands, converting large areas of currently contested or high-seas water into claimed exclusive zones — reordering fishing access, resource rights, and naval transit corridors across multiple regional seas.
% FOUNDING_PROBLEM: UNCLOS Article 121 was drafted to prevent states from manufacturing sovereignty and expanding maritime jurisdiction through engineering rather than geography — closing a loophole where a state could build a platform on a submerged shoal and claim the same 200nm EEZ as a genuine, naturally formed island.
% FOUNDING_PROBLEM_CORROBORATION: The 2016 Permanent Court of Arbitration ruling in the South China Sea arbitration (Philippines v. China) independently found that none of the disputed Spratly features were legally islands under Article 121(3) regardless of construction, corroborating that the founding problem — jurisdiction manufactured by engineering — remains an active concern rather than a settled or obsolete one. International legal scholarship outside any claimant state's government has treated the ruling's reasoning as the operative interpretation, though it lacks an enforcement mechanism against non-compliant states.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__strict_geographic_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__strict_geographic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises modestly over the interval (0.22 to 0.38) as arbitral practice (culminating in the 2016 ruling) hardened the strict reading from a textual default into an actively litigated and enforced principle — enforcement activity (freedom-of-navigation operations, arbitral proceedings, diplomatic non-recognition of claimed zones) is the enforcement mechanism, not physical coercion. Suppression tracks a similar rising curve (0.20 to 0.42) reflecting increased naval patrol activity and diplomatic pressure applied against states asserting territorial sea from artificial features. Theater ratio stays low and roughly flat (0.15 to 0.22) because the underlying function — a bright-line geographic test — remains substantively operative throughout; there is little performative drift here, unlike a piton. Resistance is comparatively high (0.68) because expansionist claimant states have not accepted the ruling's authority and continue construction and administrative activity in open defiance, meaning the constraint's persistence genuinely depends on continued naval and diplomatic enforcement rather than voluntary compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the naval-power seat this constraint is close to a pure coordination rule: a predictable, universally applicable test that keeps sea lanes open for everyone, including states with no stake in any specific dispute. From the expansionist-coastal-state seat, the identical rule looks like a targeted denial of a specific, costly, deliberate national investment — an externally imposed legal ceiling on what construction can achieve. The engine should compute these as structurally different experiences of the same rule: the beneficiary seats see low effective extraction (or subsidy), the payer seats see high effective extraction, driven by the same base ε but different directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers and non-claimant flag states are beneficiaries because the strict reading maximizes open water and minimizes zones in which they must seek coastal-state permission — their exit options are mobile-to-arbitrage, so derived directionality sits near the beneficiary end without needing an override. Expansionist coastal states are targets: their exit options are effectively trapped, since geography cannot be relocated and the sunk cost of construction cannot be un-built or redirected toward an alternative jurisdictional claim — the strict reading directly denies the return on their specific structural investment. Littoral populations dependent on the promised EEZ expansion are also targets, and more severely constrained than their own government (powerless power atom, trapped exit) — they bear the downstream cost of a legal reading their state did not choose and cannot appeal on their own behalf.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing states from manufacturing jurisdiction through engineering — remains live by the corroboration of independent tribunal reasoning, so this is not a case of mandatrophy (a rule persisting after its function died). The risk to guard against is the opposite error: treating the modest, real coordination function (predictable maritime-zone boundaries) as though it fully explains the rule's distributive effects, when the same rule structurally advantages incumbent naval and maritime powers over states attempting to convert geographic disadvantage into jurisdiction through investment. Classifying this as rope rather than tangled_rope reflects a judgment that the coordination function is not cover for extraction — the rule would exist and operate the same way even absent any beneficiary group, because it follows from the ordinary reading of the treaty text — but the omega below flags that this judgment is contestable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_crystallization_uncertainty,
    'Has the strict geographic reading, as applied in the 2016 arbitral ruling, crystallized into binding customary international law applicable even to non-parties and non-participants in that arbitration, or does it remain a persuasive-but-non-binding interpretation that expansionist states can continue to reject without formal legal consequence?',
    'Track state practice and opinio juris over subsequent decades: whether third states cite the ruling as binding precedent in their own maritime boundary negotiations, and whether any subsequent arbitral or ICJ ruling either reaffirms or narrows the 2016 reasoning.',
    'If the reading has crystallized as binding custom, its classification moves toward rope with a stronger coordination claim (near-universal predictability). If it remains contested soft law, the constraint is better read as an ongoing power contest dressed in legal language, pushing toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_crystallization_uncertainty, empirical, 'Whether the strict reading has hardened into binding customary law or remains a contested interpretation.').

omega_variable(
    coordination_versus_incumbent_advantage,
    'Is the strict natural-formation test a genuinely neutral coordination rule, or does it structurally and non-incidentally favor states that already hold naturally formed islands and states with blue-water naval reach, making the ''neutral bright line'' framing itself a form of naturalized advantage for incumbents?',
    'Comparative analysis of which states possess naturally formed islands generating large EEZs versus which states would gain EEZ area under an expansive or hybrid reading — if the distributive pattern strongly favors states that were already dominant at the treaty''s drafting, the neutrality claim weakens.',
    'If the rule systematically entrenches pre-existing maritime power distribution rather than applying a truly neutral geographic test, the classification should shift from rope toward tangled_rope, with naval powers as a more clearly extractive beneficiary class rather than incidental beneficiaries of a neutral rule.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_versus_incumbent_advantage, conceptual, 'Whether the bright-line test is neutral coordination or naturalized incumbent advantage.').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Which reading of Article 121 should be treated as the operative one for states not party to any specific dispute — the strict reading, given the 2016 ruling''s persuasive authority, or one of the sibling readings, given that no universal enforcement mechanism compels adoption of any single interpretation?',
    'This is inherent to the kernel structure itself: UNCLOS provides no supreme adjudicator with compulsory jurisdiction over all maritime disputes, so the three sibling readings persist as live alternatives held by different state coalitions rather than converging to one settled law.',
    'This ambiguity is the reason the kernel is authored as three separate constraint stories rather than one — each reading has its own ε, beneficiary/victim structure, and classification, linked by network.affects_constraints and cs_structure.reading_relations rather than merged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Documents why this kernel required decomposition into three sibling constraint stories rather than one averaged story.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1994, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1994, 0.15).
narrative_ontology:measurement(uncl_tr_t2000, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2000, 0.16).
narrative_ontology:measurement(uncl_tr_t2010, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(uncl_tr_t2016, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement(uncl_tr_t2020, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2020, 0.21).
narrative_ontology:measurement(uncl_tr_t2024, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1994, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1994, 0.22).
narrative_ontology:measurement(uncl_be_t2000, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(uncl_be_t2010, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2010, 0.31).
narrative_ontology:measurement(uncl_be_t2016, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2016, 0.36).
narrative_ontology:measurement(uncl_be_t2020, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2020, 0.37).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1994, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1994, 0.2).
narrative_ontology:measurement(uncl_su_t2000, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2000, 0.23).
narrative_ontology:measurement(uncl_su_t2010, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(uncl_su_t2016, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2016, 0.38).
narrative_ontology:measurement(uncl_su_t2020, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__strict_geographic_reading, 0.12).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, hybrid_effective_control_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'UNCLOS Article 121 island status' into structurally distinct readings of the same kernel. strict_geographic_reading (this story) authors low-moderate ε (0.38) reflecting a rule that mostly withholds an unearned expansion rather than seizing recognized entitlement, with naval powers and non-claimant states as beneficiaries and expansionist coastal states/their littoral populations as victims. expansive_construction_reading and hybrid_effective_control_reading author their own independent ε values reflecting the opposite distributive structure (coastal-state construction investment vindicated versus partially vindicated). All three share the same treaty text and underlying kernel but diverge on whether artificial construction and effective control can substitute for natural geographic formation — per the ε-invariance principle, this divergence is decomposed into three files rather than forced into one averaged classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
