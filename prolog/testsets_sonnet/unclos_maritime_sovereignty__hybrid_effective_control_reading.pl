% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__hybrid_effective_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-18
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
 *   human_readable: Hybrid Effective-Control Reading of Feature-Type Sovereignty Under UNCLOS
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This story instantiates the hybrid effective-control reading of the
 *   UNCLOS maritime sovereignty kernel: natural features generate full
 *   EEZ/territorial sea entitlements while artificial features generate only
 *   limited (500m) safety zones — but the hybrid reading adds a maturation
 *   pathway, under which prolonged, unchallenged effective control over an
 *   artificial feature can accrete toward stronger territorial recognition.
 *   This is deliberately distinct from the strict geographic reading (which
 *   forecloses any maturation regardless of duration) and from the expansive
 *   construction reading (which grants de facto territorial waters from
 *   occupation and administration alone, without requiring the intermediate
 *   safety-zone step or the 'absent challenge' condition). Only this reading
 *   is generated here; the siblings are separate constraint files linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - construction_capable_regional_powers: agenda_setter/beneficiary (institutional/arbitrage) — builds and garrisons features, presses the maturation reading
 *   - militarily_weaker_claimant_states: payer (moderate/constrained) — holds competing claims but cannot match effective-control capacity
 *   - small_island_fishing_communities: payer (powerless/trapped) — loses access as safety zones expand and patrols intensify
 *   - coast_guard_and_naval_administrators: agenda_setter/beneficiary (institutional/arbitrage) — generates the enforcement record cited as evidence of control
 *   - international_maritime_tribunals: observer (institutional/analytical) — adjudicates but cannot enforce
 *   - non_claimant_maritime_users: excluded (organized/constrained) — depend on freedom of navigation but have no seat in the disputes
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
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "Hybrid Effective-Control Reading of Feature-Type Sovereignty Under UNCLOS").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, '01519219-18bc-46a1-be08-5d97a4a7c192').
narrative_ontology:cs_kernel_codification('01519219-18bc-46a1-be08-5d97a4a7c192', fixed_text).
narrative_ontology:cs_authority_grounding('01519219-18bc-46a1-be08-5d97a4a7c192', distributed).
narrative_ontology:cs_reading_relation('01519219-18bc-46a1-be08-5d97a4a7c192', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('01519219-18bc-46a1-be08-5d97a4a7c192', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_axiom('01519219-18bc-46a1-be08-5d97a4a7c192', foundational, prolonged_unchallenged_control_generates_rights).
narrative_ontology:cs_axiom_status(prolonged_unchallenged_control_generates_rights, holdable).
narrative_ontology:cs_axiom_grounding('01519219-18bc-46a1-be08-5d97a4a7c192', prolonged_unchallenged_control_generates_rights, conventional).
narrative_ontology:cs_axiom('01519219-18bc-46a1-be08-5d97a4a7c192', secondary, feature_type_determines_baseline_entitlement).
narrative_ontology:cs_axiom_status(feature_type_determines_baseline_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('01519219-18bc-46a1-be08-5d97a4a7c192', feature_type_determines_baseline_entitlement, conventional).
narrative_ontology:cs_reference_frame('01519219-18bc-46a1-be08-5d97a4a7c192', unclos_1982_baseline_text).
narrative_ontology:cs_drift_state('01519219-18bc-46a1-be08-5d97a4a7c192', post_2016_pca_ruling, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('01519219-18bc-46a1-be08-5d97a4a7c192', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_regional_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, coast_guard_and_naval_administrators).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, small_island_fishing_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possesses the dredging fleets, naval presence, and administrative apparatus to convert submerged reefs and low-tide elevations into occupied artificial features, garrison them, and maintain uncontested presence over years. Uses the hybrid rule's silence on 'how long is prolonged control' to press for territorial-claim maturation, arguing occupation itself generates rights the plain text does not grant to artificial features.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_regional_powers, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_regional_powers, beneficiary).

% Holds competing historical or geographic claims to the same reefs but lacks the naval and construction capacity to establish comparable effective control. Watches features it once fished or patrolled become garrisoned islands with 500m safety zones that, left unchallenged, edge toward de facto territorial recognition. Legal recourse through arbitration exists but enforcement of any favorable ruling depends on powers it cannot compel.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimant_states, payer,
    moderate, biographical, constrained, regional).

% Fishing grounds and transit routes around contested reefs are progressively enclosed by expanding safety zones and naval patrols. Have no standing in interstate arbitration and no capacity to contest occupation directly; their access is a casualty of the maturation clock running in favor of whichever state holds the feature longest.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, small_island_fishing_communities, payer,
    powerless, biographical, trapped, local).

% Administers the safety-zone enforcement, patrol schedules, and incident logs that constitute the 'effective control' record later cited as evidence of maturation. Directly benefits from expanded operational area and basing rights that follow from successful maturation claims.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, coast_guard_and_naval_administrators, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, coast_guard_and_naval_administrators, beneficiary).

% Adjudicates disputes under UNCLOS Article 121 and related provisions but has no enforcement mechanism of its own; rulings on feature classification (e.g. the 2016 South China Sea arbitration) can be issued and then ignored by the losing party, leaving the tribunal's authority dependent on voluntary compliance it cannot compel.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_maritime_tribunals, observer,
    institutional, civilizational, analytical, global).

% Commercial shippers and third-state navies rely on freedom of navigation through contested waters but are not parties to the underlying sovereignty disputes; their interest in a bright-line rule (favoring the strict geographic reading) is not represented in the bilateral or regional negotiations that determine how the hybrid standard gets applied in practice.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, non_claimant_maritime_users, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_regional_powers).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__hybrid_effective_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a graduated framework distinguishing natural islands (full maritime entitlements) from artificial features (limited safety zones), avoiding the extremes of either ignoring all human presence or granting full sovereignty to any concrete platform — intended to let states manage navigation safety around installations without triggering full territorial disputes over every rock and reef.
% TRANSFER_FUNCTION: Moves de facto control over contested maritime space, fishing access, and resource rights from states and communities without construction/naval capacity to states with the capacity to build, garrison, and patrol artificial features long enough to assert 'effective control' has matured into a stronger claim.
% ABSENT_VOICES: Small island fishing communities and non-claimant maritime users have no seat in the interstate arbitration or bilateral negotiation processes that determine how 'prolonged effective control' gets interpreted in any given dispute; their exclusion is structural, not incidental — the framework's dispute-resolution channels are state-to-state only.
% DISAPPEARANCE_RATIONALE: If the hybrid maturation doctrine were abandoned in favor of a strict geographic reading, several outposts built on submerged reefs or low-tide elevations would lose any claimed trajectory toward territorial status, safety-zone enforcement would revert to a narrow 500m radius with no forward legal momentum, and the calculus of construction-as-strategy would collapse — states would no longer have an incentive to dredge and garrison marginal features in hopes of maturing a claim.
% FOUNDING_PROBLEM: UNCLOS needed a rule for features that are neither clearly natural islands (full entitlements) nor open ocean (no entitlements) — reefs, rocks, and shoals that states were beginning to modify, occupy, or fortify, without a workable legal answer for what such modification does to maritime rights.
% FOUNDING_PROBLEM_CORROBORATION: Construction-capable states attest the hybrid rule still serves a live safety and administrative need. The 2016 Permanent Court of Arbitration ruling in the South China Sea case (Philippines v. China), issued by a body independent of any claimant state, found that none of the disputed features generated an EEZ and rejected the effective-control-maturation logic as a legal basis for expanding entitlements — an external corroboration that the 'maturation' extension is not a settled feature of the treaty text but a contested gloss serving the states capable of building it.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__hybrid_effective_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__hybrid_effective_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extraction is authored as intermediate (0.58) rather than high, because the hybrid reading genuinely constrains artificial features to a narrow 500m zone absent the maturation trigger — this is real coordination value distinguishing it from the expansive reading. But extraction rises steadily over the measurement interval as construction-capable states learn to use patrol logs, administrative records, and absence of formal challenge as the raw material for maturation claims — turning a safety provision into a slow-motion sovereignty mechanism. Suppression tracks this same arc: it is not merely the treaty's suppression of alternatives but the active suppression of weaker claimants' access and objection capacity as garrisons and patrols intensify. Theater ratio rises moderately (0.40 at 2024) because a meaningful share of 'effective control' activity — flag-raising ceremonies, administrative designations, symbolic patrols — is performed specifically to build the evidentiary record for future maturation claims rather than to serve any genuine safety function.
 *
 * PERSPECTIVAL GAP:
 *   From the construction-capable power's seat, the hybrid rule is a workable coordination mechanism balancing safety and sovereignty concerns — a rope. From the weaker claimant's seat and the fishing community's seat, the same rule is a graduated extraction mechanism where the treaty's own ambiguity ('prolonged,' 'absent challenge') is a resource that only asymmetric capacity can convert into rights. The engine's per-seat computation should reflect this divergence structurally: the agenda_setter/beneficiary seats see low effective extraction; the payer seats, especially the powerless/trapped fishing communities, see high effective extraction from the identical clause.
 *
 * DIRECTIONALITY LOGIC:
 *   Construction-capable regional powers and their administrative apparatus are declared beneficiaries because the maturation pathway rewards exactly the capacity they possess — the ambiguity in 'prolonged' and 'absent challenge' is not neutral; it is a resource convertible only by states with construction and naval reach. Militarily weaker claimant states and small island fishing communities are declared victims because the same clause that offers them formal parity (natural features get full entitlements regardless of power) offers no comparable pathway for their weaker capacity — the maturation clock runs in one direction. The fishing communities carry the highest effective extraction due to trapped exit and powerless status; the weaker states carry constrained but not fully trapped exit via international arbitration, though enforcement of favorable rulings remains dependent on the powers being contested.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — providing a workable legal answer for modified/occupied marginal features — has not disappeared, but the corroboration record shows a split: the PCA's 2016 ruling, issued from outside the benefiting states, found the maturation logic legally unsupported, while the states that benefit from the hybrid reading continue to assert its validity in practice. This mismatch (contested founding_problem_status, with world_rearranges disappearance_verdict) is exactly the pattern the classification exists to surface: a rule with genuine original coordination function that has been extended, through interpretive practice by its most capable users, well past what the underlying text or its adjudicating tribunal actually supports.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maturation_threshold_ambiguity,
    'How much time and what quality of ''absent challenge'' is sufficient for an artificial feature''s effective control to mature into a stronger territorial claim, and does the treaty text actually support any such maturation at all?',
    'A binding, enforced tribunal ruling establishing a specific durational or evidentiary threshold, or a treaty amendment explicitly codifying or rejecting the maturation doctrine.',
    'If no genuine threshold exists in the text (as the PCA''s 2016 ruling suggests), the hybrid reading''s maturation component is not law but an aspirational gloss advanced by capable states — reclassifying this reading closer to the strict geographic reading''s ε profile. If a real threshold is eventually codified, the hybrid reading gains genuine coordination function and extraction may fall as the ambiguity resource disappears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maturation_threshold_ambiguity, conceptual, 'Whether ''prolonged effective control absent challenge'' names an actual legal threshold or an interpretive extension without textual support.').

omega_variable(
    enforcement_asymmetry_and_tribunal_authority,
    'Given that international maritime tribunals can rule on feature classification but cannot compel compliance, does the practical operation of the hybrid reading depend more on adjudicated law or on unilateral capacity to maintain presence?',
    'Track compliance rates with tribunal rulings on feature classification versus continued occupation/patrol activity by the losing party over subsequent years.',
    'If compliance is low, the hybrid reading''s real-world operation is closer to raw effective-control dynamics (extraction driven by capacity, not law) regardless of what the reading claims textually — supporting a higher effective ε than the doctrinal text alone would suggest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_asymmetry_and_tribunal_authority, empirical, 'Whether the hybrid reading functions as adjudicated law or as a legitimating gloss on unilateral power.').

omega_variable(
    kernel_framing_under_determination,
    'Is the ''natural vs. artificial feature'' distinction itself a stable, apolitical geological fact, or is feature classification (natural vs. substantially modified) itself contestable and subject to the same capacity asymmetry as the maturation question?',
    'Independent geological/hydrographic survey of contested features'' pre-modification state, compared against claimant states'' characterizations.',
    'If feature classification itself is contested (e.g., disputes over whether a feature was a natural rock above high tide before modification), the entire hybrid framework''s foundational premise — that natural and artificial features are cleanly distinguishable — is undermined, and the extraction may occur one level upstream of where this story locates it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether the natural/artificial feature distinction underlying all three kernel readings is itself stable or subject to the same asymmetric-capacity dynamics as the maturation doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1994, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 1994, 0.2).
narrative_ontology:measurement(uncl_tr_t2000, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(uncl_tr_t2008, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement(uncl_tr_t2014, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2014, 0.36).
narrative_ontology:measurement(uncl_tr_t2018, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2018, 0.39).
narrative_ontology:measurement(uncl_tr_t2024, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1994, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 1994, 0.28).
narrative_ontology:measurement(uncl_be_t2000, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement(uncl_be_t2008, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2008, 0.41).
narrative_ontology:measurement(uncl_be_t2014, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2014, 0.52).
narrative_ontology:measurement(uncl_be_t2018, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2018, 0.56).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1994, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 1994, 0.3).
narrative_ontology:measurement(uncl_su_t2000, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(uncl_su_t2008, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2008, 0.45).
narrative_ontology:measurement(uncl_su_t2014, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2014, 0.55).
narrative_ontology:measurement(uncl_su_t2018, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2018, 0.6).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.1).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__expansive_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the unclos_maritime_sovereignty kernel, decomposed per the ε-invariance principle rather than represented as one constraint with a measurement parameter. strict_geographic_reading (lower ε, closer to Mountain/Rope — bright-line rule, minimal ambiguity to extract from) forecloses this reading's maturation logic. expansive_construction_reading (higher ε, closer to Snare — occupation alone suffices, no safety-zone intermediate step) coexists with this reading as an alternative live position among claimant states. This hybrid reading sits structurally between the two: intermediate ε, genuine coordination function preserved for the safety-zone component, but extraction introduced via the maturation pathway's textual ambiguity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
