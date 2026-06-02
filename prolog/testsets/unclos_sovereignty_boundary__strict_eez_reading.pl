% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__strict_eez_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__strict_eez_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__strict_eez_reading
 *   human_readable: UNCLOS Exclusive Economic Zone Sovereignty (Strict Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   The UNCLOS Exclusive Economic Zone (EEZ) boundary represents one reading
 *   of a contested kernel — the proper scope of coastal state sovereignty
 *   over ocean resources. The strict reading interprets UNCLOS Article 57's
 *   200-nautical-mile limit as absolute and non-overlapping, granting coastal
 *   states exclusive control over living and non-living resources within the
 *   boundary and suppressing alternative sovereignty claims (historical
 *   fishing rights, prior customary access). This reading creates clear
 *   winners (coastal states with enforcement capacity) and clear losers
 *   (landlocked states, distant-water fishing fleets, small island states
 *   dependent on historical fishing grounds). The constraint functions
 *   simultaneously as coordination (defining who manages what, enabling
 *   sustainable fisheries governance), extraction (conferring monopoly access
 *   on coastal states), and suppression (excluding prior users through legal
 *   re-codification). The theater ratio reflects the increasing performative
 *   content of enforcement: coastal states conduct symbolic patrols and
 *   publish catch statistics while IUU fishing persists at scale in
 *   under-enforced EEZs. The suppression_requirement has intensified over 20
 *   years as enforcement infrastructure (satellites, coast guards, port-state
 *   controls) has matured, indicating that the constraint's exclusivity
 *   depends increasingly on active suppression of incursion attempts.
 *
 * KEY AGENTS:
 *   - Coastal States (Beneficiaries): Institutional/arbitrage — gain exclusive resource control, economic rent from fishing licenses, offshore energy monopoly; can negotiate terms selectively
 *   - Distant-Water Fishing Nations (Victims): Organized/constrained — lose access to historically productive grounds; can negotiate limited fishing agreements but at terms set by coastal states
 *   - Landlocked and Small Island States (Victims): Moderate/trapped — geographic closure forces dependence on coastal state goodwill; no independent fishing access
 *   - Historical Fishing Communities (Victims): Powerless/trapped — customary access revoked by legal redefinition; cannot exit or negotiate collectively
 *   - Regional Fisheries Management Organizations (Institutional/constrained): Coordinate among coastal states and fishing nations within strict EEZ framework; experience extraction of governance authority from states to international bodies
 *   - Non-Ratifier States (Institutional/arbitrage): US, China — maintain selective EEZ enforcement and strategic ambiguity; preserve freedom to operate outside UNCLOS when convenient
 *   - Analytical Observer (Analytical/analytical): Risks naturalizing contingent legal codification as geographic immutability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.58).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.72).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "UNCLOS Exclusive Economic Zone Sovereignty (Strict Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, 'eb6fa51c-77fd-465b-accf-6e8fd570e204').
narrative_ontology:cs_kernel_codification('eb6fa51c-77fd-465b-accf-6e8fd570e204', formalized).
narrative_ontology:cs_authority_grounding('eb6fa51c-77fd-465b-accf-6e8fd570e204', lineage).
narrative_ontology:cs_interpretation_layer_present('eb6fa51c-77fd-465b-accf-6e8fd570e204').
narrative_ontology:cs_reading_relation('eb6fa51c-77fd-465b-accf-6e8fd570e204', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb6fa51c-77fd-465b-accf-6e8fd570e204', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, influences).
narrative_ontology:cs_axiom('eb6fa51c-77fd-465b-accf-6e8fd570e204', foundational, exclusive_eez_boundary_exhausts_coastal_jurisdiction).
narrative_ontology:cs_axiom_status(exclusive_eez_boundary_exhausts_coastal_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('eb6fa51c-77fd-465b-accf-6e8fd570e204', exclusive_eez_boundary_exhausts_coastal_jurisdiction, conventional).
narrative_ontology:cs_axiom('eb6fa51c-77fd-465b-accf-6e8fd570e204', secondary, coastal_state_enforcement_legitimacy_derives_from_ratification).
narrative_ontology:cs_axiom_status(coastal_state_enforcement_legitimacy_derives_from_ratification, holdable).
narrative_ontology:cs_axiom_grounding('eb6fa51c-77fd-465b-accf-6e8fd570e204', coastal_state_enforcement_legitimacy_derives_from_ratification, conventional).
narrative_ontology:cs_reference_frame('eb6fa51c-77fd-465b-accf-6e8fd570e204', exclusive_eez_sovereignty).
narrative_ontology:cs_drift_state('eb6fa51c-77fd-465b-accf-6e8fd570e204', contemporary_enforcement_reality, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eb6fa51c-77fd-465b-accf-6e8fd570e204', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, coastal_states_with_eez_capacity).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, developed_maritime_economies).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, landlocked_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, small_island_developing_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_nations).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, historical_users_excluded_by_eez).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED FISHING COMMUNITY (SNARE) — Historically licensed or customary fishing grounds are now within EEZ boundaries of states with enforcement capacity but without reciprocal fishing rights. Trapped: no exit option exists; cannot fish the grounds; cannot access alternative resources at comparable cost. Experiences maximum extraction with no coordination benefit.
constraint_indexing:constraint_classification(unclos_sovereignty_boundary__strict_eez_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LANDLOCKED STATE (TANGLED ROPE) — Constrained by geographic closure and dependence on fishing agreements with coastal neighbors. Experiences high extraction (access is controlled by coastal states), but also benefits from maritime trade corridors and transit rights negotiated within UNCLOS framework. Has some agency through bilateral negotiation but faces structural disadvantage.
constraint_indexing:constraint_classification(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COASTAL STATE — BENEFICIARY (ROPE) — Gains exclusive resource control within 200nm and can exclude competitors through enforcement. Sees the constraint as functional coordination: the boundary clarifies who has management authority, enables sustainable resource planning, and provides legitimacy for enforcement. Net beneficiary with arbitrage options (can negotiate fishing licenses, control terms, exit enforcement selectively).
constraint_indexing:constraint_classification(unclos_sovereignty_boundary__strict_eez_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL FISHING COALITION (TANGLED ROPE) — Organized actors (EU, ASEAN fishing nations) negotiate joint enforcement and access agreements within the strict EEZ framework. Experience significant extraction (high compliance costs, limited migration of fishing effort) but also benefit from coordination infrastructure (regional fisheries management organizations) that stabilizes resources and enables some redistribution. Agency is collective rather than individual.
constraint_indexing:constraint_classification(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NON-RATIFIER STATE (PITON) — China, US (non-ratifiers of UNCLOS) maintain de facto EEZ enforcement while preserving strategic ambiguity about legal obligation. Theater ratio reflects selective enforcement: invoke UNCLOS when beneficial (claiming own EEZ against competitors), deny when inconvenient (contestable boundaries, military operations). Constraint persists through institutional inertia and theater rather than genuine commitment. Low perceived cost because enforcement is episodic.
constraint_indexing:constraint_classification(unclos_sovereignty_boundary__strict_eez_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, UNCLOS Article 57 codifies natural geography (continental shelf extension) and creates permanent jurisdictional boundaries analogous to physical laws. This perspective risks naturalizing what is actually a contingent institutional arrangement backed by state enforcement and differentially benefiting powerful maritime states. The engine's false-summit detector will identify this as misplaced naturalization.
constraint_indexing:constraint_classification(unclos_sovereignty_boundary__strict_eez_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unclos_sovereignty_boundary__strict_eez_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unclos_sovereignty_boundary__strict_eez_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, TR),
    TR >= 0.70.

:- end_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The strict EEZ reading grants coastal states monopoly control over a scarce resource (marine productivity), enabling significant rent extraction from excluded actors. The value reflects genuine asymmetric benefit (coastal states gain exclusive access; others lose customary access) but is not maximal (1) because the constraint includes a coordination function — defining boundaries enables sustainable management and reduces conflict over undefined access rights. The trajectory shows increasing extractiveness over the interval as enforcement capacity matures and coastal states capture more rents from licensing and energy development. Suppression (0.72): High. The constraint requires active suppression of alternative sovereignty claims — historical rights must be declared invalid, customary access terminated by legal fiat, competing boundary claims delegitimized. Non-ratifier states enforce EEZ exclusivity selectively, suggesting that suppression is not universal but concentrated on actors with lower political power. Theater ratio (0.38): Moderate-low. The strict EEZ reading has genuine functional content — boundaries actually reduce commons tragedy and enable resource management — but increasing performance content as enforcement becomes theater: symbolic patrols, published statistics, port-state controls that target small operators while large IUU fleets evade detection. The rising trajectory reflects degradation of the constraint's functional content as enforcement cannot scale to actual incursion rates.
 *
 * PERSPECTIVAL GAP:
 *   The strict EEZ reading produces sharply divergent classifications across positions. Coastal states perceive Rope (coordination benefit, resource planning, legitimate monopoly). Excluded actors perceive Snare (trapped, no alternatives, pure extraction). Organized actors (RFMOs) perceive Tangled Rope (mixed coordination and extraction). Non-ratifiers perceive Piton (performative enforcement, selective application). The analytical observer risks Mountain (natural boundary) which the false-summit detector will flag as naturalization. The perspectival gap reveals that the strict reading's legitimacy depends on the observer's structural position: beneficiaries see functional coordination; victims see coercive exclusion.
 *
 * DIRECTIONALITY LOGIC:
 *   The strict reading's directionality derives from the asymmetric benefit distribution. Coastal states are beneficiaries with arbitrage exit (can choose enforcement levels, negotiate selectively, defect to non-ratifier behavior if convenient), yielding low d (approximately 0.12-0.15) and negative f(d) → negative χ (they experience the boundary as beneficial coordination). Distant-water fishing nations are victims with constrained exit (can negotiate fishing agreements but on coastal state terms, cannot access alternative grounds at comparable cost), yielding high d (approximately 0.68-0.75) and positive f(d) → positive χ (they experience extraction). Landlocked states are victims with trapped exit (geography prevents independent fishing access), yielding maximum d (approximately 0.92) and maximum f(d) → maximum χ (they experience complete exclusion). The analytical observer occupies an intermediate position (d ≈ 0.72) reflecting the universal scope and institutional authority backing the reading. The engine derives d from these structural facts and applies the sigmoid to compute experienced extractiveness per position.
 *
 * MANDATROPHY ANALYSIS:
 *   The strict EEZ reading resolves mandatrophy by showing that the constraint is neither pure coordination (Rope) nor pure extraction (Snare) but a genuine hybrid: it coordinates resource management and reduces commons tragedy (functional content), while simultaneously concentrating rents on coastal states and excludes historical users (extractive content). The classification as Tangled Rope is structurally justified by the presence of (1) beneficiaries (coastal states), (2) victims (excluded actors), and (3) active enforcement machinery (coast guards, port controls, bilateral agreements). The Rope-only hypothesis (that this is merely efficient boundary-setting) fails to account for the extraction asymmetries and suppression of alternatives. The Snare-only hypothesis (that this is coercive rent-seeking) fails to account for the genuine coordination function and the benefit to sustainability. The tangled_rope classification is not a compromise but a structural fact: this constraint solves a coordination problem (defining maritime authority) via a mechanism that extracts rents from powerless actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    eez_legitimacy_basis_ambiguity,
    'Is the strict EEZ boundary legitimate because UNCLOS represents consensual international governance (foundational legitimacy), or because coastal states have sufficient enforcement capacity to impose it (de facto legitimacy)?',
    'Historical analysis of UNCLOS negotiation and ratification: Which states consented and which did not? For non-ratifiers (US, China), does de facto EEZ enforcement derive from international law or from unilateral power assertion? Track enforcement patterns: do coastal states enforce the boundary consistently or selectively?',
    'If consensual: Rope classification holds — legitimate coordination with differential benefits. If de facto: Snare classification prevails — enforcement backed by state power targeting weaker agents. Legitimacy ambiguity blocks definitive schema classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(eez_legitimacy_basis_ambiguity, conceptual, 'Whether EEZ legitimacy derives from consensual governance or enforcement capacity').

omega_variable(
    historical_rights_foreclosure,
    'Does the strict 200nm EEZ reading foreclose the historical-rights reading (pre-UNCLOS customary access preserved by historical continuity), or do both remain live positions in different jurisdictions?',
    'Case-law analysis: Can a coastal state legitimately deny historical fishing rights within its EEZ? Has any treaty or adjudication explicitly foreclosed historical-rights claims, or do they coexist through state discretion (bilateral agreements, regional exemptions)?',
    'If foreclosed: strict reading has logical priority — historical claims are invalid by definition. If coexisting: the readings are live alternatives, and choice depends on regional political economy. Codification of foreclosure is missing despite decades of UNCLOS practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_rights_foreclosure, conceptual, 'Whether strict EEZ reading forecloses historical-rights reading').

omega_variable(
    enforcement_capacity_threshold,
    'What enforcement capacity does a coastal state require to legitimately claim exclusive EEZ control? Does UNCLOS permit differential enforcement (rich states patrol aggressively, poor states minimally), or does legitimacy require near-universal monitoring?',
    'Empirical survey: IUU (Illegal, Unreported, Unregulated) fishing prevalence in EEZs with low enforcement capacity vs high-capacity states. If low-capacity states cannot prevent incursions, does their EEZ claim persist as valid law or degrade to performative geography?',
    'If legitimacy requires enforcement: many small-state EEZ claims are invalidated by capacity limits — cascades to snare or piton. If legitimacy is de jure: enforcement gaps are violations, not delegitimization — maintains tangled_rope. Current practice is inconsistent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_threshold, empirical, 'Enforcement capacity threshold for EEZ legitimacy').

omega_variable(
    kernel_reading_identity,
    'This constraint is ONE READING of the contested UNCLOS sovereignty kernel. The strict EEZ reading interprets Article 57''s 200nm boundary as absolute and non-negotiable within the UNCLOS framework. Sibling readings (historical_rights_reading, non_ratifier_enforcement_reading) interpret the same kernel differently. What distinguishes this reading''s foundational premises from the siblings''?',
    'The strict reading''s core axiom is: ''exclusive_eez_boundary_exhausts_coastal_jurisdiction'' — the 200nm line is complete and overrides prior customary access claims within UNCLOS signatories. Historical-rights reading''s axiom is: ''historical_use_preserves_access_rights'' — pre-UNCLOS customary fishing persists unless explicitly extinguished. Non-ratifier reading''s axiom is: ''eez_enforcement_contingent_on_ratification'' — non-signatories have no UNCLOS obligation and operate under de facto power. These axioms coexist across different state actors (some honor historical rights bilaterally, some enforce strict boundaries, some disregard UNCLOS entirely). The readings are not logically exclusive but situationally dependent — different parties hold different readings.',
    'The strict reading''s benefit distribution (coastal state beneficiaries, excluded actors as victims) assumes Article 57 is binding and overrides historical claims. If historical-rights reading dominates regionally, extraction shifts from coastal states to historical users. If non-ratifier reading dominates, enforcement becomes discretionary and theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel reading identity and sibling reading relationship').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_strict_tr_t0, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(unclos_strict_tr_t10, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(unclos_strict_tr_t20, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(unclos_strict_be_t0, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(unclos_strict_be_t10, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(unclos_strict_be_t20, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(unclos_strict_su_t0, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(unclos_strict_su_t10, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(unclos_strict_su_t20, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, iuu_fishing_suppression).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, maritime_piracy_and_enforcement).

% DUAL FORMULATION NOTE:
% The strict EEZ reading is one reading of the UNCLOS_SOVEREIGNTY_BOUNDARY kernel. The historical-rights reading and non-ratifier-enforcement reading are structurally distinct constraints with different beneficiary/victim sets and different ε values. The strict reading represents a reading that privileges exclusive coastal state control (high ε for coastal states, high victims for excluded actors). Historical-rights reading permits partial recovery of access (lower ε for coastal states, lower victims for historical users). Non-ratifier reading introduces enforcement ambiguity (higher theater ratio, conditional suppression). These are linked by kernel identity, not by causal dependency — they are competing interpretations of the same foundational commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_sovereignty_boundary__strict_eez_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
