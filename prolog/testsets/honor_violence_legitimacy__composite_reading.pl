% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__composite_reading
 *   human_readable: Honor-Violence Legitimacy: Composite Reading (Overdetermined Decline via Drop + Contraction)
 *   domain: legal_anthropology/commitment_systems/cultural_evolution
 *
 * SUMMARY:
 *   The decline of honor-based dueling violence in Western legal regimes
 *   occurred through the simultaneous operation of two structurally distinct
 *   mechanisms: (1) Drop mechanism — external costs (legal penalties, social
 *   stigma concentration, insurance/liability shifts) made dueling
 *   practically rare without delegitimizing it conceptually; (2) Contraction
 *   mechanism — intellectual and legal redefinition of honor itself to
 *   exclude violence-satisfaction as a legitimate honor restoration method,
 *   even where external costs alone would not have prevented the practice.
 *   This composite reading models the case where both mechanisms operated
 *   together with different victim sets and different extractiveness
 *   profiles. The drop mechanism extracted from duel practitioners through
 *   criminal penalty and cost. The contraction mechanism extracted from all
 *   honor claimants by delegitimizing the identity framework through which
 *   honor disputes were conceptually resolvable. Neither mechanism alone is
 *   sufficient to explain the historical outcome; understanding the decline
 *   requires modeling the interaction between external-cost foreclosure and
 *   conceptual-framework delegitimization.
 *
 * KEY AGENTS:
 *   - Subaltern Honor Claimants: Victims of contraction mechanism (powerless/trapped) — their honor claims are delegitimized even where external costs do not physically prevent violent satisfaction.
 *   - Duel-Bound Nobility: Victims of both mechanisms simultaneously (moderate/constrained) — face legal prosecution (drop) while losing the legitimacy framing that would justify either action or honorable refusal (contraction).
 *   - Reformist Legal Authority: Primary beneficiary and active enforcer (institutional/arbitrage) — extracts monopoly on legitimate violence and authority to redefine core concepts; provides genuine coordination function (dispute resolution via courts).
 *   - Bourgeois Property Interests: Secondary beneficiary (institutional/arbitrage) — benefit from dueling elimination as reduction of transaction costs and liability exposure; benefit from contraction as reinforcement of property-based status replacing honor-based status.
 *   - Traditional Honor Elite: Secondary victim (institutional/constrained) — identity framework is delegitimized; social authority partially replaced by property/legal credentials.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating the historical outcome as inevitable structural law rather than contingent result of deliberate policy interaction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, 0.68).
domain_priors:suppression_score(honor_violence_legitimacy__composite_reading, 0.72).
domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__composite_reading, "Honor-Violence Legitimacy: Composite Reading (Overdetermined Decline via Drop + Contraction)").
narrative_ontology:topic_domain(honor_violence_legitimacy__composite_reading, "legal_anthropology/commitment_systems/cultural_evolution").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__composite_reading, 'a077794e-dabc-4df4-9376-7a24b81685ae').
narrative_ontology:cs_kernel_codification('a077794e-dabc-4df4-9376-7a24b81685ae', fixed_text).
narrative_ontology:cs_authority_grounding('a077794e-dabc-4df4-9376-7a24b81685ae', extraction).
narrative_ontology:cs_interpretation_layer_present('a077794e-dabc-4df4-9376-7a24b81685ae').
narrative_ontology:cs_reading_relation('a077794e-dabc-4df4-9376-7a24b81685ae', honor_violence_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('a077794e-dabc-4df4-9376-7a24b81685ae', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_axiom('a077794e-dabc-4df4-9376-7a24b81685ae', foundational, dual_mechanism_non_decomposable).
narrative_ontology:cs_axiom_status(dual_mechanism_non_decomposable, holdable).
narrative_ontology:cs_axiom_grounding('a077794e-dabc-4df4-9376-7a24b81685ae', dual_mechanism_non_decomposable, empirically_contingent).
narrative_ontology:cs_axiom('a077794e-dabc-4df4-9376-7a24b81685ae', foundational, extraction_requires_dual_suppression).
narrative_ontology:cs_axiom_status(extraction_requires_dual_suppression, holdable).
narrative_ontology:cs_axiom_grounding('a077794e-dabc-4df4-9376-7a24b81685ae', extraction_requires_dual_suppression, deontological).
narrative_ontology:cs_reference_frame('a077794e-dabc-4df4-9376-7a24b81685ae', violence_sanctioned_honor_restoration).
narrative_ontology:cs_drift_state('a077794e-dabc-4df4-9376-7a24b81685ae', post_enlightenment_legal_reform, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('a077794e-dabc-4df4-9376-7a24b81685ae', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, reformist_legal_authority).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, bourgeois_property_interests).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, nobility_subaltern_honor_claimants).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, duel_victims_families).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBALTERN HONOR CLAIMANT (SNARE) — A non-elite figure claiming insult requires violent satisfaction, but both mechanisms now foreclose this simultaneously: external legal machinery criminalizes dueling (drop mechanism), while honor itself has been redefined to exclude satisfaction through violence (contraction mechanism). Exit is denied at the structural level — neither exit to court nor exit to violence remains available. Maximum experienced extraction with no escape vector.
constraint_indexing:constraint_classification(honor_violence_legitimacy__composite_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DUEL-BOUND NOBLEMAN (SNARE) — A nobleman socially obligated to answer a challenge faces escalating costs: refusal invokes shame under the old honor framework (contraction suppresses the legitimacy language that would justify refusal), while accepting dueling invokes criminal prosecution (drop mechanism criminalizes the act). The contraction removes the honorable framing for exit; the drop removes the honorable framing for action. Caught between two delegitimizations with no coherent alternative identity available. High suppression, high extraction.
constraint_indexing:constraint_classification(honor_violence_legitimacy__composite_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORMIST LEGAL AUTHORITY (TANGLED ROPE) — The state coordination function is genuine: eliminating dueling-as-dispute-resolution enables reliable contract enforcement and property protection (rope coordination). But the mechanism exhibits asymmetric extraction: the authority captures monopoly over legitimate violence and redefines the honor concept to delegitimize competitor frameworks (snare mechanism). Both functions coexist — coordination + extraction. Active enforcement required to maintain the redefinition (suppressing alternative honor frameworks).
constraint_indexing:constraint_classification(honor_violence_legitimacy__composite_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: BOURGEOIS PROPERTY REGIME (TANGLED ROPE) — Commercial interests benefit from dueling elimination (transaction costs drop, liability exposure drops, insurance becomes viable). The coordination function is real: property regime requires predictable conflict resolution. But the redefinition of honor extracts from nobility by delegitimizing the identity framework on which their social authority rested. Dual mechanism: drop reduces external costs (beneficiary-aligned), contraction redefines away competing legitimacy claims (extractive).
constraint_indexing:constraint_classification(honor_violence_legitimacy__composite_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: HONOR RITUAL RESIDUE (PITON) — The traditional honor ritual (elaborate insult protocols, challenge procedures, duel choreography) persists in literary, military, and diplomatic contexts long after the mechanism has been inactivated. The performance is maintained through institutional inertia and identity commitment, but the functional content has degraded. Theater ratio high (0.58+) — the ritual is evoked, cited, aestheticized, but lacks operative force. Neither mechanism can fully eliminate the language entirely; it persists as heritage performance.
constraint_indexing:constraint_classification(honor_violence_legitimacy__composite_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the transition from honor-violence to state monopoly on legitimate violence appears as an immutable structural law of societal development. All complex societies eventually eliminate private dispute resolution to consolidate state authority. This perspective risks naturalizing what is actually a contingent institutional overlay: the drop mechanism (external cost increase) and contraction mechanism (conceptual redefinition) are deliberate policy instruments, not laws of nature. Engine false-summit detection will reveal this naturalization.
constraint_indexing:constraint_classification(honor_violence_legitimacy__composite_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__composite_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(honor_violence_legitimacy__composite_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(honor_violence_legitimacy__composite_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, TR),
    TR >= 0.70.

:- end_tests(honor_violence_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High-moderate, reflecting the strong enforcement required to sustain the dual mechanism. The drop mechanism alone would produce lower extractiveness (0.40–0.50 range) — external costs are sufficient to make dueling rare without intensive suppression of the underlying legitimacy concept. The contraction mechanism alone would produce moderate extractiveness (0.50–0.60 range) — redefinition requires continuous enforcement but does not require physical criminal machinery. The composite mechanism produces higher extractiveness (0.68) because both mechanisms must be actively enforced simultaneously, creating redundant suppression. If one mechanism begins to fail (e.g., honor concept revivals in military subculture), the other mechanism (legal machinery) must compensate, driving suppression requirements upward. Suppression (0.72): High. Dual-mechanism suppression requires: (a) criminal legal apparatus criminalizing the practice; (b) intellectual apparatus delegitimizing the concept; (c) social apparatus concentrating stigma on practitioners; (d) institutional apparatus redefining status alternatives (property/legal credentials replacing honor). All four layers are necessary because any single layer alone would be insufficient. The suppression trajectory (0.42 → 0.72 over 30 units) reflects the build-up of enforcement infrastructure — initial drop mechanism relies on legal penalties alone; contraction mechanism emerges later as a secondary suppression layer; by t=30, both mechanisms are fully operant and reinforcing. Theater ratio (0.58): Moderate-high. The honor ritual persists in literary, diplomatic, and military contexts after the mechanism has been inactivated — elaborate insult protocols, code language, honor codes in military and maritime contexts. But the functional content has degraded: the ritual is evoked and aestheticized, but lacks operative force to resolve actual disputes. The trajectory (0.38 → 0.58) reflects increasing theatricality as the practice becomes rarer: the ritual is preserved precisely because it is no longer functionally operative, allowing its preservation as heritage performance rather than threat to state authority.
 *
 * PERSPECTIVAL GAP:
 *   This constraint displays the full range of DR types across its perspectives, revealing how indexical classification captures genuine structural differences. The subaltern and nobleman both experience snares but from different causal pathways (contraction vs. bilateral constraint). The legal authority and property regime both experience tangled rope (genuine coordination + asymmetric extraction) but from different benefit sources (monopoly on violence vs. transaction cost reduction). The honor ritual residue is piton (inert performance maintained by institutional inertia). The analytical observer risks mountain (naturalizing the historical outcome as inevitable societal law) but the structural data reveals this as false summit (the outcome results from deliberate policy interaction, not natural law). The perspectival gaps reveal that the composite mechanism is not a single monolithic extraction but a coordination system (courts + property rules) sustained by two supplementary extraction mechanisms (drop + contraction). Each perspective reveals which mechanism is primary from that agent's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the dual mechanism requires decomposition. Drop mechanism (legal + external cost pathway) extracts primarily from duel practitioners — beneficiary is the legal authority (monopoly on legitimate violence enforcement), victim is the practitioner (legal risk + cost). Contraction mechanism (conceptual redefinition pathway) extracts from all honor claimants — beneficiary is the legal authority (authority to define core concepts) + property-based status seekers (honor concept replaced by property concept as status basis), victim is anyone whose identity or status was honor-based. The composite mechanism exhibits higher directional complexity because the two victim sets partially overlap (nobility) but also diverge (subaltern honor claimants affected by contraction but not drop). The engine computes d separately for each victim group relative to each mechanism, then integrates across mechanisms. High suppression (0.72) reflects that maintaining both mechanisms requires high enforcement cost; if one mechanism begins to fail, the other must compensate at higher intensity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_causal_ordering,
    'Which mechanism was temporally primary in this empirical case: external costs (drop) or conceptual redefinition (contraction)?',
    'Chronological analysis of legislation, court records, and intellectual discourse. Identify which mechanism appears first in the archival record and trace how each responds to the other.',
    'If drop precedes contraction: the redefinition is reactive cover story (constraint is primarily economic). If contraction precedes drop: the redefinition is foundational (constraint is primarily cultural). If simultaneous: composite mechanism is accurate (neither is reducible to the other).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mechanism_causal_ordering, empirical, 'Temporal ordering of drop vs. contraction mechanisms in the historical record').

omega_variable(
    victim_set_overlap,
    'Are the victim sets identical, partially overlapping, or structurally distinct between the drop mechanism and the contraction mechanism?',
    'Mapping who experiences costs under each mechanism independently: duel practitioners under drop (legal risk); honor claimants relying on violence-satisfaction framing under contraction (identity delegitimization); both mechanisms targeting nobility vs. affecting subaltern claims differently.',
    'If fully overlapping: composite mechanism is descriptive shorthand for a single underlying extraction. If distinct: the mechanisms operate on different victim populations and the constraint is genuinely composite (captures multiple extraction paths). If partial overlap: composite mechanism is accurate but mandatrophy resolution is more complex (no single victim group experiences the full constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_overlap, empirical, 'Whether drop and contraction mechanisms target identical or distinct victim populations').

omega_variable(
    contraction_scope_ambiguity,
    'Does the redefinition of honor (contraction mechanism) apply universally to all agent types, or selectively to nobles while permitting alternative honor frameworks for other groups?',
    'Analysis of which agent types (military, merchant, clergy, subaltern) retain violence-based honor satisfaction after formal dueling prohibition. Identification of secondary honor frameworks that persist as legitimate.',
    'If universal: contraction is unified delegitimization (high suppression, high extraction uniformly). If selective: contraction primarily targets nobility (extraction concentrated on one group), while subaltern agents may retain alternative violence-honor frameworks (creating a different constraint for different populations).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_scope_ambiguity, empirical, 'Whether honor redefinition applies universally or selectively by agent type').

omega_variable(
    alternative_honor_framework_persistence,
    'Do alternative honor frameworks (military honor, merchant reputation codes, clergy sanctity) persist and function as operant legitimacy systems after the state delegitimizes dueling-based honor?',
    'Examination of non-dueling honor practices in the post-dueling era: military courts of honor, merchant guilds, diplomatic protocol, religious sanctity claims. Assessment of whether these frameworks provide functional alternatives to dueling-satisfaction or represent purely decorative persistence.',
    'If alternative frameworks are operative: the contraction mechanism is partial (some honor concepts retain legitimacy), and the composite constraint primarily targets nobles and elites. If alternative frameworks are merely decorative: contraction is more complete, and the composite constraint exhibits higher suppression uniformly. This affects whether the constraint is better modeled as one composite story or decomposed into separate stories for distinct agent populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_honor_framework_persistence, empirical, 'Whether alternative honor frameworks persist as operant legitimacy systems post-dueling').

omega_variable(
    reading_decomposability,
    'Can the composite reading be decomposed into the drop reading and contraction reading as separable mechanisms, or does the simultaneous operation of both mechanisms create a genuinely new structural form that neither mechanism alone explains?',
    'Comparative historical analysis: examine cases where drop occurred without contraction (dueling remained conceptually legitimate but became rare due to costs); examine cases where contraction occurred without drop (honor redefinition occurred without legal machinery; examine whether composite simultaneously-operating case exhibits properties (victim populations, suppression patterns, temporal dynamics) that are emergent properties of interaction rather than simple union of the two mechanisms.',
    'If decomposable: this composite reading is analytic convenience; the drop and contraction readings are the fundamental constraints. If non-decomposable: the composite reading represents a genuine kernel reading that captures emergent structure unique to simultaneous operation. This affects network structure and whether three separate constraint stories are needed or if composite_reading suffices as the primary story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_decomposability, conceptual, 'Whether composite mechanism is emergent or decomposable into drop + contraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__composite_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hvlc_theater_t0, honor_violence_legitimacy__composite_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(hvlc_theater_t15, honor_violence_legitimacy__composite_reading, theater_ratio, 15, 0.51).
narrative_ontology:measurement(hvlc_theater_t30, honor_violence_legitimacy__composite_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(hvlc_extract_t0, honor_violence_legitimacy__composite_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hvlc_extract_t15, honor_violence_legitimacy__composite_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(hvlc_extract_t30, honor_violence_legitimacy__composite_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hvlc_supp_t0, honor_violence_legitimacy__composite_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(hvlc_supp_t15, honor_violence_legitimacy__composite_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(hvlc_supp_t30, honor_violence_legitimacy__composite_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__composite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, state_monopoly_legitimate_violence).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, property_regime_status_replacement).

% DUAL FORMULATION NOTE:
% This constraint is part of the honor_violence_legitimacy kernel family. The kernel has three readings: (1) drop_reading — external cost mechanism alone; (2) contraction_reading — conceptual redefinition mechanism alone; (3) composite_reading — both mechanisms simultaneously (this story). All three stories share the same kernel_id but instantiate different readings. The composite_reading is the primary constraint in the empirical case; the drop and contraction readings represent alternative counterfactual decompositions. The network links all three readings and connects to downstream constraints in property regime and state authority structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_violence_legitimacy__composite_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
