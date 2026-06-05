% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__non_ratifier_enforcement_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
 *   human_readable: UNCLOS Sovereignty Boundary (Non-Ratifier Enforcement Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint instantiates the non-ratifier enforcement reading of the
 *   UNCLOS sovereignty boundary kernel. The reading claims that freedom of
 *   navigation principles are customary international law independent of
 *   UNCLOS ratification status, and are enforceable by naval presence (i.e.,
 *   non-ratifiers can invoke these principles against coastal state EEZ
 *   claims). The reading decouples freedom-of-navigation authority from the
 *   UNCLOS treaty text, grounding it instead in asserted customary practice.
 *   This reading benefits non-ratifier naval powers (particularly the United
 *   States, which has not ratified UNCLOS) by providing legal-philosophical
 *   cover for global naval operations without accepting treaty constraints on
 *   military basing, resource extraction, or passage rights. The reading
 *   simultaneously constrains coastal states attempting to enforce exclusive
 *   economic zone (EEZ) boundaries, particularly developing nations with
 *   limited naval capacity. The constraint exhibits tangled_rope structure:
 *   there is genuine coordination function (predictable navigation rules
 *   enable global maritime commerce and reduce collision risk), but the
 *   beneficiary-victim asymmetry and active enforcement requirements (naval
 *   presence as validation mechanism) meet tangled_rope gates. The
 *   theater_ratio reflects that the legal apparatus (UNCLOS, ITLOS,
 *   international law scholarship) produces legitimacy for a reading that is
 *   operationally enforced by asymmetric military capacity rather than by
 *   treaty compliance or legal adjudication.
 *
 * KEY AGENTS:
 *   - Non-ratifier Naval Powers (institutional/arbitrage): Primary beneficiaries — claim customary law authority to operate globally without UNCLOS treaty constraints; enforce the reading through naval presence and strategic interpretation of international law.
 *   - Coastal States without Naval Capacity (powerless/trapped): Primary victims — cannot defend EEZ boundaries against non-ratifier claims; no military counter-force; high suppression because enforcement requires capabilities they do not possess.
 *   - Developing Coastal States with Growing Navies (moderate/constrained): Secondary victims — face asymmetric enforcement barriers; smaller navies cannot challenge major power claims; constrained by capacity rather than legal text.
 *   - UNCLOS-Ratifying Naval Powers (powerful/constrained): Mixed position — bound by treaty but also benefit from coordination function; constrained by legal obligation but exercise agency through selective enforcement doctrine.
 *   - Coastal State EEZ Authority (collective/trapped): Victim set — the institutional concept of exclusive economic zones is undermined by the reading's assertion of customary freedom-of-navigation principles that override EEZ exclusivity.
 *   - International Maritime Legal Institutions (institutional/arbitrage): Perform legitimacy function (ITLOS, arbitral tribunals exist and issue rulings) but lack enforcement power against non-ratifier non-compliance; maintain procedural authority while functional authority is degraded.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.52).
domain_priors:suppression_score(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.68).
domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "UNCLOS Sovereignty Boundary (Non-Ratifier Enforcement Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '961a2574-132e-42ce-bc72-28e94302744a').
narrative_ontology:cs_kernel_codification('961a2574-132e-42ce-bc72-28e94302744a', formalized).
narrative_ontology:cs_authority_grounding('961a2574-132e-42ce-bc72-28e94302744a', extraction).
narrative_ontology:cs_interpretation_layer_present('961a2574-132e-42ce-bc72-28e94302744a').
narrative_ontology:cs_reading_relation('961a2574-132e-42ce-bc72-28e94302744a', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('961a2574-132e-42ce-bc72-28e94302744a', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_axiom('961a2574-132e-42ce-bc72-28e94302744a', foundational, customary_law_independence_from_treaty_text).
narrative_ontology:cs_axiom_status(customary_law_independence_from_treaty_text, holdable).
narrative_ontology:cs_axiom_grounding('961a2574-132e-42ce-bc72-28e94302744a', customary_law_independence_from_treaty_text, empirically_contingent).
narrative_ontology:cs_axiom('961a2574-132e-42ce-bc72-28e94302744a', foundational, naval_presence_as_enforcement_mechanism).
narrative_ontology:cs_axiom_status(naval_presence_as_enforcement_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('961a2574-132e-42ce-bc72-28e94302744a', naval_presence_as_enforcement_mechanism, instrumental).
narrative_ontology:cs_reference_frame('961a2574-132e-42ce-bc72-28e94302744a', customary_maritime_law_independence).
narrative_ontology:cs_drift_state('961a2574-132e-42ce-bc72-28e94302744a', contemporary_post_unclos_normalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('961a2574-132e-42ce-bc72-28e94302744a', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, non_ratifier_naval_powers).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_state_exclusive_economic_zones).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_maritime_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COASTAL STATE WITHOUT NAVAL CAPACITY (SNARE) — Cannot enforce EEZ boundaries against non-ratifiers; cannot exit the constraint without coastal navy development. Bears full extraction cost: waters claimed under UNCLOS are operationally open to enforcement evasion. Maximum perceived extraction because no counter-force exists.
constraint_indexing:constraint_classification(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UNCLOS-RATIFYING NAVAL POWER (TANGLED ROPE) — Constrained by treaty obligations but benefits from the coordination function (rules-based navigation predictability). Experiences mixed extraction: constrained by legal obligation to respect EEZ, but also coordinates global maritime commerce. Some agency through legal interpretation and selective enforcement doctrine.
constraint_indexing:constraint_classification(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: NON-RATIFIER NAVAL POWER (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: freedom of navigation principles (customary law, independent of UNCLOS ratification) enable global naval mobility without legal restriction. Net beneficiary — claims customary law authority to operate globally while evading UNCLOS treaty constraints.
constraint_indexing:constraint_classification(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEVELOPING COASTAL STATE WITH GROWING NAVY (SNARE) — Constrained by asymmetric capacity: smaller naval forces face enforcement barriers against major powers invoking customary law. Suppression is high — cannot meaningfully challenge non-ratifier freedom claims even under UNCLOS. Theater exists because the reading generates legitimacy performance ('customary law' framing masks power asymmetry).
constraint_indexing:constraint_classification(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL MARITIME LEGAL INSTITUTIONS (PITON) — The UNCLOS framework and dispute mechanisms (ITLOS, arbitral tribunals) are largely performative for this reading. Non-ratifiers invoke customary law to evade treaty constraints; the legal apparatus produces rulings without enforcement power. Institutions persist through procedural legitimacy (the tribunal exists, hears cases, issues rulings) while functional verification (whether non-ratifiers comply) is degraded. Theater ratio reflects that the legal process is disconnected from actual maritime behavior.
constraint_indexing:constraint_classification(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, freedom of navigation is a physical-geography consequence: the ocean is open, and naval forces can traverse it regardless of legal text. This perspective treats customary law as discovering an inevitable fact rather than constructing an institutional claim. However, the structural data indicates a false summit: the 'inevitability' naturalizes what is actually a strategic reading chosen by specific actors with structural power advantages.
constraint_indexing:constraint_classification(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, TR),
    TR >= 0.70.

:- end_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The reading extracts significant benefit for non-ratifier naval powers by providing legal authority for global operations without treaty restrictions. Coastal states bear corresponding costs through EEZ vulnerability. The extraction is not maximal (0.70+) because the reading competes with alternative interpretations (strict_eez_reading claims opposite authority structure) and because some UNCLOS ratifiers also exercise significant naval presence, partially offsetting the non-ratifier advantage. Suppression (0.68): High. Coastal states lack meaningful counter-force; most developing nations cannot project naval power sufficient to challenge major-power freedom-of-navigation claims. The suppression is structural — it derives from naval capacity asymmetry rather than legal text. Theater ratio (0.58): Moderate-high. The reading is enforced through naval presence (strategic positioning, passage operations, freedom-of-navigation exercises), not through legal compliance mechanisms. International legal institutions (UNCLOS, ITLOS) perform the legitimacy function — they produce the appearance of legal authority — while actual enforcement derives from military capacity. Measurements show rising theater and suppression over the interval, reflecting increasing performance-cost asymmetry as major powers conduct more frequent freedom-of-navigation operations and international institutions issue ineffectual rulings. Rising extractiveness reflects accumulating advantage for non-ratifiers as the reading becomes normalized through repeated assertion and practice.
 *
 * PERSPECTIVAL GAP:
 *   The non-ratifier reading generates stark perspectival divergence. The non-ratifier naval power (institutional/arbitrage) sees pure coordination (Rope): they experience the customary law principle as enabling efficient global navigation. The coastal state without naval capacity (powerless/trapped) sees maximal extraction (Snare): they experience their EEZ boundaries as operationally unenforceable. The developing coastal state with growing navy (moderate/constrained) sees mixed extraction with agency barriers (Snare, transitioning to Tangled Rope as navy capacity improves). The UNCLOS ratifier (powerful/constrained) sees mixed structure (Tangled Rope): they are constrained by treaty but also benefit from coordination and exercise agency through selective enforcement. The international legal institutions (institutional/arbitrage) perform legitimacy while being operationally sidelined (Piton). The civilizational analyst (analytical/analytical) risks seeing the reading as discovered natural law (Mountain — 'the ocean is open, naval forces can traverse it') when the structural data reveals it as a strategic reading chosen by and reinforcing specific power holders.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Non-ratifier naval powers are beneficiaries with arbitrage exit (can choose whether to invoke customary law or treaty constraints strategically) → d ≈ 0.10–0.20 → low effective extraction from their perspective. Coastal states without naval capacity are victims with trapped exit (cannot meaningfully challenge the reading regardless of preference) → d ≈ 0.95 → high effective extraction. Developing coastal states with growing navies are victims with constrained exit (face high costs to build counter-naval capacity but have non-zero options) → d ≈ 0.70–0.80 → moderate-high extraction. UNCLOS ratifiers are both beneficiary (coordination) and victim (treaty constraint) with constrained exit → d ≈ 0.50–0.55 → symmetric to slightly-victim-oriented extraction. International institutions are beneficiaries of procedural legitimacy (perform authoritative function) with arbitrage exit but face degraded functional authority → d ≈ 0.25 → low extraction from their perspective but high theater costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by showing that the same kernel (UNCLOS sovereignty boundary) produces different constraint structures depending on which reading's axioms are adopted. The non-ratifier reading's foundational axiom is that customary law is independent of treaty text. This axiom determines the entire constraint structure: beneficiaries are non-ratifiers, victims are coastal states attempting EEZ enforcement, and the coordination function is global navigation predictability (which benefits all users but disproportionately benefits non-ratifiers because they avoid treaty constraints). Alternative readings would produce different chi values, different beneficiary/victim structures, and potentially different types. The engine's mandatrophy resolution involves recognizing that each reading is a structurally coherent constraint with its own epsilon, and that no single type applies across all readings. The perspectival gap indicates not measurement confusion but committer-frame difference: the readings are held simultaneously by different parties (non-ratifier navies, coastal states, treaty institutions), and the constraint family models this simultaneity through network linkage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_grounding,
    'Does ''customary international law'' derive from immemorial practice (natural law candidate) or from repeated assertion by power holders during the UNCLOS negotiation era (strategic reading)?',
    'Historical tracing of freedom-of-navigation claims pre-1945 vs post-1945; correlation between claimant naval power and adoption of ''customary law'' framing; analysis of whether pre-UNCLOS state practice consistently invoked freedom principles or deployed them selectively.',
    'If pre-1945 consistent practice: classification approaches mountain (genuine customary origin). If post-1945 constructed framing: classification confirms tangled_rope (strategic reading masquerading as natural law).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(customary_law_grounding, empirical, 'Whether customary law derives from immemorial practice or strategic post-UNCLOS construction').

omega_variable(
    eez_exclusivity_enforcement_capacity,
    'What percentage of alleged EEZ violations by non-ratifiers are detected and challenged by coastal state navies? What percentage result in actual enforcement (boarding, fine, or escalation)?',
    'Analysis of UNCLOS dispute case frequency; incident reports from coastal state navies; correlation between coastal state naval capacity and enforcement rate; counterfactual: do UNCLOS ratifiers'' EEZ claims fare differently in practice than non-ratifier claims?',
    'If enforcement rate < 5% globally: suppression metric confirmed (coastal states cannot meaningfully defend EEZ against non-ratifier claims, regardless of legal text). If enforcement rate > 30%: suppression metric should be downgraded (EEZ is more functionally defended than this reading suggests).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eez_exclusivity_enforcement_capacity, empirical, 'Actual enforcement rate of EEZ exclusivity against non-ratifier naval presence').

omega_variable(
    reading_kernel_ambiguity,
    'This reading instantiates ONE interpretation of the UNCLOS sovereignty boundary kernel. The sibling readings (strict_eez_reading, historical_rights_reading) interpret the same kernel differently. Does the engine recognize these as structurally distinct constraints or as perspectival variants of one constraint?',
    'Corpus inspection: each sibling reading is authored as a separate constraint file with its own constraint_id, epsilon value, beneficiary/victim structure, and network linkage. The engine processes them as distinct stories, not as contextual variants.',
    'If recognized as distinct constraints: network decomposition is correct; each reading has its own chi, classification, and drift analysis. If collapsed to single constraint: mandatrophy arises (no single classification captures all readings; committer frame collapses into false summit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether kernel readings are structurally distinct constraints or perspectival variants').

omega_variable(
    naval_power_consensus_shift,
    'Is the non-ratifier enforcement reading in structural decline as more states achieve naval capacity? That is, as coastal state navies improve, does the ability to enforce EEZ exclusivity against non-ratifier claims increase, reducing the functional advantage of the non-ratifier reading?',
    'Longitudinal analysis of naval capacity distribution; trend analysis of EEZ enforcement incident rates; comparison of enforcement success between ratifier and non-ratifier coastal states; projection of naval capacity distribution in 2050.',
    'If enforcement capacity rising: extractiveness should trend downward over generational timescale; the reading''s dominance may shift to historical_rights_reading or strict_eez_reading. If enforcement capacity flat or declining: extractiveness stable or rising; non-ratifier reading maintains structural advantage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naval_power_consensus_shift, empirical, 'Whether rising coastal state naval capacity undermines non-ratifier enforcement reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_nr_tr_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(unclos_nr_tr_t10, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(unclos_nr_tr_t20, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(unclos_nr_be_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(unclos_nr_be_t10, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(unclos_nr_be_t20, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(unclos_nr_su_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(unclos_nr_su_t10, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(unclos_nr_su_t20, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_infrastructure).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__historical_rights_reading).

% DUAL FORMULATION NOTE:
% The UNCLOS sovereignty boundary kernel admits multiple readings with structurally distinct beneficiary/victim sets and epsilon values. This story (non-ratifier_enforcement_reading) instantiates one reading (ε=0.52, Tangled Rope). Sibling stories instantiate alternative readings with different epsilon values and different constraint structures. All three stories are linked via network.affects_constraints to show that they are readings of a common kernel, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
