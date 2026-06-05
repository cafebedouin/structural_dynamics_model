% ============================================================================
% CONSTRAINT STORY: self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_self_determination_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: self_determination_reading
 *   human_readable: Self-Determination Reading of Territorial Sovereignty Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel
 *   'territorial sovereignty legitimacy' — the self-determination reading
 *   asserts that state legitimacy derives from the modern principle of
 *   popular self-determination applied to the Arab population with
 *   demographic majority and continuous residence in the territory during the
 *   19th-20th centuries. This reading emerged as the dominant international
 *   law framing in the post-WWII decolonization period and has been
 *   institutionalized in UN General Assembly resolutions since 1974. It
 *   competes with two sibling readings: the covenant continuity reading
 *   (grounding legitimacy in Jewish historical connection and
 *   religious/national covenant claim) and the existential matrix reading
 *   (grounding legitimacy in geopolitical stability, refugee prevention, and
 *   deterrence of regional conflict). The self-determination reading
 *   generates significant structural asymmetry: it asserts Palestinian
 *   statehood as restoration of suppressed self-determination while framing
 *   Israeli statehood as colonial imposition. The constraint exhibits tangled
 *   rope structure — genuine coordination function through international
 *   self-determination doctrine alongside asymmetric extraction (enforcement
 *   of the reading against alternative framings, suppression of competing
 *   legitimacy claims, material consequences for Palestinian refugees denied
 *   return). The measurement trajectory shows increasing extractiveness and
 *   rising theater ratio: extractiveness rises from 0.35 (1974, initial UN
 *   assertion) to 0.58 (contemporary, deepened entrenchment without
 *   operational implementation), while theater ratio rises from 0.32 to 0.48
 *   as the reading becomes increasingly performatively invoked in
 *   international forums without enforcement mechanisms producing results.
 *
 * KEY AGENTS:
 *   - Arab Population with Demographic Majority: Primary beneficiary (organized/constrained) — asserted by this reading as the legitimacy holder; constrained by military occupation and international non-enforcement
 *   - Palestinian Authority and Arab State Coalition: Institutional beneficiary (institutional/constrained) — coordinates through UN General Assembly, Arab League; constrained by enforcement barriers and competing power interests
 *   - International Self-Determination Doctrine: Doctrinal beneficiary (institutional/arbitrage) — the reading instrumentalizes this principle; institutional actors benefit from its legitimacy provision
 *   - Israeli State: Primary victim of the reading's asymmetric structure (powerful/arbitrage) — experiences suppression of competing legitimacy claims; maintains arbitrage through military enforcement and counter-narratives
 *   - Jewish Historical Presence Framework: Competing legitimacy framework — victimized by this reading's exclusion of pre-modern historical claims from legitimacy calculus
 *   - International Legal/Political System: Institutional maintainer (institutional/constrained) — performs the reading through UN resolutions while constrained from enforcement by Security Council veto and geopolitical interests
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(self_determination_reading, 0.58).
domain_priors:suppression_score(self_determination_reading, 0.65).
domain_priors:theater_ratio(self_determination_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(self_determination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(self_determination_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(self_determination_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(self_determination_reading, tangled_rope).
narrative_ontology:human_readable(self_determination_reading, "Self-Determination Reading of Territorial Sovereignty Legitimacy").
narrative_ontology:topic_domain(self_determination_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(self_determination_reading, '87a700e6-fa40-4978-8e45-c9588065942f').
narrative_ontology:cs_created_at('87a700e6-fa40-4978-8e45-c9588065942f', '').
narrative_ontology:cs_kernel_codification('87a700e6-fa40-4978-8e45-c9588065942f', formalized).
narrative_ontology:cs_authority_grounding('87a700e6-fa40-4978-8e45-c9588065942f', lineage).
narrative_ontology:cs_interpretation_layer_present('87a700e6-fa40-4978-8e45-c9588065942f').
narrative_ontology:cs_kernel_id(self_determination_reading, territorial_sovereignty_legitimacy).
narrative_ontology:cs_reading_relation('87a700e6-fa40-4978-8e45-c9588065942f', covenant_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('87a700e6-fa40-4978-8e45-c9588065942f', existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('87a700e6-fa40-4978-8e45-c9588065942f', foundational, modern_period_continuous_residence_binds_legitimacy).
narrative_ontology:cs_axiom_status(modern_period_continuous_residence_binds_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('87a700e6-fa40-4978-8e45-c9588065942f', modern_period_continuous_residence_binds_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('87a700e6-fa40-4978-8e45-c9588065942f', foundational, external_imposition_delegitimizes_partition).
narrative_ontology:cs_axiom_status(external_imposition_delegitimizes_partition, holdable).
narrative_ontology:cs_axiom_grounding('87a700e6-fa40-4978-8e45-c9588065942f', external_imposition_delegitimizes_partition, deontological).
narrative_ontology:cs_reference_frame('87a700e6-fa40-4978-8e45-c9588065942f', continuous_arab_majority_self_governance).
narrative_ontology:cs_drift_state('87a700e6-fa40-4978-8e45-c9588065942f', contemporary_post_1967, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(self_determination_reading, arab_population_with_demographic_majority).
narrative_ontology:constraint_beneficiary(self_determination_reading, international_self_determination_doctrine).
narrative_ontology:constraint_victim(self_determination_reading, jewish_historical_claim_framework).
narrative_ontology:constraint_victim(self_determination_reading, alternative_legitimacy_readings).
narrative_ontology:constraint_victim(self_determination_reading, territorial_integrity_absent_consent).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED ARAB POPULATION (SNARE) — Structurally trapped by military occupation, legal exclusion from return, and international non-enforcement of self-determination principle. Bears full suppression cost (physical barriers to return, legal prohibition, settlement expansion). No exit mechanism; the self-determination reading asserts their historical claim but the constraint itself denies its realization. Maximum experienced extraction — the principle is invoked but not operationalized for this agent.
constraint_indexing:constraint_classification(self_determination_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ARAB STATE COALITION (TANGLED ROPE) — Coordinated through the Arab League, UN General Assembly resolutions, and international law doctrines around self-determination. Genuine coordination function: collective framing of Palestinian statehood as legitimate recovery of suppressed self-determination. Asymmetric extraction: advocates mobilize the principle but face enforcement barriers (Israeli security doctrine, US veto power, international recognition asymmetry). Constrained exit — states benefit from the norm but cannot unilaterally enforce it; withdrawal from the claim incurs diplomatic and legitimacy costs.
constraint_indexing:constraint_classification(self_determination_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SELF-DETERMINATION DOCTRINE (ROPE) — The UN Charter principle of self-determination coordinates post-colonial decolonization globally. This reading instrumentalizes the doctrine for the Palestinian case. Pure coordination benefit: the doctrine is invoked consistently across decolonization contexts (India, Algeria, Vietnam, Sub-Saharan Africa). Institutional actors (UN bodies, states employing the doctrine) experience the constraint as coordination — it provides a legitimacy framework for addressing territorial disputes. Arbitrage exit: the doctrine can be applied to or withheld from specific cases based on power politics; institutional actors retain flexibility in deployment.
constraint_indexing:constraint_classification(self_determination_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ISRAELI STATE / ALLIED STRATEGIC POWERS (TANGLED ROPE) — Coordinated through military alliance, settlement expansion (coordination of territorial control), and security doctrine. Experiences this reading as extraction: self-determination framework, if applied symmetrically, would subordinate Israeli territorial claims to Palestinian majority demographics and historical presence. The reading generates suppression mechanisms (international delegitimization discourse, BDS campaigns, legal challenges to settlements) that Israeli actors resist through military enforcement and counter-narratives. Arbitrage exit: Israeli state retains option to negotiate away portions of territory, maintain control through force, or seek alternative legitimacy framings (e.g., historical connection reading). High asymmetry: the reading suppresses Israeli counter-claims while asserting Palestinian ones.
constraint_indexing:constraint_classification(self_determination_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Frames self-determination as an irreducible principle of modern political legitimacy: no state can claim legitimacy without consent of the governed, majority self-determination is ontologically prior to competing territorial claims. This reading naturalizes a 20th-century doctrine as civilizational law. However, the structural data contradicts mountain classification — identifiable beneficiaries (Arab population, self-determination doctrine) and victims (competing legitimacy frameworks) exist; extraction mechanisms are visible (suppression of alternative readings, international enforcement asymmetry). The engine's false summit detector will identify this as naturalization of a contingent reading rather than discovery of natural law.
constraint_indexing:constraint_classification(self_determination_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL FRAMEWORK (PITON) — The self-determination principle has become a theatrical invocation in international bodies: UN General Assembly resolutions assert Palestinian self-determination consistently (since 1974), but enforcement mechanisms are absent (Security Council vetoes, lack of binding mechanisms, no implementation authority). Theater ratio (0.48 baseline, rising) reflects that the legal doctrine is maintained through repeated assertions and ceremonial invocation despite non-enforcement. The framework's primary function (legitimizing state territorial claims) has degraded relative to its operational capacity. Institutional actors maintain the performative assertion because alternatives (silence on self-determination, explicit denial) would incur legitimacy costs, but the constraint persists through inertia rather than effective coordination.
constraint_indexing:constraint_classification(self_determination_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(self_determination_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(self_determination_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(self_determination_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(self_determination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(self_determination_reading, TR),
    TR >= 0.70.

:- end_tests(self_determination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reading asserts a strong legitimacy claim (Palestinian self-determination) that directly contradicts alternative framings (Jewish historical legitimacy). Base extraction reflects the suppression of competing frameworks and the material asymmetry in implementation: Palestinian refugees remain displaced while Israeli settlements expand, yet the international legal framework asserts Palestinian self-determination without enforcement. The reading generates extraction through selective application of self-determination doctrine (invoked for Palestinian case, withheld from Israeli case despite post-1948 Jewish demographic establishment). Rising trajectory (0.35 → 0.58 over 50 years) reflects that as the reading became institutionalized in international law, the gap between asserted legitimacy and actual implementation widened, increasing the experienced extraction for those asserting the claim (constrained enforcement) and those suppressed by it (competing frameworks). Suppression (0.65): High. Material suppression mechanisms include military occupation, legal restrictions on Palestinian return and settlement, Israeli military enforcement of territorial claims, and settlement expansion. Doctrinal suppression includes framing alternative legitimacy readings as illegitimate or colonialist. International suppression mechanisms are weaker (UN resolutions lack enforcement) but significant (delegitimization discourse, BDS campaigns, legal challenges to settlements). Theater ratio (0.48): Moderate-high and rising. The self-determination reading is performatively invoked in UN General Assembly (repeated resolutions, ceremonial affirmation) with minimal operational enforcement. The international legal framework asserts the principle while enforcement remains blocked by Security Council veto (US protection of Israeli state), creating theater gap. Theater rises over time as the gap between repeated assertion and non-implementation widens, increasing the performative component relative to functional legitimacy transfer.
 *
 * PERSPECTIVAL GAP:
 *   The self-determination reading generates maximum perspectival divergence across all six types. From the Arab population perspective (powerless/trapped), the reading appears as a Snare: the principle asserts legitimacy but military occupation and legal exclusion prevent implementation, making the constraint functionally extractive despite rhetorical affirmation. From the international self-determination doctrine perspective (institutional/arbitrage), the reading appears as pure Rope: the doctrine coordinates decolonization globally, providing legitimacy frameworks, and institutional actors maintain it through repeated invocation. From the Israeli state perspective (powerful/arbitrage), the reading appears as Tangled Rope: it suppresses Israeli counter-claims while asserting Palestinian ones, generating asymmetric extraction, yet some coordination function exists (the reading's legitimacy framework applies consistently to self-determination cases globally). From the Arab state coalition perspective (organized/constrained), the reading appears as Tangled Rope: genuine coordination through UN bodies and legitimacy doctrine, but constrained by enforcement barriers and geopolitical power asymmetry. From the analytical civilizational perspective, the reading risks appearing as a Mountain (natural law of legitimacy) but structural beneficiaries and victims suggest false summit: the reading naturalizes a contingent post-1945 doctrine as immutable principle. The piton perspective reveals that the reading is increasingly performative (theater rising) as repeated assertions (UN resolutions) fail to produce implementation (no right-of-return enforcement, settlement expansion continues), maintaining legitimacy appearance through ceremonial invocation despite functional degradation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position: beneficiary status (Arab majority population, self-determination doctrine) and exit options (trapped/constrained/arbitrage) determine experienced extractiveness through the sigmoid f(d). The Arab population appears as trapped victims in perspective 1, deriving high d (0.90+), producing high f(d) (≈1.28–1.42) and high experienced χ. The institutional self-determination doctrine appears as beneficiary with arbitrage options in perspective 3, deriving low d (≈0.15), producing negative f(d) (≈−0.01), reducing apparent extraction for doctrine-maintaining actors. The Israeli state appears as victim of this reading's extraction in perspective 4, deriving moderate-high d (≈0.65–0.75) from powerful/arbitrage position but victim status within this specific reading, producing moderate f(d) (≈1.00–1.15). The pipeline does not assign d directly — instead, the engine derives d from beneficiary/victim declarations and exit options: trapped victims get high d; arbitrage beneficiaries get low d. The asymmetry between perspectives 3 and 4 (both institutional, both arbitrage, but opposite beneficiary/victim roles) illustrates how d differentiates based on structural position within the specific constraint, not global power status.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy dissolution by maintaining coherence as a Tangled Rope from multiple perspectives while acknowledging non-implementation. The mandatrophy risk is whether the reading is legitimate coordination (self-determination doctrine is a genuine post-colonial principle applying to all decolonization cases) or pure extraction (the reading suppresses alternative legitimacy frameworks selectively applied to benefit Arab coalition while restraining Israeli state). The reading resolves this by asserting that self-determination doctrine is genuinely universalist — it should apply equally to all populations' democratic self-determination — but has been asymmetrically applied due to power politics and enforcement failures. From this frame, the extraction is not inherent to the reading but to international system non-enforcement. The tangled rope classification persists because the reading genuinely coordinates around self-determination principle while simultaneously suppressing competing frameworks. If the reading fully enforced self-determination symmetrically (Israeli self-determination equally asserted as Palestinian), it would become pure Rope (coordination without asymmetry); if enforcement remains blocked asymmetrically, it remains Tangled Rope. The constraint avoids Snare classification by maintaining the doctrinal coordination function (self-determination as legitimate international principle) even where implementation fails, distinguishing this reading from purely extractive constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temporal_scope_boundary,
    'Does the legitimacy boundary at the 19th-20th century modern period exclude or include earlier waves of settlement, immigration, and population movement?',
    'Historical periodization analysis: specify which centuries count as ''modern period''; clarify whether Palestinian settlement before 1800 counts; specify Ottoman imperial period framing; determine whether Jewish immigration/settlement in late 19th century is included or excluded from the continuous residence requirement.',
    'If early Islamic period (7th-19th centuries) is excluded: Palestinian majority claim is stronger. If included: Jewish immigration in 19th century partially satisfies alternative reading''s continuous presence claims. If Ottoman period is treated differently: legitimacy timeline becomes contestable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(temporal_scope_boundary, conceptual, 'What temporal boundary defines the modern period for continuous residence requirement').

omega_variable(
    demographic_majority_measurement,
    'How is demographic majority measured and at what geographic scale? Does majority status apply to entire territory, specific regions, or at specific historical moments?',
    'Ottoman census data (1831, 1850s), British Mandate population statistics (1922-1948), 1967 post-war demography, and contemporary data. Clarify: Does Palestinian majority require territorial contiguity or statistical majority (possibly non-contiguous)? Which historical moment is binding — 1900, 1920, 1947, 1967?',
    'If pre-1900: Arab majority is overwhelming. If 1920: majority declines due to Jewish immigration during Mandate period. If 1967: Palestinian population within pre-1967 borders has demographic majority, but Jewish population within 1948-1967 Israel has majority (reading becomes ambiguous on territorial scope). If post-1967 (inclusive of settlements): Jewish population has grown, eroding majority claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_majority_measurement, empirical, 'Definition and measurement of demographic majority at specific territorial and temporal scales').

omega_variable(
    competing_presence_claim_foreclosure,
    'Does asserting continuous Arab residence as legitimacy ground foreclose or coexist with Jewish historical presence claims that predate the modern period?',
    'Conceptual analysis: Can both readings hold within a single legitimacy framework (coexists), or does Arab self-determination reading logically exclude Jewish historical connection reading (forecloses)? Empirically distinguish: What population movements does each reading count as legitimate presence?',
    'If forecloses: reading asserts exclusivity of Arab modern-period claim; Jewish historical framework is ruled out. If coexists: readings represent genuinely different but non-contradictory frameworks held by different parties. Determines whether the constraint is a zero-sum territorial claim (Snare) or a partitioning of legitimacy schemes (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_presence_claim_foreclosure, conceptual, 'Whether continuous Arab residence claim logically forecloses competing historical presence claims').

omega_variable(
    external_imposition_causal_claim,
    'To what extent does framing partition and Israeli statehood as ''external imposition'' depend on specific counterfactual assumptions about unforced consent?',
    'Historical counterfactual analysis: If partition had been negotiated with Palestinian consent (or rejected by Palestinian leadership with different consequence structures), would the legitimacy claim change? What constitutes ''genuine consent'' vs. coerced consent in a context of asymmetric power?',
    'If ''external'' is empirically descriptive (British/UN action without Palestinian consent secured): all partition readings agree. If ''external'' is a moral claim (imposition is illegitimate because unconsented): depends on whether forced choice is itself illegitimate, which is a meta-principle not resolved by self-determination doctrine alone. May shift reading from tangled_rope to snare if imposition is treated as non-recoverable violation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_imposition_causal_claim, conceptual, 'What constitutes external imposition and whether it invalidates legitimacy claims derived from partition').

omega_variable(
    right_of_return_constraint_vs_operational_principle,
    'Is the right of return derived from this reading as a fundamental principle, or as an operationally contingent claim dependent on negotiated settlements?',
    'Textual analysis: Does the reading assert return as non-negotiable entitlement, or as a negotiating position within a self-determination framework? Compare to other post-colonial decolonization cases (Indian Partition, Algeria, Vietnam) — are return rights asserted uniformly or treated as contextual?',
    'If non-negotiable: right of return becomes maximalist claim that may prevent settlements; reading becomes more extractive toward Israeli actors (suppression of alternative outcomes). If contingent: reading permits negotiation over return ratios, compensation, alternatives; reduces extraction asymmetry and permits tangled_rope classification to persist through bargaining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_constraint_vs_operational_principle, preference, 'Whether right of return is fundamental principle or negotiable outcome of self-determination').

omega_variable(
    self_determination_doctrine_applicability,
    'Is self-determination doctrine meant to apply equally to all resident populations, or does it prioritize indigenous/historical majority populations over immigrant populations?',
    'International law textual analysis: UN Charter self-determination language (General Assembly resolutions, ICJ opinions); decolonization case law (did Algeria apply self-determination excluding French colonists? Did India apply it as majority-Hindu principle despite Muslim minority?). Clarify: Is the reading''s version of self-determination majoritarian (demographic majority), nationalist (historically rooted people), or universalist (all residents)?',
    'If majoritarian: Jewish population in post-1948 Israel cannot claim self-determination (minority position). If nationalist: continuous Arab residence supports claim. If universalist: both Jewish and Arab residents have self-determination rights, potentially requiring binational or federal solution incompatible with this reading''s state-based framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_determination_doctrine_applicability, conceptual, 'Scope and applicability of self-determination doctrine to different resident populations').

omega_variable(
    false_summit_natural_law_risk,
    'Does this reading naturalize contingent political doctrine (UN Charter self-determination as immutable principle) or describe a genuine structural feature of political legitimacy?',
    'Historical comparison: Have legitimacy principles shifted before? Was divine right naturalized until challenged? Has self-determination remained constant since 1945 or has it been reinterpreted? Does the reading''s claim survive if self-determination doctrine is superseded by alternative legitimacy frameworks (e.g., historical rights, strategic stability, hybrid governance)?',
    'If natural law: reading is civilizationally immutable, false summit detector fires, engine reclassifies. If contingent doctrine: reading is one legitimate framing among contested alternatives, coexists_with sibling readings. Affects whether this reading can claim immunity from mandatrophy or must be treated as contestable political position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, conceptual, 'Whether self-determination reading naturalizes contingent doctrine as immutable legitimacy principle').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(self_determination_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(self_det_theater_t0, self_determination_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(self_det_theater_t25, self_determination_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(self_det_theater_t50, self_determination_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(self_det_extract_t0, self_determination_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(self_det_extract_t25, self_determination_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(self_det_extract_t50, self_determination_reading, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(self_determination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(self_determination_reading, 0.12).
narrative_ontology:affects_constraint(self_determination_reading, covenant_continuity_reading).
narrative_ontology:affects_constraint(self_determination_reading, existential_matrix_reading).
narrative_ontology:affects_constraint(self_determination_reading, international_self_determination_doctrine).
narrative_ontology:affects_constraint(self_determination_reading, right_of_return_enforcement).

% DUAL FORMULATION NOTE:
% The self-determination reading is one member of a constraint family decomposing 'territorial sovereignty legitimacy' into three structurally distinct readings with different ε values, different beneficiary/victim structures, and different time horizons. The sibling readings (covenant_continuity_reading, existential_matrix_reading) instantiate alternative legitimacy framings with different core claims and different extraction asymmetries. This reading (self_determination_reading, ε=0.58) is moderately extractive because it suppresses alternative frameworks while facing enforcement barriers. The covenant continuity reading (expected ε≈0.42, Tangled Rope) suppresses demographic-majority claims while asserting historical connection. The existential matrix reading (expected ε≈0.35, Rope) coordinates around security architecture without suppressing either demographic or historical claims. Each constraint story gets its own perspectives, measurements, and omega variables. The family is linked through network.affects_constraints: each reading affects the others by establishing competing legitimacy conditions that downstream constraints must accommodate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(self_determination_reading, powerful, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
