% ============================================================================
% CONSTRAINT STORY: national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_national_liberation_reading, []).

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
 *   constraint_id: national_liberation_reading
 *   human_readable: Zionism as National Liberation Movement (National Liberation Reading)
 *   domain: political_history/nationalism/decolonization
 *
 * SUMMARY:
 *   The national liberation reading frames Zionism as a legitimate
 *   anticolonial movement: a persecuted indigenous people returning to their
 *   ancestral homeland after 2000 years of diaspora and systematic
 *   oppression. This reading emphasizes Jewish historical connection to the
 *   land, the failure of European emancipation, the Holocaust as proof that
 *   diaspora existence was untenable, and the necessity of territorial
 *   sovereignty for collective survival. Arab opposition is framed as denial
 *   of Jewish rights rather than as resistance to displacement. This
 *   constraint is ONE READING of the contested kernel
 *   'zionist_legitimacy_basis' — sibling readings (settler_colonial_reading,
 *   religious_restoration_reading) describe the same historical phenomenon
 *   with different structural emphases and different beneficiary/victim
 *   framings. The readings coexist as live positions held by different
 *   interpretive communities; none has achieved consensus.
 *
 * KEY AGENTS:
 *   - European Jewish Refugees (1930s-1940s): Primary beneficiaries at crisis point (powerless/trapped) — experienced Zionism as life-saving coordination
 *   - Palestinian Arab Population: Primary victims (powerless/trapped) — experienced displacement, land confiscation, refugee status, ongoing occupation
 *   - Mizrahi Jewish Immigrants: Mixed position (moderate/constrained) — benefited from refuge but bore costs of ethnic hierarchy within Israeli society
 *   - Zionist Institutional Leadership: Primary beneficiaries (institutional/arbitrage) — state-building actors with international mobility and resource access
 *   - Two-State Solution Advocates: Organized coalition (organized/mobile) — see national liberation framing as temporary, requiring sunset into shared sovereignty
 *   - International Humanitarian Framework: Mixed position (institutional/constrained) — coordinates refugee aid but constrained by great power politics and complicit in occupation infrastructure
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination (Jewish safety) and substantial extraction (Palestinian dispossession)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(national_liberation_reading, 0.68).
domain_priors:suppression_score(national_liberation_reading, 0.75).
domain_priors:theater_ratio(national_liberation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(national_liberation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(national_liberation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(national_liberation_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(national_liberation_reading, rope).
narrative_ontology:human_readable(national_liberation_reading, "Zionism as National Liberation Movement (National Liberation Reading)").
narrative_ontology:topic_domain(national_liberation_reading, "political_history/nationalism/decolonization").

domain_priors:requires_active_enforcement(national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(national_liberation_reading, 'f9ba2cb1-8dce-45ab-8e80-978b7f90cbde').
narrative_ontology:cs_kernel_codification('f9ba2cb1-8dce-45ab-8e80-978b7f90cbde', distributed).
narrative_ontology:cs_authority_grounding('f9ba2cb1-8dce-45ab-8e80-978b7f90cbde', lineage).
narrative_ontology:cs_interpretation_layer_present('f9ba2cb1-8dce-45ab-8e80-978b7f90cbde').
narrative_ontology:cs_reading_relation('f9ba2cb1-8dce-45ab-8e80-978b7f90cbde', national_liberation_reading__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9ba2cb1-8dce-45ab-8e80-978b7f90cbde', national_liberation_reading__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('f9ba2cb1-8dce-45ab-8e80-978b7f90cbde', foundational, persecution_justifies_refuge).
narrative_ontology:cs_axiom_status(persecution_justifies_refuge, holdable).
narrative_ontology:cs_axiom_grounding('f9ba2cb1-8dce-45ab-8e80-978b7f90cbde', persecution_justifies_refuge, empirically_contingent).
narrative_ontology:cs_axiom('f9ba2cb1-8dce-45ab-8e80-978b7f90cbde', foundational, historical_connection_grants_sovereignty).
narrative_ontology:cs_axiom_status(historical_connection_grants_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('f9ba2cb1-8dce-45ab-8e80-978b7f90cbde', historical_connection_grants_sovereignty, conventional).
narrative_ontology:cs_axiom('f9ba2cb1-8dce-45ab-8e80-978b7f90cbde', secondary, demographic_majority_not_required).
narrative_ontology:cs_axiom_status(demographic_majority_not_required, holdable).
narrative_ontology:cs_axiom_grounding('f9ba2cb1-8dce-45ab-8e80-978b7f90cbde', demographic_majority_not_required, instrumental).
narrative_ontology:cs_reference_frame('f9ba2cb1-8dce-45ab-8e80-978b7f90cbde', pre_state_zionist_consensus).
narrative_ontology:cs_drift_state('f9ba2cb1-8dce-45ab-8e80-978b7f90cbde', post_1967_occupation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f9ba2cb1-8dce-45ab-8e80-978b7f90cbde', '').
narrative_ontology:cs_kernel_id(national_liberation_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(national_liberation_reading, jewish_diaspora_populations).
narrative_ontology:constraint_beneficiary(national_liberation_reading, zionist_institutional_framework).
narrative_ontology:constraint_beneficiary(national_liberation_reading, israeli_state_apparatus).
narrative_ontology:constraint_victim(national_liberation_reading, palestinian_arab_population).
narrative_ontology:constraint_victim(national_liberation_reading, regional_stability).
narrative_ontology:constraint_victim(national_liberation_reading, alternative_jewish_political_movements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EUROPEAN JEWISH REFUGEES (ROPE) — Trapped by persecution with no exit options in Europe, experiencing Zionism as genuine coordination solving an existential collective-action problem. The constraint provides escape from genocide and coordinates resettlement. From this position, the movement is pure coordination with negligible extraction — the beneficiary perspective at maximum crisis.
constraint_indexing:constraint_classification(national_liberation_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: PALESTINIAN ARAB VILLAGERS (SNARE) — Trapped by military displacement and legal exclusion with no exit options. Experience the constraint as pure extraction: land confiscation, village destruction, refugee status, denial of return. The coordination story (Jewish safety) is experienced as cover for dispossession. Maximum extraction from the victim position.
constraint_indexing:constraint_classification(national_liberation_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: MIZRAHI JEWISH IMMIGRANTS (TANGLED ROPE) — Constrained by both persecution in Arab states and subordinate status within Israeli ethnic hierarchy. Benefit from refuge but bear costs of Ashkenazi cultural dominance and economic marginalization. Genuine coordination (escape from persecution) coexists with asymmetric extraction (second-class citizenship, cultural erasure). Mixed experience.
constraint_indexing:constraint_classification(national_liberation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ZIONIST INSTITUTIONAL LEADERSHIP (ROPE) — Institutional actors with arbitrage-level exit options (international mobility, resource access, diplomatic channels). Experience the constraint as coordination: building state institutions, negotiating international recognition, organizing immigration. Net beneficiaries of the framework with agency to shape outcomes.
constraint_indexing:constraint_classification(national_liberation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TWO-STATE SOLUTION ADVOCATES (SCAFFOLD) — Organized coalitions (Peace Now, J Street, Palestinian moderates) see the national liberation framing as temporary: legitimate in response to persecution but requiring sunset into binational or two-state framework. The justification is the transition (persecution → safety) not the steady state (permanent ethnic sovereignty over contested territory). Sunset logic: once Jewish safety is secured, exclusive sovereignty becomes extractive.
constraint_indexing:constraint_classification(national_liberation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL HUMANITARIAN FRAMEWORK (TANGLED ROPE) — UN agencies, human rights organizations, international law institutions experience mixed coordination and extraction. Genuine coordination function: refugee protection, humanitarian aid, conflict mediation. Asymmetric extraction: complicity in occupation infrastructure, inability to enforce resolutions, legitimation of fait accompli. Constrained by great power politics but institutional actors with some agency.
constraint_indexing:constraint_classification(national_liberation_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/universal perspective, the constraint exhibits both genuine coordination (solving Jewish persecution through collective self-determination) and substantial extraction (Palestinian dispossession, ongoing occupation, suppression of alternatives). The national liberation framing coordinates one population's safety while extracting from another. Requires active enforcement to maintain. Structural tangled rope.
constraint_indexing:constraint_classification(national_liberation_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(national_liberation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(national_liberation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(national_liberation_reading, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): Substantial. The constraint coordinates Jewish refuge and state-building while extracting from Palestinian population through displacement, land confiscation, and ongoing occupation. The extraction has increased over the interval: early Zionist settlement (1920s) had lower extraction (0.35) as land purchases were partly voluntary and displacement limited; 1948 Nakba sharply increased extraction (0.68) through mass displacement; post-1967 occupation and settlement expansion sustained high extraction (0.72-0.78). The base value (0.68) reflects the 1948 inflection point. Suppression (0.75): High. Significant barriers to Palestinian exit or resistance: military control, legal exclusion, denial of return, settlement expansion, checkpoint systems, permit regimes. Suppression increased sharply at 1948 (0.65) with mass displacement and has remained high (0.72-0.75) through occupation period. Early period (1920s) had lower suppression (0.25) when Palestinian majority retained demographic and political agency. Theater ratio (0.42): Moderate. The national liberation framing has genuine functional content (solving Jewish persecution) but also substantial performative elements: historical connection claims that elide 2000-year gap and Palestinian presence; security discourse that justifies indefinite occupation; peace process theater that maintains status quo. Theater has increased over interval as the gap between liberation rhetoric and occupation reality has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. European Jewish refugees in the 1930s-1940s experienced pure coordination (Rope) — Zionism solved an existential collective-action problem with no viable alternatives. Palestinian villagers experienced pure extraction (Snare) — displacement with no exit and no benefit. Mizrahi immigrants experienced mixed coordination and extraction (Tangled Rope) — refuge from persecution coexisting with subordinate status in ethnic hierarchy. Zionist institutional leadership experienced coordination (Rope) — state-building with agency and resources. Two-state advocates see temporary coordination requiring sunset (Scaffold) — legitimate response to persecution that must transition to shared sovereignty. International humanitarian framework experiences mixed coordination and extraction (Tangled Rope) — refugee protection coexisting with complicity in occupation. The analytical observer sees structural tangled rope: genuine coordination function (Jewish safety) requiring active enforcement to maintain extraction (Palestinian dispossession). The gap between beneficiary and victim perspectives is nearly maximal — what one group experiences as survival, the other experiences as catastrophe.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position. European Jewish refugees at crisis point are full beneficiaries (d ≈ 0.0) — the constraint solves their existential problem with negligible cost to them. Palestinian villagers are full targets (d ≈ 1.0) — they bear maximum extraction with no benefit. Mizrahi immigrants have intermediate directionality (d ≈ 0.4) — they benefit from refuge but bear costs of ethnic subordination. Zionist institutional leadership are beneficiaries (d ≈ 0.1) — they capture state-building advantages with arbitrage exit options. Two-state advocates have low directionality (d ≈ 0.3) — they see coordination value but recognize extraction requiring correction. International humanitarian framework has moderate directionality (d ≈ 0.5) — mixed position with both coordination and extraction. The analytical observer computes structural directionality from the beneficiary/victim balance: substantial extraction from Palestinians, substantial benefit to Jewish populations, net tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the national liberation reading is ONE INTERPRETATION of a contested kernel, not an objective description. The reading's claimed type (Rope — pure coordination) diverges from the analytical classification (Tangled Rope — mixed coordination and extraction). This divergence is the measurement: the national liberation framing emphasizes Jewish persecution and historical connection while de-emphasizing Palestinian displacement and ongoing extraction. The framing is not false — Jewish persecution was real, historical connection exists, refuge was necessary — but it is incomplete. The settler-colonial reading (sibling) emphasizes the extraction the national liberation reading minimizes. The religious-restoration reading (sibling) emphasizes theological claims the national liberation reading secularizes. No single reading captures the full structure. The kernel framework models this: multiple readings of the same historical phenomenon, each emphasizing different structural features, each held by different interpretive communities, none achieving consensus. The mandatrophy is resolved by recognizing that 'which type is Zionism?' is an incomplete question — the answer depends on which reading's axioms you adopt, and the readings coexist as live positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persecution_threshold_for_displacement,
    'What threshold of persecution justifies displacing an existing population to secure refuge for a persecuted group?',
    'Comparative analysis of refugee resettlement precedents; international law doctrine on competing rights; historical cases of persecution-driven migration vs. displacement',
    'If threshold is genocide-level: 1930s-1940s migration justified, post-1948 expansion not. If threshold is any systematic discrimination: broader territorial claims justified. If no threshold justifies displacement: entire framework is extractive from Palestinian perspective regardless of Jewish persecution severity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(persecution_threshold_for_displacement, preference, 'Threshold of persecution justifying displacement of existing population').

omega_variable(
    historical_connection_temporal_limit,
    'How long does historical connection to territory remain valid as basis for territorial claims? Does 2000-year diaspora preserve indigenous status?',
    'Cross-cultural comparison of indigenous land claims; international law precedents on historical territorial rights; analysis of other diaspora return movements',
    'If ancient connection remains valid: Zionist territorial claims strengthened. If connection requires continuous presence: Palestinian claims take precedence. If neither historical nor current presence grants exclusive sovereignty: binational framework required.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_connection_temporal_limit, conceptual, 'Temporal limits on historical territorial connection as rights basis').

omega_variable(
    alternative_refuge_counterfactual,
    'Would alternative refuge solutions (territorial autonomy in Europe, resettlement in other regions, international protection guarantees) have solved the persecution problem without displacement?',
    'Historical analysis of proposed alternatives (Uganda Plan, Birobidzhan, post-war European reconstruction); assessment of their feasibility given actual political constraints; comparison with other refugee crises',
    'If alternatives were viable: Palestinian displacement was unnecessary extraction. If alternatives were foreclosed by antisemitism and geopolitics: displacement was tragic necessity. If alternatives were suppressed by Zionist movement: coordination story is cover for ideological preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_refuge_counterfactual, empirical, 'Viability of alternative solutions to Jewish persecution').

omega_variable(
    reading_disambiguation,
    'Is this constraint (national liberation reading) the structurally accurate description of Zionism, or is it one reading of a contested kernel where sibling readings (settler-colonial, religious-restoration) describe the same historical phenomenon with different beneficiary/victim structures?',
    'Cross-reading comparison: Do the readings share the same base extractiveness and suppression values (same constraint, different perspectives) or do they have structurally different ε values (different constraints)? Analysis of which structural features are reading-invariant vs reading-dependent.',
    'If readings share ε: they are perspectives on one constraint, and the kernel framework correctly models the dispute as interpretive. If readings have different ε: they are separate constraints describing different aspects of Zionism, and the kernel should decompose into a constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_disambiguation, conceptual, 'Whether this is one reading of a kernel or a distinct constraint requiring decomposition').

omega_variable(
    axiom_empirical_grounding,
    'Are the foundational axioms of this reading (persecution_justifies_refuge, historical_connection_grants_sovereignty) grounded in empirically falsifiable claims about security and historical continuity, or are they deontological commitments that cannot be refuted by evidence?',
    'Analysis of how proponents respond to empirical challenges (evidence of alternative refuge options, evidence of Palestinian indigeneity, evidence of ongoing extraction). Do they revise claims or reassert axioms? Comparison with other nationalist movements'' responses to empirical challenges.',
    'If empirically grounded: axioms are vulnerable to foreclosure by evidence (e.g., demonstration that refuge was achievable without displacement). If deontological: axioms persist regardless of empirical record, and the reading is immune to evidence-based challenge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_empirical_grounding, conceptual, 'Epistemic status of foundational axioms (empirical vs deontological)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(national_liberation_reading, 0, 106).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(natlib_theater_1920, national_liberation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(natlib_theater_1948, national_liberation_reading, theater_ratio, 28, 0.28).
narrative_ontology:measurement(natlib_theater_1967, national_liberation_reading, theater_ratio, 47, 0.35).
narrative_ontology:measurement(natlib_theater_1993, national_liberation_reading, theater_ratio, 73, 0.38).
narrative_ontology:measurement(natlib_theater_2026, national_liberation_reading, theater_ratio, 106, 0.42).

% Extraction over time
narrative_ontology:measurement(natlib_extract_1920, national_liberation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(natlib_extract_1935, national_liberation_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(natlib_extract_1948, national_liberation_reading, base_extractiveness, 28, 0.68).
narrative_ontology:measurement(natlib_extract_1967, national_liberation_reading, base_extractiveness, 47, 0.72).
narrative_ontology:measurement(natlib_extract_1993, national_liberation_reading, base_extractiveness, 73, 0.75).
narrative_ontology:measurement(natlib_extract_2026, national_liberation_reading, base_extractiveness, 106, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(natlib_suppress_1920, national_liberation_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(natlib_suppress_1948, national_liberation_reading, suppression_requirement, 28, 0.65).
narrative_ontology:measurement(natlib_suppress_1967, national_liberation_reading, suppression_requirement, 47, 0.75).
narrative_ontology:measurement(natlib_suppress_1993, national_liberation_reading, suppression_requirement, 73, 0.72).
narrative_ontology:measurement(natlib_suppress_2026, national_liberation_reading, suppression_requirement, 106, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(national_liberation_reading, identity_coordination).
narrative_ontology:affects_constraint(national_liberation_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(national_liberation_reading, religious_restoration_reading).
narrative_ontology:affects_constraint(national_liberation_reading, palestinian_national_movement).
narrative_ontology:affects_constraint(national_liberation_reading, two_state_framework).
narrative_ontology:affects_constraint(national_liberation_reading, international_recognition_regime).

% DUAL FORMULATION NOTE:
% The national liberation reading is one of three sibling readings of the zionist_legitimacy_basis kernel. Each reading has its own constraint story with its own metrics and perspectives. They are linked via network.affects_constraints to model how interpretive communities influence each other's legitimacy claims. The readings do not decompose into separate observables (they describe the same historical phenomenon) but they do instantiate different constraints (different beneficiary/victim structures, different axioms, different ε values).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
