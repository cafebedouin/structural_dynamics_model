% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__land_promise_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__land_promise_constraint, []).

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
 *   constraint_id: abrahamic_covenant__land_promise_constraint
 *   human_readable: Abrahamic Covenant Land Promise: Territorial Legitimacy and Displacement
 *   domain: religious_studies/institutional_authority/geopolitical_conflict
 *
 * SUMMARY:
 *   The Abrahamic covenant land promise represents one reading of a contested
 *   kernel: a sacred text (Genesis 12:7, 13:15, 15:18, 17:8; Deuteronomy 1:8,
 *   6:10) claiming God granted specific territory to Abraham's descendants in
 *   perpetuity. This reading instantiates a constraint by making territorial
 *   legitimacy dependent on covenantal authority and by suppressing
 *   alternative readings (Ishmael covenant, conditional interpretation,
 *   fulfilled-not-ongoing framing) that would distribute territorial claims
 *   differently. The constraint exhibits high extractiveness (0.68) because
 *   it produces material consequences: institutional beneficiaries (Israeli
 *   state apparatus) leverage covenant reading to justify territorial
 *   expansion and settlement; institutional victims (Palestinian populations,
 *   alternative theological readings) face suppression, displacement, and
 *   institutional marginalization. The suppression requirement has risen from
 *   0.45 (1948, immediate post-independence) to 0.72 (2025) as the constraint
 *   requires increasing military, legal, and institutional enforcement to
 *   maintain territorial claims against competing readings and populations.
 *   Theater ratio (0.58) reflects that theological justifications are
 *   increasingly performative in international discourse — actual enforcement
 *   is military and legal, not scriptural — yet covenant framing persists in
 *   legitimation narratives within certain institutional and constituency
 *   domains.
 *
 * KEY AGENTS:
 *   - Israeli State Apparatus: Primary beneficiary (institutional/arbitrage) — leverages covenant reading for territorial legitimacy, international support, and settlement expansion; experiences constraint as Rope (coordination between institutional legitimacy and territorial sovereignty)
 *   - Palestinian Populations: Primary victim (powerless/trapped) — face displacement, settlement expansion, military enforcement, and institutional suppression of alternative territorial readings; generational timeframe shows no exit option
 *   - Palestinian Authority/Alternative Covenant Readings: Secondary victim (moderate/constrained) — institutional voice for competing readings (Ishmael covenant, conditional interpretation) but faces suppression through power asymmetries; constrained by territorial diminishment and international pressure to recognize dominant reading
 *   - International Legal Frameworks: Moderately powerful institutional actor (powerful/constrained) — contain embedded tension between Israeli territorial claims and Palestinian self-determination; suppression moderate-high through selective enforcement favoring more powerful state actors
 *   - Religious Institutional Frameworks: Institutional actors maintaining interpretation authority (institutional/arbitrage) — Jewish, Christian, Islamic theological establishments that transmit and adjudicate covenant readings; increasingly performative as actual enforcement is military/legal rather than theological
 *   - Analytical Observer: Risks naturalizing covenant reading as immutable theology rather than recognizing it as contingent institutional arrangement benefiting identifiable agents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, 0.68).
domain_priors:suppression_score(abrahamic_covenant__land_promise_constraint, 0.72).
domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, extractiveness, 0.68).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__land_promise_constraint, snare).
narrative_ontology:human_readable(abrahamic_covenant__land_promise_constraint, "Abrahamic Covenant Land Promise: Territorial Legitimacy and Displacement").
narrative_ontology:topic_domain(abrahamic_covenant__land_promise_constraint, "religious_studies/institutional_authority/geopolitical_conflict").

domain_priors:requires_active_enforcement(abrahamic_covenant__land_promise_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__land_promise_constraint, '8022bf83-9193-4771-9ed1-1c0037f80725').
narrative_ontology:cs_kernel_codification('8022bf83-9193-4771-9ed1-1c0037f80725', fixed_text).
narrative_ontology:cs_authority_grounding('8022bf83-9193-4771-9ed1-1c0037f80725', extraction).
narrative_ontology:cs_interpretation_layer_present('8022bf83-9193-4771-9ed1-1c0037f80725').
narrative_ontology:cs_reading_relation('8022bf83-9193-4771-9ed1-1c0037f80725', isaac_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('8022bf83-9193-4771-9ed1-1c0037f80725', ishmael_covenant_reading, forecloses).
narrative_ontology:cs_axiom('8022bf83-9193-4771-9ed1-1c0037f80725', foundational, perpetual_territorial_grant_to_abrahams_lineage).
narrative_ontology:cs_axiom_status(perpetual_territorial_grant_to_abrahams_lineage, holdable).
narrative_ontology:cs_axiom_grounding('8022bf83-9193-4771-9ed1-1c0037f80725', perpetual_territorial_grant_to_abrahams_lineage, deontological).
narrative_ontology:cs_axiom('8022bf83-9193-4771-9ed1-1c0037f80725', secondary, isaac_line_sole_covenantal_heir).
narrative_ontology:cs_axiom_status(isaac_line_sole_covenantal_heir, holdable).
narrative_ontology:cs_axiom_grounding('8022bf83-9193-4771-9ed1-1c0037f80725', isaac_line_sole_covenantal_heir, deontological).
narrative_ontology:cs_reference_frame('8022bf83-9193-4771-9ed1-1c0037f80725', covenantal_perpetuity_and_isaac_succession).
narrative_ontology:cs_drift_state('8022bf83-9193-4771-9ed1-1c0037f80725', contemporary_secular_state_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8022bf83-9193-4771-9ed1-1c0037f80725', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(abrahamic_covenant__land_promise_constraint, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, zionist_institutional_frameworks).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, palestinian_populations).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, dispossessed_territorial_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, covenantal_alternative_readings).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED PALESTINIAN COMMUNITIES (SNARE) — Structurally trapped by settlement expansion, military enforcement, and legal frameworks that anchor territorial claims in covenant readings. No exit option from the territorial constraint; bears extraction through displacement, resource denial, and legitimation of occupancy through scriptural authority. Maximum experienced extraction — powerless populations with generational time horizon show no mobility.
constraint_indexing:constraint_classification(abrahamic_covenant__land_promise_constraint, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PALESTINIAN STATE AUTHORITY / ALTERNATIVE READINGS (SNARE) — Constrained by territorial diminishment and institutional pressure to recognize Israeli territorial claims grounded in covenant reading. Faces high suppression through military force, legal subordination, and institutional capture by dominant covenant reading. Attempts to maintain competing readings (Ishmael covenant, conditional promise, fulfilled-not-ongoing framings) but these are institutionally marginalized. High extraction without comparable beneficiary position.
constraint_indexing:constraint_classification(abrahamic_covenant__land_promise_constraint, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ISRAELI STATE APPARATUS (ROPE) — Primary beneficiary. Experiences the covenant land promise reading as a pure coordination mechanism: the institutional structure (state legitimacy, territorial sovereignty, population settlement) aligns with and is reinforced by covenant framing. Net beneficiary with arbitrage options — can shift emphasis between secular and religious legitimacy claims, can leverage the covenant reading for international legitimacy within certain constituencies, and faces minimal suppression of this particular reading within institutional control.
constraint_indexing:constraint_classification(abrahamic_covenant__land_promise_constraint, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL LEGAL/DIPLOMATIC FRAMEWORKS (TANGLED ROPE) — Constrained by need to acknowledge both Israeli sovereignty claims and Palestinian self-determination principles. International law contains genuine coordination function (UN resolutions, human rights frameworks) but also embedded extraction favoring more powerful institutional actors who can leverage covenant reading within dominant Western institutional frameworks. Suppression is moderate-high through diplomatic pressure and selective enforcement of competing norms.
constraint_indexing:constraint_classification(abrahamic_covenant__land_promise_constraint, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: RELIGIOUS INSTITUTIONAL FRAMEWORKS (PITON) — Maintains covenant reading interpretation systems largely through institutional inertia and theological authority claims. Theater ratio reflects that theological justifications for territorial claims are increasingly performative in contemporary discourse — the actual enforcement mechanism is state military and legal apparatus, not covenantal theology. Yet religious frameworks persist in legitimation narratives, sustained by their institutional roles despite reduced functional verification of theological claims.
constraint_indexing:constraint_classification(abrahamic_covenant__land_promise_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — Risks naturalizing the covenant reading as immutable: 'Sacred texts always ground territorial claims,' 'Religious nationalism is inherent to covenant theology,' 'Territorial disputes over promised lands are unavoidable in Abrahamic traditions.' This perspective treats the constraint as a natural law of textual authority and collective identity. However, the structural data reveals this as a false summit: identifiable institutional beneficiaries (Israeli state apparatus), victims (displaced Palestinians), and enforced suppression mechanisms (military, legal) indicate contingent institutional arrangements, not immutable theological law.
constraint_indexing:constraint_classification(abrahamic_covenant__land_promise_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__land_promise_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(abrahamic_covenant__land_promise_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(abrahamic_covenant__land_promise_constraint, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, TR),
    TR >= 0.70.

:- end_tests(abrahamic_covenant__land_promise_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High-severe. The covenant land promise reading produces direct extraction from Palestinians through displacement, resource denial, and territorial subordination. The extraction is severe rather than moderate because the mechanism (covenantal authority) makes alternative territorial claims illegitimate within institutional frameworks controlled by beneficiaries. The reading does not present itself as extraction — it frames territorial control as fulfillment of divine promise — which is the covering mechanism for snare-type extraction. Suppression (0.72): High. Rising from 0.45 to 0.72 over 30 years indicates enforcement intensification: settlements require military protection, legal frameworks must suppress competing territorial claims, institutional resistance to alternative readings must be maintained through educational systems and media control. Theater ratio (0.58): Moderate-high. Covenant theological justification is increasingly performative in contemporary international discourse, where territorial claims are actually enforced through military, legal, and economic mechanisms rather than scriptural persuasion. Within certain constituencies (religious settlers, some evangelical Christian communities), the theology has more functional force; globally, it operates more as legitimation narrative than as persuasive justification. The theater has risen from 0.42 to 0.58 as actual enforcement mechanisms have shifted from settlement-rhetoric persuasion to institutional coercion.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. The beneficiary (Israeli state) experiences it as Rope — pure coordination of institutional legitimacy with territorial sovereignty, no experienced extraction, arbitrage options to shift between religious and secular justification. The powerless victims experience it as Snare — no exit, maximum extraction through displacement. The moderate victims (alternative readings, Palestinian authority) experience it as Snare — high extraction without comparative beneficiary position. International frameworks experience it as Tangled Rope — genuine coordination function (stabilizing territorial governance) but asymmetric extraction (favors powerful state actors). Religious institutions experience it as Piton — maintaining interpretive authority through inertia while actual enforcement is military/legal. The analytical observer risks experiencing it as Mountain — naturalizing covenant reading as inherent to Abrahamic theology rather than recognizing it as contingent institutional selection benefiting specific agents. The perspectival divergence is not merely observational variation but reflects real power asymmetries in who can instantiate which reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's effective extractiveness (chi) derives from beneficiary/victim status and exit options via the formula χ = ε × f(d) × σ(S). The Israeli state apparatus has low d (beneficiary + arbitrage exit) yielding negative or near-zero f(d) — they experience the constraint as enabling rather than extractive. Displaced Palestinians have high d (victim + trapped exit) yielding maximum f(d) ≈ 1.42 — they experience the full base extractiveness χ ≈ 0.68 × 1.42 × 1.0 ≈ 0.96. Alternative institutional readings have moderate-high d (victim/constrained + moderate power) yielding elevated f(d) ≈ 1.0 — they experience χ ≈ 0.68 × 1.0 × 1.0 ≈ 0.68. International frameworks have mixed beneficiary/victim status with some arbitrage, moderate suppression. The regional scope (σ ≈ 0.9) slightly dampens chi relative to global scope, reflecting that the constraint's enforcement is regionally concentrated despite aspirations to universal theological authority. Override note: The Israeli state institutional actor appears as both beneficiary and architect of enforcement, which structurally positions it as extracting the constraint rather than merely benefiting from it — this is reflected in the 'requires_active_enforcement' flag and the snare/tangled_rope classifications from victim perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandate-atrophy paradox by showing how a legitimation narrative (covenant promise) becomes increasingly performative while enforcement mechanisms (military, legal, settlement) become materially dominant. Initial mandatrophy (1948): covenant reading provided genuine coordination function — legitimating new state to populations with historical/religious connection to territory. Current mandatrophy (2025): covenant reading is increasingly theater — actual enforcement is through state institutions, military occupation, legal frameworks, and settlement expansion, not through theological persuasion. Theater ratio rise (0.42 → 0.58) documents this drift. However, mandatrophy is NOT resolution — the performative legitimation narrative persists because it serves institutional interests. For Israeli state apparatus, the covenant reading remains functionally important for international constituency support (evangelical Christians, religious settlers) and domestic legitimacy framing, even though the actual territorial control rests on military/legal mechanisms. For Palestinians, the suppression of alternative readings persists despite its performative character because the institutional framework controlling interpretation (Israeli state, Western-aligned international institutions) has material power to enforce the preferred reading. Mandatrophy is resolved by recognizing that legitimation narratives persist not because they are functional explanations for territorial control but because they serve institutional extraction interests — the theater itself IS the extraction mechanism (delegitimizing alternative claims makes displacement appear legitimate rather than coercive).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_conditionality_reading,
    'Is the covenant promise conditional (dependent on Israel''s adherence to covenant obligations) or unconditional and permanent?',
    'Textual exegesis of Genesis, Deuteronomy, and Prophetic literature; comparison of Tanakh passages treating covenant fulfillment conditions; historical analysis of how Jewish and Christian hermeneutics have treated covenant revocation or suspension',
    'If conditional: covenant reading permits revocation narratives and legitimates Palestinian alternative readings. If unconditional: covenant reading forecloses competing territorial claims and supports Israeli institutional position. If ambiguous: resolution authority becomes determinative — whichever institutional framework controls interpretation gains extraction power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(covenant_conditionality_reading, conceptual, 'Whether covenant promise is conditional or unconditional').

omega_variable(
    covenant_fulfillment_temporality,
    'Has the covenant promise been fulfilled in historical return from exile, or is it ongoing with continuous territorial requirement?',
    'Historical survey of Jewish and Christian theological treatment of exile-and-return narrative; identification of which historical moments have been claimed as fulfillment; analysis of whether Second Temple restoration, Diaspora integration, or modern state formation are treated as fulfillment terminus or as intermediate stages',
    'If fulfilled in past: territorial claim becomes historical or theological rather than ongoing legal claim; supports alternative readings and limits extraction. If ongoing: territorial claim persists as live obligation; supports maximalist covenant reading and Israeli expansion narratives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_fulfillment_temporality, conceptual, 'Whether covenant fulfillment is complete or ongoing').

omega_variable(
    ishmael_sibling_reading_suppression,
    'Why is the Ishmael covenant reading (Genesis 17:20, 21:13-21) institutionally marginalized despite being textually equal in covenantal status?',
    'Historical analysis of Islamic jurisprudence on Ishmael covenant; examination of how Jewish and Christian hermeneutics have treated Ishmael versus Isaac covenants; institutional analysis of power asymmetries in whose covenant reading gets weaponized for territorial claims',
    'If suppression is textual/theological: some readings are genuinely weaker by exegetical standards. If suppression is institutional/political: it reveals that covenant reading selection is driven by power rather than theological merit, and omega_variable becomes evidence that the constraint is Snare extraction rather than natural theological law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ishmael_sibling_reading_suppression, empirical, 'Institutional suppression of Ishmael covenant reading relative to Isaac reading').

omega_variable(
    secular_state_covenant_decoupling,
    'To what extent does Israeli territorial claim actually depend on covenant legitimacy versus secular international law and historical presence arguments? Can the institutional extraction persist if covenant reading is abandoned?',
    'Analysis of Israeli institutional rhetoric: proportion of territorial claims justified via covenant versus secular precedent; examination of whether removal of religious covenant language from state justifications would materially weaken territorial position internationally; counterfactual: would Israeli state still claim same territories on purely secular grounds?',
    'If covenant reading is merely rhetorical overlay: constraint is institutional power (Rope or Tangled Rope) masquerading as theological natural law. If covenant reading is functionally necessary: it is core extraction mechanism and constraint is genuinely dependent on theology-to-power pipeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_state_covenant_decoupling, empirical, 'Degree of institutional dependence on covenant legitimacy versus secular justifications').

omega_variable(
    natural_law_false_summit_marker,
    'Is this constraint a natural law (immutable pattern of how sacred texts ground territorial claims) or a constructed institutional arrangement that benefits identifiable agents?',
    'Comparison with other covenant-based territorial disputes (Māori lands in New Zealand Waitangi Treaty, Native American treaty rights in North America, Hindu nationalist claims to temple sites): Are these structurally identical to Abrahamic covenant constraint, or does institutional variation suggest contingency? If contingent, what institutional choices would decompose the constraint?',
    'If natural law: classification as Mountain is correct; territorial claims are inherent to Abrahamic theology. If constructed: constraint is False Summit; beneficiary-driven institutional selection of which covenant reading to enforce reveals it as Snare with theological legitimation theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_false_summit_marker, conceptual, 'Whether constraint is natural theological law or constructed institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__land_promise_constraint, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(covenant_land_theater_1948, abrahamic_covenant__land_promise_constraint, theater_ratio, 0, 0.42).
narrative_ontology:measurement(covenant_land_theater_1978, abrahamic_covenant__land_promise_constraint, theater_ratio, 10, 0.48).
narrative_ontology:measurement(covenant_land_theater_2000, abrahamic_covenant__land_promise_constraint, theater_ratio, 20, 0.55).
narrative_ontology:measurement(covenant_land_theater_2025, abrahamic_covenant__land_promise_constraint, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(covenant_land_extractiveness_1948, abrahamic_covenant__land_promise_constraint, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(covenant_land_extractiveness_1978, abrahamic_covenant__land_promise_constraint, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(covenant_land_extractiveness_2000, abrahamic_covenant__land_promise_constraint, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(covenant_land_extractiveness_2025, abrahamic_covenant__land_promise_constraint, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(covenant_land_suppression_1948, abrahamic_covenant__land_promise_constraint, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(covenant_land_suppression_1982, abrahamic_covenant__land_promise_constraint, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(covenant_land_suppression_2025, abrahamic_covenant__land_promise_constraint, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__land_promise_constraint, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__land_promise_constraint, 0.15).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, palestinian_territorial_claim_instantiation).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, settlement_expansion_constraint).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, west_bank_legal_framework_constraint).

% DUAL FORMULATION NOTE:
% The land promise constraint is one reading within a constraint family: ABRAHAMIC_COVENANT kernel generates at minimum LAND_PROMISE_CONSTRAINT (this story), ISAAC_COVENANT_READING (emphasizing heir designation), and ISHMAEL_COVENANT_READING (emphasizing alternative lineage). Each instantiates different ε values and victim/beneficiary structures. LAND_PROMISE_CONSTRAINT (ε=0.68, Snare) feeds upstream into downstream constraints about settlement expansion and legal frameworks that enforce the territorial claim. Sibling readings (Isaac, Ishmael) have different ε values reflecting different institutional suppression levels and beneficiary positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abrahamic_covenant__land_promise_constraint, institutional, 0.15).
constraint_indexing:directionality_override(abrahamic_covenant__land_promise_constraint, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
