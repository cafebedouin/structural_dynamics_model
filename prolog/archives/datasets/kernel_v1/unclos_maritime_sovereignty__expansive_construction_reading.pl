% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__expansive_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__expansive_construction_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__expansive_construction_reading
 *   human_readable: UNCLOS Maritime Sovereignty: Expansive Construction Reading
 *   domain: international_law/maritime_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the EXPANSIVE CONSTRUCTION READING of
 *   the UNCLOS maritime sovereignty kernel. Under this reading, artificial
 *   islands, structures, or substantially modified features at submerged
 *   locations or low-tide elevations, if effectively occupied and
 *   administratively controlled, generate de facto territorial seas (12
 *   nautical miles) or broader jurisdictional zones. This reading licenses
 *   the material transformation of seabed geography as a mechanism for
 *   expanding state territory and exclusive resource control. The constraint
 *   exhibits tangled_rope character because it serves a coordination function
 *   (resolving ambiguity about submerged feature status) while simultaneously
 *   concentrating extraction benefits in states with construction capacity
 *   and military enforcement capability. The extractiveness value (0.58)
 *   reflects steady growth over the measurement interval as construction
 *   projects accumulate and become precedent. The theater ratio (0.61)
 *   reflects increasing performative reinterpretation of UNCLOS text to
 *   justify outcomes; suppression requirement (0.68) reflects the military
 *   enforcement infrastructure necessary to sustain sovereignty claims
 *   against contested neighbors. This reading is one of three competing
 *   interpretations of the UNCLOS kernel, each generating structurally
 *   distinct constraints with different beneficiary/victim asymmetries.
 *
 * KEY AGENTS:
 *   - Island-Constructing States (institutional/arbitrage): Primary beneficiary — converts marine resources and geopolitical position into expanded maritime territory; captures fishing grounds and resource claims
 *   - Freedom-of-Navigation States (powerless/trapped): Primary victim — loses transit rights and high-seas freedoms; no exit mechanism except acquiescence or costly military confrontation
 *   - Neighboring Maritime Claimants (moderate/constrained): Secondary victim — reduced access to disputed fishing grounds; constrained by military asymmetry and limited international legal remedy
 *   - Fishing-Rights-Dependent Communities (powerless/trapped): Tertiary victim — access to traditional fishing grounds disappears through jurisdictional transformation; no compensation mechanism
 *   - International Law Reform Coalition (organized/constrained): Organized opposition to the reading; advocates strict geographic interpretation; constrained by diplomatic cost of opposing incumbent beneficiaries
 *   - UNCLOS Adjudication Mechanism (institutional/constrained): Institutional actor developing case law limiting scope of feature-to-jurisdiction conversion; sees the expansive reading as temporary configuration undergoing refinement
 *   - Analytical Observer (analytical/analytical): Textualist reading noting that Article 60 explicitly excludes artificial islands from generating territorial seas; perspective at risk of naturalizing institutional noncompliance as legal principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.58).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.68).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "UNCLOS Maritime Sovereignty: Expansive Construction Reading").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international_law/maritime_governance").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, '6d9606e4-8e6f-412c-a604-9263fd521fd5').
narrative_ontology:cs_kernel_codification('6d9606e4-8e6f-412c-a604-9263fd521fd5', formalized).
narrative_ontology:cs_authority_grounding('6d9606e4-8e6f-412c-a604-9263fd521fd5', extraction).
narrative_ontology:cs_interpretation_layer_present('6d9606e4-8e6f-412c-a604-9263fd521fd5').
narrative_ontology:cs_reading_relation('6d9606e4-8e6f-412c-a604-9263fd521fd5', unclos_maritime_sovereignty__strict_geographic_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d9606e4-8e6f-412c-a604-9263fd521fd5', unclos_maritime_sovereignty__hybrid_effective_control_reading, influences).
narrative_ontology:cs_axiom('6d9606e4-8e6f-412c-a604-9263fd521fd5', foundational, artificial_features_generate_maritime_zones).
narrative_ontology:cs_axiom_status(artificial_features_generate_maritime_zones, holdable).
narrative_ontology:cs_axiom_grounding('6d9606e4-8e6f-412c-a604-9263fd521fd5', artificial_features_generate_maritime_zones, deontological).
narrative_ontology:cs_axiom('6d9606e4-8e6f-412c-a604-9263fd521fd5', foundational, effective_occupation_suffices_for_sovereignty).
narrative_ontology:cs_axiom_status(effective_occupation_suffices_for_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('6d9606e4-8e6f-412c-a604-9263fd521fd5', effective_occupation_suffices_for_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('6d9606e4-8e6f-412c-a604-9263fd521fd5', article_60_expansive_interpretation).
narrative_ontology:cs_drift_state('6d9606e4-8e6f-412c-a604-9263fd521fd5', post_nine_dash_line_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6d9606e4-8e6f-412c-a604-9263fd521fd5', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, geopolitical_hegemon).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_maritime_claimants).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, fishing_rights_dependent_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FREEDOM-OF-NAVIGATION STATES (SNARE) — Trapped in a fait accompli system where constructed features retroactively generate maritime jurisdiction. No exit mechanism; choice is to recognize new sovereignty or engage in costly confrontation. High suppression through military presence, administrative control, and international forum stalling tactics. Pure extraction — navigation rights disappear without compensation.
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__expansive_construction_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NEIGHBORING MARITIME CLAIMANTS (SNARE) — Constrained by military asymmetry and limited international legal remedy. Constructed features in contested zones reduce their accessible fishing grounds and resource claims. Suppression through military deterrence and de facto control enforcement. High extraction of maritime rights without legal process or consent.
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__expansive_construction_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERNATIONAL LAW REFORM COALITION (TANGLED ROPE) — Organized states and NGOs advocating strict geographic interpretation see genuine coordination function (UNCLOS legal stability) alongside asymmetric extraction (expansive reading benefits military powers). Constrained by diplomatic cost of opposing incumbent beneficiaries. Can organize but faces powerful counterparties. Mixed experience of coordination benefit and extracted jurisdiction.
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__expansive_construction_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ISLAND-CONSTRUCTING STATE (ROPE) — Institutional beneficiary with arbitrage flexibility. Experiences the constraint as pure coordination: converting submerged features into settlements enables legitimate jurisdiction claims under this reading. Net beneficiary position. Can execute construction projects and reap sovereignty expansion through control enforcement. Low suppression requirement because sovereignty expansion is treated as lawful administrative action.
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__expansive_construction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: UNCLOS ADJUDICATION MECHANISM (SCAFFOLD) — Institutional actor (ITLOS, arbitral tribunals) sees the expansive construction reading as a temporary institutional configuration. The adjudication mechanism has a built-in sunset: as case law accumulates through UNCLOS dispute mechanisms, precedent-based limitations are emerging. Tribunal rulings increasingly narrow the scope of feature-to-jurisdiction conversion. Low theater because the adjudication process has genuine authority-settling function, even when contested.
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__expansive_construction_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / UNCLOS TEXTUALISM (PITON) — Textualist reading of UNCLOS Article 60 holds that artificial islands are explicitly excluded from generating territorial seas; the expansive reading is theater applied over a clear textual prohibition. The constraint persists through institutional inertia (states ignore the article's plain language) rather than functional necessity. Theater ratio 0.61 reflects the performative reinterpretation of legal text to justify geopolitical outcomes. This perspective risks naturalizing as a legal limit what is actually a failure of textual compliance enforcement.
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__expansive_construction_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unclos_maritime_sovereignty__expansive_construction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unclos_maritime_sovereignty__expansive_construction_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, TR),
    TR >= 0.70.

:- end_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The reading permits states to unilaterally transform submerged features into sovereign territory, extracting fishing rights, resource claims, and geopolitical position from neighboring states and freedom-of-navigation states without consent or compensation. The extractiveness is not maximal (0.70+) because some construction activities have coordination value — they resolve genuine ambiguity about seabed feature status — and because international legal contestation creates uncertainty about the reading's durability. Rising trajectory (0.35 → 0.50 → 0.58) reflects accumulated construction projects becoming precedent, reducing ambiguity and increasing extractive capacity. Suppression (0.68): High and rising. The constraint requires military presence, administrative control, and international forum management to suppress challenger claims. Rising trajectory (0.52 → 0.61 → 0.68) reflects escalating enforcement infrastructure as neighboring states resist sovereignty claims — surveillance systems, coast guard presence, diplomatic friction-raising. Theater ratio (0.61): Moderate-high. The reading rests on reinterpretation of UNCLOS Article 60, which explicitly states that artificial islands do not generate territorial seas or EEZ. The performative component reflects the gap between textual prohibition and claimed practice. The theater is not maximal (0.70+) because construction itself has material reality — features exist, are inhabited or militarized — creating a degree of facticity beyond pure performance. Rising trajectory (0.48 → 0.55 → 0.61) reflects increasing reliance on interpretive framing as construction intensity rises and textual contradiction becomes harder to ignore.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. Island-constructing states experience rope — the reading solves the coordination problem of feature status ambiguity, enabling lawful expansion. Freedom-of-navigation states experience snare — pure extraction with no coordination benefit, no exit option. Neighboring claimants experience snare from immediate perspective (trapped by military asymmetry) but scaffold from generational perspective (case law is accumulating limits to the reading). The reform coalition experiences tangled_rope — genuine coordination value (UNCLOS stability) but also extraction (the reading asymmetrically concentrates benefits). ITLOS sees scaffold — the constraint is temporary, case law is eroding it. The analytical observer risks piton (naturalizing contested interpretation as legal truth). This perspectival divergence is diagnostic: it reveals that the constraint's classification depends entirely on which agents' structural position you measure from. No single type is objective; the presheaf of perspectives IS the answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from power level, exit options, and structural relationship to the extraction flow. Island-constructing states are institutional beneficiaries with arbitrage options (multiple paths to expansion, choice of construction locations) → low d → negative χ → rope classification. Freedom-of-navigation states are powerless with trapped exit (no choice but to accept loss of transit rights or face military confrontation) → high d → high χ → snare. Neighboring claimants are moderate power with constrained exit (can protest diplomatically, cannot exit the region) → moderate d → moderate χ → snare at biographical, tangled_rope at generational. The organized reform coalition has agency (can coordinate protest, adjudication advocacy) but faces powerful counterparties → constrained exit → moderate d. UNCLOS adjudication mechanism has arbitrage flexibility (can develop jurisprudence in multiple directions) but institutionally committed to treaty text → constrained exit → moderate d. The analytical observer is positioned to see the entire structure but risks naturalizing the reading as law rather than contestation → analytical position captures the oracle gap (Theorem 4) where the observer's native instruments are captured by the reading they are meant to evaluate.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by making explicit that the UNCLOS maritime sovereignty kernel is not a single constraint but a presheaf of competing readings. The expansive construction reading generates tangled_rope + snare + rope + scaffold + piton + (false summit mountain) depending on which agent and time horizon you measure from. The mandatrophy question 'which type is it really?' has no single answer because the constraint is constituted through competing interpretive frameworks. The resolution is not to choose one type but to acknowledge that the constraint's classification IS the distribution of perspectives. The expansive reading itself (ε=0.58) is one point in the structure; the sibling readings (strict geographic, hybrid effective control) are other points. Each reading has its own ε, its own beneficiary/victim asymmetry, and its own classification distribution. The analytical observer's false-summit perspective (piton trying to naturalize as mountain) is exactly the diagnostic signal that the reading lacks sufficient legal grounding to count as natural law — if UNCLOS Article 60 explicitly excludes artificial islands, the reading is theater, not law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    artificial_versus_natural_distinction,
    'Does the expansive reading collapse the artificial/natural feature distinction embedded in UNCLOS Articles 60 and 121, or is the distinction itself ambiguous in borderline cases (cays, sand spits, dredged features)?',
    'Systematic analysis of contested features (Nine-Dash Line features, Maldivian artificial islands, Dutch polder expansions) and their UNCLOS classification. Determination of whether feature genesis or current materiality matters more.',
    'If distinction collapses: any occupied feature can generate territorial sea (maximally expansive). If distinction holds: only naturally occurring features can do so, and construction is merely ''building on existing rock'' — dramatically constrains the reading. Classification would shift from tangled_rope toward snare (pure extraction without coordination value).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artificial_versus_natural_distinction, conceptual, 'Whether artificial features can legitimately generate territorial seas under UNCLOS').

omega_variable(
    effective_occupation_threshold,
    'What threshold of administrative control, habitation, or economic activity counts as ''effective occupation'' sufficient to generate maritime jurisdiction under this reading?',
    'Case law analysis from UNCLOS tribunals (Philippines v. China precedent, ongoing ITLOS cases). Identification of minimally acceptable occupation levels. Comparison with terra nullius doctrine standards.',
    'If threshold is low (presence of military installations or administrative posts suffices): maximally expansive, incentivizes rapid militarization (snare classification from all victim perspectives). If threshold is high (requires permanent civil habitation and economic self-sufficiency): constrains construction strategies, reduces extractive capacity (rope or tangled_rope classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effective_occupation_threshold, empirical, 'Threshold for ''effective occupation'' generating jurisdiction').

omega_variable(
    contravention_of_article_60_plain_text,
    'Is this reading a legitimate interpretive expansion of UNCLOS or a violation of Article 60''s explicit statement that artificial islands do not generate territorial seas or an exclusive economic zone?',
    'Comparative jurisprudence: how do international courts distinguish between permitted ''interpretation in light of applicable rules of international law'' (Vienna Convention Article 31) versus ''rewriting the treaty''? Examination of whether the expansive reading can be reconciled with Article 60''s plain language.',
    'If violation: the reading lacks juridical foundation; classification should be snare (pure extraction) or piton (performative theater over defunct rules). If legitimate interpretation: the reading has legal grounding and should remain tangled_rope (mixed coordination and extraction). This omega directly maps to the piton perspective''s concern that the constraint is theater applied to textually clear prohibition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contravention_of_article_60_plain_text, conceptual, 'Whether expansive construction reading violates UNCLOS Article 60 plain text').

omega_variable(
    geopolitical_incentive_structure,
    'Does the expansive reading benefit all states equally or concentrate extraction capacity in powers with construction resources and military capability to enforce sovereignty claims?',
    'Institutional analysis: which states have actually conducted large-scale maritime feature construction and successfully asserted jurisdiction? Which states lack resources or strategic incentive to do so? Distribution of construction-based jurisdiction claims across power hierarchy.',
    'If benefit is asymmetric (concentrated in hegemons): reading is structurally extractive, beneficiaries are concentrated, classification remains snare/tangled_rope from victim perspective. If benefits are available to all states equally: reading is genuinely coordination-enabling, and classification might shift toward rope from some perspectives. Current evidence strongly suggests asymmetric benefit — only a few military powers have conducted construction at scale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geopolitical_incentive_structure, empirical, 'Whether expansive reading benefits all states or concentrates in hegemons').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_exp_theater_t0, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(unclos_exp_theater_t5, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(unclos_exp_theater_t10, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(unclos_exp_basex_t0, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(unclos_exp_basex_t5, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(unclos_exp_basex_t10, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(unclos_exp_supp_t0, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(unclos_exp_supp_t5, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(unclos_exp_supp_t10, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__expansive_construction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__expansive_construction_reading, 0.18).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, south_china_sea_de_facto_sovereignty).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, pacific_island_exclusive_economic_zone_expansion).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, maldivian_sea_level_rise_artificial_island_strategy).

% DUAL FORMULATION NOTE:
% The UNCLOS maritime sovereignty kernel decomposes into three structurally distinct constraints, each with its own ε value and beneficiary/victim asymmetry. The expansive reading (this file) has ε=0.58 and benefits island-constructing hegemons at the cost of freedom-of-navigation states. The strict geographic reading has ε=0.08 (minimal extraction, pure legal clarity) and distributes benefits evenly. The hybrid reading has ε=0.35 (moderate tangled rope) and creates incentives for genuine settlement over military occupation. These are not different measurements of one constraint — they are different constraints from different readings. Family links: expansive → strict (coexists), expansive → hybrid (influences). See constraint family documentation for network topology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
