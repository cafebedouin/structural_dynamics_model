% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__palestinian_autochthony_reading, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_legitimacy_dual__palestinian_autochthony_reading
 *   human_readable: Palestinian Legitimacy and Right of Return (Autochthony Reading)
 *   domain: political_theory/territorial_sovereignty/international_relations
 *
 * SUMMARY:
 *   The Palestinian autochthony reading asserts territorial legitimacy
 *   through continuous habitation predating Zionist settlement, cultural and
 *   demographic continuity (despite displacement), and the status of 1948 as
 *   an ongoing injustice (not a historical event to be accepted). The 1948
 *   displacement, in this reading, is not a tragedy to be compensated but an
 *   unresolved crime requiring restoration: the right of return is
 *   non-negotiable because the dispossession itself is non-negotiable. The
 *   constraint emerges from the structural tension between this legitimacy
 *   claim and the Israeli state's security and demographic requirements,
 *   which depend on excluding or severely restricting Palestinian return.
 *   From the powerless Palestinian refugee perspective, the constraint is a
 *   snare: territorial dispossession enforced through military, legal, and
 *   demographic barriers with no exit. From the organized Palestinian civil
 *   society perspective, it is tangled rope: genuine coordination functions
 *   (preserving identity, building institutions) alongside asymmetric
 *   extraction (movement restrictions, political subordination). From the
 *   Israeli institutional perspective, the constraint is perceived as rope or
 *   scaffold (a coordination mechanism for security and state stability), but
 *   this classification inverts the extraction direction — what the powerful
 *   agent experiences as necessary coordination is the snare mechanism
 *   experienced by the powerless. The analytical observer risks naturalizing
 *   the constraint as an immutable feature of territorial state formation
 *   (all modern states rest on historical displacement) — a false summit that
 *   masks what is a contingent institutional arrangement backed by ongoing
 *   military and legal enforcement.
 *
 * KEY AGENTS:
 *   - Palestinian Refugee Diaspora: Primary victim (powerless/trapped) — descendants of 1948 displaced persons; bear full weight of dispossession; no material path to return; generational trauma; trapped by legal barriers and military power
 *   - Palestinian Population in Occupied Territories: Primary victim (powerless/trapped) — subject to military occupation, administrative detention, movement restrictions, property confiscation; structural exclusion from political authority and resource access
 *   - Dispossessed Land Claimants: Primary victim (moderate/constrained) — Palestinians with documented property claims in 1948 boundaries; face legal barriers to restitution; constrained by property law complexity and Israeli claims
 *   - Israeli State and Security Apparatus: Institutional beneficiary (institutional/arbitrage) — benefits from territorial control, demographic majority preservation, and military enforcement of boundaries; has arbitrage option (can modify policy unilaterally but chooses not to)
 *   - Palestinian Authority and State-Building Institutions: Secondary actor (organized/constrained) — builds Palestinian institutions within constraints imposed by Oslo framework; coordinates diaspora and occupied population around legitimacy claims; constrained by lack of full sovereignty
 *   - International Palestinian Diaspora and Civil Society: Organized secondary actor (organized/constrained) — maintains cultural identity, documents historical claims, mobilizes international support; constrained by geopolitical isolation and recognition gaps
 *   - UN and International Legal Institutions: Institutional actor (institutional/arbitrage) — formally recognize Palestinian statehood and right of return (Resolution 194) but lack enforcement mechanisms; maintain piton-like structure (performative institutional recognition without material enforcement)
 *   - Analytical Observer: Neutral observer (analytical/analytical) — risks naturalizing the constraint as immutable feature of territorial state formation rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.68).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.78).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Palestinian Legitimacy and Right of Return (Autochthony Reading)").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political_theory/territorial_sovereignty/international_relations").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, '2e2afe5f-66c7-4317-bbe6-edcae69ee334').
narrative_ontology:cs_kernel_codification('2e2afe5f-66c7-4317-bbe6-edcae69ee334', distributed).
narrative_ontology:cs_authority_grounding('2e2afe5f-66c7-4317-bbe6-edcae69ee334', distributed).
narrative_ontology:cs_reading_relation('2e2afe5f-66c7-4317-bbe6-edcae69ee334', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('2e2afe5f-66c7-4317-bbe6-edcae69ee334', territorial_legitimacy_dual__two_state_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('2e2afe5f-66c7-4317-bbe6-edcae69ee334', foundational, continuous_habitation_establishes_autochthony).
narrative_ontology:cs_axiom_status(continuous_habitation_establishes_autochthony, holdable).
narrative_ontology:cs_axiom_grounding('2e2afe5f-66c7-4317-bbe6-edcae69ee334', continuous_habitation_establishes_autochthony, deontological).
narrative_ontology:cs_axiom('2e2afe5f-66c7-4317-bbe6-edcae69ee334', foundational, displacement_as_ongoing_injustice_not_historical_tragedy).
narrative_ontology:cs_axiom_status(displacement_as_ongoing_injustice_not_historical_tragedy, holdable).
narrative_ontology:cs_axiom_grounding('2e2afe5f-66c7-4317-bbe6-edcae69ee334', displacement_as_ongoing_injustice_not_historical_tragedy, deontological).
narrative_ontology:cs_created_at('2e2afe5f-66c7-4317-bbe6-edcae69ee334', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_population).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, displaced_refugee_diaspora).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, dispossessed_land_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REFUGEE FAMILY — From the perspective of a Palestinian family displaced in 1948 and their descendants, the constraint is pure extraction without exit. Territorial dispossession is irreversible within their lifetime; legal barriers prevent return; demographic and military power asymmetry preclude unilateral restoration. The extraction compounds across generations — displacement trauma becomes inherited identity. Powerless, trapped, no alternatives.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__palestinian_autochthony_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PALESTINIAN CIVIL SOCIETY — Moderate power through international advocacy, documentation, and institutional presence (UN recognition, human rights documentation). Constrained by resource dependence on international donors, legal restrictions on organizing in occupied territories, and military domination. Experience mixed coordination (building Palestinian institutions, preserving cultural identity) and extraction (labor restrictions, movement controls, political subordination). Tangled rope: genuine coordination function alongside asymmetric coercion.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__palestinian_autochthony_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNATIONAL DIASPORA NETWORK — Organized actors (diaspora communities, Palestinian Authority, international advocacy groups) experience the constraint as coordination of dispersed populations around shared legitimacy claim. The dispersal itself is extractive, but the coordination function is genuine: maintaining cultural identity, mobilizing international support, documenting historical claims. Constrained by geopolitical isolation and recognition gaps. Perspective sees more rope-like features (coordination) than snare, though extraction persists.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__palestinian_autochthony_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PALESTINIAN AUTHORITY — State-building institutions (PA governance, security forces, educational systems) experience the constraint as mixed coordination and extraction. Building institutions requires acceptance of territorial and political constraints (Oslo framework, limitations on sovereignty). Coordination function is genuine (providing services, maintaining law); extraction is the structural subordination to external authority and military domination. Constrained power with partial institutional authority but without full sovereign control.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__palestinian_autochthony_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ISRAELI STATE — From the Israeli institutional perspective, the constraint appears as rope or even scaffold (a coordination mechanism for security, territorial stability, and Jewish majority preservation). However, this perspective's classification contains an embedded ethical paradox: the extraction the powerful agent perceives as necessary security (suppression of Palestinian return claims, military enforcement of territorial boundaries) is the snare mechanism experienced by the powerless. Israeli power to control the constraint mechanism inverts the classification — not due to different structural data, but due to power asymmetry. The Israeli beneficiary sees coordination; the Palestinian victim sees extraction.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__palestinian_autochthony_reading, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: UN AND INTERNATIONAL LEGAL INSTITUTIONS — UN resolutions affirming Palestinian right of return (Resolution 194) and Palestinian self-determination exist but lack enforcement mechanisms. The international legal apparatus is largely performative: resolutions pass but occupation continues; international courts lack jurisdiction; enforcement depends on state consent. Theater ratio high (repeated declarations without implementation). The constraint persists through institutional inertia — the UN framework maintains the legitimacy claim as abstract entitlement while material enforcement remains with the powerful state. Piton: degraded institutional structure maintaining a formal norm without functional power to enforce.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__palestinian_autochthony_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — From a civilizational analytical stance, territorial displacement might appear as an immutable feature of state formation (empires and nations are constituted through historical displacement; all modern states rest on conquered or annexed land). This view naturalizes the Palestinian condition as inevitable historical cost. However, the false summit detection algorithm will trigger: the constraint exhibits intentional suppression mechanisms, identifiable beneficiaries (Israeli state), and victim populations organized enough to articulate claims. The 'natural law' framing masks what is a contingent institutional arrangement backed by military enforcement.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__palestinian_autochthony_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(territorial_legitimacy_dual__palestinian_autochthony_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(territorial_legitimacy_dual__palestinian_autochthony_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, TR),
    TR >= 0.70.

:- end_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68, high): The constraint extracts significant value from Palestinian populations through dispossession (land, property, political sovereignty), while concentrating control and benefit in the Israeli state. The extraction includes: (1) historical land takeover (1948, 1967); (2) ongoing restriction of return (denying access to original homes and land); (3) territorial confinement and reduced resource access; (4) political subordination without full sovereignty. However, extractiveness is not at maximum (0.85+) because Palestinian institutions retain partial coordination functions (PA governance, cultural preservation, international advocacy), and there remain micro-scale cooperative arrangements. Suppression (0.78, high): Suppression mechanisms are structural and intensifying. Barriers include: (1) military enforcement of occupation; (2) legal framework (property law, citizenship rules) that bars return and restitution; (3) demographic dominance (Israeli Jewish population majority makes Palestinian majority restoration impossible); (4) international recognition gaps (UN resolutions lack enforcement); (5) practical barriers (destroyed homes, absent documentation, resource scarcity). Suppression increased slightly from 1948 (0.75) to 1993 (0.82) and stabilized, reflecting maturation of legal and administrative enforcement infrastructure. Theater ratio (0.55, moderate): The constraint has a genuine grievance component (documented displacement, historical records, property claims) but also relies on performative institutional structures (UN resolutions without enforcement, international law frameworks that exist but lack mechanisms). The theater has increased over time (0.40 in 1948 to 0.55 in 2024) as the constraint moved from raw military enforcement to institutionalized legal structures. Claimed type: SNARE. The constraint meets snare thresholds: extractiveness ≥ 0.46 (0.68), suppression ≥ 0.60 (0.78), and suppression ≥ 0.60 (0.78). No meaningful coordination function exists from the victim's perspective — the constraint exists primarily to extract and maintain subordination. The Israeli beneficiary perspective perceives coordination (security, stability), but the extraction direction is objectively from Palestinian to Israeli agents.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence across the two primary agents. From the refugee and trapped Palestinian population perspective (powerless/trapped), the constraint is pure snare: irreversible dispossession enforced through military and legal mechanisms with no exit. From the Israeli institutional perspective (institutional/arbitrage), the constraint is perceived as rope or even scaffold: a coordination mechanism for security, stability, and demographic preservation. The same structural phenomenon — territorial boundaries maintained through suppression of return claims — appears as extraction to the powerless and as coordination/safety to the powerful. The Palestinian diaspora and civil society perspectives fall between these poles, experiencing tangled rope: genuine coordination functions (identity preservation, institution building) alongside extraction (resource constraints, geopolitical isolation). The piton perspective (UN institutions) observes that the constraint persists through performative institutional structures: resolutions affirm Palestinian rights while enforcement depends on state consent. The analytical observer risks a false summit: naturalizing the constraint as an immutable feature of territorial state formation rather than recognizing it as a contingent institutional arrangement backed by ongoing enforcement. The perspectival gap is not bridgeable by additional data — it reflects fundamental power asymmetry: the beneficiary controls the mechanism and can perceive it as coordination; the victim bears the costs and perceives extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position: power level + exit options + beneficiary/victim status. REFUGEES (powerless/trapped/victim): d ≈ 0.95 (maximum target). No exit option; zero alternatives; full extraction experienced. f(d) ≈ 1.42 (maximum experienced extractiveness). PALESTINIAN CIVIL SOCIETY (moderate/constrained/victim): d ≈ 0.65 (mixed). Some organizing capacity; some international support; constrained by resource dependence and military domination. f(d) ≈ 1.00. PALESTINIAN DIASPORA (organized/constrained/victim): d ≈ 0.55 (moderate target). Organized collective; international advocacy networks; constrained by recognition gaps. f(d) ≈ 0.75. PALESTINIAN AUTHORITY (institutional/constrained/mixed): d ≈ 0.50 (symmetric). Partial institutional authority; constrained by lack of full sovereignty; some coordination functions but subordinated. f(d) ≈ 0.65. ISRAELI STATE (institutional/arbitrage/beneficiary): d ≈ 0.05 (beneficiary). Benefits from territorial control and demographic dominance; has unilateral policy options (arbitrage). f(d) ≈ -0.12 (negative/low effective extraction from their perspective — they perceive gains). Directionality inversion: The Israeli perspective sees the constraint as rope or scaffold (coordination for security); the Palestinian perspective sees it as snare. This is not a measurement error but a structural property: power asymmetry inverts the classification. The beneficiary's coordination becomes the victim's snare.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolved via SNARE confirmation. The constraint meets all snare thresholds (extractiveness 0.68 ≥ 0.46, suppression 0.78 ≥ 0.60, χ > 0.66 at victim perspectives). The classification prevents misidentification as coordination (rope) or compromise (two-state coexistence reading). The snare classification is perspectival — from the Israeli beneficiary view, the constraint could appear as rope or scaffold. But the mandatrophy resolution establishes that from the victim's perspective (powerless/trapped Palestinian refugees and populations), the constraint is unambiguously snare: high extraction, suppression exceeds 0.60, no meaningful exit, no genuine coordination function from victim's structural position. The analytical observer's false summit risk is mitigated by declaring the natural-law perspective and flagging its omega uncertainty. The constraint is not immutable; it persists through identifiable enforcement mechanisms (military, legal, demographic) that are subject to political change. Mandatrophy is fully resolved: the snare classification stands across all victim perspectives, distinguishing this from tangled rope (where some coordination is present from victim's view) and from scaffold (which has sunset mechanisms that this constraint lacks).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    right_of_return_material_feasibility,
    'Is literal implementation of unlimited right of return materially feasible given demographic composition, property law complexity, and state security concerns?',
    'Scenario modeling: cost-benefit analysis of various return frameworks (full literal return vs compensation vs limited return corridors); international precedents (refugee return in Balkans, Rwanda); demographic stability projections with different return scenarios',
    'If feasible: right of return is negotiable policy outcome, shifting constraint toward tangled_rope. If infeasible: right of return becomes aspirational claim, entrench snare classification and require focus on interim remedies (compensation, limited return, recognition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_material_feasibility, empirical, 'Feasibility of literal right of return implementation').

omega_variable(
    autochthony_claim_historical_grounding,
    'Does continuous Palestinian habitation and cultural presence constitute a stronger territorial legitimacy claim than Zionist historical and religious connection?',
    'Archaeological evidence, demographic records, Ottoman and British Mandate documentation; comparative historical analysis of competing autochthony claims; international law precedent on territorial legitimacy (recency of habitation vs historical occupation vs cultural continuity)',
    'This question is empirically contingent but fundamentally conceptual: different legal traditions and moral frameworks weight evidence differently. Palestinian autochthony reading asserts that continuous presence outweighs historical connection; zionist reading reverses the weighting. No amount of empirical data resolves the normative prioritization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autochthony_claim_historical_grounding, conceptual, 'Comparative weight of autochthony vs historical connection in legitimacy grounding').

omega_variable(
    displacement_trauma_intergenerational_persistence,
    'Does trauma from 1948 displacement constitute a continuously binding obligation on successor states, or does its normative force diminish across generations?',
    'Comparative analysis of historical displacement remedy frameworks (Native American reparations, Holocaust restitution, post-apartheid South Africa); psychological studies on transgenerational trauma transmission; legal precedent on statute of limitations for territorial claims; moral philosophy debate on descendant liability for historical injustice',
    'If trauma persists as binding: right of return and compensation remain non-negotiable. If force diminishes: transition toward two-state coexistence reading becomes more structurally viable. This distinction tracks the generational axis: at T=immediate, trauma binding is strongest; at T=civilizational, it may be weaker.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displacement_trauma_intergenerational_persistence, preference, 'Intergenerational persistence of displacement trauma as binding obligation').

omega_variable(
    israeli_state_legitimacy_contestation,
    'Can Palestinian autochthony claim coexist with Israeli state legitimacy, or does assertion of Palestinian right of return functionally foreclose Israeli Jewish majority preservation?',
    'This is the core structural clash between this reading and the two-state coexistence reading. Test via scenario: if unlimited right of return is implemented, does Israeli Jewish character persist? If not, does Palestinian autochthony reading require Israeli state delegitimization as logical consequence?',
    'If coexistence is possible: constraint moves toward tangled_rope (hybrid coordination and extraction). If the claims foreclose each other: constraint remains snare from Palestinian perspective and snare/scaffold from Israeli perspective, but now understood as zero-sum. This determines whether a negotiated settlement preserving both legitimacies is structurally viable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(israeli_state_legitimacy_contestation, conceptual, 'Logical compatibility of Palestinian right of return with Israeli state legitimacy').

omega_variable(
    suppression_mechanism_military_vs_institutional,
    'Is the suppression of Palestinian territorial claims and return primarily a military enforcement mechanism, or is it embedded in international legal institutions that formally recognize Palestinian statehood while materially constraining it?',
    'Institutional analysis: compare actual constraints imposed by military occupation vs constraints imposed by international law framework (Oslo Accords, UN recognition without enforcement); track whether suppression persists if military occupation were withdrawn',
    'If primarily military: constraint would weaken with military disengagement. If institutional: suppression persists through legal frameworks even under reduced military presence, suggesting a more durable extraction mechanism. This affects the feasibility of the scaffold perspective (temporary enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_military_vs_institutional, empirical, 'Relative weight of military vs institutional suppression mechanisms').

omega_variable(
    one_reading_of_contested_kernel,
    'Is this reading (Palestinian autochthony grounding legitimacy in continuous habitation and displacement trauma) the only defensible reading of territorial legitimacy, or is it one among several live competing readings?',
    'The kernel context names three readings: palestinian_autochthony_reading (this one), zionist_refuge_reading, and two_state_coexistence_reading. All three are held by coherent actors with historical and normative grounding. No single reading can be derived from ''the facts alone'' — each reading selects which facts are salient (autochthony emphasizes continuous presence; Zionism emphasizes historical and religious connection; coexistence reading emphasizes both parties'' mutual recognition).',
    'Acknowledging this as one reading among live alternatives opens the analytical space to study how readings coexist, influence each other, and compete for institutional adoption. This reading is not ''false'' — it is structurally complete and coherent. But it is also not uniquely true. The constraint story models the snare from this specific reading''s perspective; other readings produce different classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(one_reading_of_contested_kernel, conceptual, 'This constraint as one reading of a contested kernel vs universal truth claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_legit_pal_theater_1948, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(terr_legit_pal_theater_1973, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(terr_legit_pal_theater_1993, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 45, 0.58).
narrative_ontology:measurement(terr_legit_pal_theater_2024, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 75, 0.55).

% Extraction over time
narrative_ontology:measurement(terr_legit_pal_extract_1948, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(terr_legit_pal_extract_1973, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(terr_legit_pal_extract_1993, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 45, 0.68).
narrative_ontology:measurement(terr_legit_pal_extract_2024, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 75, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(terr_legit_pal_suppress_1948, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(terr_legit_pal_suppress_1973, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 25, 0.8).
narrative_ontology:measurement(terr_legit_pal_suppress_1993, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 45, 0.82).
narrative_ontology:measurement(terr_legit_pal_suppress_2024, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 75, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a kernel family: territorial_legitimacy_dual. The kernel is a contested claim about what grounds territorial legitimacy in the Israeli-Palestinian context. Three readings instantiate three structurally distinct constraints with different epsilon values and beneficiary/victim structures: (1) palestinian_autochthony_reading (this file, ε=0.68, snare from Palestinian victim perspective); (2) zionist_refuge_reading (separate file, ε=?, rope/scaffold from Israeli beneficiary perspective); (3) two_state_coexistence_reading (separate file, ε=?, tangled rope from both-parties-organize perspective). All three readings are live and coherent. They are NOT alternative measurements of a single constraint; they are competing normative readings of a shared institutional kernel. The ε values differ because the readings select different facts as salient (autochthony emphasizes continuous presence and dispossession harm; Zionism emphasizes historical connection and persecution; coexistence reading emphasizes mutual recognition and compromise). Each reading produces its own classification presheaf. The network links document their mutual influence: this reading's assertion of Palestinian right of return influences the zionist reading's emphasis on refugee security; both readings influence the coexistence reading's compromise framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
